test_that("OS locks prevent simultaneous saved sessions and release after normal exit or process death", {
  skip_if_not_installed("filelock")
  skip_if_no_psock()
  td <- withr::local_tempdir(); path <- file.path(td, "locked.csv")
  cluster <- parallel::makePSOCKcluster(1, outfile = file.path(td, "child.log"))
  withr::defer(try(parallel::stopCluster(cluster), silent = TRUE))
  held <- parallel::clusterCall(cluster, function(path) {
    assign("decision_test_lock", filelock::lock(path, timeout = 0), .GlobalEnv)
    Sys.getpid()
  }, paste0(path, ".lock"))[[1L]]
  calls <- 0L
  local_mocked_bindings(.decision_sync_submit = function(req, spec) {
    calls <<- calls + 1L; decision_bulk_response()
  }, .package = "pairwiseLLM")
  args <- decision_bulk_args(pairs = decision_bulk_pairs(1), save_path = path,
                             execution_control = list(lock_timeout_ms = 1))
  expect_error(do.call(submit_llm_pairs, args), "bounded lock wait expired")
  expect_identical(calls, 0L)
  expect_false(dir.exists(paste0(path, ".checkpoint")))
  parallel::clusterCall(cluster, function() filelock::unlock(get("decision_test_lock", .GlobalEnv)))
  expect_equal(nrow(do.call(submit_llm_pairs, args)$results), 1L)
  parallel::clusterCall(cluster, function(path) {
    assign("decision_test_lock", filelock::lock(path, timeout = 0), .GlobalEnv)
    NULL
  }, paste0(path, ".lock"))
  expect_error(do.call(submit_llm_pairs, args), "checkpoint lock")
  # Terminate only the test-owned child. This is not a signal-0 liveness probe.
  expect_true(tools::pskill(held, tools::SIGTERM))
  args$execution_control$lock_timeout_ms <- 5000
  expect_equal(nrow(do.call(submit_llm_pairs, args)$results), 1L)
  expect_identical(calls, 1L)
  expect_true(file.exists(paste0(path, ".lock")))
})

test_that("errors and interruptions release the saved-run OS lock", {
  skip_if_not_installed("filelock")
  td <- withr::local_tempdir(); path <- file.path(td, "released.csv")
  local_mocked_bindings(.decision_sync_submit = function(req, spec) decision_bulk_interrupt(), .package = "pairwiseLLM")
  args <- decision_bulk_args(pairs = decision_bulk_pairs(1), save_path = path)
  expect_s3_class(tryCatch(do.call(submit_llm_pairs, args), interrupt = identity), "interrupt")
  lock <- filelock::lock(paste0(path, ".lock"), timeout = 0)
  expect_false(is.null(lock))
  filelock::unlock(lock)
  args$trait_description <- "Mismatch"
  expect_error(do.call(submit_llm_pairs, args), class = "pairwiseLLM_decision_checkpoint_error")
  lock <- filelock::lock(paste0(path, ".lock"), timeout = 0)
  expect_false(is.null(lock))
  filelock::unlock(lock)
})

test_that("a process killed after reservation resumes under the same persistent budget", {
  skip_if_not_installed("filelock"); skip_if_no_psock()
  decision_bulk_clock()
  td <- withr::local_tempdir(); path <- file.path(td, "killed.csv")
  cluster <- parallel::makePSOCKcluster(1, outfile = file.path(td, "killed-child.log"))
  withr::defer(try(parallel::stopCluster(cluster), silent = TRUE))
  source_path <- normalizePath(test_path("..", ".."), winslash = "/")
  # Only this test-owned child terminates; the provider operation is replaced.
  result <- tryCatch(parallel::clusterCall(cluster, function(root, libs, path) {
    .libPaths(libs)
    if (file.exists(file.path(root, "R", "decision_bulk.R"))) pkgload::load_all(root, quiet = TRUE) else library(pairwiseLLM)
    testthat::with_mocked_bindings({
      pairwiseLLM::submit_llm_pairs(
        data.frame(ID1="A", text1="Synthetic sample A.", ID2="B", text2="Synthetic sample B.", pair_uid="occurrence-1"),
        model="gpt-6-luna", endpoint="decisions", trait_name="Organization",
        trait_description="Logical sequencing.", api_key="synthetic-bulk-key",
        verbose=FALSE, progress=FALSE, save_path=path)
    }, .decision_sync_submit = function(req, spec) {
      tools::pskill(Sys.getpid(), tools::SIGTERM)
      stop("test-owned child survived termination")
    }, .package="pairwiseLLM")
  }, source_path, .libPaths(), path), error = identity)
  expect_s3_class(result, "error")
  expect_equal(length(decision_bulk_events(path)), 1L)
  local_mocked_bindings(.decision_sync_submit = function(req, spec) decision_bulk_response(), .package = "pairwiseLLM")
  out <- do.call(submit_llm_pairs, decision_bulk_args(pairs = decision_bulk_pairs(1), save_path = path))
  expect_equal(nrow(out$results), 1L)
  expect_identical(out$results$attempt_count, 2L)
  expect_identical(out$results$ambiguous_reissues, 1L)
  expect_identical(out$failed_attempts$reconciliation_status, "unsupported")
})
