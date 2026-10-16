test_that("interruption after a durable response never resubmits that occurrence", {
  skip_if_not_installed("filelock")
  decision_bulk_clock()
  td <- withr::local_tempdir(); path <- file.path(td, "resume.csv")
  calls <- character()
  commit <- pairwiseLLM:::.decision_checkpoint_commit
  interrupt <- TRUE
  local_mocked_bindings(.decision_sync_submit = function(req, spec) {
    calls <<- c(calls, spec$request$pair_uid); decision_bulk_response()
  }, .decision_checkpoint_commit = function(journal, uid, state, include_raw) {
    commit(journal, uid, state, include_raw)
    if (interrupt && state$status == "completed") decision_bulk_interrupt()
  }, .package = "pairwiseLLM")
  args <- decision_bulk_args(save_path = path)
  interrupted <- tryCatch(do.call(submit_llm_pairs, args), interrupt = identity)
  expect_s3_class(interrupted, "interrupt")
  expect_identical(calls, "occurrence-1")
  interrupt <- FALSE
  out <- do.call(submit_llm_pairs, args)
  expect_identical(calls, decision_bulk_pairs()$pair_uid)
  expect_equal(nrow(out$results), 4L)
  expect_equal(nrow(utils::read.csv(path)), 4L)
})

test_that("ambiguous recovery permits one reissue across all restarts and retains lineage", {
  skip_if_not_installed("filelock")
  decision_bulk_clock()
  td <- withr::local_tempdir(); path <- file.path(td, "ambiguous.csv")
  calls <- 0L; bytes <- list()
  local_mocked_bindings(.decision_sync_submit = function(req, spec) {
    calls <<- calls + 1L; bytes[[calls]] <<- req$body$data
    decision_bulk_interrupt()
  }, .package = "pairwiseLLM")
  args <- decision_bulk_args(pairs = decision_bulk_pairs(1), save_path = path)
  for (restart in 1:2) {
    expect_s3_class(tryCatch(do.call(submit_llm_pairs, args), interrupt = identity), "interrupt")
  }
  out <- do.call(submit_llm_pairs, args)
  expect_identical(calls, 2L)
  expect_identical(bytes[[1]], bytes[[2]])
  expect_equal(nrow(out$results), 0L)
  expect_equal(nrow(out$failed_pairs), 1L)
  expect_identical(out$failed_pairs$ambiguous_reissues, 1L)
  expect_identical(out$failed_pairs$recovery_status, "ambiguous")
  expect_identical(out$failed_attempts$attempt_reason, c("initial", "ambiguous_recovery"))
  expect_identical(out$failed_attempts$reconciliation_status, rep("unsupported", 2))
  expect_identical(out$failed_attempts$attempt_id, paste0("occurrence-1/attempt-", 1:2))
  expect_true(all(out$failed_attempts$possible_duplicate_charge))
  expect_identical(do.call(submit_llm_pairs, args)$failed_attempts$attempt_id, out$failed_attempts$attempt_id)
  expect_identical(calls, 2L)
})

test_that("ambiguity opt-out preserves the allowance and recovery success commits once", {
  skip_if_not_installed("filelock")
  decision_bulk_clock()
  td <- withr::local_tempdir(); path <- file.path(td, "opt-out.csv")
  calls <- 0L; interrupt <- TRUE
  local_mocked_bindings(.decision_sync_submit = function(req, spec) {
    calls <<- calls + 1L
    if (interrupt) decision_bulk_interrupt()
    decision_bulk_response()
  }, .package = "pairwiseLLM")
  args <- decision_bulk_args(pairs = decision_bulk_pairs(1), save_path = path)
  tryCatch(do.call(submit_llm_pairs, args), interrupt = function(e) NULL)
  opted <- args; opted$execution_control <- list(retry_ambiguous = FALSE)
  out <- do.call(submit_llm_pairs, opted)
  expect_identical(calls, 1L)
  expect_equal(nrow(out$failed_pairs), 1L)
  interrupt <- FALSE
  resumed <- do.call(submit_llm_pairs, args)
  expect_identical(calls, 2L)
  expect_equal(nrow(resumed$results), 1L)
  expect_true(resumed$results$possible_duplicate_charge)
  expect_equal(nrow(resumed$failed_attempts), 1L)
  expect_equal(nrow(do.call(submit_llm_pairs, args)$results), 1L)
  expect_identical(calls, 2L)
})

test_that("transient retry budgets and admission windows persist across restarts", {
  skip_if_not_installed("filelock")
  clock <- decision_bulk_clock()
  td <- withr::local_tempdir(); path <- file.path(td, "transient.csv")
  calls <- numeric(); interrupt <- TRUE
  local_mocked_bindings(.decision_sync_submit = function(req, spec) {
    calls <<- c(calls, clock$now); decision_bulk_response(status = 503L)
  }, .decision_live_wait = function(seconds) {
    if (interrupt) decision_bulk_interrupt()
    clock$now <- clock$now + seconds
  }, .package = "pairwiseLLM")
  args <- decision_bulk_args(pairs = decision_bulk_pairs(1), save_path = path,
                             execution_control = list(requests_per_minute = 1))
  expect_s3_class(tryCatch(do.call(submit_llm_pairs, args), interrupt = identity), "interrupt")
  expect_identical(calls, 1000)
  interrupt <- FALSE
  out <- do.call(submit_llm_pairs, args)
  expect_identical(calls, c(1000, 1060, 1120))
  expect_equal(nrow(out$failed_attempts), 3L)
  expect_equal(nrow(out$failed_pairs), 1L)
  do.call(submit_llm_pairs, args)
  expect_length(calls, 3L)
  changed <- args; changed$execution_control$max_attempts <- 4L
  expect_error(do.call(submit_llm_pairs, changed), "persistent attempt budget")
})

test_that("mutated scientific bodies and attached retry bytes fail without provider calls", {
  skip_if_not_installed("filelock")
  decision_bulk_clock()
  td <- withr::local_tempdir(); path <- file.path(td, "integrity.csv")
  calls <- 0L
  local_mocked_bindings(.decision_sync_submit = function(req, spec) {
    calls <<- calls + 1L; decision_bulk_response()
  }, .package = "pairwiseLLM")
  args <- decision_bulk_args(pairs = decision_bulk_pairs(1), save_path = path)
  do.call(submit_llm_pairs, args)
  build <- pairwiseLLM:::.build_decision_request
  local_mocked_bindings(.build_decision_request = function(...) {
    request <- build(...)
    request$body$input <- '"mutated sample with unchanged fingerprint"'
    request
  }, .package = "pairwiseLLM")
  expect_error(do.call(submit_llm_pairs, args), class = "pairwiseLLM_decision_request_error")
  expect_identical(calls, 1L)
})

test_that("asynchronous interruption preserves per-occurrence recovery budgets and wire identity", {
  skip_if_not_installed("filelock"); skip_if_not_installed("promises"); skip_if_not_installed("later")
  decision_bulk_clock()
  td <- withr::local_tempdir(); path <- file.path(td, "async-interrupted.csv")
  first <- resumed <- list()
  local_mocked_bindings(.decision_async_submit = function(req, spec, pool, success, failure) {
    first[[spec$request$pair_uid]] <<- req$body$data
    promises::promise(function(resolve, reject) NULL)
  }, .decision_async_wait = function(seconds) decision_bulk_interrupt(),
  .decision_sync_submit = function(req, spec) {
    resumed[[spec$request$pair_uid]] <<- req$body$data
    decision_bulk_response()
  }, .package = "pairwiseLLM")
  args <- decision_bulk_args(save_path = path, parallel = TRUE, workers = 2)
  expect_s3_class(tryCatch(do.call(submit_llm_pairs, args), interrupt = identity), "interrupt")
  expect_length(first, 2L)
  args$parallel <- FALSE
  out <- do.call(submit_llm_pairs, args)
  expect_equal(nrow(out$results), 4L)
  expect_identical(out$results$ambiguous_reissues, c(1L, 1L, 0L, 0L))
  expect_equal(nrow(out$failed_attempts), 2L)
  for (uid in names(first)) expect_identical(first[[uid]], resumed[[uid]])
})

test_that("a recomputed checkpoint checksum cannot bypass scientific or response validation", {
  skip_if_not_installed("filelock")
  decision_bulk_clock()
  td <- withr::local_tempdir(); path <- file.path(td, "forged.csv")
  calls <- 0L
  local_mocked_bindings(.decision_sync_submit = function(req, spec) {
    calls <<- calls + 1L; decision_bulk_response()
  }, .package = "pairwiseLLM")
  args <- decision_bulk_args(pairs = decision_bulk_pairs(1), save_path = path)
  do.call(submit_llm_pairs, args)
  final <- file.path(paste0(path, ".checkpoint"), "000000000002.rds")
  original <- pairwiseLLM:::.decision_record_read(final)$value
  for (change in c("wire_body_sha256", "p_sample_1", "request_provenance_json")) {
    event <- original
    event$state$row[[change]] <- if (change == "p_sample_1") .2 else "changed"
    unlink(final)
    pairwiseLLM:::.decision_record_write(event, final)
    expect_error(do.call(submit_llm_pairs, args), class = "pairwiseLLM_decision_checkpoint_error")
  }
  expect_identical(calls, 1L)
})
