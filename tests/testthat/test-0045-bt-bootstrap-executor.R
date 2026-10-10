test_that("an ordered external executor preserves outputs and bounds preparation", {
  withr::local_seed(450L)
  prepare <- pairwiseLLM:::.bt_bootstrap_prepare
  preparation_count <- 0L
  testthat::local_mocked_bindings(.bt_bootstrap_prepare = function(spec) {
    preparation_count <<- preparation_count + 1L
    prepare(spec)
  }, .package = "pairwiseLLM")
  args <- list(object = c(a = .5, b = -.5), mode = "fixed", n_rep = 35L, seed = 325L,
    schedule = bootstrap_data(), estimator = "alpha", estimator_args = list(alpha = .5),
    keep = "theta")
  expected <- do.call(bootstrap_bt_model, args)
  expect_identical(preparation_count, 1L)
  preparation_count <- 0L
  before <- .Random.seed
  blocks <- list()
  executor <- function(X, FUN, ...) {
    blocks[[length(blocks) + 1L]] <<- X
    lapply(X, FUN, ...)
  }
  actual <- do.call(bootstrap_bt_model, c(args, list(workers = 2L, executor = executor)))
  expect_identical(actual, expected)
  expect_identical(.Random.seed, before)
  expect_identical(length(blocks), 3L)
  expect_identical(preparation_count, as.integer(sum(lengths(blocks))))
  expect_true(all(lengths(blocks) <= 2L))
  expect_true(all(lengths(unlist(blocks, recursive = FALSE)) <= 8L))
  expect_identical(unname(unlist(blocks)), seq_len(35L))

  args$keep <- "full"
  args$n_rep <- 7L
  blocks <- list()
  actual <- do.call(bootstrap_bt_model, c(args, list(workers = 2L, executor = executor)))
  expect_identical(actual, do.call(bootstrap_bt_model, args))
  expect_true(all(lengths(unlist(blocks, recursive = FALSE)) <= 2L))
})

test_that("external execution validates ordered results without retry or fallback", {
  args <- list(object = c(a = 0, b = 0), mode = "fixed", n_rep = 4L, seed = 325L,
    schedule = bootstrap_data(), estimator = "alpha", estimator_args = list(alpha = .5),
    workers = 2L)
  for (executor in list(1L, list())) {
    expect_error(do.call(bootstrap_bt_model, c(args, list(executor = executor))), "executor")
  }
  calls <- 0L
  executor <- function(X, FUN, ...) {
    calls <<- calls + 1L
    stop("executor transport failure")
  }
  expect_error(do.call(bootstrap_bt_model, c(args, list(executor = executor))), "transport failure")
  expect_identical(calls, 1L)
  for (executor in list(function(...) NULL, function(X, FUN, ...) rev(lapply(X, FUN, ...)),
                        function(X, FUN, ...) rep(lapply(X[1L], FUN, ...), length(X)),
                        function(X, FUN, ...) list(NULL, NULL),
                        function(X, FUN, ...) {
                          out <- lapply(X, FUN, ...)
                          out[[1L]][[1L]]$diagnostics$success <- FALSE
                          out
                        },
                        function(X, FUN, ...) {
                          out <- lapply(X, FUN, ...)
                          out[[1L]][[1L]]$diagnostics$selector <- NULL
                          out
                        })) {
    expect_error(do.call(bootstrap_bt_model, c(args, list(executor = executor))), "executor")
  }
})

test_that("a supplied executor needs no future dependency and runs with one worker", {
  require_ns <- pairwiseLLM:::.require_ns
  testthat::local_mocked_bindings(.require_ns = function(pkg) {
    if (pkg %in% c("future", "future.apply")) return(FALSE)
    require_ns(pkg)
  }, .package = "pairwiseLLM")
  args <- list(object = c(a = 0, b = 0), mode = "fixed", n_rep = 3L, seed = 325L,
    schedule = bootstrap_data(), estimator = "alpha", estimator_args = list(alpha = .5))
  expected <- do.call(bootstrap_bt_model, args)
  for (workers in c(1L, 2L)) {
    actual <- do.call(bootstrap_bt_model, c(args, list(workers = workers, executor = lapply)))
    expect_identical(actual, expected)
  }
  expect_error(do.call(bootstrap_bt_model, c(args, list(workers = 2L))), "future")
})

test_that("external execution preserves adaptive refits and failed-replicate evidence", {
  for (strategy in c("random", "trueskill_p50", "trueskill_pollitt", "hybrid")) {
    args <- bt_schedule_args_325(strategy)
    expected <- bt_result_325(bootstrap_bt_model, args)
    actual <- bt_result_325(bootstrap_bt_model, c(args, list(workers = 2L, executor = lapply)))
    expect_identical(bt_scientific_325(actual), bt_scientific_325(expected))
  }
  args$schedule_fit_fn <- function(...) stop("controlled native failure")
  expected <- bt_result_325(bootstrap_bt_model, args)
  actual <- bt_result_325(bootstrap_bt_model, c(args, list(workers = 2L, executor = lapply)))
  expect_identical(bt_scientific_325(actual), bt_scientific_325(expected))
})

test_that("a real caller-owned pool persists across conditions with all-policy parity", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  skip_if_no_psock()
  if (pkgload::is_dev_package("pairwiseLLM")) {
    lib <- withr::local_tempdir()
    log <- system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", "--no-byte-compile",
      "--no-test-load", "--no-docs", paste0("--library=", shQuote(lib)),
      shQuote(getNamespaceInfo("pairwiseLLM", "path"))), stdout = TRUE, stderr = TRUE)
    expect_null(attr(log, "status"), info = paste(log, collapse = "\n"))
    withr::local_libpaths(c(lib, .libPaths()))
  }
  old_plan <- future::plan("list")
  withr::defer(future::plan(old_plan))
  future::plan(future::multisession, workers = 2L)
  executor <- function(X, FUN, ...) future.apply::future_lapply(X, FUN, ..., future.seed = TRUE)
  pids <- function() sort(unlist(executor(1:2, function(i) Sys.getpid())))
  original_pids <- pids()
  withr::local_seed(325L)
  original_rng <- .Random.seed
  for (strategy in c("random", "trueskill_p50", "trueskill_pollitt", "hybrid")) {
    args <- bt_schedule_args_325(strategy, budget = 12L)
    serial <- do.call(bootstrap_bt_model, args)
    pooled <- do.call(bootstrap_bt_model, c(args, list(workers = 2L, executor = executor)))
    expect_identical(bt_scientific_325(pooled), bt_scientific_325(serial))
    expect_identical(.Random.seed, original_rng)
    expect_identical(pids(), original_pids)
    # The PID probe uses the future RNG mechanism outside the bootstrap call.
    original_rng <- .Random.seed
  }
})
