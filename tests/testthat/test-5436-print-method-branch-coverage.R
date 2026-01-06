testthat::test_that("5436-01 print.pairwiseLLM_run aborts on non-list inputs", {
  x <- 1
  class(x) <- "pairwiseLLM_run"
  testthat::expect_error(print(x), "`x` must be a list-like run object")
})

testthat::test_that("5436-02 print.pairwiseLLM_run handles bt_stop_summary errors + unknown fields", {
  run <- list(
    # results missing -> <unknown> results branch
    results = NULL,
    theta = NULL,
    estimates = NULL,
    pairing_diagnostics = NULL
  )
  class(run) <- "pairwiseLLM_run"

  testthat::local_mocked_bindings(
    bt_stop_summary = function(run) stop("boom"),
    .package = "pairwiseLLM"
  )

  out <- testthat::capture_output(print(run))

  testthat::expect_true(any(grepl("type:", out, fixed = TRUE)))
  testthat::expect_true(any(grepl("results:     <unknown>", out, fixed = TRUE)))
  testthat::expect_true(any(grepl("stop:        <unknown>", out, fixed = TRUE)))
  testthat::expect_true(any(grepl("theta engine: <unknown>", out, fixed = TRUE)))
})

testthat::test_that("5436-03 print.pairwiseLLM_run infers run type via heuristics", {
  run_acl <- list(
    bt_data = list(),
    results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
    theta = NULL,
    estimates = NULL,
    pairing_diagnostics = NULL,
    theta_engine = "bt",
    stop_reason = "max_rounds",
    stop_round = 1L
  )
  class(run_acl) <- "pairwiseLLM_run"

  out1 <- testthat::capture_output(print(run_acl))
  testthat::expect_true(any(grepl("adaptive_core_linking", out1, fixed = TRUE)))

  run_cl <- list(
    core_ids = c("A", "B"),
    batches = list(1L),
    results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
    theta = NULL,
    estimates = NULL,
    pairing_diagnostics = NULL,
    theta_engine = "bt",
    stop_reason = "blocked",
    stop_round = NA_integer_
  )
  class(run_cl) <- "pairwiseLLM_run"

  out2 <- testthat::capture_output(print(run_cl))
  testthat::expect_true(any(grepl("core_linking", out2, fixed = TRUE)))
})
