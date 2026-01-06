testthat::test_that("print.pairwiseLLM_run() does not error and prints key fields", {
  run <- list(
    results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
    theta = tibble::tibble(ID = c("A", "B"), theta = c(0.5, -0.5)),
    estimates = NULL,
    pairing_diagnostics = NULL,
    theta_engine = "mock",
    stop_reason = "max_rounds",
    stop_round = 3L
  )
  attr(run, "run_type") <- "adaptive"
  class(run) <- "pairwiseLLM_run"

  out <- testthat::capture_output(print(run))
  testthat::expect_true(any(grepl("type:", out, fixed = TRUE)))
  testthat::expect_true(any(grepl("results:", out, fixed = TRUE)))
  testthat::expect_true(any(grepl("stop:", out, fixed = TRUE)))
  testthat::expect_true(any(grepl("theta engine:", out, fixed = TRUE)))
  testthat::expect_true(any(grepl("top theta:", out, fixed = TRUE)))
})


testthat::test_that("print.pairwiseLLM_run() works for linking-style runs without theta", {
  run <- list(
    results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
    theta = NULL,
    estimates = NULL,
    pairing_diagnostics = NULL,
    theta_engine = "mock",
    stop_reason = "blocked",
    stop_round = 1L
  )
  attr(run, "run_type") <- "core_linking"
  class(run) <- "pairwiseLLM_run"

  out <- testthat::capture_output(print(run))
  testthat::expect_true(any(grepl("type:", out, fixed = TRUE)))
  testthat::expect_true(any(grepl("theta engine:", out, fixed = TRUE)))
  # No theta -> no "top theta" line (but print should still succeed)
  testthat::expect_false(any(grepl("top theta:", out, fixed = TRUE)))
})
