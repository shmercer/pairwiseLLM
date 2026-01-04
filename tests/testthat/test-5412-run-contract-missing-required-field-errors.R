test_that("run contract: missing required fields error", {
  x <- list(
    results = NULL,
    estimates = NULL,
    theta = NULL,
    theta_engine = NA_character_,
    fit_provenance = list(),
    stop_reason = NA_character_,
    stop_round = NA_integer_,
    pairing_diagnostics = NULL
  )

  x_missing <- x
  x_missing$theta_engine <- NULL

  expect_error(pairwiseLLM:::validate_pairwise_run_output(x_missing))
})
