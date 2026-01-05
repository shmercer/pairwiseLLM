test_that("run contract: allows extra top-level fields", {
  x <- list(
    results = NULL,
    estimates = NULL,
    theta = NULL,
    theta_engine = NA_character_,
    fit_provenance = list(),
    stop_reason = NA_character_,
    stop_round = NA_integer_,
    pairing_diagnostics = NULL,
    extra_field = 123
  )

  expect_silent(pairwiseLLM:::validate_pairwise_run_output(x))
})
