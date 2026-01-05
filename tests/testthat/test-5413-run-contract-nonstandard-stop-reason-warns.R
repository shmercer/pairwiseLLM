test_that("run contract: nonstandard stop_reason warns (not errors)", {
  x <- list(
    results = NULL,
    estimates = NULL,
    theta = NULL,
    theta_engine = NA_character_,
    fit_provenance = list(),
    stop_reason = "mystery_reason",
    stop_round = 1L,
    pairing_diagnostics = NULL
  )

  expect_warning(pairwiseLLM:::validate_pairwise_run_output(x))
})
