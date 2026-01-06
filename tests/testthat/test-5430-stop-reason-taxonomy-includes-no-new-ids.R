test_that("5430-01 validate_pairwise_run_output recognizes no_new_ids as standard", {
  x <- list(
    results = tibble::tibble(),
    estimates = NULL,
    theta = tibble::tibble(
      ID = character(),
      theta = double(),
      se = double(),
      rank = integer()
    ),
    theta_engine = NA_character_,
    fit_provenance = list(),
    stop_reason = "no_new_ids",
    stop_round = 0L,
    pairing_diagnostics = NULL
  )

  expect_no_warning(pairwiseLLM:::validate_pairwise_run_output(x, strict = FALSE))
})
