testthat::test_that("fit_bayes_btl_mcmc validates cmdstan output_dir", {
  results <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2026-01-02 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test"
  )

  expect_error(
    pairwiseLLM:::fit_bayes_btl_mcmc(
      results,
      ids = c("A", "B"),
      cmdstan = list(output_dir = c("a", "b"))
    ),
    "length-1 character path"
  )
})
