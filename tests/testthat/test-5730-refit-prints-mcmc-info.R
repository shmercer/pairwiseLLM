testthat::test_that("refit progress block prints MCMC config info", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(progress = TRUE, progress_every_refit = 1L, progress_level = "refit")
  )

  round_row <- pairwiseLLM:::.adaptive_round_log_defaults()
  round_row$round_id <- 1L
  round_row$iter_at_refit <- 1L
  round_row$mcmc_chains <- 4L
  round_row$mcmc_parallel_chains <- 2L
  round_row$mcmc_cores_detected_physical <- 8L
  round_row$mcmc_cores_detected_logical <- 16L
  round_row$mcmc_core_fraction <- 0.5

  out <- capture.output({
    pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row)
  })
  text <- paste(out, collapse = "\n")

  testthat::expect_true(grepl("chains", text))
  testthat::expect_true(grepl("parallel", text))
  testthat::expect_true(grepl("\\d+", text))
})
