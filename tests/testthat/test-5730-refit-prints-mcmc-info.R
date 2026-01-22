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

  round_row <- tibble::tibble(
    round_id = 1L,
    iter_at_refit = 1L,
    mcmc_chains = 4L,
    mcmc_parallel_chains = 2L,
    mcmc_cores_detected_physical = 8L,
    mcmc_cores_detected_logical = 16L,
    mcmc_core_fraction = 0.5
  )

  out <- capture.output({
    pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row)
  })
  text <- paste(out, collapse = "\n")

  testthat::expect_true(grepl("chains", text))
  testthat::expect_true(grepl("parallel", text))
  testthat::expect_true(grepl("\\d+", text))
})
