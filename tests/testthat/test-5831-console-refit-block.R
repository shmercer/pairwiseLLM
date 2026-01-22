testthat::test_that("refit progress block includes EAP reliability", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(progress = TRUE, progress_every_refit = 1L, progress_level = "refit")
  )
  state$phase <- "phase2"
  state$checks_passed_in_row <- 1L

  round_row <- tibble::tibble(
    round_id = 1L,
    iter_at_refit = 1L,
    divergences = 0L,
    max_rhat = 1.01,
    min_ess_bulk = 1200,
    epsilon_mean = 0.09,
    reliability_EAP = 0.87,
    diagnostics_pass = TRUE,
    theta_sd_median = 0.48,
    tau = 0.80,
    theta_sd_pass = TRUE,
    U0 = 0.0012,
    U_abs = 0.0024,
    U_pass = TRUE,
    rank_stability_pass = TRUE,
    frac_weak_adj = 0.03,
    min_adj_prob = 0.72,
    n_unique_pairs_seen = 3L,
    hard_cap_threshold = 12L,
    hard_cap_reached = FALSE,
    mcmc_chains = 2L,
    mcmc_parallel_chains = 2L,
    mcmc_cores_detected_physical = 4L,
    mcmc_cores_detected_logical = 8L,
    mcmc_core_fraction = 0.6
  )

  out <- capture.output({
    pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row)
  })

  testthat::expect_true(any(grepl("rel_EAP", out)))
})
