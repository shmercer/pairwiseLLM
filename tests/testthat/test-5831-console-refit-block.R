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
    scheduled_pairs = 10L,
    completed_pairs = 8L,
    backlog_unjudged = 2L,
    divergences = 0L,
    max_rhat = 1.01,
    min_ess_bulk = 1200,
    epsilon_mean = 0.09,
    reliability_EAP = 0.87,
    diagnostics_pass = TRUE,
    theta_sd_eap = 0.48,
    rho_theta_lag = 0.99,
    delta_sd_theta_lag = 0.01,
    rho_rank_lag = 0.99,
    rank_stability_pass = TRUE,
    stop_passes = 1L,
    stop_eligible = TRUE,
    stop_decision = FALSE,
    stop_reason = NA_character_,
    n_unique_pairs_seen = 3L,
    hard_cap_threshold = 12L,
    hard_cap_reached = FALSE,
    mcmc_chains = 2L,
    mcmc_parallel_chains = 2L,
    mcmc_cores_detected_physical = 4L,
    mcmc_cores_detected_logical = 8L,
    mcmc_core_fraction = 0.8
  )

  out <- capture.output({
    pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row)
  })

  testthat::expect_true(any(grepl("Reliability:", out)))
})
