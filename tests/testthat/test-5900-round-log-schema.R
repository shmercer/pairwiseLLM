testthat::test_that("round log schema matches contract", {
  schema <- round_log_schema()

  expected <- c(
    "round_id",
    "iter_at_refit",
    "n_items",
    "total_pairs",
    "new_pairs",
    "batch_size",
    "window_W",
    "exploration_rate",
    "mean_degree",
    "min_degree",
    "pos_balance_sd",
    "gini_degree",
    "gini_pos_A",
    "epsilon_mean",
    "epsilon_ci90_lo",
    "epsilon_ci90_hi",
    "reliability_EAP",
    "theta_sd_median",
    "tau",
    "theta_sd_pass",
    "U0",
    "U_top_median",
    "U_abs",
    "U_pass",
    "U_dup_threshold",
    "hard_cap_reached",
    "hard_cap_threshold",
    "n_unique_pairs_seen",
    "scheduled_pairs",
    "proposed_pairs",
    "completed_pairs",
    "starve_rate_since_last_refit",
    "fallback_rate_since_last_refit",
    "fallback_used_mode",
    "starvation_reason_mode",
    "rank_stability_pass",
    "frac_weak_adj",
    "min_adj_prob",
    "weak_adj_threshold",
    "weak_adj_frac_max",
    "min_adj_prob_threshold",
    "min_new_pairs_for_check",
    "divergences",
    "min_ess_bulk",
    "max_rhat",
    "diagnostics_pass",
    "mcmc_chains",
    "mcmc_parallel_chains",
    "mcmc_core_fraction",
    "mcmc_cores_detected_physical",
    "mcmc_cores_detected_logical",
    "mcmc_threads_per_chain",
    "mcmc_cmdstanr_version",
    "stop_decision",
    "stop_reason",
    "mode"
  )

  testthat::expect_identical(colnames(schema), expected)
  testthat::expect_true(is.integer(schema$round_id))
  testthat::expect_true(is.double(schema$exploration_rate))
  testthat::expect_true(is.logical(schema$theta_sd_pass))
  testthat::expect_true(is.character(schema$stop_reason))
})

testthat::test_that("round log builder emits a single typed row", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$v3 <- adaptive_v3_config(state$N, list())

  fit <- list(
    theta_draws = matrix(
      c(0.1, -0.1, 0.2, -0.2),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, state$ids)
    )
  )
  metrics <- list(
    theta_sd_median_S = 0.1,
    tau = 0.2,
    theta_sd_pass = TRUE,
    U0 = 0.5,
    U_pass = TRUE,
    diagnostics_pass = TRUE
  )
  stop_out <- list(stop_decision = FALSE, stop_reason = NA_character_)

  row <- build_round_log_row(
    state = state,
    fit = fit,
    metrics = metrics,
    stop_out = stop_out,
    config = state$config$v3
  )

  testthat::expect_equal(nrow(row), 1L)
  testthat::expect_identical(colnames(row), colnames(round_log_schema()))
  testthat::expect_true(is.integer(row$round_id))
  testthat::expect_true(is.double(row$reliability_EAP))
})
