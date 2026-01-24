testthat::test_that("round log schema matches contract", {
  schema <- round_log_schema()

  expected <- c(
    "round_id",
    "iter_at_refit",
    "mode",
    "model_variant",
    "n_items",
    "total_pairs",
    "hard_cap_threshold",
    "n_unique_pairs_seen",
    "scheduled_pairs",
    "completed_pairs",
    "backlog_unjudged",
    "new_pairs",
    "proposed_pairs",
    "batch_size",
    "window_W",
    "exploration_rate",
    "mean_degree",
    "min_degree",
    "pos_balance_mean",
    "pos_balance_sd",
    "epsilon_mean",
    "epsilon_p2.5",
    "epsilon_p5",
    "epsilon_p50",
    "epsilon_p95",
    "epsilon_p97.5",
    "b_mean",
    "b_p2.5",
    "b_p5",
    "b_p50",
    "b_p95",
    "b_p97.5",
    "divergences",
    "max_rhat",
    "min_ess_bulk",
    "diagnostics_pass",
    "reliability_EAP",
    "theta_sd_eap",
    "rho_theta_lag",
    "delta_sd_theta_lag",
    "rho_rank_lag",
    "rank_stability_pass",
    "stop_passes",
    "stop_eligible",
    "stop_decision",
    "stop_reason",
    "starve_rate_since_last_refit",
    "fallback_rate_since_last_refit",
    "fallback_used_mode",
    "starvation_reason_mode",
    "mcmc_chains",
    "mcmc_parallel_chains",
    "mcmc_core_fraction",
    "mcmc_cores_detected_physical",
    "mcmc_cores_detected_logical",
    "mcmc_threads_per_chain",
    "mcmc_cmdstanr_version"
  )

  testthat::expect_identical(colnames(schema), expected)
  obsolete <- c(
    "gini_degree",
    "gini_pos_A",
    "frac_weak_adj",
    "min_adj_prob",
    "weak_adj_threshold",
    "weak_adj_frac_max",
    "min_adj_prob_threshold",
    "min_new_pairs_for_check",
    "theta_sd_pass",
    "U_pass",
    "U0",
    "U_top_median",
    "U_abs",
    "U_dup_threshold",
    "tau",
    "hard_cap_reached"
  )
  testthat::expect_false(any(obsolete %in% colnames(schema)))
  testthat::expect_true(is.integer(schema$round_id))
  testthat::expect_true(is.double(schema$exploration_rate))
  testthat::expect_true(is.logical(schema$stop_decision))
  testthat::expect_true(is.character(schema$stop_reason))
})

testthat::test_that("round log builder emits a single typed row", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$v3 <- adaptive_v3_config(state$N, list())
  state$stop_candidate <- TRUE
  state$checks_passed_in_row <- 2L

  fit <- list(
    theta_draws = matrix(
      c(0.1, -0.1, 0.2, -0.2),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, state$ids)
    ),
    b_summary = tibble::tibble(
      b_mean = 0.15,
      b_p2.5 = 0.01,
      b_p5 = 0.02,
      b_p50 = 0.15,
      b_p95 = 0.28,
      b_p97.5 = 0.3
    ),
    model_variant = "mcmc"
  )
  metrics <- list(
    diagnostics_pass = TRUE,
    scheduled_pairs = 10L,
    completed_pairs = 7L,
    rho_theta_lag = 0.1,
    delta_sd_theta_lag = 0.2,
    rho_rank_lag = 0.3
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
  testthat::expect_equal(row$backlog_unjudged[[1L]], 3L)
  testthat::expect_equal(row$stop_passes[[1L]], 2L)
  testthat::expect_true(row$stop_eligible[[1L]])
  testthat::expect_equal(row$model_variant[[1L]], "mcmc")
  testthat::expect_equal(row$b_p50[[1L]], 0.15)
  testthat::expect_equal(row$rho_theta_lag[[1L]], 0.1)
})
