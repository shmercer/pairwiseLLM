test_that("adaptive_rank_run_live can stop early via BTL refit", {
  items <- make_test_items(4)
  state <- pairwiseLLM:::new_adaptive_state(items)
  judge <- make_deterministic_judge("i_wins")
  stub <- make_deterministic_fit_fn(state$item_ids)

  btl_config <- list(
    refit_pairs_target = 2L,
    ess_bulk_min = 100,
    ess_bulk_min_near_stop = 100,
    max_rhat = 1.01,
    divergences_max = 0L,
    eap_reliability_min = 0.90,
    stability_lag = 1L,
    theta_corr_min = 0.90,
    theta_sd_rel_change_max = 0.20,
    rank_spearman_min = 0.90
  )

  withr::local_seed(1)
  out <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 10L,
    fit_fn = stub$fit_fn,
    btl_config = btl_config,
    progress = "none"
  )

  expect_true(inherits(out, "adaptive_state"))
  expect_equal(nrow(out$step_log), 4L)
  expect_equal(nrow(out$round_log), 2L)
  expect_true(isTRUE(out$round_log$stop_decision[[2L]]))
  expect_true(!is.na(out$round_log$stop_reason[[2L]]))
  expect_true(all(c(
    "ts_sigma_mean",
    "ci95_theta_width_median",
    "near_tie_adj_frac",
    "cov_trace_theta",
    "top20_boundary_entropy_mean",
    "nn_diff_sd_mean",
    "diagnostics_ess_pass",
    "lag_eligible",
    "mcmc_chains",
    "mcmc_parallel_chains"
  ) %in% names(out$round_log)))
})
