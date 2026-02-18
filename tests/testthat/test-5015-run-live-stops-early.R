test_that("adaptive_rank_run_live can stop early via BTL refit", {
  items <- make_test_items(12)
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
  expect_true(nrow(out$step_log) >= 4L)
  expect_true(nrow(out$round_log) >= 2L)
  expect_true(isTRUE(utils::tail(out$round_log$stop_decision, 1L)))
  expect_true(!is.na(utils::tail(out$round_log$stop_reason, 1L)))
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

test_that("adaptive_rank_run_live logs post-stop fields with default immediate-stop behavior", {
  items <- make_test_items(12)
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

  withr::local_seed(11)
  out <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 10L,
    fit_fn = stub$fit_fn,
    btl_config = btl_config,
    progress = "none"
  )

  expect_true(is.integer(out$round_log$max_pairs_after_stop))
  expect_true(is.integer(out$round_log$pairs_committed_after_stop))
  expect_true(all(out$round_log$max_pairs_after_stop == 0L))
  expect_true(all(out$round_log$pairs_committed_after_stop == 0L))
  expect_identical(out$meta$stop_reason, "btl_converged")
})

test_that("adaptive_rank_run_live enforces bounded committed comparisons after stop boundary", {
  items <- make_test_items(12)
  state <- pairwiseLLM:::new_adaptive_state(items)
  judge <- make_deterministic_judge("i_wins")
  stub <- make_deterministic_fit_fn(state$item_ids)

  btl_config <- list(
    refit_pairs_target = 1L,
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

  withr::local_seed(12)
  out <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 20L,
    fit_fn = stub$fit_fn,
    adaptive_config = list(max_pairs_after_stop = 2L),
    btl_config = btl_config,
    progress = "none"
  )

  boundary_rows <- which(out$round_log$stop_decision %in% TRUE)
  expect_true(length(boundary_rows) >= 1L)
  boundary_idx <- boundary_rows[[1L]]
  boundary_step <- as.integer(out$round_log$step_id_at_refit[[boundary_idx]])
  committed_after <- sum(
    as.integer(out$step_log$step_id) > boundary_step &
      !is.na(out$step_log$pair_id)
  )
  expect_identical(as.integer(committed_after), 2L)
  expect_identical(as.integer(out$meta$pairs_committed_after_stop), 2L)
  expect_identical(out$meta$stop_reason, "max_pairs_after_stop_exhausted")
  expect_true(all(out$round_log$max_pairs_after_stop == 2L))
  expect_true(all(out$round_log$pairs_committed_after_stop >= 0L))
})
