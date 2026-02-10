test_that("compute_stop_metrics and should_stop follow thresholds", {
  items <- make_test_items(3)
  state <- pairwiseLLM:::new_adaptive_state(items)
  ids <- state$item_ids

  draws <- matrix(rep(seq_along(ids), each = 5L), nrow = 5L)
  colnames(draws) <- ids
  fit <- make_test_btl_fit(
    ids,
    draws = draws,
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 500)
  )
  state$btl_fit <- fit
  state$refit_meta$theta_mean_history <- list(
    stats::setNames(as.double(seq_along(ids)), ids),
    stats::setNames(as.double(seq_along(ids)), ids)
  )

  config <- list(
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

  metrics <- pairwiseLLM:::compute_stop_metrics(state, config = config)
  expect_true(is.list(metrics))
  expect_true(isTRUE(metrics$diagnostics_pass))
  expect_true(isTRUE(metrics$diagnostics_divergences_pass))
  expect_true(isTRUE(metrics$diagnostics_rhat_pass))
  expect_true(isTRUE(metrics$diagnostics_ess_pass))
  expect_true(isTRUE(metrics$eap_pass))
  expect_true(isTRUE(metrics$lag_eligible))
  expect_true(isTRUE(pairwiseLLM:::should_stop(metrics, config = config)))

  strict_config <- config
  strict_config$eap_reliability_min <- 1.01
  expect_false(pairwiseLLM:::should_stop(metrics, config = strict_config))

  metrics_report_only <- metrics
  metrics_report_only$cov_trace_theta <- 0.99
  metrics_report_only$top20_boundary_entropy_mean <- 0.49
  metrics_report_only$nn_diff_sd_mean <- 0.49
  expect_identical(
    pairwiseLLM:::should_stop(metrics_report_only, config = config),
    pairwiseLLM:::should_stop(metrics, config = config)
  )

  phase3_config <- config
  phase3_config$eap_reliability_min <- 0.10
  metrics2 <- pairwiseLLM:::compute_stop_metrics(state, config = phase3_config)
  state2 <- pairwiseLLM:::.adaptive_maybe_enter_phase3(state, metrics2, phase3_config)
  expect_true(isTRUE(state2$refit_meta$near_stop))

  expect_false(pairwiseLLM:::should_stop(NULL, config = config))
})

test_that("near-stop ESS threshold switch occurs only after near-stop entry at refit", {
  items <- make_test_items(3)
  state <- pairwiseLLM:::new_adaptive_state(items)
  ids <- state$item_ids
  draws <- matrix(rep(seq_along(ids), each = 5L), nrow = 5L)
  colnames(draws) <- ids
  state$btl_fit <- make_test_btl_fit(
    ids,
    draws = draws,
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 150)
  )
  state$refit_meta$theta_mean_history <- list(
    stats::setNames(as.double(seq_along(ids)), ids),
    stats::setNames(as.double(seq_along(ids)), ids)
  )
  cfg <- list(
    ess_bulk_min = 100,
    ess_bulk_min_near_stop = 200,
    max_rhat = 1.01,
    divergences_max = 0L,
    eap_reliability_min = 0.90,
    stability_lag = 1L,
    theta_corr_min = 0.90,
    theta_sd_rel_change_max = 0.20,
    rank_spearman_min = 0.90
  )

  metrics_before <- pairwiseLLM:::compute_stop_metrics(state, config = cfg)
  expect_equal(metrics_before$ess_bulk_required, 100)
  expect_true(isTRUE(metrics_before$diagnostics_pass))

  state_near <- pairwiseLLM:::.adaptive_maybe_enter_phase3(state, metrics_before, cfg)
  expect_true(isTRUE(state_near$refit_meta$near_stop))

  metrics_after <- pairwiseLLM:::compute_stop_metrics(state_near, config = cfg)
  expect_equal(metrics_after$ess_bulk_required, 200)
  expect_false(isTRUE(metrics_after$diagnostics_pass))
})

test_that("lagged stability metrics are NA when lag ineligible", {
  items <- make_test_items(3)
  state <- pairwiseLLM:::new_adaptive_state(items)
  ids <- state$item_ids
  draws <- matrix(rep(seq_along(ids), each = 5L), nrow = 5L)
  colnames(draws) <- ids
  state$btl_fit <- make_test_btl_fit(
    ids,
    draws = draws,
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 500)
  )
  state$refit_meta$theta_mean_history <- list(
    stats::setNames(as.double(seq_along(ids)), ids)
  )
  cfg <- list(
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

  metrics <- pairwiseLLM:::compute_stop_metrics(state, config = cfg)
  expect_false(isTRUE(metrics$lag_eligible))
  expect_true(is.na(metrics$rho_theta))
  expect_true(is.na(metrics$delta_sd_theta))
  expect_true(is.na(metrics$rho_rank))
})
