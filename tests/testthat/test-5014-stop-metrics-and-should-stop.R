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
  expect_true(isTRUE(metrics$eap_pass))
  expect_true(isTRUE(metrics$lag_eligible))
  expect_true(isTRUE(pairwiseLLM:::should_stop(metrics, config = config)))

  strict_config <- config
  strict_config$eap_reliability_min <- 1.01
  expect_false(pairwiseLLM:::should_stop(metrics, config = strict_config))

  phase3_config <- config
  phase3_config$eap_reliability_min <- 0.10
  metrics2 <- pairwiseLLM:::compute_stop_metrics(state, config = phase3_config)
  state2 <- pairwiseLLM:::.adaptive_maybe_enter_phase3(state, metrics2, phase3_config)
  expect_true(isTRUE(state2$refit_meta$near_stop))

  expect_false(pairwiseLLM:::should_stop(NULL, config = config))
})
