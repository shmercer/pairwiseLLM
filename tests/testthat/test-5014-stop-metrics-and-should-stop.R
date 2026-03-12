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

test_that("linking Phase A stop metrics use active set scope", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s1", "s2"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs1", "gs2")
  )
  state <- adaptive_rank_start(
    items,
    seed = 33L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, phase_a_mode = "run")
  )
  draws <- cbind(
    h1 = 2 + seq(-0.1, 0.1, length.out = 40L),
    h2 = -2 + seq(-0.1, 0.1, length.out = 40L),
    s1 = seq(-2, 2, length.out = 40L),
    s2 = seq(-2, 2, length.out = 40L) + 0.05
  )
  state$btl_fit <- make_test_btl_fit(
    state$item_ids,
    draws = draws,
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 500)
  )
  theta_now <- as.double(colMeans(draws))
  names(theta_now) <- colnames(draws)
  theta_lag <- theta_now
  theta_lag[c("h1", "h2")] <- theta_lag[c("h1", "h2")] + c(-0.02, 0.02)
  state$refit_meta$theta_mean_history <- list(theta_lag, theta_now)
  state$refit_meta$theta_mean_history_by_phase_a_set <- list(
    `1` = list(
      stats::setNames(as.double(c(theta_lag["h1"], theta_lag["h2"])), c("h1", "h2")),
      stats::setNames(as.double(c(theta_now["h1"], theta_now["h2"])), c("h1", "h2"))
    )
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

  expect_identical(metrics$phase_scope, "phase_a_set")
  expect_identical(metrics$phase_scope_set_id, 1L)
  expect_identical(metrics$phase_scope_n_items, 2L)
  expect_true(metrics$reliability_EAP_scope > metrics$reliability_EAP)
  expect_true(isTRUE(metrics$lag_eligible_scope))
  expect_true(is.finite(metrics$rho_theta_scope))
  expect_true(is.finite(metrics$rho_rank_scope))
  expect_true(is.finite(metrics$delta_sd_theta_scope))

  cfg$eap_reliability_min <- as.double((metrics$reliability_EAP_scope + metrics$reliability_EAP) / 2)
  expect_true(isTRUE(pairwiseLLM:::should_stop(metrics, config = cfg)))
  metrics_global <- metrics
  metrics_global$phase_scope <- "global"
  expect_false(isTRUE(pairwiseLLM:::should_stop(metrics_global, config = cfg)))
})

test_that("Phase B global stop metrics reconstruct the canonical Phase A plus linking domain", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s1", "s2"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs1", "gs2")
  )
  state <- adaptive_rank_start(
    items,
    seed = 99L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import"
    )
  )
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())

  hub_draws <- cbind(
    h1 = c(1.20, 1.10, 1.15, 1.18),
    h2 = c(-1.10, -1.05, -1.02, -1.08)
  )
  spoke_draws <- cbind(
    s1 = c(0.20, 0.24, 0.18, 0.22),
    s2 = c(-0.25, -0.20, -0.22, -0.18)
  )
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("import", "import"),
      status = c("ready", "ready"),
      validation_message = c("ok", "ok"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(
      `1` = list(
        set_id = 1L,
        items = tibble::tibble(
          item_id = c("h1", "h2"),
          global_item_id = c("gh1", "gh2"),
          theta_raw_mean = colMeans(hub_draws),
          theta_raw_sd = apply(hub_draws, 2, stats::sd),
          rank_mu_raw = c(1, 2)
        ),
        posterior_draws = hub_draws
      ),
      `2` = list(
        set_id = 2L,
        items = tibble::tibble(
          item_id = c("s1", "s2"),
          global_item_id = c("gs1", "gs2"),
          theta_raw_mean = colMeans(spoke_draws),
          theta_raw_sd = apply(spoke_draws, 2, stats::sd),
          rank_mu_raw = c(1, 2)
        ),
        posterior_draws = spoke_draws
      )
    ),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L),
    set_stop_pass_by_set = list(`1` = TRUE, `2` = TRUE),
    phase = "phase_b",
    ready_spokes = 2L,
    active_phase_a_set = NA_integer_,
    phase_b_started_at_step = 1L
  )
  state$controller$link_transform_policy <- "auto"
  state$controller$link_transform_state_by_spoke <- list(`2` = "shift_only")
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_state = "shift_only",
      delta_spoke_mean = 0.80,
      log_alpha_spoke_mean = NA_real_
    )
  )
  poor_draws <- cbind(
    h1 = c(0.10, -0.15, 0.20, -0.05),
    h2 = c(-0.10, 0.05, -0.20, 0.15),
    s1 = c(0.00, 0.10, -0.05, 0.05),
    s2 = c(0.05, -0.10, 0.10, -0.05)
  )
  state$btl_fit <- make_test_btl_fit(
    state$item_ids,
    draws = poor_draws,
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 500)
  )
  state$refit_meta$phase_b_global_theta_mean_history <- list(
    c(h1 = 1.05, h2 = -1.02, s1 = 0.96, s2 = 0.54),
    c(h1 = 1.16, h2 = -1.06, s1 = 1.01, s2 = 0.59)
  )

  metrics <- pairwiseLLM:::compute_stop_metrics(
    state,
    config = list(
      ess_bulk_min = 100,
      ess_bulk_min_near_stop = 100,
      max_rhat = 1.01,
      divergences_max = 0L,
      eap_reliability_min = 0.10,
      stability_lag = 1L,
      theta_corr_min = 0.90,
      theta_sd_rel_change_max = 0.50,
      rank_spearman_min = 0.90
    )
  )

  combined_draws <- pairwiseLLM:::.adaptive_phase_b_global_metric_draws(state)
  expect_equal(
    metrics$reliability_EAP,
    pairwiseLLM:::compute_reliability_EAP(combined_draws)
  )
  expect_gt(
    metrics$reliability_EAP,
    pairwiseLLM:::compute_reliability_EAP(state$btl_fit$btl_posterior_draws)
  )
  expect_true(isTRUE(metrics$lag_eligible))
  expect_true(is.finite(metrics$rho_theta))
  expect_true(is.finite(metrics$rho_rank))
  expect_true(is.finite(metrics$delta_sd_theta))
})
