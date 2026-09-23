test_that("refit history preserves gaps and computes aligned global RMSE", {
  state <- task09_link_state()
  draws <- state$btl_fit$btl_posterior_draws
  expect_null(.adaptive_phase_b_global_metric_draws(adaptive_rank_start(make_test_items(3))))
  state <- .adaptive_phase_b_global_metric_history_update(state, 3L, draws)
  expect_null(.adaptive_phase_b_global_theta_history_at_refit(state, 1L))
  expect_null(.adaptive_phase_b_global_theta_history_at_refit(state, 0L))
  expect_equal(.adaptive_phase_b_global_theta_history_at_refit(state, 3L), colMeans(draws))
  expect_error(.adaptive_phase_b_global_metric_history_update(state, 0L, draws), "positive")
  current <- c(a = 1, b = 3, c = 7)
  lagged <- c(c = 9, b = 2, a = 0)
  expect_equal(.adaptive_link_theta_global_rmse_from_maps(current, lagged, c("a", "b", "c")), sqrt(2))
  for (ids in list("a", c("a", "missing"))) {
    expect_true(is.na(.adaptive_link_theta_global_rmse_from_maps(current, lagged, ids)))
  }
  expect_true(is.na(.adaptive_link_theta_global_rmse_from_maps(unname(current), lagged, c("a", "b"))))
  current[1:2] <- NA_real_
  expect_true(is.na(.adaptive_link_theta_global_rmse_from_maps(current, lagged, names(current))))
})

test_that("epoch identity explains each artifact and policy change", {
  components <- .adaptive_link_epoch_signature_components("shift_only", "shift_only", "soft_lock",
    list(refit_id = 1L, fit_config_hash = "hub"), list(refit_id = 2L, fit_config_hash = "spoke"))
  signature <- .adaptive_link_epoch_signature_string(components)
  expect_true(is.na(.adaptive_link_epoch_reset_reason(signature, components)))
  expect_true(is.na(.adaptive_link_epoch_reset_reason(paste0(signature, "|extra"), components)))
  expect_true(is.na(.adaptive_link_epoch_reset_reason(NULL, components)))
  expect_identical(.adaptive_link_epoch_reset_reason("old|schema", components), "legacy_epoch_signature_schema")
  reasons <- c("link_estimation_mode_change", "transform_state_change", "link_refit_mode_change",
    "hub_lock_mode_change", "hub_artifact_replaced", "spoke_artifact_replaced",
    "hub_artifact_reloaded", "spoke_artifact_reloaded")
  for (i in seq_along(components)) {
    changed <- components
    changed[i] <- "changed"
    expect_identical(.adaptive_link_epoch_reset_reason(signature, changed), reasons[i])
  }
  components[5:8] <- NA_character_
  expect_true(is.na(.adaptive_link_epoch_reset_reason(.adaptive_link_epoch_signature_string(components), components)))
})

test_that("concurrent budgets respect capacities, floors and deterministic reallocation", {
  stats <- list(`2` = list(candidate_count = 2L, utility_mass = 100),
    `3` = list(candidate_count = 10L, utility_mass = 0),
    `4` = list(candidate_count = 10L, utility_mass = 0))
  expect_identical(.adaptive_link_concurrent_targets(stats, 10L, 1L), c(`2` = 2L, `3` = 4L, `4` = 4L))
  expect_identical(.adaptive_link_concurrent_targets(stats, 2L, 1L), c(`2` = 1L, `3` = 1L, `4` = 0L))
  expect_identical(.adaptive_link_concurrent_targets(stats, 100L, 0L), c(`2` = 2L, `3` = 10L, `4` = 10L))
  stats[[1]]$candidate_count <- 0L
  stats[[2]]$candidate_count <- NA_integer_
  stats[[2]]$utility_mass <- NA_real_
  stats[[3]]$utility_mass <- -1
  expect_identical(.adaptive_link_concurrent_targets(stats, 9L, 1L), c(`2` = 0L, `3` = 5L, `4` = 4L))
  expect_identical(.adaptive_link_concurrent_targets(stats, 0L, 1L), c(`2` = 0L, `3` = 0L, `4` = 0L))
  expect_identical(.adaptive_link_concurrent_targets(list(), 10L, 1L), integer())
  expect_identical(.adaptive_link_concurrent_targets(list(`2` = list(candidate_count = 0L)), 1L, 1L), c(`2` = 0L))
})

test_that("retained score maps preserve item identity and reject malformed summaries", {
  state <- task09_link_state()
  expect_identical(.adaptive_link_spoke_ids(state, 1L), 2:3)
  for (field in c("mean", "sd")) {
    f <- getFromNamespace(paste0(".adaptive_link_theta_", field, "_map"), "pairwiseLLM")
    expected <- state$btl_fit[[paste0("theta_", field)]][c("c", "d")]
    expect_equal(f(state, 2L), expected)
    bad <- state
    bad$btl_fit <- NULL
    expect_length(f(bad, 2L), 0L)
    bad$btl_fit <- list()
    bad$btl_fit[[paste0("theta_", field)]] <- 1:6
    expect_length(f(bad, 2L), 0L)
    bad <- state
    bad$btl_fit[[paste0("theta_", field)]]["c"] <- NA_real_
    expect_identical(names(f(bad, 2L)), "d")
  }
  state$btl_fit$theta_sd["c"] <- -1
  expect_identical(names(.adaptive_link_theta_sd_map(state, 2L)), "d")
  expect_equal(.adaptive_phase_a_artifact_item_field_map(state, 2L, "theta_raw_mean"), c(c = -.5, d = .5))
  state$linking$phase_a$artifacts[["2"]]$items <- state$linking$phase_a$artifacts[["2"]]$items[2:1, ]
  expect_equal(.adaptive_phase_a_artifact_item_field_map(state, 2L, "theta_raw_mean"), c(c = -.5, d = .5))
})

test_that("retained reliability and rank summaries require enough finite aligned items", {
  state <- task09_link_state()
  draws <- state$btl_fit$btl_posterior_draws[, 1:4]
  expected <- var(colMeans(draws)) / (var(colMeans(draws)) + mean(apply(draws, 2, var)))
  expect_equal(.adaptive_link_reliability_active(state, letters[1:4]), expected)
  expect_true(is.na(.adaptive_link_reliability_active(state, "a")))
  expect_true(is.na(.adaptive_link_reliability_active(list(), letters[1:4])))
  expect_true(is.na(.adaptive_link_reliability_decomposition(1, .1, 1e-6, 1e-6)$reliability))
  expect_equal(.adaptive_link_reliability_transformed_active(state, letters[1:4], 2L, 1L,
    "shift_only", 0), .adaptive_link_global_score_stats_active(state, letters[1:4], 2L, 1L,
    "shift_only", 0)$reliability)
  means <- c(a = 1, b = 2, c = 3, d = 4)
  state$trueskill_state$items$mu <- seq_len(6)
  f <- .adaptive_link_ts_btl_rank_spearman_active
  expect_equal(f(state, names(means), means), 1)
  expect_equal(f(state, names(means), -means), -1)
  expect_true(is.na(f(list(), names(means), means)))
  expect_true(is.na(f(state, names(means), unname(means))))
  expect_true(is.na(f(state, "a", means)))
  expect_true(is.na(f(state, c("a", "z"), means)))
  means[1] <- NA_real_
  expect_true(is.na(f(state, names(means), means)))
  means[] <- 1
  expect_true(is.na(f(state, names(means), means)))
})

test_that("legacy transform metadata and epoch starts have deterministic defaults", {
  state <- task09_link_state()
  f <- .adaptive_link_transform_state_for_spoke
  ctl <- task09_transform_controller(state)
  expect_identical(f(ctl, 2L), "shift_only")
  ctl$link_transform_policy <- "auto"
  expect_identical(f(ctl, 2L), "shift_only")
  ctl$link_transform_state_by_spoke <- list(`2` = "shift_scale", `3` = "bad")
  expect_identical(f(ctl, 2L), "shift_scale")
  expect_identical(f(ctl, 3L), "shift_only")
  expect_identical(.adaptive_link_epoch_start_step_default(state, 2L), 1L)
  state$linking$phase_a$phase_b_started_at_step <- 7L
  expect_identical(.adaptive_link_epoch_start_step_for_spoke(state, 2L), 7L)
  state$controller$link_epoch_start_step_by_spoke <- list(`2` = 12L)
  expect_identical(.adaptive_link_epoch_start_step_for_spoke(state, 2L), 12L)
})

test_that("deferred audit supports capped draws and historical payloads", {
  f <- .adaptive_deferred_audit_draw_index
  expect_error(f(1L), "at least two")
  expect_error(f(10L, NA_real_), "positive integer")
  expect_error(f(10L, 1L), ">= 2")
  expect_identical(f(10L, NULL), 1:10)
  expect_identical(f(10L, Inf), 1:10)
  expect_identical(f(10L, 20L), 1:10)
  expect_identical(f(10L, 3L), c(1L, 6L, 10L))
  draws <- task09_link_state()$btl_fit$btl_posterior_draws
  expected <- .adaptive_round_log_deferred_audit_from_draws(draws, .4, .6)
  expect_equal(.adaptive_round_log_deferred_audit_from_payload(list(draws = draws)), expected)
  expect_equal(.adaptive_round_log_deferred_audit_from_payload(NULL), .adaptive_round_log_deferred_audit_na_values())
  expect_equal(.adaptive_round_log_deferred_audit_from_payload(list(summary = expected)), expected)
})

test_that("refit cache commits distinguish repeated active and held-out observations", {
  state <- task09_link_state()
  rid <- .adaptive_link_refit_window_id(state)
  state <- .adaptive_link_refit_summary_store(state, .adaptive_link_refit_summary_empty(rid, 2L))
  row <- tibble::tibble(pair_id = 1L, is_cross_set = TRUE, run_mode = "link_multi_spoke",
    link_spoke_id = 2L, A = 1L, B = 3L, is_probe_step = FALSE, link_stage = "anchor_link")
  f <- function(s, r) .adaptive_link_refit_summary_update_after_commit(s, s, r)
  active <- f(state, row)
  summary <- .adaptive_link_refit_summary_current(active, rid, 2L)
  expect_identical(summary$n_cross_edges_active_since_last_refit, 1L)
  expect_identical(summary$stage_realized[["anchor_link"]], 1L)
  probe <- row
  probe$run_mode <- "link_probe_holdout"
  probe$is_probe_step <- TRUE
  probe$fallback_used <- "probe_panel_fixed_refit"
  out <- f(active, probe)
  summary <- .adaptive_link_refit_summary_current(out, rid, 2L)
  expect_identical(summary$n_pairs_cross_set_done, 2L)
  expect_identical(summary$n_unique_cross_pairs_seen, 1L)
  expect_identical(summary$n_cross_edges_probe_since_last_refit, 1L)
  expect_true(summary$probe_panel_acceleration_used_since_last_refit)
  expect_identical(f(state, row[FALSE, ]), state)
  for (change in list(list(pair_id = NA_integer_), list(is_cross_set = FALSE),
    list(run_mode = "within_set"), list(link_spoke_id = NA_integer_), list(link_stage = "invalid"))) {
    changed <- row
    for (name in names(change)) changed[[name]] <- change[[name]]
    expect_identical(f(state, changed), state)
  }
  row$A <- 99L
  expect_error(f(state, row), "valid.*item indices")
})
