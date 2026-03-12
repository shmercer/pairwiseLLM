make_lowcov_link_state <- function(run_mode = "link_multi_spoke") {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23", "s31", "s32"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs23", "gs31", "gs32")
  )
  state <- adaptive_rank_start(
    items,
    seed = 101L,
    adaptive_config = list(
      run_mode = run_mode,
      hub_id = 1L,
      multi_spoke_mode = "concurrent",
      probe_pairs_per_refit_per_spoke = 2L,
      probe_edges_min_for_stop = 2L
    )
  )
  state$warm_start_done <- TRUE

  make_artifact <- function(set_id, ids, theta, sd = 0.10) {
    draws <- matrix(rep(theta, each = 4L), nrow = 4L)
    colnames(draws) <- ids
    list(
      set_id = as.integer(set_id),
      quality_gate_accepted = TRUE,
      n_pairs_committed = 4L,
      diagnostics = list(
        diagnostics_pass = TRUE,
        reliability_EAP_within = 0.95
      ),
      posterior_draws = draws,
      items = tibble::tibble(
        global_item_id = ids,
        theta_raw_mean = as.double(theta),
        theta_raw_sd = rep(sd, length(ids)),
        rank_mu_raw = seq_along(ids)
      )
    )
  }

  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L, 3L),
      source = c("import", "import", "import"),
      status = c("ready", "ready", "ready"),
      validation_message = c("ok", "ok", "ok"),
      artifact_path = c(NA_character_, NA_character_, NA_character_)
    ),
    artifacts = list(
      `1` = make_artifact(1L, c("gh1", "gh2", "gh3"), c(0.8, 0.2, -0.2)),
      `2` = make_artifact(2L, c("gs21", "gs22", "gs23"), c(0.3, -0.1, -0.4)),
      `3` = make_artifact(3L, c("gs31", "gs32"), c(0.1, -0.3))
    ),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L, 3L),
    ready_spokes = c(2L, 3L),
    set_stop_pass_by_set = list(`1` = TRUE, `2` = TRUE, `3` = TRUE),
    active_phase_a_set = NA_integer_,
    phase = "phase_b",
    phase_b_started_at_step = 1L
  )

  state$controller$current_link_spoke_id <- 2L
  state$controller$link_epoch_id_by_spoke <- list(`2` = 1L, `3` = 1L)
  state$controller$link_transform_state_by_spoke <- list(`2` = "shift_only", `3` = "shift_scale")
  state$controller$link_transform_frozen_by_spoke <- list(`2` = FALSE, `3` = FALSE)
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      delta_spoke_mean = 0.25,
      log_alpha_spoke_mean = NA_real_,
      link_epoch_id = 1L,
      link_identified = FALSE,
      link_stop_eligible = FALSE,
      probe_panel_shortfall = 1,
      probe_brier = 0.25,
      probe_pred_rmse_lagged = 0.03,
      theta_global_rmse_lagged = 0.07,
      delta_spoke_sd = 0.11
    ),
    `3` = list(
      delta_spoke_mean = -0.15,
      log_alpha_spoke_mean = log(1.2),
      link_epoch_id = 1L,
      link_identified = TRUE,
      link_stop_eligible = FALSE
    )
  )
  state$controller$link_budget_refit_id <- 1L
  state$controller$link_budget_map <- list(
    `2` = list(B_spoke_refit_budget = 2L),
    `3` = list(B_spoke_refit_budget = 1L)
  )
  state$refit_meta$last_refit_step <- 0L
  state$refit_meta$refit_pairs_target_current <- 4L

  draws <- rbind(
    c(0.8, 0.2, -0.2, 0.3, -0.1, -0.4, 0.1, -0.3),
    c(0.9, 0.3, -0.1, 0.4, 0.0, -0.3, 0.2, -0.2),
    c(0.7, 0.1, -0.3, 0.2, -0.2, -0.5, 0.0, -0.4),
    c(0.85, 0.25, -0.15, 0.35, -0.05, -0.35, 0.15, -0.25)
  )
  colnames(draws) <- state$item_ids
  state$btl_fit <- make_test_btl_fit(state$item_ids, draws = draws, model_variant = "btl_e_b")
  state$btl_fit$beta_draws <- c(-0.1, 0.0, 0.1, 0.2)
  state$btl_fit$epsilon_draws <- c(0.0, 0.1, 0.2, 0.0)
  state$btl_fit$beta_mean <- 0
  state$btl_fit$epsilon_mean <- 0.05

  state
}

append_link_step <- function(state,
                             step_id,
                             A_id,
                             B_id,
                             Y = 1L,
                             pair_id = step_id,
                             spoke_id = 2L,
                             run_mode = "link_multi_spoke",
                             stage = "anchor_link",
                             is_probe_step = FALSE,
                             is_holdout_probe_step = FALSE,
                             is_drift_probe_step = FALSE) {
  ids <- as.character(state$item_ids)
  A <- match(A_id, ids)
  B <- match(B_id, ids)
  set_map <- stats::setNames(as.integer(state$items$set_id), as.character(state$items$item_id))

  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = as.integer(step_id),
      timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + as.integer(step_id),
      pair_id = as.integer(pair_id),
      i = as.integer(A),
      j = as.integer(B),
      A = as.integer(A),
      B = as.integer(B),
      Y = as.integer(Y),
      set_i = as.integer(set_map[[A_id]]),
      set_j = as.integer(set_map[[B_id]]),
      is_cross_set = as.integer(set_map[[A_id]]) != as.integer(set_map[[B_id]]),
      link_spoke_id = as.integer(spoke_id),
      run_mode = as.character(run_mode),
      round_stage = as.character(stage),
      link_stage = as.character(stage),
      is_probe_step = is_probe_step,
      is_holdout_probe_step = is_holdout_probe_step,
      is_drift_probe_step = is_drift_probe_step
    )
  )

  state
}

add_link_stage_row <- function(state,
                               refit_id = 1L,
                               spoke_id = 2L,
                               link_epoch_id = 1L,
                               probe_panel_id = NA_character_,
                               probe_edges_realized = 0L,
                               probe_edges_planned = 2L,
                               transform_frozen = FALSE) {
  link_stage_log <- state$link_stage_log
  if (is.null(link_stage_log)) {
    link_stage_log <- pairwiseLLM:::new_link_stage_log()
  }
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    link_stage_log,
    list(
      refit_id = as.integer(refit_id),
      spoke_id = as.integer(spoke_id),
      hub_id = 1L,
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_refit_mode = "shift_only",
      hub_lock_mode = "soft_lock",
      link_epoch_id = as.integer(link_epoch_id),
      probe_panel_id = probe_panel_id,
      link_fit_method = "cmdstan_hmc",
      link_uncertainty_approximation = "cmdstan_posterior_draws",
      reliability_link_global = 0.92,
      linking_identified = FALSE,
      link_stop_eligible = FALSE,
      link_stop_pass = FALSE,
      transform_frozen = transform_frozen,
      stop_recent_pass_count = 0L,
      stop_recent_window_size = 3L,
      stability_window_refits_used = 3L,
      stability_passes_required_used = 2L,
      escalation_recent_pass_count = 0L,
      escalation_recent_window_size = 3L,
      link_transform_escalation_window_refits_used = 3L,
      link_transform_escalation_passes_required_used = 2L,
      n_pairs_cross_set_done = 2L,
      n_unique_cross_pairs_seen = 2L,
      n_cross_edges_active_since_last_refit = 0L,
      n_cross_edges_probe_since_last_refit = 0L,
      n_cross_edges_total_since_last_refit = 0L,
      coverage_bins_used = 3L,
      B_spoke_refit_budget = 2L,
      B_spoke_refit_budget_source = "concurrent_allocator",
      stage_target_anchor_link = 1L,
      stage_target_long_link = 1L,
      stage_target_mid_link = 0L,
      stage_target_local_link = 0L,
      feasible_stage_capacity_anchor_link = 1L,
      feasible_stage_capacity_long_link = 1L,
      feasible_stage_capacity_mid_link = 0L,
      feasible_stage_capacity_local_link = 0L,
      feasibility_budget_released = 0L,
      feasibility_reallocation_used = FALSE,
      feasibility_reallocation_rule = "none",
      stage_realized_anchor_link = 0L,
      stage_realized_long_link = 0L,
      stage_realized_mid_link = 0L,
      stage_realized_local_link = 0L,
      stage_shortfall_anchor_link = 1L,
      stage_shortfall_long_link = 1L,
      stage_shortfall_mid_link = 0L,
      stage_shortfall_local_link = 0L,
      stage_reallocation_used = FALSE,
      stage_reallocation_rule_used = "none",
      stage_budget_unfilled = 0L,
      probe_edges_realized_before_refit = 0L,
      probe_edges_realized_delta_since_last_refit = 0L,
      probe_edges_planned = as.integer(probe_edges_planned),
      probe_edges_realized = as.integer(probe_edges_realized),
      probe_panel_shortfall = max(0L, as.integer(probe_edges_planned - probe_edges_realized)),
      probe_shortfall_reason = "none",
      probe_brier = 0.24,
      probe_brier_max_used = 0.19,
      probe_brier_pass = FALSE,
      probe_pred_rmse_lagged = 0.02,
      probe_pred_rmse_max_used = 0.015,
      probe_pred_rmse_pass = FALSE,
      theta_global_rmse_lagged = 0.06,
      theta_global_rmse_max_used = 0.05,
      theta_global_rmse_pass = FALSE,
      link_diagnostics_divergences = 1L,
      link_diagnostics_divergences_pass = FALSE,
      link_diagnostics_max_rhat = 1.05,
      link_diagnostics_rhat_pass = FALSE,
      link_diagnostics_min_ess_bulk = 100,
      link_diagnostics_ess_pass = FALSE,
      link_stop_gate_open = FALSE,
      link_lag_eligible = FALSE,
      resumed_from_session = FALSE
    )
  )

  state
}

test_that("low-coverage state, simulation, and cost helpers cover edge branches", {
  expect_error(
    pairwiseLLM:::.adaptive_normalize_link_transform_policy(policy = "bogus"),
    "must be one of"
  )
  expect_identical(
    pairwiseLLM:::.adaptive_normalize_link_transform_policy(policy = "shift_only"),
    "fixed_shift_only"
  )
  expect_error(
    pairwiseLLM:::.adaptive_normalize_link_transform_state(state = "bogus"),
    "must be one of"
  )

  normalized <- pairwiseLLM:::.adaptive_controller_normalize_legacy_fields(
    list(
      link_transform_mode = "shift_scale",
      link_transform_mode_by_spoke = list(`2` = "shift_only"),
      shift_only_theta_treatment = "normal_prior",
      stability_consecutive_k = 2L,
      link_transform_escalation_refits_required = 3L,
      link_stop_consecutive_pass_count_by_spoke = list(`2` = 2L),
      link_escalation_consecutive_pass_count_by_spoke = list(`2` = 3L)
    ),
    n_items = 6L
  )
  expect_identical(normalized$link_transform_policy, "fixed_shift_scale")
  expect_identical(normalized$link_transform_state_by_spoke$`2`, "shift_only")
  expect_identical(normalized$shift_only_theta_treatment, "fixed_eap_plugin_var")
  expect_identical(normalized$stability_passes_required, 2L)
  expect_identical(normalized$link_transform_escalation_window_refits, 3L)
  expect_identical(normalized$link_stop_recent_pass_window_by_spoke$`2`, c(TRUE, TRUE))

  expect_identical(pairwiseLLM:::.adaptive_link_result_window_normalize(NULL, max_size = 2L), logical())
  expect_identical(
    pairwiseLLM:::.adaptive_link_result_window_normalize(c(TRUE, FALSE, TRUE), max_size = 2L),
    c(FALSE, TRUE)
  )
  expect_error(
    pairwiseLLM:::.adaptive_link_result_window_append(logical(), NA, max_size = 2L),
    "single non-missing logical"
  )
  expect_identical(pairwiseLLM:::.adaptive_link_result_window_pass_count(c(TRUE, FALSE, TRUE)), 2L)

  link_items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22")
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(run_mode = "link_one_spoke", shift_only_theta_treatment = ""),
      n_items = 4L,
      set_ids = link_items$set_id
    ),
    "single string value"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(run_mode = "link_one_spoke", phase_a_compatible_model_ids = c("ok", NA_character_)),
      n_items = 4L,
      set_ids = link_items$set_id
    ),
    "phase_a_compatible_model_ids"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(run_mode = "link_one_spoke", stage_quota_frac_anchor_link = 0.5),
      n_items = 4L,
      set_ids = link_items$set_id
    ),
    "must sum to 1.0"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(run_mode = "link_one_spoke"),
      n_items = 4L,
      set_ids = c(1L, 2L, 3L)
    ),
    "exactly one spoke set"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(run_mode = "link_one_spoke", stability_window_refits = 2L, stability_passes_required = 3L),
      n_items = 4L,
      set_ids = link_items$set_id
    ),
    "stability_passes_required"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(
        run_mode = "link_one_spoke",
        link_transform_escalation_window_refits = 2L,
        link_transform_escalation_passes_required = 3L
      ),
      n_items = 4L,
      set_ids = link_items$set_id
    ),
    "link_transform_escalation_passes_required"
  )
  expect_error(
    pairwiseLLM:::.adaptive_link_refit_budget_default(4L, controller = list(refit_pairs_target = -1L)),
    "non-negative integer"
  )
  expect_identical(
    pairwiseLLM:::.adaptive_weighted_largest_remainder(0L, c(a = 1, b = 2), c("a", "b"))$add,
    c(a = 0L, b = 0L)
  )
  expect_identical(
    sum(pairwiseLLM:::.adaptive_link_compute_stage_targets(
      budget = 1L,
      controller = pairwiseLLM:::.adaptive_controller_defaults(20L)
    )),
    1L
  )
  expect_error(
    pairwiseLLM:::.adaptive_link_compute_stage_targets(
      budget = -1L,
      controller = pairwiseLLM:::.adaptive_controller_defaults(20L)
    ),
    "non-negative integer budget"
  )
  controller <- pairwiseLLM:::.adaptive_controller_defaults(20L)
  controller$current_link_spoke_id <- 2L
  controller$link_refit_stats_by_spoke <- list(`2` = list(probe_panel_shortfall = -1, probe_brier = NA_real_))
  weights <- pairwiseLLM:::.adaptive_link_blocker_weights_for_spoke(controller, spoke_id = 2L)
  expect_true(all(weights >= 0))
  expect_equal(
    pairwiseLLM:::.adaptive_link_blocker_stage_weights(
      c(
        probe_panel_shortfall = -1,
        probe_brier = 0,
        probe_pred_rmse_lagged = 0,
        theta_global_rmse_lagged = 2,
        delta_spoke_sd = 0
      ),
      linking_identified = TRUE
    )[["anchor_link"]],
    1
  )
  quotas <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = 1L,
    n_items = 12L,
    controller = utils::modifyList(
      pairwiseLLM:::.adaptive_controller_defaults(12L),
      list(round_pairs_target = 3L, global_identified = TRUE)
    )
  )
  expect_identical(sum(quotas), pairwiseLLM:::adaptive_defaults(12L)$round_pairs_target)

  custom_items <- tibble::tibble(item_id = c("a", "b"), quality_score = c(0.2, 0.8))
  run <- pairwiseLLM:::.adaptive_simulation_run(
    scenario = "baseline",
    items = custom_items,
    run_seed = 11L,
    judge_seed = 12L,
    n_steps = 1L
  )
  expect_identical(run$state$n_items, 2L)
  expect_error(
    pairwiseLLM:::.adaptive_simulation_run(
      scenario = "baseline",
      items = tibble::tibble(item_id = "a"),
      run_seed = 1L,
      judge_seed = 1L,
      n_steps = 1L
    ),
    "must contain `item_id` and `quality_score`"
  )
  empty_quota <- pairwiseLLM:::.adaptive_stage_quota_summary(
    tibble::tibble(step_id = 1L, round_stage = "warm_start")
  )
  expect_identical(nrow(empty_quota), 0L)
  expect_false(pairwiseLLM:::.adaptive_warm_start_connectivity(
    tibble::tibble(round_stage = "warm_start", pair_id = 1L, i = 99L, j = 100L),
    item_ids = c("a", "b")
  ))

  pairs <- tibble::tibble(
    ID1 = paste0("A", seq_len(7)),
    text1 = c("a", rep("bbbb", 2), rep("cccccccc", 2), rep("dddddddddddd", 2)),
    ID2 = paste0("B", seq_len(7)),
    text2 = c("x", rep("yyyy", 2), rep("zzzzzzzz", 2), rep("wwwwwwwwwwww", 2))
  )
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  est <- estimate_llm_pairs_cost(
    pairs = pairs,
    model = "gpt-4.1-mini",
    backend = "openai",
    endpoint = "chat.completions",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    n_test = 5L,
    test_strategy = "stratified_prompt_bytes",
    seed = 123L,
    cost_per_million_input = 1,
    cost_per_million_output = 1,
    progress = FALSE,
    verbose = FALSE,
    .submit_fun = function(pairs, ...) {
      list(
        results = tibble::tibble(
          ID1 = pairs$ID1,
          ID2 = pairs$ID2,
          better_id = pairs$ID1
        ),
        failed_attempts = tibble::tibble(message = "retry")
      )
    }
  )
  expect_identical(est$summary$n_test, 5L)
  expect_true(is.list(est$calibration))
})

test_that("low-coverage Phase B helpers cover resume, probe, and global metric branches", {
  state <- make_lowcov_link_state()
  state <- append_link_step(state, 1L, "h1", "s21", spoke_id = 2L)
  state <- append_link_step(state, 2L, "h2", "s22", spoke_id = 2L)
  state <- add_link_stage_row(state, refit_id = 1L, spoke_id = 2L, link_epoch_id = 1L)

  legacy_link_stage <- tibble::tibble(
    refit_id = 1L,
    spoke_id = 2L,
    hub_id = 1L,
    link_transform_mode = "shift_only",
    link_refit_mode = "shift_only",
    hub_lock_mode = "soft_lock",
    ppc_calibration_id = "old",
    cross_set_ppc_brier_max_used = 0.2,
    reliability_EAP_link = 0.91,
    stop_consecutive_pass_count = 2L,
    escalation_consecutive_pass_count = 1L,
    link_transform_escalation_refits_required_used = 3L
  )
  aligned <- pairwiseLLM:::.adaptive_align_log_schema_for_resume(
    legacy_link_stage,
    pairwiseLLM:::schema_link_stage_log,
    "link_stage_log"
  )
  expect_identical(aligned$link_transform_policy[[1L]], "fixed_shift_only")
  expect_identical(aligned$link_transform_state[[1L]], "shift_only")
  expect_false("link_transform_mode" %in% names(aligned))
  expect_false("ppc_calibration_id" %in% names(aligned))
  expect_identical(aligned$reliability_link_global[[1L]], 0.91)
  expect_identical(aligned$stop_recent_pass_count[[1L]], 2L)
  expect_identical(aligned$link_transform_escalation_window_refits_used[[1L]], 3L)

  expect_no_error(pairwiseLLM:::.adaptive_link_probe_resume_validate_current_window(
    state = state,
    spoke_id = 2L,
    panel_epoch = 1L,
    panel = tibble::tibble()
  ))

  panel <- pairwiseLLM:::.adaptive_link_probe_construct_panel(state, state$controller, spoke_id = 2L)
  state$linking$probe$panels_by_spoke <- list(`2` = panel)
  panel_id <- unique(panel$probe_panel_id)[[1L]]
  state$linking$probe$realized_edges <- tibble::tibble(
    spoke_id = 2L,
    link_epoch_id = 1L,
    pair_key = panel$pair_key[[1L]],
    probe_panel_id = panel_id,
    step_id = 3L,
    pair_id = 3L,
    run_mode = "link_probe_holdout",
    Y = 1L
  )
  state$linking$probe$prediction_cache <- tibble::tibble(
    refit_id = 1L,
    spoke_id = 2L,
    link_epoch_id = 1L,
    pair_key = panel$pair_key[[1L]],
    pred_prob = 0.25
  )
  state <- append_link_step(
    state,
    3L,
    panel$hub_item_id[[1L]],
    panel$spoke_item_id[[1L]],
    spoke_id = 2L,
    run_mode = "link_probe_holdout",
    stage = "local_link",
    is_probe_step = TRUE,
    is_holdout_probe_step = TRUE
  )
  expect_no_error(pairwiseLLM:::.adaptive_link_probe_resume_validate_current_window(
    state = state,
    spoke_id = 2L,
    panel_epoch = 1L,
    panel = panel
  ))

  bad_state <- state
  bad_state$step_log$A[nrow(bad_state$step_log)] <- match("h1", bad_state$item_ids)
  bad_state$step_log$B[nrow(bad_state$step_log)] <- match("s31", bad_state$item_ids)
  expect_error(
    pairwiseLLM:::.adaptive_link_probe_resume_validate_current_window(
      state = bad_state,
      spoke_id = 2L,
      panel_epoch = 1L,
      panel = panel
    ),
    "not contained in the current panel"
  )

  mismatch_state <- state
  mismatch_state$linking$probe$realized_edges <- mismatch_state$linking$probe$realized_edges[0, , drop = FALSE]
  expect_error(
    pairwiseLLM:::.adaptive_link_probe_resume_validate_current_window(
      state = mismatch_state,
      spoke_id = 2L,
      panel_epoch = 1L,
      panel = panel
    ),
    "do not reconcile"
  )

  wrong_spoke <- state
  wrong_panel <- panel
  wrong_panel$spoke_id[[1L]] <- 99L
  wrong_spoke$linking$probe$panels_by_spoke <- list(`2` = wrong_panel)
  expect_error(
    pairwiseLLM:::.adaptive_link_probe_resume_validate_spoke(wrong_spoke, 2L),
    "different `spoke_id`"
  )

  dup_panel <- state
  dup_rows <- panel[rep(1L, 2L), , drop = FALSE]
  dup_rows$spoke_id <- 2L
  dup_panel$linking$probe$panels_by_spoke <- list(`2` = dup_rows)
  expect_error(
    pairwiseLLM:::.adaptive_link_probe_resume_validate_spoke(dup_panel, 2L),
    "duplicate `pair_key`"
  )

  ctrl_mismatch <- state
  ctrl_mismatch$controller$link_epoch_id_by_spoke$`2` <- 9L
  expect_error(
    pairwiseLLM:::.adaptive_link_probe_resume_validate_spoke(ctrl_mismatch, 2L),
    "does not match persisted panel epoch"
  )

  realized_key_mismatch <- state
  realized_key_mismatch$linking$probe$realized_edges$pair_key[[1L]] <- "missing::pair"
  expect_error(
    pairwiseLLM:::.adaptive_link_probe_resume_validate_spoke(realized_key_mismatch, 2L),
    "include pair keys not present"
  )

  realized_panel_mismatch <- state
  realized_panel_mismatch$linking$probe$realized_edges$probe_panel_id[[1L]] <- "other-panel"
  expect_error(
    pairwiseLLM:::.adaptive_link_probe_resume_validate_spoke(realized_panel_mismatch, 2L),
    "probe_panel_id"
  )

  count_mismatch <- state
  count_mismatch$link_stage_log$probe_edges_realized[[nrow(count_mismatch$link_stage_log)]] <- 99L
  expect_error(
    pairwiseLLM:::.adaptive_link_probe_resume_validate_spoke(count_mismatch, 2L),
    "is inconsistent with canonical realized count"
  )

  expect_true(pairwiseLLM:::.adaptive_link_phase_b_active(state))
  expect_error(
    pairwiseLLM:::.adaptive_phase_a_artifact_item_ids(state, list(items = tibble::tibble()), set_id = 99L),
    "No state items found"
  )

  artifact_global <- list(items = tibble::tibble(global_item_id = c("gs21", "gs22", "gs23")))
  expect_identical(
    pairwiseLLM:::.adaptive_phase_a_artifact_item_ids(state, artifact_global, set_id = 2L),
    c("s21", "s22", "s23")
  )
  expect_error(
    pairwiseLLM:::.adaptive_phase_a_artifact_item_ids(
      state,
      list(items = tibble::tibble(global_item_id = c("gs21", "gs22"))),
      set_id = 2L
    ),
    "item domain mismatch"
  )
  expect_error(
    pairwiseLLM:::.adaptive_phase_a_artifact_draws_for_phase_b_global(state, 99L),
    "requires a Phase A artifact"
  )
  broken_artifact_state <- state
  broken_artifact_state$linking$phase_a$artifacts$`2`$posterior_draws <- matrix(1, nrow = 1L, ncol = 3L)
  expect_error(
    pairwiseLLM:::.adaptive_phase_a_artifact_draws_for_phase_b_global(broken_artifact_state, 2L),
    "at least two draws"
  )

  unnamed_draws_state <- state
  unnamed_draws_state$linking$phase_a$artifacts$`2`$posterior_draws <- matrix(1:12, nrow = 4L, ncol = 3L)
  draws_out <- pairwiseLLM:::.adaptive_phase_a_artifact_draws_for_phase_b_global(unnamed_draws_state, 2L)
  expect_identical(colnames(draws_out), c("s21", "s22", "s23"))

  stats_from_controller <- pairwiseLLM:::.adaptive_phase_b_global_metric_transform_stats(state, spoke_id = 2L)
  expect_identical(stats_from_controller$link_transform_state, "shift_only")
  expect_identical(stats_from_controller$delta_spoke_mean, 0.25)

  missing_delta <- state
  missing_delta$controller$link_refit_stats_by_spoke$`2`$link_transform_state <- "bogus"
  expect_error(
    pairwiseLLM:::.adaptive_phase_b_global_metric_transform_stats(missing_delta, spoke_id = 2L),
    "valid transform state"
  )

  shift_scale_missing_alpha <- state
  shift_scale_missing_alpha$controller$link_refit_stats_by_spoke$`2`$link_transform_state <- "shift_scale"
  shift_scale_missing_alpha$controller$link_refit_stats_by_spoke$`2`$log_alpha_spoke_mean <- NA_real_
  expect_error(
    pairwiseLLM:::.adaptive_phase_b_global_metric_transform_stats(shift_scale_missing_alpha, spoke_id = 2L),
    "requires a finite log-alpha"
  )

  metrics <- pairwiseLLM:::.adaptive_link_probe_metrics_current(state, refit_id = 1L, spoke_id = 2L)
  expect_true(is.finite(metrics$probe_brier))
  expect_identical(metrics$realized_n, 1L)
  expect_true(is.na(pairwiseLLM:::.adaptive_link_probe_pred_rmse_lagged(
    state,
    refit_id = 1L,
    spoke_id = 2L,
    lag_refit_id = 99L,
    epoch_id = 1L
  )))

  state$linking$probe$prediction_cache <- tibble::tibble(
    refit_id = c(1L, 2L),
    spoke_id = c(2L, 2L),
    link_epoch_id = c(1L, 1L),
    pair_key = rep(panel$pair_key[[1L]], 2L),
    pred_prob = c(0.3, 0.5)
  )
  expect_true(is.finite(pairwiseLLM:::.adaptive_link_probe_pred_rmse_lagged(
    state,
    refit_id = 2L,
    spoke_id = 2L,
    lag_refit_id = 1L,
    epoch_id = 1L
  )))
  expect_true(is.na(pairwiseLLM:::.adaptive_link_theta_global_rmse_lagged(
    state,
    spoke_id = 2L,
    hub_id = 1L,
    scope_ids = "s21",
    transform_mode = "shift_only",
    delta_mean = 0.2,
    log_alpha_mean = NA_real_,
    lag_row = tibble::tibble()
  )))
  expect_identical(
    sort(pairwiseLLM:::.adaptive_link_theta_global_scope_ids(state, 2L, "all_spoke_items")),
    c("s21", "s22", "s23")
  )
})

test_that("low-coverage CmdStan, concurrent allocation, selector, and print helpers cover branch gaps", {
  state <- make_lowcov_link_state()
  state <- append_link_step(state, 1L, "h1", "s21", spoke_id = 2L)
  state <- append_link_step(state, 2L, "h2", "s22", spoke_id = 2L)
  state <- add_link_stage_row(state, refit_id = 1L, spoke_id = 2L, link_epoch_id = 1L)

  fit_missing <- list(
    diagnostic_summary = function() tibble::tibble(other = 1L),
    summary = function(variables) tibble::tibble()
  )
  diag_missing <- pairwiseLLM:::.adaptive_link_cmdstan_collect_diagnostics(fit_missing, "delta")
  expect_true(any(grepl("missing num_divergent", diag_missing$notes)))
  expect_true(any(grepl("summary not available", diag_missing$notes)))

  fit_partial <- list(
    diagnostic_summary = function() tibble::tibble(num_divergent = c(0, NA_real_)),
    summary = function(variables) tibble::tibble(rhat = c(NA_real_, 1.01))
  )
  diag_partial <- pairwiseLLM:::.adaptive_link_cmdstan_collect_diagnostics(fit_partial, "delta")
  expect_identical(diag_partial$divergences, 0L)
  expect_true(any(grepl("ess_bulk", diag_partial$notes)))

  expect_error(
    pairwiseLLM:::.adaptive_link_cmdstan_validate_diagnostics(
      list(divergences = NA_integer_, max_rhat = 1, min_ess_bulk = 1000),
      list(divergences_max = 0L, max_rhat = 1.01, min_ess_bulk = 500)
    ),
    "missing or malformed"
  )
  validated <- pairwiseLLM:::.adaptive_link_cmdstan_validate_diagnostics(
    list(divergences = 1L, max_rhat = 1.02, min_ess_bulk = 400),
    list(divergences_max = 0L, max_rhat = 1.01, min_ess_bulk = 500)
  )
  expect_false(validated$diagnostics_divergences_pass)
  expect_false(validated$diagnostics_rhat_pass)
  expect_false(validated$diagnostics_ess_pass)
  expect_true(file.exists(pairwiseLLM:::.adaptive_link_cmdstan_file()))
  expect_error(
    testthat::with_mocked_bindings(
      system.file = function(...) "",
      file.exists = function(path) FALSE,
      pairwiseLLM:::.adaptive_link_cmdstan_file(),
      .package = "base"
    ),
    "Stan model file"
  )
  expect_true(grepl(
    "link_transform_refit-",
    pairwiseLLM:::.adaptive_link_cmdstan_output_basename(tempdir())
  ))

  tiny_targets <- pairwiseLLM:::.adaptive_link_concurrent_targets(
    spoke_stats = list(`2` = list(candidate_count = 1L), `3` = list(candidate_count = 1L)),
    total_pairs = 1L,
    floor_pairs = 1L
  )
  expect_identical(sum(tiny_targets), 1L)
  redistributed <- pairwiseLLM:::.adaptive_link_concurrent_targets(
    spoke_stats = list(
      `2` = list(candidate_count = 1L, utility_mass = 10),
      `3` = list(candidate_count = 3L, utility_mass = 0),
      `4` = list(candidate_count = 3L, utility_mass = 5)
    ),
    total_pairs = 4L,
    floor_pairs = 1L
  )
  expect_identical(sum(redistributed), 4L)
  expect_lte(redistributed[["2"]], 1L)

  backfilled <- pairwiseLLM:::.adaptive_link_stage_backfill_audit_columns(
    tibble::tibble(
      stage_target_anchor_link = 1L,
      stage_target_long_link = 2L,
      stage_target_mid_link = 0L,
      stage_target_local_link = 0L
    )
  )
  expect_true("feasible_stage_capacity_anchor_link" %in% names(backfilled))
  expect_true(isTRUE(pairwiseLLM:::.adaptive_assert_link_stage_rows_completeness(
    tibble::as_tibble(pairwiseLLM:::new_link_stage_log())
  )))
  expect_true(is.na(pairwiseLLM:::.adaptive_link_reconstruct_taper_from_logs(tibble::tibble())))

  state_no_rows <- state
  state_no_rows$link_stage_log <- pairwiseLLM:::new_link_stage_log()
  no_rows <- pairwiseLLM:::.adaptive_print_link_state_line(state_no_rows, list(stopped_spokes = integer()))
  expect_identical(no_rows, character())
  phase_line <- pairwiseLLM:::.adaptive_print_link_phase_line(state)
  expect_true(any(grepl("^linking: phase_b", phase_line)))
  expect_identical(pairwiseLLM:::.adaptive_print_compact_values(c(NA, "")), NA_character_)
  expect_true(pairwiseLLM:::.adaptive_meets_threshold(0.5, 0.5, "ge"))
  expect_true(pairwiseLLM:::.adaptive_meets_threshold(0.5, 0.6, "le"))
  expect_false(pairwiseLLM:::.adaptive_meets_threshold(NA_real_, 0.6, "le"))

  selection_notes <- pairwiseLLM:::.adaptive_progress_selection_notes(
    row = tibble::tibble(
      fallback_rate_since_last_refit = 0.25,
      fallback_used_mode = "refresh",
      starve_rate_since_last_refit = 0.10,
      starvation_reason_mode = "filtered_by_duplicates"
    ),
    link_stage_rows = tibble::tibble(stage_budget_unfilled = 1L, probe_panel_shortfall = 2L, probe_shortfall_reason = "panel_size")
  )
  expect_true(any(grepl("fallback=refresh", selection_notes)))
  expect_true(any(grepl("candidate_starved=", selection_notes)))
  expect_true(any(grepl("probe_shortfall=2", selection_notes)))

  diag_lines <- pairwiseLLM:::.adaptive_progress_diagnostics_lines(
    row = tibble::tibble(
      diagnostics_pass = FALSE,
      divergences = 1L,
      divergences_max_allowed = 0L,
      diagnostics_divergences_pass = FALSE,
      max_rhat = 1.02,
      max_rhat_allowed = 1.01,
      diagnostics_rhat_pass = FALSE,
      min_ess_bulk = 400,
      ess_bulk_required = 500,
      diagnostics_ess_pass = FALSE
    ),
    link_stage_rows = state$link_stage_log
  )
  expect_true(any(grepl("^Diagnostics: global divergences=", diag_lines)))
  expect_true(any(grepl("Diagnostics: spoke=2", diag_lines)))

  phase_a_lines <- pairwiseLLM:::.adaptive_progress_phase_a_lines(
    row = tibble::tibble(
      refit_id = 2L,
      step_id_at_refit = 5L,
      new_pairs_since_last_refit = 3L,
      phase_scope = "phase_a_set",
      phase_scope_set_id = 2L,
      diagnostics_pass = TRUE,
      reliability_EAP_scope = 0.91,
      theta_corr_min = 0.90,
      eap_reliability_min = 0.90,
      rho_theta_scope = 0.80,
      theta_sd_rel_change_max = 0.10,
      delta_sd_theta_scope = 0.11,
      rank_spearman_min = 0.90,
      rho_rank_scope = 0.95,
      lag_eligible_scope = TRUE,
      stop_decision = FALSE
    ),
    thresholds = list(
      eap_reliability_min = 0.90,
      theta_corr_min = 0.90,
      theta_sd_rel_change_max = 0.10,
      rank_spearman_min = 0.90
    )
  )
  expect_true(any(grepl("phase_scope=phase_a_set", phase_a_lines)))
  expect_true(any(grepl("^Blocker: rho_theta_scope$", phase_a_lines)))

  phase_b_none <- pairwiseLLM:::.adaptive_progress_phase_b_spoke_lines(
    link_stage_rows = tibble::tibble(),
    thresholds = list(),
    stability_window_refits = 3L,
    stability_passes_required = 2L
  )
  expect_identical(phase_b_none[[2L]], "  none")
  frozen_lines <- pairwiseLLM:::.adaptive_progress_phase_b_spoke_lines(
    link_stage_rows = tibble::tibble(
      spoke_id = 2L,
      transform_frozen = TRUE,
      transform_frozen_refit_id = 3L,
      link_transform_state = "shift_only"
    ),
    thresholds = list(),
    stability_window_refits = 3L,
    stability_passes_required = 2L
  )
  expect_true(any(grepl("frozen_refit=3", frozen_lines)))
  generic_probe <- pairwiseLLM:::adaptive_progress_step_event(
    tibble::tibble(
      step_id = 9L,
      round_stage = "mid_link",
      run_mode = "link_one_spoke",
      is_probe_step = TRUE,
      is_cross_set = TRUE,
      link_spoke_id = 2L,
      candidate_starved = TRUE,
      status = "ok",
      fallback_used = "base"
    ),
    pairwiseLLM:::.adaptive_progress_config(
      progress = "all",
      progress_redraw_every = 1L,
      progress_show_events = TRUE,
      progress_errors = TRUE
    )
  )
  expect_match(generic_probe, "probe=probe")

  expect_false(pairwiseLLM:::.adaptive_long_link_gate_has_posterior(state))
  state$round_log <- tibble::tibble(diagnostics_pass = TRUE, phase_scope = "global", phase_scope_set_id = NA_integer_)
  expect_true(pairwiseLLM:::.adaptive_long_link_gate_has_posterior(state))
  no_names_fit <- state
  no_names_fit$btl_fit$btl_posterior_draws <- matrix(1:8, nrow = 2L, ncol = 4L)
  expect_true(is.na(pairwiseLLM:::.adaptive_long_link_gate_posterior_prob(no_names_fit, "h1", "h2")))
  expect_identical(
    pairwiseLLM:::.adaptive_link_theta_global_map_for_items(state, state$controller, "missing"),
    stats::setNames(numeric(), character())
  )
  expect_true(is.finite(pairwiseLLM:::.adaptive_link_logdet_spd(diag(2), ridge = -1)))
  expect_true(is.na(pairwiseLLM:::.adaptive_link_d_opt_gain_logdet(diag(1), diag(2))))

  predictive <- pairwiseLLM:::.adaptive_link_attach_predictive_utility(
    candidates = tibble::tibble(i = c("h1", "h2"), j = c("s21", "s22")),
    state = state,
    controller = state$controller,
    spoke_id = 2L
  )
  expect_true(all(c("link_p", "link_u", "link_d_opt_gain") %in% names(predictive)))
  expect_true(is.finite(pairwiseLLM:::.adaptive_link_predictive_prob_oriented(
    state = state,
    controller = state$controller,
    spoke_id = 2L,
    A_id = "h1",
    B_id = "s21"
  )))

  candidates <- tibble::tibble(
    i = c("1", "2"),
    j = c("3", "4"),
    p = c(0.1, 0.9),
    u0 = c(0.2, 0.1)
  )
  selector_state <- adaptive_rank_start(make_test_items(4), seed = 9L)
  selector_state$warm_start_done <- TRUE
  selector_state$round$staged_active <- TRUE
  selector_state$round$stage_index <- 2L
  selector_state$round$stage_order <- pairwiseLLM:::.adaptive_stage_order()
  selector_state$round$stage_quotas <- as.list(stats::setNames(rep.int(1L, 4L), selector_state$round$stage_order))
  selector_state$round$stage_committed <- as.list(stats::setNames(rep.int(0L, 4L), selector_state$round$stage_order))
  selector_state$btl_fit <- make_test_btl_fit(selector_state$item_ids)
  selector_state$round_log <- tibble::tibble(diagnostics_pass = TRUE)
  gated <- pairwiseLLM:::.adaptive_select_stage(
    stage = list(name = "base", pair_type = "long_link", dup_policy = "default", explore_boost = 1),
    state = selector_state,
    config = pairwiseLLM:::adaptive_defaults(4L),
    controller = pairwiseLLM:::.adaptive_controller_resolve(selector_state),
    generation_stage = "long_link",
    round = selector_state$round,
    history = pairwiseLLM:::.adaptive_history_tbl(selector_state),
    counts = pairwiseLLM:::.adaptive_pair_counts(pairwiseLLM:::.adaptive_history_tbl(selector_state), selector_state$item_ids),
    step_id = 1L,
    seed_base = 1L,
    candidates = candidates
  )
  expect_true(is.logical(gated$long_gate_pass))
})

test_that("low-coverage probe panel restoration, run helpers, and cost estimator cover follow-up branches", {
  state <- make_lowcov_link_state()
  panel <- pairwiseLLM:::.adaptive_link_probe_construct_panel(state, state$controller, spoke_id = 2L)
  panel_id <- unique(panel$probe_panel_id)[[1L]]

  resumed <- add_link_stage_row(
    state,
    refit_id = 1L,
    spoke_id = 2L,
    link_epoch_id = 1L,
    probe_panel_id = "legacy-panel",
    probe_edges_realized = 1L,
    probe_edges_planned = nrow(panel)
  )
  resumed$meta$resumed_from_session <- TRUE
  resumed$linking$probe$panels_by_spoke <- list()
  resumed$linking$probe$realized_edges <- tibble::tibble(
    spoke_id = 2L,
    link_epoch_id = 1L,
    pair_key = panel$pair_key[[1L]],
    probe_panel_id = "legacy-panel",
    step_id = 1L,
    pair_id = 1L,
    run_mode = "link_probe_holdout",
    Y = 1L
  )
  normalized <- testthat::with_mocked_bindings(
    .adaptive_link_probe_construct_panel = function(state, controller, spoke_id) panel,
    pairwiseLLM:::.adaptive_link_probe_ensure_panels(
      resumed,
      controller = resumed$controller,
      spoke_ids = 2L
    ),
    .package = "pairwiseLLM"
  )
  expect_identical(normalized$linking$probe$panels_by_spoke$`2`$probe_panel_id[[1L]], panel_id)

  built_fresh <- pairwiseLLM:::.adaptive_link_probe_ensure_panels(
    state,
    controller = state$controller,
    spoke_ids = 2L
  )
  expect_identical(built_fresh$linking$probe$panels_by_spoke$`2`$probe_panel_id[[1L]], panel_id)

  bad_planned <- resumed
  bad_planned$link_stage_log$probe_edges_planned[[nrow(bad_planned$link_stage_log)]] <- nrow(panel) + 1L
  expect_error(
    pairwiseLLM:::.adaptive_link_probe_ensure_panels(
      bad_planned,
      controller = bad_planned$controller,
      spoke_ids = 2L
    ),
    "probe_panel_id"
  )

  panel_state <- state
  dup_panel <- panel[rep(seq_len(min(2L, nrow(panel))), length.out = 2L), , drop = FALSE]
  dup_panel$probe_panel_id <- c("p-one", "p-two")
  dup_panel$pair_key <- c("k-one", "k-two")
  panel_state$linking$probe$panels_by_spoke <- list(`2` = dup_panel)
  expect_identical(
    nrow(pairwiseLLM:::.adaptive_link_probe_panel_for_spoke(panel_state, spoke_id = 2L, epoch_id = 1L)),
    2L
  )

  guard_state <- state
  guard_state$controller$link_budget_refit_id <- NA_integer_
  guard <- testthat::with_mocked_bindings(
    .adaptive_link_effective_active_spokes = function(...) c(2L, 3L),
    .adaptive_link_refit_window_id = function(...) 1L,
    .adaptive_link_budget_map_for_refit = function(...) {
      list(
        `2` = list(B_spoke_refit_budget = 2L),
        `3` = list(B_spoke_refit_budget = 0L)
      )
    },
    .adaptive_link_cross_edges = function(state, spoke_id, last_refit_step = NULL) {
      if (identical(as.integer(spoke_id), 2L)) {
        tibble::tibble()
      } else {
        tibble::tibble(is_probe_step = FALSE)
      }
    },
    pairwiseLLM:::.adaptive_link_probe_active_progress_guard(
      state = guard_state,
      controller = guard_state$controller,
      eligible_spoke_ids = c(2L, 3L)
    ),
    .package = "pairwiseLLM"
  )
  expect_true(guard$block_probes)
  expect_identical(guard$pending_spokes, 2L)
  expect_identical(guard$budgeted_spokes, 2L)

  holdout_state <- add_link_stage_row(state, refit_id = 1L, spoke_id = 2L, link_epoch_id = 1L)
  holdout_state <- add_link_stage_row(holdout_state, refit_id = 1L, spoke_id = 3L, link_epoch_id = 1L)
  next_spoke <- testthat::with_mocked_bindings(
    .adaptive_link_phase_context = function(...) list(active_spokes = c(2L, 3L)),
    .adaptive_link_probe_holdout_total_since_last_refit = function(...) 0L,
    .adaptive_link_probe_active_progress_guard = function(...) list(block_probes = FALSE),
    .adaptive_link_ranked_spokes = function(...) integer(),
    .adaptive_link_probe_effort_plan = function(state, controller, spoke_id) {
      if (identical(as.integer(spoke_id), 2L)) {
        list(
          realized_total = 0L,
          realized_refit = 0L,
          effective_cap = 2L,
          remaining_to_min_start = 1L,
          acceleration_used = TRUE
        )
      } else {
        list(
          realized_total = 0L,
          realized_refit = 0L,
          effective_cap = 2L,
          remaining_to_min_start = 2L,
          acceleration_used = FALSE
        )
      }
    },
    .adaptive_link_probe_panel_for_spoke = function(state, spoke_id, epoch_id = NULL) {
      if (identical(as.integer(spoke_id), 2L)) {
        tibble::tibble(
          probe_panel_id = "p",
          link_epoch_id = 1L,
          pair_key = "k"
        )
      } else {
        tibble::tibble()
      }
    },
    pairwiseLLM:::.adaptive_link_probe_next_holdout_spoke(
      holdout_state,
      controller = holdout_state$controller,
      eligible_spoke_ids = c(2L, 3L)
    ),
    .package = "pairwiseLLM"
  )
  expect_identical(next_spoke, 2L)

  blocked_holdout <- holdout_state
  blocked_holdout$refit_meta$refit_pairs_target_current <- 1L
  expect_true(is.na(pairwiseLLM:::.adaptive_link_probe_next_holdout_spoke(
    blocked_holdout,
    controller = blocked_holdout$controller,
    eligible_spoke_ids = c(2L, 3L)
  )))

  fully_realized <- state
  full_panel <- panel
  full_panel$realized <- TRUE
  fully_realized$linking$probe$panels_by_spoke <- list(`2` = full_panel)
  expect_null(pairwiseLLM:::.adaptive_link_probe_next_pair(fully_realized, spoke_id = 2L, epoch_id = 1L))

  pairs <- tibble::tibble(
    ID1 = paste0("A", seq_len(8)),
    text1 = c("a", "bbbb", "cccc", "dddddddd", "eeeeeeee", "ffffffffffff", "gg", "hhhhhhhhhhhhhhhh"),
    ID2 = paste0("B", seq_len(8)),
    text2 = c("x", "yy", "zzzz", "wwwwwwww", "qqqqqqqq", "rrrrrrrrrrrr", "ss", "tttttttttttttttt")
  )
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  zero_pilot <- estimate_llm_pairs_cost(
    pairs = pairs,
    model = "gpt-4.1-mini",
    backend = "openai",
    endpoint = "chat.completions",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    n_test = 0L,
    test_strategy = "first",
    seed = 123L,
    cost_per_million_input = 1,
    cost_per_million_output = 1,
    progress = FALSE,
    verbose = FALSE,
    .submit_fun = function(...) rlang::abort("pilot should not run when n_test = 0")
  )
  expect_identical(zero_pilot$summary$n_test, 0L)

  stratified <- estimate_llm_pairs_cost(
    pairs = pairs,
    model = "gpt-4.1-mini",
    backend = "openai",
    endpoint = "chat.completions",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    n_test = 6L,
    test_strategy = "stratified_prompt_bytes",
    seed = 123L,
    cost_per_million_input = 1,
    cost_per_million_output = 1,
    progress = FALSE,
    verbose = FALSE,
    .submit_fun = function(pairs, ...) {
      list(
        results = tibble::tibble(
          ID1 = pairs$ID1,
          ID2 = pairs$ID2,
          better_id = pairs$ID1,
          prompt_tokens = seq_len(nrow(pairs)) + 100L,
          completion_tokens = rep(6L, nrow(pairs)),
          status_code = 200L
        ),
        failed_attempts = tibble::tibble()
      )
    }
  )
  expect_identical(stratified$summary$n_test, 6L)
  expect_identical(nrow(stratified$test_pairs), 6L)
})

test_that("low-coverage select and refit helpers cover direct edge branches", {
  state <- make_lowcov_link_state()

  phase_a_state <- state
  phase_a_state$linking$phase_a$phase <- "phase_a"
  phase_a_state$linking$phase_a$active_phase_a_set <- 2L
  phase_a_state$round_log <- tibble::tibble(
    diagnostics_pass = TRUE,
    phase_scope = "global",
    phase_scope_set_id = NA_integer_
  )
  expect_false(testthat::with_mocked_bindings(
    .adaptive_refit_phase_a_scope = function(state) list(active = TRUE, set_id = 2L),
    pairwiseLLM:::.adaptive_long_link_gate_has_posterior(phase_a_state),
    .package = "pairwiseLLM"
  ))
  phase_a_state$round_log <- tibble::tibble(
    diagnostics_pass = TRUE,
    phase_scope = "phase_a_set",
    phase_scope_set_id = 2L
  )
  expect_true(testthat::with_mocked_bindings(
    .adaptive_refit_phase_a_scope = function(state) list(active = TRUE, set_id = 2L),
    pairwiseLLM:::.adaptive_long_link_gate_has_posterior(phase_a_state),
    .package = "pairwiseLLM"
  ))

  prob_state <- state
  draws <- prob_state$btl_fit$btl_posterior_draws
  colnames(draws) <- prob_state$item_ids
  prob_state$btl_fit$btl_posterior_draws <- draws
  prob_state$btl_fit$beta_draws <- 1
  prob_state$btl_fit$epsilon_draws <- c(Inf, -1, 2, NA_real_)
  expect_true(is.finite(pairwiseLLM:::.adaptive_long_link_gate_posterior_prob(prob_state, "h1", "h2")))

  shift_state <- state
  shift_state$controller$link_refit_mode <- "joint_refit"
  shift_state$controller$link_transform_state_by_spoke$`2` <- "shift_scale"
  shift_state$controller$link_refit_stats_by_spoke$`2`$delta_spoke_mean <- NA_real_
  shift_state$controller$link_refit_stats_by_spoke$`2`$log_alpha_spoke_mean <- log(1.5)
  theta_map <- pairwiseLLM:::.adaptive_link_theta_global_map_for_items(
    shift_state,
    shift_state$controller,
    c("h1", "s21")
  )
  expect_true(all(c("h1", "s21") %in% names(theta_map)))
  expect_equal(
    unname(theta_map[["s21"]]),
    exp(log(1.5)) * pairwiseLLM:::.adaptive_link_theta_mean_map(shift_state, set_id = 2L)[["s21"]],
    tolerance = 1e-8
  )

  expect_true(is.na(pairwiseLLM:::.adaptive_link_logdet_spd(matrix(1:6, nrow = 2L))))
  expect_true(is.na(pairwiseLLM:::.adaptive_link_logdet_spd(matrix(c(1, 2, 2, 1), nrow = 2L))))

  no_theta <- pairwiseLLM:::.adaptive_link_attach_predictive_utility(
    candidates = tibble::tibble(i = "missing_a", j = "missing_b"),
    state = state,
    controller = state$controller,
    spoke_id = 2L
  )
  expect_true(all(is.na(no_theta$link_p)))
  expect_true(all(is.na(no_theta$link_u)))

  expect_true(is.na(pairwiseLLM:::.adaptive_link_predictive_prob_oriented(
    state,
    state$controller,
    spoke_id = 2L,
    A_id = NA_character_,
    B_id = "s21"
  )))
  expect_error(
    pairwiseLLM:::.adaptive_link_predictive_prob_oriented(
      state,
      state$controller,
      spoke_id = 2L,
      A_id = "missing",
      B_id = "s21"
    ),
    "subscript out of bounds"
  )

  panel <- pairwiseLLM:::.adaptive_link_probe_construct_panel(state, state$controller, spoke_id = 2L)
  panel_id <- unique(panel$probe_panel_id)[[1L]]
  state$linking$probe$panels_by_spoke <- list(`2` = panel)
  state$linking$probe$realized_edges <- tibble::tibble(
    spoke_id = 2L,
    link_epoch_id = 1L,
    pair_key = panel$pair_key[[1L]],
    probe_panel_id = panel_id,
    step_id = 2L,
    pair_id = 2L,
    run_mode = "link_probe_holdout",
    Y = 1L
  )
  state <- append_link_step(
    state,
    2L,
    panel$hub_item_id[[1L]],
    panel$spoke_item_id[[1L]],
    spoke_id = 2L,
    run_mode = "link_probe_holdout",
    stage = "probe_panel",
    is_probe_step = TRUE,
    is_holdout_probe_step = TRUE
  )
  realized_edges <- pairwiseLLM:::.adaptive_link_probe_edges_realized(state, spoke_id = 2L, epoch_id = 1L)
  expect_identical(nrow(realized_edges), 1L)

  empty_panel_state <- state
  empty_panel_state$linking$probe$panels_by_spoke <- list(`2` = pairwiseLLM:::.adaptive_link_probe_empty_panel())
  expect_identical(
    nrow(pairwiseLLM:::.adaptive_link_probe_edges_realized(empty_panel_state, spoke_id = 2L, epoch_id = 1L)),
    0L
  )

  expect_true(is.na(pairwiseLLM:::.adaptive_link_probe_prior_realized_max(
    tibble::tibble(
      spoke_id = 2L,
      link_epoch_id = 1L,
      refit_id = 1L,
      probe_edges_realized = NA_integer_
    ),
    spoke_id = 2L,
    epoch_id = 1L,
    refit_id = 2L
  )))
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_prior_realized_max(
      tibble::tibble(
        spoke_id = c(2L, 2L),
        link_epoch_id = c(1L, 1L),
        refit_id = c(1L, 2L),
        probe_edges_realized = c(1L, 3L)
      ),
      spoke_id = 2L,
      epoch_id = 1L,
      refit_id = 3L
    ),
    3L
  )

  edges <- tibble::tibble(
    hub_item = c("h1", "h1"),
    spoke_item = c("s21", "missing"),
    spoke_in_A = c(TRUE, FALSE),
    y_spoke = c(1L, 2L)
  )
  hub_theta <- c(h1 = 0.8)
  spoke_theta <- c(s21 = 0.3)
  posterior_draws <- list(
    delta = rep(0.1, 250L),
    log_alpha = 0.2,
    theta_hub = matrix(rep(0.8, 250L), ncol = 1L, dimnames = list(NULL, "h1")),
    theta_spoke = matrix(rep(0.3, 250L), ncol = 1L, dimnames = list(NULL, "s21"))
  )
  cross_edges <- edges
  attr(cross_edges, "judge_params") <- list(beta = Inf, epsilon = Inf)
  expect_true(is.finite(pairwiseLLM:::.adaptive_link_ppc_brier_cross(
    cross_edges = cross_edges,
    hub_theta = hub_theta,
    spoke_theta = spoke_theta,
    delta_mean = 0.1,
    posterior_draws = posterior_draws
  )))

  probs <- pairwiseLLM:::.adaptive_link_cross_probabilities(
    edges = edges,
    hub_theta = hub_theta,
    spoke_theta = spoke_theta,
    delta_mean = 0.1,
    log_alpha_mean = log(2),
    judge_params = list(beta = 0.2, epsilon = 0.1)
  )
  expect_true(is.finite(probs[[1L]]))
  expect_true(is.na(probs[[2L]]))

  expect_true(is.na(pairwiseLLM:::.adaptive_link_probe_brier_for_fit(
    edges = tibble::tibble(
      hub_item = "h1",
      spoke_item = "s21",
      spoke_in_A = TRUE,
      y_spoke = 2L
    ),
    hub_theta = hub_theta,
    spoke_theta = spoke_theta,
    delta_mean = 0.1
  )))
  expect_true(is.na(pairwiseLLM:::.adaptive_link_probe_pred_rmse_lagged_for_fit(
    edges = tibble::tibble(hub_item = "missing", spoke_item = "s21", spoke_in_A = TRUE),
    hub_theta = hub_theta,
    spoke_theta = spoke_theta,
    delta_mean = 0.1,
    log_alpha_mean = NA_real_,
    lag_delta_mean = 0.2,
    lag_log_alpha_mean = NA_real_
  )))
  expect_false(pairwiseLLM:::.adaptive_link_fit_transform_alt_shift_scale(
    tibble::tibble(),
    hub_theta = hub_theta,
    spoke_theta = spoke_theta
  )$converged)
  expect_false(pairwiseLLM:::.adaptive_link_fit_transform_alt_shift_scale(
    tibble::tibble(
      hub_item = "missing",
      spoke_item = "missing",
      y_spoke = 2L,
      spoke_in_A = TRUE
    ),
    hub_theta = hub_theta,
    spoke_theta = spoke_theta
  )$converged)
})

test_that("low-coverage holdout commit, selector fallback, CmdStan wrapper, and forced cost branches are covered", {
  state <- make_lowcov_link_state()
  panel <- pairwiseLLM:::.adaptive_link_probe_construct_panel(state, state$controller, spoke_id = 2L)
  state$linking$probe$panels_by_spoke <- list(`2` = panel)

  selected <- pairwiseLLM:::.adaptive_link_probe_select_holdout(state, step_id = 10L, spoke_id = 2L)
  expect_identical(selected$run_mode, "link_probe_holdout")
  expect_identical(selected$fallback_used, "probe_panel")
  expect_identical(selected$probe_panel_id, panel$probe_panel_id[[1L]])

  committed <- pairwiseLLM:::.adaptive_link_probe_register_commit(
    state,
    tibble::tibble(
      step_id = 10L,
      pair_id = 10L,
      run_mode = "link_probe_holdout",
      is_probe_step = TRUE,
      link_spoke_id = 2L,
      A = selected$A,
      B = selected$B,
      Y = 1L
    )
  )
  expect_identical(nrow(committed$linking$probe$realized_edges), 1L)
  expect_true(any(committed$linking$probe$panels_by_spoke$`2`$realized))

  empty_commit <- state
  empty_commit$linking$probe$panels_by_spoke <- list()
  expect_error(
    pairwiseLLM:::.adaptive_link_probe_register_commit(
      empty_commit,
      tibble::tibble(
        step_id = 10L,
        pair_id = 10L,
        run_mode = "link_probe_holdout",
        is_probe_step = TRUE,
        link_spoke_id = 2L,
        A = selected$A,
        B = selected$B,
        Y = 1L
      )
    ),
    "no current panel"
  )

  selector_state <- adaptive_rank_start(make_test_items(4), seed = 9L)
  selector_state$warm_start_done <- TRUE
  selector_state$round$staged_active <- TRUE
  selector_state$round$stage_index <- 2L
  selector_state$round$stage_order <- pairwiseLLM:::.adaptive_stage_order()
  selector_state$round$stage_quotas <- as.list(stats::setNames(rep.int(1L, 4L), selector_state$round$stage_order))
  selector_state$round$stage_committed <- as.list(stats::setNames(rep.int(0L, 4L), selector_state$round$stage_order))
  selector_state$btl_fit <- make_test_btl_fit(selector_state$item_ids)
  selector_state$round_log <- tibble::tibble(diagnostics_pass = TRUE)
  long_fallback <- testthat::with_mocked_bindings(
    .adaptive_long_link_gate_has_posterior = function(state) TRUE,
    .adaptive_long_link_gate_posterior_prob = function(state, i_id, j_id) NA_real_,
    pairwiseLLM:::.adaptive_select_stage(
      stage = list(name = "base", pair_type = "long_link", dup_policy = "default", explore_boost = 1),
      state = selector_state,
      config = pairwiseLLM:::adaptive_defaults(4L),
      controller = utils::modifyList(
        pairwiseLLM:::.adaptive_controller_resolve(selector_state),
        list(global_identified = TRUE, p_long_low = 0.2, p_long_high = 0.8)
      ),
      generation_stage = "long_link",
      round = selector_state$round,
      history = pairwiseLLM:::.adaptive_history_tbl(selector_state),
      counts = pairwiseLLM:::.adaptive_pair_counts(
        pairwiseLLM:::.adaptive_history_tbl(selector_state),
        selector_state$item_ids
      ),
      step_id = 1L,
      seed_base = 1L,
      candidates = tibble::tibble(
        i = c("1", "2"),
        j = c("3", "4"),
        p = c(0.4, 0.6),
        u0 = c(0.2, 0.1)
      )
    ),
    .package = "pairwiseLLM"
  )
  expect_identical(long_fallback$long_gate_reason, "posterior_unavailable_fallback")

  fake_fit <- list(
    draws = function(variables, format) matrix(1:4, nrow = 2L),
    diagnostic_summary = function() tibble::tibble(num_divergent = c(0, 0)),
    summary = function(variables) tibble::tibble(rhat = 1, ess_bulk = 1000)
  )
  fake_model <- function(path, cpp_options) {
    list(sample = function(...) fake_fit)
  }
  schedule <- pairwiseLLM:::.adaptive_link_cmdstan_schedule(attempt = 3L, n_param = 5L, joint_used = TRUE)
  expect_identical(schedule$chains, 4L)
  expect_gt(schedule$iter_sampling, schedule$iter_warmup)
  expect_error(
    pairwiseLLM:::.adaptive_link_cmdstan_draws_matrix(
      list(draws = function(...) rlang::abort("boom")),
      "delta"
    ),
    "did not return draws"
  )
  fit_result <- pairwiseLLM:::.adaptive_link_fit_transform_cmdstan(
    stan_data = list(N = 1L),
    variable_names = "delta",
    cmdstan = list(iter_warmup = 10L, iter_sampling = 20L, output_dir = tempdir()),
    seed = 123L,
    model_fn = fake_model
  )
  expect_true(is.matrix(fit_result$draws_matrix))
  expect_true(is.list(fit_result$diagnostics))
  expect_error(
    pairwiseLLM:::.adaptive_link_fit_transform_cmdstan(
      stan_data = list(N = 1L),
      variable_names = "delta",
      cmdstan = list(iter_warmup = 10L, iter_sampling = 20L, output_dir = NA_character_),
      seed = 123L,
      model_fn = fake_model
    ),
    "output_dir"
  )
  expect_error(
    pairwiseLLM:::.adaptive_link_fit_transform_cmdstan(
      stan_data = list(N = 1L),
      variable_names = "delta",
      cmdstan = list(iter_warmup = 10L, iter_sampling = 20L, output_dir = tempdir()),
      seed = 123L,
      model_fn = 1
    ),
    "must be a function"
  )
  expect_gte(
    pairwiseLLM:::.adaptive_link_refit_seed(
      tibble::tibble(step_id = c(1, -Inf), y_spoke = c(1L, 2L)),
      transform_mode = "shift_scale",
      link_refit_mode = "joint_refit"
    ),
    1L
  )

  cost_pairs <- tibble::tibble(
    ID1 = paste0("A", seq_len(8)),
    text1 = rep("left", 8L),
    ID2 = paste0("B", seq_len(8)),
    text2 = rep("right", 8L)
  )
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  forced_estimate <- testthat::with_mocked_bindings(
    quantile = function(x, probs, na.rm = TRUE, type = 7) c(0, 2, 4, 6, 8, 10),
    testthat::with_mocked_bindings(
      .prompt_bytes_for_pairs = function(...) c(0, 0, 0, 10, 10, 10, 10, 10),
      .pairwiseLLM_with_seed = function(seed, fn) fn(),
      pairwiseLLM::estimate_llm_pairs_cost(
        pairs = cost_pairs,
        model = "gpt-4.1-mini",
        backend = "openai",
        endpoint = "chat.completions",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        n_test = 6L,
        test_strategy = "stratified_prompt_bytes",
        cost_per_million_input = 1,
        cost_per_million_output = 1,
        progress = FALSE,
        verbose = FALSE,
        .submit_fun = function(pairs, ...) {
          list(
            results = tibble::tibble(
              ID1 = pairs$ID1,
              ID2 = pairs$ID2,
              better_id = pairs$ID1
            ),
            failed_attempts = NULL
          )
        }
      ),
      .package = "pairwiseLLM"
    ),
    .package = "stats"
  )
  expect_identical(forced_estimate$summary$n_test, 6L)

  bound_estimate <- testthat::with_mocked_bindings(
    bind_rows = function(...) tibble::tibble(),
    pairwiseLLM::estimate_llm_pairs_cost(
      pairs = cost_pairs,
      model = "gpt-4.1-mini",
      backend = "openai",
      endpoint = "chat.completions",
      trait_name = td$name,
      trait_description = td$description,
      prompt_template = tmpl,
      n_test = 1L,
      test_strategy = "first",
      cost_per_million_input = 1,
      cost_per_million_output = 1,
      progress = FALSE,
      verbose = FALSE,
      .submit_fun = function(pairs, ...) {
        list(
          results = tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1),
          failed_attempts = NULL
        )
      }
    ),
    .package = "dplyr"
  )
  expect_identical(bound_estimate$summary$n_test, 1L)
})

test_that("low-coverage authoritative link refit covers joint CmdStan happy-path and lock validation", {
  cross_edges <- tibble::tibble(
    hub_item = c("h1", "h2"),
    spoke_item = c("s21", "s22"),
    spoke_in_A = c(TRUE, FALSE),
    y_spoke = c(1L, 0L),
    step_id = c(1L, 2L)
  )
  attr(cross_edges, "within_hub_edges") <- tibble::tibble(
    A_item = c("h1", "missing"),
    B_item = c("h2", "h1"),
    y_A = c(1L, 2L)
  )
  attr(cross_edges, "within_spoke_edges") <- tibble::tibble(
    A_item = c("s21", "missing"),
    B_item = c("s22", "s21"),
    y_A = c(0L, 2L)
  )
  attr(cross_edges, "judge_params") <- list(
    mode = "global_shared",
    scope = "link",
    beta = Inf,
    epsilon = Inf,
    cold_start_fallback_used = TRUE
  )

  hub_theta <- c(h1 = 0.8, h2 = 0.2)
  attr(hub_theta, "theta_sd") <- c(h1 = 0.1, h2 = 0.2)
  attr(hub_theta, "theta_prior_center") <- c(h1 = 0.75, h2 = NA_real_)
  attr(hub_theta, "theta_init") <- c(h1 = 0.85, h2 = NA_real_)

  spoke_theta <- c(s21 = 0.3, s22 = -0.1)
  attr(spoke_theta, "theta_sd") <- c(s21 = 0.15, s22 = 0.25)
  attr(spoke_theta, "theta_init") <- c(s21 = 0.35, s22 = NA_real_)

  fake_cmdstan_fit <- function(stan_data, variable_names, cmdstan, seed, model_fn = NULL) {
    draws <- cbind(
      delta = c(0.10, 0.20),
      log_alpha = c(log(1.1), log(1.2)),
      `theta_hub[1]` = c(0.82, 0.84),
      `theta_hub[2]` = c(0.18, 0.22),
      `theta_spoke[1]` = c(0.31, 0.33),
      `theta_spoke[2]` = c(-0.08, -0.05)
    )
    list(
      draws_matrix = draws,
      diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 1000),
      mcmc_config_used = list(
        chains = 4L,
        parallel_chains = 4L,
        threads_per_chain = 1L,
        cmdstanr_version = "test"
      )
    )
  }

  attr(cross_edges, "refit_contract") <- list(
    link_refit_mode = "joint_refit",
    hub_lock_mode = "soft_lock",
    hub_lock_kappa = 2,
    link_transform_policy = "auto",
    shift_only_theta_treatment = "fixed_eap_plugin_var",
    cmdstan = list(chains = 4L, parallel_chains = 4L, threads_per_chain = 1L),
    cmdstan_fit_fn = fake_cmdstan_fit
  )

  fit <- pairwiseLLM:::.adaptive_link_fit_transform(
    cross_edges = cross_edges,
    hub_theta = hub_theta,
    spoke_theta = spoke_theta,
    transform_mode = "shift_scale"
  )
  expect_true(is.finite(fit$delta_mean))
  expect_true(is.finite(fit$log_alpha_mean))
  expect_identical(names(fit$theta_hub_post), c("h1", "h2"))
  expect_identical(names(fit$theta_spoke_post), c("s21", "s22"))
  expect_true(isTRUE(fit$fit_contract$joint_refit$used))
  expect_identical(fit$fit_contract$judge$beta, 0)
  expect_identical(fit$fit_contract$judge$epsilon, 0)
  expect_identical(fit$fit_contract$lock$hub_lock_mode, "soft_lock")
  expect_true(all(dim(fit$posterior_draws$theta_hub) == c(2L, 2L)))
  expect_true(all(dim(fit$posterior_draws$theta_spoke) == c(2L, 2L)))

  bad_lock <- cross_edges
  attr(bad_lock, "refit_contract") <- utils::modifyList(
    attr(cross_edges, "refit_contract"),
    list(hub_lock_mode = "bogus")
  )
  expect_error(
    pairwiseLLM:::.adaptive_link_fit_transform(
      cross_edges = bad_lock,
      hub_theta = hub_theta,
      spoke_theta = spoke_theta,
      transform_mode = "shift_scale"
    ),
    "Unsupported `hub_lock_mode`"
  )

  bad_cmdstan_fn <- cross_edges
  attr(bad_cmdstan_fn, "refit_contract") <- utils::modifyList(
    attr(cross_edges, "refit_contract"),
    list(cmdstan_fit_fn = 1)
  )
  expect_error(
    pairwiseLLM:::.adaptive_link_fit_transform(
      cross_edges = bad_cmdstan_fn,
      hub_theta = hub_theta,
      spoke_theta = spoke_theta,
      transform_mode = "shift_scale"
    ),
    "cmdstan_fit_fn"
  )

  missing_delta_fit <- function(stan_data, variable_names, cmdstan, seed, model_fn = NULL) {
    list(
      draws_matrix = cbind(log_alpha = c(log(1.1), log(1.2))),
      diagnostics = list(
        divergences = 0L,
        max_rhat = 1,
        min_ess_bulk = 1000,
        diagnostics_divergences_pass = TRUE,
        diagnostics_rhat_pass = TRUE,
        diagnostics_ess_pass = TRUE
      ),
      mcmc_config_used = list(chains = 4L, parallel_chains = 4L, threads_per_chain = 1L)
    )
  }
  missing_delta <- cross_edges
  attr(missing_delta, "refit_contract") <- utils::modifyList(
    attr(cross_edges, "refit_contract"),
    list(cmdstan_fit_fn = missing_delta_fit)
  )
  expect_error(
    pairwiseLLM:::.adaptive_link_fit_transform(
      cross_edges = missing_delta,
      hub_theta = hub_theta,
      spoke_theta = spoke_theta,
      transform_mode = "shift_only"
    ),
    "missing delta draws"
  )

  missing_alpha_fit <- function(stan_data, variable_names, cmdstan, seed, model_fn = NULL) {
    list(
      draws_matrix = cbind(delta = c(0.1, 0.1)),
      diagnostics = list(
        divergences = 0L,
        max_rhat = 1,
        min_ess_bulk = 1000,
        diagnostics_divergences_pass = TRUE,
        diagnostics_rhat_pass = TRUE,
        diagnostics_ess_pass = TRUE
      ),
      mcmc_config_used = list(chains = 4L, parallel_chains = 4L, threads_per_chain = 1L)
    )
  }
  missing_alpha <- cross_edges
  attr(missing_alpha, "refit_contract") <- utils::modifyList(
    attr(cross_edges, "refit_contract"),
    list(cmdstan_fit_fn = missing_alpha_fit)
  )
  expect_error(
    pairwiseLLM:::.adaptive_link_fit_transform(
      cross_edges = missing_alpha,
      hub_theta = hub_theta,
      spoke_theta = spoke_theta,
      transform_mode = "shift_scale"
    ),
    "missing log_alpha draws"
  )

  missing_hub_fit <- function(stan_data, variable_names, cmdstan, seed, model_fn = NULL) {
    list(
      draws_matrix = cbind(
        delta = c(0.1, 0.2),
        log_alpha = c(log(1.1), log(1.2)),
        `theta_spoke[1]` = c(0.3, 0.35),
        `theta_spoke[2]` = c(-0.1, -0.05)
      ),
      diagnostics = list(
        divergences = 0L,
        max_rhat = 1,
        min_ess_bulk = 1000,
        diagnostics_divergences_pass = TRUE,
        diagnostics_rhat_pass = TRUE,
        diagnostics_ess_pass = TRUE
      ),
      mcmc_config_used = list(chains = 4L, parallel_chains = 4L, threads_per_chain = 1L)
    )
  }
  missing_hub <- cross_edges
  attr(missing_hub, "refit_contract") <- utils::modifyList(
    attr(cross_edges, "refit_contract"),
    list(cmdstan_fit_fn = missing_hub_fit)
  )
  expect_error(
    pairwiseLLM:::.adaptive_link_fit_transform(
      cross_edges = missing_hub,
      hub_theta = hub_theta,
      spoke_theta = spoke_theta,
      transform_mode = "shift_scale"
    ),
    "missing theta_hub draws"
  )

  missing_spoke_fit <- function(stan_data, variable_names, cmdstan, seed, model_fn = NULL) {
    list(
      draws_matrix = cbind(
        delta = c(0.1, 0.2),
        log_alpha = c(log(1.1), log(1.2)),
        `theta_hub[1]` = c(0.82, 0.84),
        `theta_hub[2]` = c(0.18, 0.22)
      ),
      diagnostics = list(
        divergences = 0L,
        max_rhat = 1,
        min_ess_bulk = 1000,
        diagnostics_divergences_pass = TRUE,
        diagnostics_rhat_pass = TRUE,
        diagnostics_ess_pass = TRUE
      ),
      mcmc_config_used = list(chains = 4L, parallel_chains = 4L, threads_per_chain = 1L)
    )
  }
  missing_spoke <- cross_edges
  attr(missing_spoke, "refit_contract") <- utils::modifyList(
    attr(cross_edges, "refit_contract"),
    list(cmdstan_fit_fn = missing_spoke_fit)
  )
  expect_error(
    pairwiseLLM:::.adaptive_link_fit_transform(
      cross_edges = missing_spoke,
      hub_theta = hub_theta,
      spoke_theta = spoke_theta,
      transform_mode = "shift_scale"
    ),
    "missing theta_spoke draws"
  )
})

test_that("low-coverage Phase B stage row builder covers canonical refit row assembly", {
  state <- make_lowcov_link_state()
  state <- append_link_step(state, 1L, "h1", "s21", spoke_id = 2L, stage = "anchor_link")
  state <- append_link_step(state, 2L, "h2", "s22", spoke_id = 2L, stage = "long_link")

  panel <- pairwiseLLM:::.adaptive_link_probe_construct_panel(state, state$controller, spoke_id = 2L)
  state$linking$probe$panels_by_spoke <- list(`2` = panel)
  state$linking$probe$realized_edges <- tibble::tibble(
    spoke_id = 2L,
    link_epoch_id = 1L,
    pair_key = panel$pair_key[[1L]],
    probe_panel_id = panel$probe_panel_id[[1L]],
    step_id = 3L,
    pair_id = 3L,
    run_mode = "link_probe_holdout",
    hub_item_id = panel$hub_item_id[[1L]],
    spoke_item_id = panel$spoke_item_id[[1L]],
    Y = 1L
  )
  state$controller$link_budget_refit_id <- 1L
  state$controller$link_budget_map <- list(
    `2` = list(B_spoke_refit_budget = 2L, B_spoke_refit_budget_source = "concurrent_allocator"),
    `3` = list(B_spoke_refit_budget = 1L, B_spoke_refit_budget_source = "concurrent_allocator")
  )
  state$controller$link_d_opt_it_by_spoke <- list(
    `1::2` = list(it = matrix(1, nrow = 1L, ncol = 1L), it_n_pairs_accumulated = 1L, it_logdet_start = 0)
  )
  state$round_log <- tibble::tibble(diagnostics_pass = TRUE)

  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  expect_true(nrow(rows) >= 1L)
  expect_true(all(c(
    "probe_panel_id",
    "B_spoke_refit_budget",
    "stage_target_anchor_link",
    "stage_realized_anchor_link"
  ) %in% names(rows)))
  expect_true(any(as.integer(rows$spoke_id) == 2L))
})

test_that("low-coverage early adaptive_btl_refit helpers cover remaining guard branches", {
  state <- make_lowcov_link_state()

  inactive_phase_a <- state
  inactive_phase_a$linking$phase_a$phase <- "phase_a"
  inactive_phase_a$linking$phase_a$active_phase_a_set <- NA_integer_
  expect_identical(
    pairwiseLLM:::.adaptive_refit_phase_a_scope(inactive_phase_a),
    list(active = FALSE, set_id = NA_integer_)
  )

  bad_draw_count <- state
  bad_draw_count$linking$phase_a$artifacts$`2`$posterior_draws <- matrix(1:8, nrow = 4L, ncol = 2L)
  expect_error(
    pairwiseLLM:::.adaptive_phase_a_artifact_draws_for_phase_b_global(bad_draw_count, set_id = 2L),
    "item count"
  )
  missing_named_cols <- state
  bad_named <- matrix(1:8, nrow = 4L, ncol = 2L)
  colnames(bad_named) <- c("s21", "s22")
  missing_named_cols$linking$phase_a$artifacts$`2`$posterior_draws <- bad_named
  expect_error(
    pairwiseLLM:::.adaptive_phase_a_artifact_draws_for_phase_b_global(missing_named_cols, set_id = 2L),
    "missing required item ids"
  )

  missing_delta <- state
  missing_delta$controller$link_refit_stats_by_spoke$`2`$link_transform_state <- "shift_only"
  missing_delta$controller$link_refit_stats_by_spoke$`2`$delta_spoke_mean <- NA_real_
  expect_error(
    pairwiseLLM:::.adaptive_phase_b_global_metric_transform_stats(missing_delta, spoke_id = 2L),
    "requires a finite delta"
  )

  non_phase_b <- adaptive_rank_start(make_test_items(4), seed = 1L)
  expect_null(pairwiseLLM:::.adaptive_phase_b_global_metric_draws(non_phase_b))
  empty_required <- state
  empty_required$linking$phase_a$required_sets <- integer()
  expect_error(
    pairwiseLLM:::.adaptive_phase_b_global_metric_draws(empty_required),
    "required_sets"
  )
  incomplete_domain <- state
  incomplete_domain$linking$phase_a$required_sets <- c(1L, 2L)
  incomplete_domain$linking$phase_a$artifacts$`1`$items <- tibble::tibble(item_id = c("h1", "h2", "h3"))
  colnames(incomplete_domain$linking$phase_a$artifacts$`1`$posterior_draws) <- c("h1", "h2", "h3")
  incomplete_domain$linking$phase_a$artifacts$`2`$items <- tibble::tibble(item_id = c("s21", "s22", "s23"))
  colnames(incomplete_domain$linking$phase_a$artifacts$`2`$posterior_draws) <- c("s21", "s22", "s23")
  incomplete_domain$controller$link_refit_stats_by_spoke$`2`$link_transform_state <- "shift_only"
  incomplete_domain$controller$link_refit_stats_by_spoke$`2`$delta_spoke_mean <- 0.1
  expect_error(
    pairwiseLLM:::.adaptive_phase_b_global_metric_draws(incomplete_domain),
    "full runtime item domain"
  )
  valid_history_state <- state
  valid_history_state$linking$phase_a$artifacts$`1`$items <- tibble::tibble(item_id = c("h1", "h2", "h3"))
  colnames(valid_history_state$linking$phase_a$artifacts$`1`$posterior_draws) <- c("h1", "h2", "h3")
  valid_history_state$linking$phase_a$artifacts$`2`$items <- tibble::tibble(item_id = c("s21", "s22", "s23"))
  colnames(valid_history_state$linking$phase_a$artifacts$`2`$posterior_draws) <- c("s21", "s22", "s23")
  valid_history_state$linking$phase_a$artifacts$`3`$items <- tibble::tibble(item_id = c("s31", "s32"))
  colnames(valid_history_state$linking$phase_a$artifacts$`3`$posterior_draws) <- c("s31", "s32")
  valid_history_state$controller$link_refit_stats_by_spoke$`2`$link_transform_state <- "shift_only"
  valid_history_state$controller$link_refit_stats_by_spoke$`2`$delta_spoke_mean <- 0.1
  valid_history_state$controller$link_refit_stats_by_spoke$`3`$link_transform_state <- "shift_scale"
  valid_history_state$controller$link_refit_stats_by_spoke$`3`$delta_spoke_mean <- -0.1
  valid_history_state$controller$link_refit_stats_by_spoke$`3`$log_alpha_spoke_mean <- log(1.1)
  expect_error(
    pairwiseLLM:::.adaptive_phase_b_global_metric_history_update(valid_history_state, refit_id = 0L),
    "positive `refit_id`"
  )

  scope_empty <- state
  scope_empty <- append_link_step(scope_empty, 1L, "h1", "h2", spoke_id = 2L, run_mode = "link_multi_spoke")
  expect_identical(
    nrow(pairwiseLLM:::.adaptive_results_from_step_log(scope_empty, scope_ids = c("s21", "s22"))),
    0L
  )
  phase_ready <- state
  phase_ready$step_log <- tibble::tibble(
    step_id = 1L,
    timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    pair_id = 1L,
    A = 1L,
    B = 2L,
    Y = 1L
  )
  phase_ready$linking$phase_a$ready_for_phase_b <- TRUE
  phase_ready$linking$phase_a$phase_b_started_at_step <- NA_integer_
  phase_results <- pairwiseLLM:::.adaptive_results_from_step_log(phase_ready)
  expect_true(all(phase_results$phase == "phase3"))

  no_ts <- state
  no_ts$trueskill_state <- NULL
  expect_true(is.na(pairwiseLLM:::.adaptive_ts_btl_rank_spearman(no_ts, c(h1 = 1))))
  flat_theta <- stats::setNames(rep(1, length(state$item_ids)), state$item_ids)
  expect_true(is.na(pairwiseLLM:::.adaptive_ts_btl_rank_spearman(state, flat_theta)))

  invalid_epoch <- state
  invalid_epoch$controller$link_epoch_start_step_by_spoke <- list(`2` = 0L)
  invalid_epoch$linking$phase_a$phase_b_started_at_step <- 5L
  expect_identical(pairwiseLLM:::.adaptive_link_epoch_start_step_for_spoke(invalid_epoch, 2L), 5L)

  decomp <- pairwiseLLM:::.adaptive_link_reliability_decomposition(
    mu_vals = 1,
    var_vals = 0.1,
    var_mu_epsilon = 1e-6,
    total_var_epsilon = 1e-6
  )
  expect_false(decomp$defined)
  expect_true(is.finite(pairwiseLLM:::.adaptive_link_delta_sd_max_derived(state, hub_id = 1L, delta_sd_mult = 2)))

  empty_probe <- state
  empty_probe$linking$probe$prediction_cache <- tibble::tibble()
  empty_probe$linking$probe$realized_edges <- tibble::tibble()
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_metrics_current(empty_probe, refit_id = 1L, spoke_id = 2L),
    list(probe_brier = NA_real_, realized_n = 0L)
  )

  phase_specific <- state
  phase_specific$controller$judge_param_mode <- "phase_specific"
  phase_specific$btl_fit$beta_within_mean <- 0.2
  phase_specific$btl_fit$epsilon_within_mean <- 0.1
  phase_specific$btl_fit$beta_link_mean <- NA_real_
  phase_specific$btl_fit$epsilon_link_mean <- NA_real_
  judge_fallback <- pairwiseLLM:::.adaptive_link_judge_params(
    phase_specific,
    phase_specific$controller,
    scope = "link",
    allow_cold_start_fallback = TRUE,
    expected_link_params = FALSE
  )
  expect_true(judge_fallback$cold_start_fallback_used)
  expect_identical(judge_fallback$beta, 0.2)
  expect_identical(judge_fallback$epsilon, 0.1)
  phase_specific$btl_fit$beta_within_mean <- NA_real_
  phase_specific$btl_fit$epsilon_within_mean <- NA_real_
  expect_error(
    pairwiseLLM:::.adaptive_link_judge_params(
      phase_specific,
      phase_specific$controller,
      scope = "within",
      allow_cold_start_fallback = FALSE,
      expected_link_params = TRUE
    ),
    "beta_within_mean"
  )

  within_set_state <- adaptive_rank_start(make_test_items(4), seed = 2L)
  expect_false(pairwiseLLM:::.adaptive_link_phase_b_startup_gap_for_spoke(within_set_state, 2L))
  no_stats <- state
  no_stats$controller$link_refit_stats_by_spoke <- list()
  no_stats$step_log <- pairwiseLLM:::new_step_log()
  expect_true(pairwiseLLM:::.adaptive_link_phase_b_startup_gap_for_spoke(no_stats, 2L))

  missing_cols_state <- state
  missing_cols_state$step_log <- tibble::tibble()
  expect_identical(
    nrow(pairwiseLLM:::.adaptive_link_cross_edges(missing_cols_state, spoke_id = 2L)),
    0L
  )
  no_match_state <- state
  no_match_state <- append_link_step(no_match_state, 1L, "h1", "s31", spoke_id = 2L)
  expect_identical(
    nrow(pairwiseLLM:::.adaptive_link_cross_edges(no_match_state, spoke_id = 2L)),
    0L
  )
  cross_state <- state
  cross_state <- append_link_step(cross_state, 1L, "s21", "h1", Y = 0L, spoke_id = 2L)
  cross_edges <- pairwiseLLM:::.adaptive_link_cross_edges(cross_state, spoke_id = 2L)
  expect_identical(cross_edges$hub_item[[1L]], "h1")
  expect_identical(cross_edges$spoke_item[[1L]], "s21")

  empty_within <- pairwiseLLM:::.adaptive_link_within_edges(missing_cols_state, set_id = 1L)
  expect_identical(nrow(empty_within), 0L)
  invalid_within <- state
  invalid_within$step_log <- pairwiseLLM:::append_step_log(
    invalid_within$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      pair_id = 1L,
      A = 1L,
      B = 2L,
      Y = 2L,
      set_i = 1L,
      set_j = 1L
    )
  )
  expect_identical(nrow(pairwiseLLM:::.adaptive_link_within_edges(invalid_within, set_id = 1L)), 0L)
})

test_that("low-coverage late adaptive_btl_refit helper notes and score guards are covered", {
  state <- make_lowcov_link_state()

  too_small <- pairwiseLLM:::.adaptive_link_global_score_stats_active(
    state = state,
    active_ids = "h1",
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = 0
  )
  expect_false(too_small$defined)

  delta_na <- pairwiseLLM:::.adaptive_link_global_score_stats_active(
    state = state,
    active_ids = c("h1", "s21"),
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = NA_real_
  )
  expect_true(all(is.na(delta_na$mean_map)))

  alpha_na <- pairwiseLLM:::.adaptive_link_global_score_stats_active(
    state = state,
    active_ids = c("h1", "s21"),
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_scale",
    delta_mean = 0.1,
    log_alpha_mean = NA_real_
  )
  expect_true(all(is.na(alpha_na$mean_map)))

  joint_stats <- pairwiseLLM:::.adaptive_link_global_score_stats_active(
    state = state,
    active_ids = c("h1", "s21", "missing_item"),
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_scale",
    delta_mean = 0.1,
    log_alpha_mean = log(1.1),
    fit = list(
      theta_hub_post = c(h1 = 0.8, h2 = 0.2, h3 = -0.2),
      theta_spoke_post = c(s21 = 0.3, s22 = -0.1, s23 = -0.4),
      posterior_draws = list(
        delta = c(0.1, 0.2),
        log_alpha = c(log(1.1), log(1.2)),
        theta_hub = matrix(
          c(0.8, 0.85, 0.2, 0.25, -0.2, -0.15),
          nrow = 2L,
          dimnames = list(NULL, c("h1", "h2", "h3"))
        ),
        theta_spoke = matrix(
          c(0.3, 0.32, -0.1, -0.08, -0.4, -0.35),
          nrow = 2L,
          dimnames = list(NULL, c("s21", "s22", "s23"))
        )
      )
    ),
    refit_mode = "joint_refit",
    hub_lock_mode = "soft_lock",
    shift_only_theta_treatment = "fixed_eap_plugin_var"
  )
  expect_true(is.finite(joint_stats$mean_map[["h1"]]))
  expect_true(is.na(joint_stats$mean_map[["missing_item"]]))

  diag_notes <- pairwiseLLM:::.adaptive_link_cmdstan_collect_diagnostics(
    list(
      diagnostic_summary = function() tibble::tibble(num_divergent = c(NA_real_, Inf)),
      summary = function(variables) tibble::tibble(ess_bulk = c(NA_real_, NA_real_))
    ),
    variables = "delta"
  )
  expect_true(any(grepl("Divergence count not finite", diag_notes$notes)))
  expect_true(any(grepl("missing rhat", diag_notes$notes)))
  expect_true(any(grepl("ESS bulk values missing or non-finite", diag_notes$notes)))

  panel <- pairwiseLLM:::.adaptive_link_probe_construct_panel(state, state$controller, spoke_id = 2L)
  state$linking$probe$panels_by_spoke <- list(`2` = panel)
  state$linking$probe$realized_edges <- tibble::tibble(
    spoke_id = 2L,
    link_epoch_id = 1L,
    pair_key = panel$pair_key[[1L]],
    probe_panel_id = panel$probe_panel_id[[1L]],
    step_id = 1L,
    pair_id = 1L,
    run_mode = "link_probe_holdout",
    hub_item_id = panel$hub_item_id[[1L]],
    spoke_item_id = panel$spoke_item_id[[1L]],
    Y = 1L
  )
  expect_identical(
    nrow(pairwiseLLM:::.adaptive_link_probe_edges_realized(state, spoke_id = 2L, epoch_id = 1L)),
    0L
  )
})

test_that("low-coverage linking refit update covers escalation path", {
  state <- make_lowcov_link_state()
  state$round_log <- tibble::tibble(dummy = 1L)
  state <- add_link_stage_row(
    state,
    refit_id = 1L,
    spoke_id = 2L,
    link_epoch_id = 1L,
    probe_edges_realized = 1L,
    probe_edges_planned = 1L
  )
  state$link_stage_log$probe_panel_id[[1L]] <- "panel-stable"
  state$link_stage_log$delta_spoke_mean[[1L]] <- 0.05
  state$link_stage_log$log_alpha_spoke_mean[[1L]] <- 0.02
  state$controller$min_refits_in_phase_b <- 1L
  state$controller$probe_edges_min_for_stop <- 1L
  state$controller$link_transform_escalation_window_refits <- 1L
  state$controller$link_transform_escalation_passes_required <- 1L
  state$controller$stability_window_refits <- 1L
  state$controller$stability_passes_required <- 1L
  state$controller$shift_scale_min_cross_set_edges <- 1L
  state$controller$shift_scale_min_distinct_spoke_items_per_bin <- 1L
  state$controller$spoke_quantile_coverage_bins <- 1L
  state$controller$probe_brier_delta_min <- 0.001
  state$controller$logalpha_sd_guardrail <- 0.2
  state$controller$link_epoch_start_step_by_spoke <- list(`2` = 1L)
  state$config$btl_config$stability_lag <- 1L

  updated <- testthat::with_mocked_bindings(
    .adaptive_link_phase_context = function(...) list(phase = "phase_b", active_spokes = 2L),
    .adaptive_link_spoke_ids = function(...) 2L,
    .adaptive_link_probe_panel_for_spoke = function(...) {
      tibble::tibble(probe_panel_id = "panel-stable")
    },
    .adaptive_link_probe_realized_count = function(...) 1L,
    .adaptive_link_phase_a_theta_map = function(state, set_id, field) {
      if (identical(as.integer(set_id), 1L)) {
        if (identical(field, "theta_raw_mean")) {
          return(c(h1 = 0.8, h2 = 0.2, h3 = -0.2))
        }
        return(c(h1 = 0.1, h2 = 0.1, h3 = 0.1))
      }
      if (identical(field, "theta_raw_mean")) {
        return(c(s21 = 0.3, s22 = -0.1, s23 = -0.4))
      }
      c(s21 = 0.1, s22 = 0.1, s23 = 0.1)
    },
    .adaptive_link_cross_edges = function(state, spoke_id, last_refit_step = NULL) {
      tibble::tibble(
        spoke_item = c("s21", "s22"),
        hub_item = c("h1", "h2"),
        y_spoke = c(1L, 0L),
        step_id = c(1L, 2L),
        spoke_in_A = c(TRUE, TRUE),
        run_mode = c("link_multi_spoke", "link_multi_spoke"),
        is_probe_step = c(FALSE, FALSE)
      )
    },
    .adaptive_link_phase_b_startup_gap_for_spoke = function(...) FALSE,
    .adaptive_link_judge_params = function(...) list(mode = "global_shared", scope = "link", beta = 0, epsilon = 0),
    .adaptive_link_within_edges = function(...) tibble::tibble(
      A_item = character(),
      B_item = character(),
      y_A = integer(),
      step_id = integer()
    ),
    .adaptive_link_fit_transform = function(...) list(
      delta_mean = 0.1,
      delta_sd = 0.01,
      log_alpha_mean = 0.05,
      log_alpha_sd = NA_real_,
      theta_hub_post = c(h1 = 0.8, h2 = 0.2, h3 = -0.2),
      theta_spoke_post = c(s21 = 0.3, s22 = -0.1, s23 = -0.4),
      posterior_draws = list(),
      diagnostics = list(
        divergences = 0L,
        max_rhat = 1,
        min_ess_bulk = 1000,
        diagnostics_divergences_pass = TRUE,
        diagnostics_rhat_pass = TRUE,
        diagnostics_ess_pass = TRUE
      ),
      fit_contract = list(
        estimation_method = "cmdstan_hmc",
        uncertainty_approximation = "cmdstan_posterior_draws"
      )
    ),
    .adaptive_link_active_item_ids = function(...) list(
      active_all = c("h1", "h2", "s21", "s22"),
      active_hub = c("h1", "h2"),
      active_spoke = c("s21", "s22")
    ),
    .adaptive_link_global_score_stats_active = function(...) list(reliability = 0.95, V_mu = 0.2, V_post = 0.05),
    .adaptive_link_reliability_transformed_active = function(...) NA_real_,
    .adaptive_link_transform_theta_mean_for_spoke = function(...) {
      stats::setNames(c(0.8, 0.2, 0.3, -0.1), c("h1", "h2", "s21", "s22"))
    },
    .adaptive_link_ts_btl_rank_spearman_active = function(...) 0.99,
    .adaptive_link_rank_stability_lagged = function(...) {
      list(lag_eligible = TRUE, rho_rank_lagged = 0.99, rho_rank_lagged_pass = TRUE)
    },
    .adaptive_link_theta_global_scope_ids = function(...) c("s21", "s22"),
    .adaptive_link_theta_global_rmse_lagged = function(...) 0.001,
    .adaptive_link_probe_edges_realized = function(...) tibble::tibble(
      hub_item = "h1",
      spoke_item = "s21",
      spoke_in_A = TRUE,
      y_spoke = 1L
    ),
    .adaptive_link_probe_brier_for_fit = function(edges,
                                                  hub_theta,
                                                  spoke_theta,
                                                  delta_mean,
                                                  log_alpha_mean,
                                                  judge_params) {
      if (isTRUE(all.equal(delta_mean, 0.2))) {
        return(0.04)
      }
      0.10
    },
    .adaptive_link_probe_pred_rmse_lagged_for_fit = function(...) 0.001,
    .adaptive_link_phase_b_routing_scores = function(...) c(s21 = 0.2, s22 = 0.8),
    .adaptive_link_probe_quantile_bins = function(items, scores, bins) stats::setNames(rep(1L, length(items)), items),
    .adaptive_link_fit_transform_alt_shift_scale = function(...) list(
      converged = TRUE,
      delta_mean = 0.2,
      log_alpha_mean = 0.1,
      log_alpha_sd = 0.01,
      fit_method = "map_laplace_hessian",
      uncertainty_approximation = "laplace_hessian"
    ),
    .adaptive_link_epoch_signature_components = function(...) list(sig = "x"),
    .adaptive_link_epoch_signature_string = function(...) "sig",
    .adaptive_link_stop_blockers = function(...) list(codes = character()),
    .adaptive_link_budget_map_for_refit = function(...) {
      list(`2` = list(B_spoke_refit_budget = 2L, B_spoke_refit_budget_source = "concurrent_allocator"))
    },
    pairwiseLLM:::.adaptive_linking_refit_update_state(
      state = state,
      refit_context = list(last_refit_step = 0L)
    ),
    .package = "pairwiseLLM"
  )

  expect_identical(updated$controller$link_transform_state_by_spoke$`2`, "shift_scale")
  expect_identical(updated$controller$link_epoch_id_by_spoke$`2`, 2L)
  expect_true(is.finite(updated$controller$link_refit_stats_by_spoke$`2`$probe_brier))
  expect_true(isTRUE(updated$controller$link_refit_stats_by_spoke$`2`$link_stop_gate_open))
  expect_true(isTRUE(updated$controller$link_refit_stats_by_spoke$`2`$escalated_this_refit))
  expect_identical(
    updated$controller$link_refit_stats_by_spoke$`2`$lag_domain_reset_reason,
    "transform_state_change"
  )
  expect_identical(updated$controller$link_refit_stats_by_spoke$`2`$reliability_link_global, 0.95)
  expect_identical(updated$controller$link_refit_stats_by_spoke$`2`$probe_brier_shift_scale, 0.04)
  expect_false(updated$controller$link_refit_stats_by_spoke$`2`$link_stop_eligible)
})

test_that("low-coverage linking refit update covers probe panel mismatch failures", {
  base_state <- make_lowcov_link_state()
  base_state <- add_link_stage_row(
    base_state,
    refit_id = 1L,
    spoke_id = 2L,
    link_epoch_id = 1L,
    probe_panel_id = "panel-old",
    probe_edges_realized = 0L,
    probe_edges_planned = 1L
  )

  resumed_state <- base_state
  resumed_state$meta$resumed_from_session <- TRUE

  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_link_phase_context = function(...) list(phase = "phase_b", active_spokes = 2L),
      .adaptive_link_spoke_ids = function(...) 2L,
      .adaptive_link_probe_panel_for_spoke = function(...) {
        tibble::tibble(probe_panel_id = "panel-new")
      },
      .adaptive_link_phase_a_theta_map = function(state, set_id, field) {
        if (identical(as.integer(set_id), 1L)) {
          if (identical(field, "theta_raw_mean")) {
            return(c(h1 = 0.8, h2 = 0.2, h3 = -0.2))
          }
          return(c(h1 = 0.1, h2 = 0.1, h3 = 0.1))
        }
        if (identical(field, "theta_raw_mean")) {
          return(c(s21 = 0.3, s22 = -0.1, s23 = -0.4))
        }
        c(s21 = 0.1, s22 = 0.1, s23 = 0.1)
      },
      .adaptive_link_cross_edges = function(state, spoke_id, last_refit_step = NULL) {
        tibble::tibble(
          spoke_item = c("s21", "s22"),
          hub_item = c("h1", "h2"),
          y_spoke = c(1L, 0L),
          step_id = c(1L, 2L),
          spoke_in_A = c(TRUE, TRUE),
          run_mode = c("link_multi_spoke", "link_multi_spoke"),
          is_probe_step = c(FALSE, FALSE)
        )
      },
      .adaptive_link_phase_b_startup_gap_for_spoke = function(...) FALSE,
      .adaptive_link_judge_params = function(...) {
        list(mode = "global_shared", scope = "link", beta = 0, epsilon = 0)
      },
      .adaptive_link_within_edges = function(...) tibble::tibble(
        A_item = character(),
        B_item = character(),
        y_A = integer(),
        step_id = integer()
      ),
      .adaptive_link_fit_transform = function(...) list(
        delta_mean = 0.1,
        delta_sd = 0.01,
        log_alpha_mean = 0.05,
        log_alpha_sd = NA_real_,
        theta_hub_post = c(h1 = 0.8, h2 = 0.2, h3 = -0.2),
        theta_spoke_post = c(s21 = 0.3, s22 = -0.1, s23 = -0.4),
        posterior_draws = list(),
        diagnostics = list(
          divergences = 0L,
          max_rhat = 1,
          min_ess_bulk = 1000,
          diagnostics_divergences_pass = TRUE,
          diagnostics_rhat_pass = TRUE,
          diagnostics_ess_pass = TRUE
        ),
        fit_contract = list(
          estimation_method = "cmdstan_hmc",
          uncertainty_approximation = "cmdstan_posterior_draws"
        )
      ),
      pairwiseLLM:::.adaptive_linking_refit_update_state(
        state = resumed_state,
        refit_context = list(last_refit_step = 0L)
      ),
      .package = "pairwiseLLM"
    ),
    "refusing to rebuild the panel mid-epoch"
  )

  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_link_phase_context = function(...) list(phase = "phase_b", active_spokes = 2L),
      .adaptive_link_spoke_ids = function(...) 2L,
      .adaptive_link_probe_panel_for_spoke = function(...) {
        tibble::tibble(probe_panel_id = "panel-new")
      },
      .adaptive_link_probe_construct_panel = function(...) tibble::tibble(),
      .adaptive_link_phase_a_theta_map = function(state, set_id, field) {
        if (identical(as.integer(set_id), 1L)) {
          if (identical(field, "theta_raw_mean")) {
            return(c(h1 = 0.8, h2 = 0.2, h3 = -0.2))
          }
          return(c(h1 = 0.1, h2 = 0.1, h3 = 0.1))
        }
        if (identical(field, "theta_raw_mean")) {
          return(c(s21 = 0.3, s22 = -0.1, s23 = -0.4))
        }
        c(s21 = 0.1, s22 = 0.1, s23 = 0.1)
      },
      .adaptive_link_cross_edges = function(state, spoke_id, last_refit_step = NULL) {
        tibble::tibble(
          spoke_item = c("s21", "s22"),
          hub_item = c("h1", "h2"),
          y_spoke = c(1L, 0L),
          step_id = c(1L, 2L),
          spoke_in_A = c(TRUE, TRUE),
          run_mode = c("link_multi_spoke", "link_multi_spoke"),
          is_probe_step = c(FALSE, FALSE)
        )
      },
      .adaptive_link_phase_b_startup_gap_for_spoke = function(...) FALSE,
      .adaptive_link_judge_params = function(...) {
        list(mode = "global_shared", scope = "link", beta = 0, epsilon = 0)
      },
      .adaptive_link_within_edges = function(...) tibble::tibble(
        A_item = character(),
        B_item = character(),
        y_A = integer(),
        step_id = integer()
      ),
      .adaptive_link_fit_transform = function(...) list(
        delta_mean = 0.1,
        delta_sd = 0.01,
        log_alpha_mean = 0.05,
        log_alpha_sd = NA_real_,
        theta_hub_post = c(h1 = 0.8, h2 = 0.2, h3 = -0.2),
        theta_spoke_post = c(s21 = 0.3, s22 = -0.1, s23 = -0.4),
        posterior_draws = list(),
        diagnostics = list(
          divergences = 0L,
          max_rhat = 1,
          min_ess_bulk = 1000,
          diagnostics_divergences_pass = TRUE,
          diagnostics_rhat_pass = TRUE,
          diagnostics_ess_pass = TRUE
        ),
        fit_contract = list(
          estimation_method = "cmdstan_hmc",
          uncertainty_approximation = "cmdstan_posterior_draws"
        )
      ),
      pairwiseLLM:::.adaptive_linking_refit_update_state(
        state = base_state,
        refit_context = list(last_refit_step = 0L)
      ),
      .package = "pairwiseLLM"
    ),
    "after probe-panel rebuild reset"
  )
})
