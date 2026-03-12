test_that("step-entry invariant guard aborts on non-link phase_b routing", {
  state <- adaptive_rank_start(make_test_items(4), seed = 5L)
  expect_error(
    pairwiseLLM:::.adaptive_assert_step_entry_invariants(
      state = state,
      controller = list(run_mode = "within_set"),
      phase_ctx = list(
        phase = "phase_b",
        pending_run_sets = integer(),
        ready_spokes = 2L,
        active_phase_a_set = NA_integer_
      )
    ),
    "non-link run_mode cannot use phase_b routing"
  )
})

test_that("step row linking completeness guard rejects missing cross-set fields", {
  bad_row <- list(
    run_mode = "link_one_spoke",
    is_cross_set = TRUE,
    set_i = 1L,
    set_j = 2L,
    link_spoke_id = 2L,
    round_stage = "anchor_link",
    link_stage = "anchor_link",
    posterior_win_prob_pre = NA_real_,
    cross_set_utility_pre = 0.25
  )

  expect_error(
    pairwiseLLM:::.adaptive_assert_step_row_linking_completeness(bad_row),
    "required non-NA columns missing"
  )
})

test_that("step row linking completeness guard rejects malformed linking metadata", {
  expect_error(
    pairwiseLLM:::.adaptive_assert_step_row_linking_completeness(
      list(run_mode = "within_set", is_cross_set = FALSE)
    ),
    "missing required linking columns"
  )

  bad_utility <- list(
    run_mode = "within_set",
    is_cross_set = FALSE,
    set_i = 1L,
    set_j = 1L,
    link_spoke_id = NA_integer_,
    round_stage = "local_link",
    link_stage = NA_character_,
    utility_mode = "bad_mode",
    posterior_win_prob_pre = NA_real_,
    cross_set_utility_pre = NA_real_
  )
  expect_error(
    pairwiseLLM:::.adaptive_assert_step_row_linking_completeness(bad_utility),
    "utility_mode` must be one of"
  )

  bad_cross_utility <- list(
    run_mode = "link_one_spoke",
    is_cross_set = TRUE,
    set_i = 1L,
    set_j = 2L,
    link_spoke_id = 2L,
    round_stage = "anchor_link",
    link_stage = "anchor_link",
    utility_mode = "pairing_trueskill_u0",
    posterior_win_prob_pre = 0.5,
    cross_set_utility_pre = 0.2
  )
  expect_error(
    pairwiseLLM:::.adaptive_assert_step_row_linking_completeness(bad_cross_utility),
    "must be linking_d_optimal"
  )

  bad_non_cross_cols <- list(
    run_mode = "within_set",
    is_cross_set = FALSE,
    set_i = 1L,
    set_j = 1L,
    link_spoke_id = NA_integer_,
    round_stage = "local_link",
    link_stage = NA_character_,
    posterior_win_prob_pre = NA_real_,
    cross_set_utility_pre = NA_real_,
    delta_spoke_estimate_pre = 0.1,
    delta_spoke_sd_pre = NA_real_,
    link_transform_state = NA_character_,
    log_alpha_spoke_estimate_pre = NA_real_,
    log_alpha_spoke_sd_pre = NA_real_,
    hub_lock_mode = NA_character_,
    hub_lock_kappa = NA_real_
  )
  expect_error(
    pairwiseLLM:::.adaptive_assert_step_row_linking_completeness(bad_non_cross_cols),
    "link-only columns to NA"
  )
})

test_that("link-stage append completeness guard rejects missing key/mode fields", {
  bad_rows <- tibble::tibble(
    refit_id = 1L,
    spoke_id = NA_integer_,
    hub_id = 1L,
    link_transform_state = NA_character_,
    link_refit_mode = "shift_only",
    hub_lock_mode = "soft_lock",
    reliability_link_global = 0.9,
    linking_identified = TRUE,
    link_stop_eligible = TRUE,
    link_stop_pass = TRUE,
    transform_frozen = FALSE,
    n_pairs_cross_set_done = 1L,
    n_unique_cross_pairs_seen = 1L,
    n_cross_edges_active_since_last_refit = 1L,
    n_cross_edges_probe_since_last_refit = 0L,
    n_cross_edges_total_since_last_refit = 1L,
    coverage_bins_used = 3L
  )

  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_rows_completeness(bad_rows),
    "missing required columns|key fields refit_id/spoke_id/hub_id must be non-NA"
  )
})

test_that("link-stage completeness guard requires canonical policy/state fields", {
  legacy_rows <- tibble::tibble(
    refit_id = 1L,
    spoke_id = 2L,
    hub_id = 1L,
    link_transform_mode = "shift_only"
  )

  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_rows_completeness(legacy_rows),
    "missing required columns: link_transform_policy, link_transform_state"
  )
})

test_that("link-stage budget invariant guard rejects target sum mismatches", {
  bad_rows <- tibble::tibble(
    B_spoke_refit_budget = 5L,
    stage_target_anchor_link = 2L,
    stage_target_long_link = 2L,
    stage_target_mid_link = 2L,
    stage_target_local_link = 0L,
    stage_realized_anchor_link = 1L,
    stage_realized_long_link = 1L,
    stage_realized_mid_link = 1L,
    stage_realized_local_link = 0L,
    stage_shortfall_anchor_link = 1L,
    stage_shortfall_long_link = 1L,
    stage_shortfall_mid_link = 1L,
    stage_shortfall_local_link = 0L,
    stage_reallocation_used = FALSE,
    stage_reallocation_rule_used = "none",
    stage_budget_unfilled = 2L
  )

  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_budget_invariants(bad_rows),
    "targets must sum to the per-spoke budget"
  )
})

test_that("step-entry invariant guard rejects empty ready spokes and pending run sets in phase_b", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22")
  )
  state <- adaptive_rank_start(
    items,
    seed = 6L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )

  expect_error(
    pairwiseLLM:::.adaptive_assert_step_entry_invariants(
      state = state,
      controller = list(run_mode = "link_one_spoke"),
      phase_ctx = list(
        phase = "phase_b",
        pending_run_sets = integer(),
        ready_spokes = integer(),
        active_phase_a_set = NA_integer_
      )
    ),
    "phase marked phase_b but no ready spokes are available"
  )

  expect_error(
    pairwiseLLM:::.adaptive_assert_step_entry_invariants(
      state = state,
      controller = list(run_mode = "link_one_spoke"),
      phase_ctx = list(
        phase = "phase_b",
        pending_run_sets = c(2L),
        ready_spokes = c(2L),
        active_phase_a_set = NA_integer_
      )
    ),
    "pending Phase A run sets remain"
  )
})

test_that("step-entry invariant guard rejects unsupported linking cross-set utility config", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22")
  )
  state <- adaptive_rank_start(
    items,
    seed = 16L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )

  expect_error(
    pairwiseLLM:::.adaptive_assert_step_entry_invariants(
      state = state,
      controller = list(run_mode = "link_one_spoke", cross_set_utility = "entropy"),
      phase_ctx = list(
        phase = "phase_b",
        pending_run_sets = integer(),
        ready_spokes = 2L,
        active_phase_a_set = NA_integer_
      )
    ),
    "cross_set_utility"
  )
})

test_that("step row completeness guard validates structure and non-cross-set spoke NA", {
  expect_error(
    pairwiseLLM:::.adaptive_assert_step_row_linking_completeness(tibble::tibble()),
    "expects exactly one row"
  )

  bad_non_cross <- list(
    run_mode = "within_set",
    is_cross_set = FALSE,
    set_i = 1L,
    set_j = 1L,
    link_spoke_id = 2L,
    round_stage = "local_link",
    link_stage = NA_character_,
    posterior_win_prob_pre = NA_real_,
    cross_set_utility_pre = NA_real_
  )
  expect_error(
    pairwiseLLM:::.adaptive_assert_step_row_linking_completeness(bad_non_cross),
    "non-cross-set rows must set `link_spoke_id = NA`"
  )

  bad_cross_stage <- list(
    run_mode = "link_one_spoke",
    is_cross_set = TRUE,
    set_i = 1L,
    set_j = 2L,
    link_spoke_id = 2L,
    round_stage = "anchor_link",
    link_stage = NA_character_,
    posterior_win_prob_pre = 0.5,
    cross_set_utility_pre = 0.25
  )
  expect_error(
    pairwiseLLM:::.adaptive_assert_step_row_linking_completeness(bad_cross_stage),
    "`link_stage` must be populated"
  )
})

test_that("step row completeness rejects legacy link_probe runtime rows", {
  ok_probe <- list(
    run_mode = "link_probe",
    is_cross_set = TRUE,
    set_i = 1L,
    set_j = 2L,
    link_spoke_id = 2L,
    round_stage = "anchor_link",
    link_stage = "anchor_link",
    posterior_win_prob_pre = 0.6,
    cross_set_utility_pre = NA_real_,
    utility_mode = NA_character_
  )
  expect_error(
    pairwiseLLM:::.adaptive_assert_step_row_linking_completeness(ok_probe),
    "legacy-only"
  )
})

test_that("validate_judge_result and apply_step_update guard branches are exercised", {
  expect_identical(
    pairwiseLLM:::validate_judge_result("bad", "a", "b")$invalid_reason,
    "invalid_contract"
  )
  expect_identical(
    pairwiseLLM:::validate_judge_result(list(is_valid = "yes"), "a", "b")$invalid_reason,
    "invalid_contract"
  )
  expect_identical(
    pairwiseLLM:::validate_judge_result(list(is_valid = TRUE), "a", "b")$invalid_reason,
    "invalid_contract"
  )
  expect_identical(
    pairwiseLLM:::validate_judge_result(list(is_valid = TRUE, Y = 2L), "a", "b")$invalid_reason,
    "invalid_contract"
  )
  expect_identical(
    pairwiseLLM:::validate_judge_result(list(is_valid = FALSE, invalid_reason = ""), "a", "b")$invalid_reason,
    "invalid_contract"
  )

  state <- adaptive_rank_start(make_test_items(3), seed = 10L)
  state$step_log <- tibble::tibble(step_id = integer(), timestamp = as.POSIXct(character()))
  step <- list(
    row = list(step_id = 1L, timestamp = Sys.time()),
    is_valid = FALSE,
    invalid_reason = "invalid",
    A_id = NA_character_,
    B_id = NA_character_,
    Y = NA_integer_
  )
  out <- pairwiseLLM:::apply_step_update(state, step)
  expect_true(all(names(pairwiseLLM:::schema_step_log) %in% names(out$step_log)))
})

test_that("run_one_step argument guards and warm-start NULL branch are exercised", {
  expect_error(pairwiseLLM:::run_one_step(list(), make_deterministic_judge("i_wins")), "adaptive_state")
  state <- adaptive_rank_start(make_test_items(3), seed = 12L)
  expect_error(pairwiseLLM:::run_one_step(state, 1L), "`judge` must be a function")
})

test_that("validate_judge_result catches non-scalar Y branch", {
  bad <- pairwiseLLM:::validate_judge_result(list(is_valid = TRUE, Y = c(1L, 0L)), "a", "b")
  expect_false(isTRUE(bad$is_valid))
  expect_identical(bad$invalid_reason, "invalid_contract")
})

test_that("all-spokes-stopped helper is phase and mode aware", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = paste0("g", seq_len(6L))
  )
  state <- adaptive_rank_start(
    items,
    seed = 77L,
    adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L)
  )

  expect_false(pairwiseLLM:::.adaptive_link_all_spokes_stopped(state))

  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L, 3L),
      source = c("run", "run", "run"),
      status = c("ready", "ready", "ready"),
      validation_message = c("ok", "ok", "ok"),
      artifact_path = c(NA_character_, NA_character_, NA_character_)
    ),
    artifacts = list(),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L, 3L),
    set_stop_pass_by_set = list(`1` = TRUE, `2` = TRUE, `3` = TRUE),
    phase = "phase_b",
    ready_spokes = c(2L, 3L),
    active_phase_a_set = NA_integer_,
    phase_b_started_at_step = 1L
  )
  state$controller$link_stopped_by_spoke <- list(`2` = TRUE, `3` = FALSE)
  expect_false(pairwiseLLM:::.adaptive_link_all_spokes_stopped(state))

  state$controller$probe_pairs_per_refit_per_spoke <- 0L
  state$controller$link_stopped_by_spoke <- list(`2` = TRUE, `3` = TRUE)
  expect_true(pairwiseLLM:::.adaptive_link_all_spokes_stopped(state))
})
