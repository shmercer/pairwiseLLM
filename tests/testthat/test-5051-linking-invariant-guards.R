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

test_that("link-stage append completeness guard rejects missing key/mode fields", {
  bad_rows <- tibble::tibble(
    refit_id = 1L,
    spoke_id = NA_integer_,
    hub_id = 1L,
    link_transform_mode = NA_character_,
    link_refit_mode = "shift_only",
    hub_lock_mode = "soft_lock",
    reliability_EAP_link = 0.9,
    linking_identified = TRUE,
    link_stop_eligible = TRUE,
    link_stop_pass = TRUE,
    n_pairs_cross_set_done = 1L,
    n_unique_cross_pairs_seen = 1L,
    n_cross_edges_since_last_refit = 1L,
    coverage_bins_used = 3L
  )

  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_rows_completeness(bad_rows),
    "key fields refit_id/spoke_id/hub_id must be non-NA"
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
