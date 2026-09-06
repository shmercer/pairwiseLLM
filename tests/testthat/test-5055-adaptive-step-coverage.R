test_that("low-coverage adaptive step helpers cover warm-start, completeness, and D-opt guards", {
  state <- adaptive_rank_start(make_test_items(4L), seed = 1L)
  state$warm_start_done <- FALSE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  state$warm_start_idx <- 0L
  expect_null(pairwiseLLM:::.adaptive_warm_start_selection(state, step_id = 1L))

  expect_true(isTRUE(pairwiseLLM:::.adaptive_assert_step_entry_invariants(
    state = state,
    controller = list(run_mode = "within_set"),
    phase_ctx = list(phase = "phase_a")
  )))

  expect_error(
    pairwiseLLM:::.adaptive_assert_step_row_linking_completeness(tibble::tibble(
      run_mode = "link_probe_holdout",
      is_cross_set = TRUE,
      set_i = 1L,
      set_j = 2L,
      link_spoke_id = 2L,
      round_stage = "probe_panel",
      link_stage = "probe_panel",
      posterior_win_prob_ij_pre = 0.5,
      is_holdout_probe_step = FALSE
    )),
    "is_holdout_probe_step"
  )
  expect_error(
    pairwiseLLM:::.adaptive_assert_step_row_linking_completeness(tibble::tibble(
      run_mode = "link_one_spoke",
      is_cross_set = TRUE,
      set_i = 1L,
      set_j = 2L,
      link_spoke_id = 2L,
      round_stage = "anchor_link",
      link_stage = "anchor_link",
      posterior_win_prob_ij_pre = 0.5,
      is_drift_probe_step = TRUE,
      cross_set_utility_pre = 0.2,
      utility_mode = "linking_d_optimal_transform"
    )),
    "is_drift_probe_step"
  )
  expect_error(
    pairwiseLLM:::.adaptive_assert_step_row_linking_completeness(tibble::tibble(
      run_mode = "link_one_spoke",
      is_cross_set = TRUE,
      set_i = 1L,
      set_j = 2L,
      link_spoke_id = 2L,
      round_stage = "anchor_link",
      link_stage = "anchor_link",
      posterior_win_prob_ij_pre = 0.5,
      is_probe_step = TRUE,
      cross_set_utility_pre = 0.2,
      utility_mode = "linking_d_optimal_transform"
    )),
    "is_probe_step"
  )
  expect_error(
    pairwiseLLM:::.adaptive_assert_step_row_linking_completeness(tibble::tibble(
      run_mode = "link_one_spoke",
      is_cross_set = TRUE,
      set_i = 1L,
      set_j = 2L,
      link_spoke_id = 2L,
      round_stage = "anchor_link",
      link_stage = "anchor_link",
      posterior_win_prob_ij_pre = 0.5,
      utility_mode = "pairing_trueskill_u0"
    )),
    "required non-NA columns missing"
  )
  expect_error(
    pairwiseLLM:::.adaptive_assert_step_row_linking_completeness(tibble::tibble(
      run_mode = "link_one_spoke",
      is_cross_set = TRUE,
      set_i = 1L,
      set_j = 2L,
      link_spoke_id = 2L,
      round_stage = "anchor_link",
      link_stage = "anchor_link",
      posterior_win_prob_ij_pre = 0.5,
      cross_set_utility_pre = 0.2,
      utility_mode = "pairing_trueskill_u0"
    )),
    "must be linking_d_optimal_transform"
  )
  expect_error(
    pairwiseLLM:::.adaptive_assert_step_row_linking_completeness(tibble::tibble(
      run_mode = "link_probe_holdout",
      is_cross_set = TRUE,
      set_i = 1L,
      set_j = 2L,
      link_spoke_id = 2L,
      round_stage = "probe_panel",
      link_stage = "probe_panel",
      posterior_win_prob_ij_pre = 0.5,
      cross_set_utility_pre = NA_real_,
      utility_mode = "linking_d_optimal_transform"
    )),
    "must not use a linking D-optimal audit label"
  )
  expect_error(
    pairwiseLLM:::.adaptive_assert_step_row_linking_completeness(tibble::tibble(
      run_mode = "link_probe_holdout",
      is_cross_set = TRUE,
      set_i = 1L,
      set_j = 2L,
      link_spoke_id = 2L,
      round_stage = "probe_panel",
      link_stage = "probe_panel",
      posterior_win_prob_ij_pre = 0.5,
      cross_set_utility_pre = 0.2
    )),
    "cross_set_utility_pre"
  )
  expect_error(
    pairwiseLLM:::.adaptive_assert_step_row_linking_completeness(tibble::tibble(
      run_mode = "link_one_spoke",
      is_cross_set = TRUE,
      set_i = 1L,
      set_j = 2L,
      link_spoke_id = 2L,
      round_stage = "anchor_link",
      link_stage = "anchor_link",
      posterior_win_prob_ij_pre = 2,
      cross_set_utility_pre = 0.2,
      utility_mode = "linking_d_optimal_transform"
    )),
    "must be finite in \\[0,1\\]"
  )

  state_before <- adaptive_rank_start(
    tibble::tibble(
      item_id = c("h1", "s21"),
      set_id = c(1L, 2L),
      global_item_id = c("gh1", "gs21")
    ),
    seed = 1L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state_after <- state_before
  step_row <- tibble::tibble(
    utility_mode = "linking_d_optimal_transform",
    is_probe_step = FALSE,
    is_cross_set = FALSE,
    link_spoke_id = NA_integer_
  )
  expect_identical(
    pairwiseLLM:::.adaptive_link_d_opt_update_after_commit(state_before, state_after, step_row),
    state_after
  )
})
