test_that("batch log fallback fields are scalar and enums are valid", {
  schema <- pairwiseLLM:::batch_log_schema()
  new_cols <- c(
    "fallback_used",
    "fallback_path",
    "a1_stage",
    "a1_W_used",
    "a1_anchor_pool",
    "a1_n_generated",
    "a1_n_survive",
    "a1_n_selected",
    "a2_stage",
    "a2_W_used",
    "a2_anchor_pool",
    "a2_n_generated",
    "a2_n_survive",
    "a2_n_selected",
    "a3_stage",
    "a3_W_used",
    "a3_anchor_pool",
    "a3_n_generated",
    "a3_n_survive",
    "a3_n_selected",
    "aN_tried",
    "aN_best_stage",
    "aN_best_n_generated",
    "aN_best_n_survive",
    "aN_best_n_selected",
    "starvation_reason",
    "fallback_exhausted"
  )
  expect_true(all(new_cols %in% names(schema)))
  expect_false(any(vctrs::vec_is_list(schema)))

  row <- pairwiseLLM:::build_batch_log_row(
    iter = 1L,
    phase = "phase2",
    mode = "adaptive",
    created_at = as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
    batch_size_target = 5L,
    n_pairs_selected = 3L,
    n_pairs_completed = 2L,
    n_pairs_failed = 0L,
    backlog_unjudged = 1L,
    n_explore_target = 2L,
    n_explore_selected = 1L,
    n_exploit_target = 3L,
    n_exploit_selected = 2L,
    n_candidates_generated = 10L,
    n_candidates_after_filters = 8L,
    candidate_starved = TRUE,
    fallback_exhausted = TRUE,
    fallback_used = "expand_2x",
    fallback_path = "base_window>expand_2x",
    a1_stage = "base_window",
    a1_W_used = 5L,
    a1_anchor_pool = "default",
    a1_n_generated = 10L,
    a1_n_survive = 8L,
    a1_n_selected = 3L,
    a2_stage = "expand_2x",
    a2_W_used = 10L,
    a2_anchor_pool = "default",
    a2_n_generated = 20L,
    a2_n_survive = 15L,
    a2_n_selected = 5L,
    aN_tried = 0L,
    aN_best_stage = NA_character_,
    aN_best_n_generated = NA_integer_,
    aN_best_n_survive = NA_integer_,
    aN_best_n_selected = NA_integer_,
    starvation_reason = "unknown",
    reason_short_batch = "expand_2x",
    W_used = 5L,
    explore_rate_used = 0.2,
    utility_selected_p50 = 0.1,
    utility_selected_p90 = 0.2,
    utility_candidate_p90 = 0.3,
    iter_exit_path = NA_character_
  )

  expect_false(any(vctrs::vec_is_list(row)))

  allowed <- pairwiseLLM:::.adaptive_fallback_used_levels()
  if (!is.na(row$fallback_used[[1L]])) {
    expect_true(row$fallback_used[[1L]] %in% allowed)
  }
})
