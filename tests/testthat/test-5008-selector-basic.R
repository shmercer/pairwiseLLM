test_that("select_next_pair returns one valid pair", {
  items <- make_test_items(4)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)

  out <- pairwiseLLM:::select_next_pair(state)
  out2 <- pairwiseLLM:::select_next_pair(state, step_id = 1L)
  out3 <- pairwiseLLM:::select_next_pair(state, step_id = 1L)

  expect_false(is.na(out$i))
  expect_false(is.na(out$j))
  expect_true(out$i != out$j)
  expect_true(out$fallback_used %in% c(
    "base",
    "expand_locality",
    "uncertainty_pool",
    "dup_relax",
    "global_safe"
  ))
  expect_true(is.logical(out$candidate_starved))
  expect_false(out$candidate_starved)
  expect_true(is.double(out$explore_rate_used))
  expect_true(is.character(out$local_priority_mode))
  expect_true(is.logical(out$long_gate_pass))
  expect_true(is.character(out$long_gate_reason))
  expect_true(is.logical(out$star_override_used))
  expect_true(is.character(out$star_override_reason))
  expect_true(is.integer(out$star_cap_reject_items))
  expect_equal(out2$i, out3$i)
  expect_equal(out2$j, out3$j)
  expect_equal(out2$A, out3$A)
  expect_equal(out2$B, out3$B)
})

test_that("selector helper edge branches are covered", {
  expect_identical(pairwiseLLM:::.adaptive_underrep_set(integer()), character())
  expect_null(pairwiseLLM:::.adaptive_underrep_set(c(1L, 2L)))
  expect_identical(pairwiseLLM:::.adaptive_underrep_set(c(a = 1L, b = 5L)), "a")
  expect_identical(pairwiseLLM:::.adaptive_underrep_set(c(a = 1L, b = 1L)), c("a", "b"))

  empty_cand <- tibble::tibble(i = character(), j = character())
  rank_index <- stats::setNames(seq_len(3L), c("a", "b", "c"))
  defaults <- pairwiseLLM:::adaptive_defaults(3L)
  out_empty <- pairwiseLLM:::.adaptive_stage_candidate_filter(
    candidates = empty_cand,
    stage_name = "local_link",
    fallback_name = "base",
    rank_index = rank_index,
    defaults = defaults
  )
  expect_identical(nrow(out_empty), 0L)
})

test_that("history-state cache rebuild matches canonical history and preserves selector output", {
  items <- make_test_items(6)
  trueskill_state <- make_test_trueskill_state(items)
  history <- tibble::tibble(
    A_id = c("1", "2", "3", "2", "4"),
    B_id = c("4", "5", "6", "4", "6")
  )
  state_cached <- make_test_state(items, trueskill_state, history = history)
  expect_history_state_matches_history(state_cached)

  state_uncached <- state_cached
  state_uncached$history_state <- NULL

  out_cached <- pairwiseLLM:::select_next_pair(state_cached, step_id = 7L)
  out_uncached <- pairwiseLLM:::select_next_pair(state_uncached, step_id = 7L)

  expect_identical(out_uncached$i, out_cached$i)
  expect_identical(out_uncached$j, out_cached$j)
  expect_identical(out_uncached$A, out_cached$A)
  expect_identical(out_uncached$B, out_cached$B)
  expect_identical(out_uncached$deg_i, out_cached$deg_i)
  expect_identical(out_uncached$deg_j, out_cached$deg_j)
  expect_identical(out_uncached$recent_deg_i, out_cached$recent_deg_i)
  expect_identical(out_uncached$recent_deg_j, out_cached$recent_deg_j)
})
