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
