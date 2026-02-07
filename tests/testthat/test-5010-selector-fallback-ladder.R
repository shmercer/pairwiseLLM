test_that("selector falls back when base window has no candidates", {
  items <- make_test_items(4)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)

  empty_candidates <- tibble::tibble(i = integer(), j = integer())
  out <- pairwiseLLM:::select_next_pair(state, candidates = empty_candidates)

  expect_false(out$candidate_starved)
  expect_equal(out$fallback_used, "expand_locality")
  expect_equal(out$fallback_path, "base>expand_locality")
  expect_false(is.na(out$i))
  expect_false(is.na(out$j))
})
