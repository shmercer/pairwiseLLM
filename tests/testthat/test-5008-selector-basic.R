test_that("select_next_pair returns one valid pair", {
  items <- make_test_items(4)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)

  out <- pairwiseLLM:::select_next_pair(state)

  expect_false(is.na(out$i))
  expect_false(is.na(out$j))
  expect_true(out$i != out$j)
  expect_true(out$fallback_used %in% c(
    "base_window",
    "expand_2x",
    "expand_4x",
    "uncertainty_pool",
    "dup_relax",
    "global_safe"
  ))
  expect_true(is.logical(out$candidate_starved))
  expect_false(out$candidate_starved)
})
