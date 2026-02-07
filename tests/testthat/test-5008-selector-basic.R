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
  expect_true(is.integer(out$star_cap_reject_items))
  expect_equal(out2$i, out3$i)
  expect_equal(out2$j, out3$j)
  expect_equal(out2$A, out3$A)
  expect_equal(out2$B, out3$B)
})
