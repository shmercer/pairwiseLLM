test_that("repeated pairs reverse order and invalid steps do not affect last order", {
  items <- make_test_items(2)
  judge_valid <- make_deterministic_judge("i_wins")
  judge_invalid <- make_deterministic_judge("invalid")

  withr::local_seed(1)
  state <- pairwiseLLM:::new_adaptive_state(items)
  state <- pairwiseLLM:::run_one_step(state, judge_valid)
  first <- tail(state$step_log, 1L)

  state <- pairwiseLLM:::run_one_step(state, judge_valid)
  second <- tail(state$step_log, 1L)

  expect_equal(second$A, first$B)
  expect_equal(second$B, first$A)
  expect_true(!is.na(first$pair_id))
  expect_true(!is.na(second$pair_id))

  state <- pairwiseLLM:::new_adaptive_state(items)
  state <- pairwiseLLM:::run_one_step(state, judge_valid)
  first <- tail(state$step_log, 1L)

  state <- pairwiseLLM:::run_one_step(state, judge_invalid)
  invalid <- tail(state$step_log, 1L)
  expect_true(is.na(invalid$pair_id))

  state <- pairwiseLLM:::run_one_step(state, judge_valid)
  third <- tail(state$step_log, 1L)
  expect_equal(third$A, first$B)
  expect_equal(third$B, first$A)
})
