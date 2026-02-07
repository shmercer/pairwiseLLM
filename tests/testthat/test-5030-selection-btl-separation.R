test_that("pair selection does not depend on BTL posterior draws", {
  items <- make_test_items(5)
  judge <- make_deterministic_judge("i_wins")

  state_base <- pairwiseLLM:::new_adaptive_state(items)
  withr::local_seed(1)
  for (idx in seq_len(4L)) {
    state_base <- pairwiseLLM:::run_one_step(state_base, judge)
  }

  state_with_fit <- state_base
  ids <- state_with_fit$item_ids
  bad_draws <- matrix(-1000, nrow = 10L, ncol = length(ids))
  colnames(bad_draws) <- ids
  state_with_fit$btl_fit <- make_test_btl_fit(ids, draws = bad_draws)

  withr::local_seed(77)
  selected_base <- pairwiseLLM:::select_next_pair(state_base)
  withr::local_seed(77)
  selected_with_fit <- pairwiseLLM:::select_next_pair(state_with_fit)

  expect_identical(selected_with_fit$i, selected_base$i)
  expect_identical(selected_with_fit$j, selected_base$j)
  expect_identical(selected_with_fit$A, selected_base$A)
  expect_identical(selected_with_fit$B, selected_base$B)
  expect_identical(selected_with_fit$p, selected_base$p)
  expect_identical(selected_with_fit$u0, selected_base$u0)
})
