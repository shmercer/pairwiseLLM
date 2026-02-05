test_that("maybe_refit_btl follows step-based cadence", {
  items <- make_test_items(6)
  state <- pairwiseLLM:::new_adaptive_state(items)
  judge <- make_deterministic_judge("i_wins")
  stub <- make_deterministic_fit_fn(state$item_ids)

  withr::local_seed(1)
  for (step in seq_len(9L)) {
    state <- pairwiseLLM:::run_one_step(state, judge)
    refit <- pairwiseLLM:::maybe_refit_btl(
      state,
      config = list(refit_pairs_target = 3L),
      fit_fn = stub$fit_fn
    )
    state <- refit$state
    if (step < 3L) {
      expect_false(refit$refit_performed)
    }
  }

  expect_equal(stub$get_calls(), 3L)
  expect_equal(state$refit_meta$last_refit_M_done, 9L)
  expect_equal(state$refit_meta$last_refit_step, 9L)
})
