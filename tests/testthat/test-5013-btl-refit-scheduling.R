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

test_that(".adaptive_btl_extract_fit_contract handles wrapped fit outputs", {
  fit_direct <- list(theta_draws = matrix(0, nrow = 2L, ncol = 2L))
  fit_named <- list(theta_draws = matrix(1, nrow = 2L, ncol = 2L))
  fit_last <- list(theta_draws = matrix(2, nrow = 2L, ncol = 2L))

  out_direct <- pairwiseLLM:::.adaptive_btl_extract_fit_contract(fit_direct)
  out_fit <- pairwiseLLM:::.adaptive_btl_extract_fit_contract(list(fit = fit_named))
  out_fits <- pairwiseLLM:::.adaptive_btl_extract_fit_contract(list(fits = list(fit_named, fit_last)))

  expect_identical(out_direct, fit_direct)
  expect_identical(out_fit, fit_named)
  expect_identical(out_fits, fit_last)
})
