test_that("refit round_log records MCMC chain settings from fit output", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items, seed = 11)
  judge <- make_deterministic_judge("i_wins")

  fit <- make_test_btl_fit(
    ids = state$item_ids,
    mcmc_config_used = list(
      chains = 6L,
      parallel_chains = 3L,
      core_fraction = 0.75,
      cores_detected_physical = 8L,
      cores_detected_logical = 16L,
      threads_per_chain = 2L,
      cmdstanr_version = "test"
    )
  )
  stub <- make_deterministic_fit_fn(state$item_ids, fit = fit)

  withr::local_seed(1)
  out <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 2L,
    fit_fn = stub$fit_fn,
    btl_config = list(refit_pairs_target = 2L),
    progress = "none"
  )

  round_log <- adaptive_round_log(out)
  expect_equal(round_log$mcmc_chains[[1L]], 6L)
  expect_equal(round_log$mcmc_parallel_chains[[1L]], 3L)
  expect_equal(round_log$mcmc_core_fraction[[1L]], 0.75)
  expect_equal(round_log$mcmc_threads_per_chain[[1L]], 2L)
})
