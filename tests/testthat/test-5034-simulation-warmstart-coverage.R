test_that("warm-start scenario yields connected graph and reproducible coverage", {
  withr::local_seed(1)

  run <- pairwiseLLM:::.adaptive_simulation_run(
    scenario = "warm_start_connectivity",
    run_seed = 21L,
    judge_seed = 22L,
    n_items = 10L
  )

  connected <- pairwiseLLM:::.adaptive_warm_start_connectivity(
    run$step_log,
    item_ids = run$state$item_ids
  )
  expect_true(isTRUE(connected))

  traj <- pairwiseLLM:::.adaptive_committed_degree_trajectory(
    run$step_log,
    item_ids = run$state$item_ids
  )
  expect_true(nrow(traj) >= 1L)
  expect_true(any(traj$covered_items == length(run$state$item_ids)))

  run_again <- pairwiseLLM:::.adaptive_simulation_run(
    scenario = "warm_start_connectivity",
    run_seed = 21L,
    judge_seed = 22L,
    n_items = 10L
  )
  traj_again <- pairwiseLLM:::.adaptive_committed_degree_trajectory(
    run_again$step_log,
    item_ids = run_again$state$item_ids
  )
  expect_equal(traj, traj_again)
})
