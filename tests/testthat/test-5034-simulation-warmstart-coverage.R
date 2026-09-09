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


test_that("predictive priors do not change initial pairing queues or within-set utility", {
  ids <- paste0("i", 1:5)
  cold <- adaptive_rank_start(ids, seed = 123)
  warm <- adaptive_rank_start(ids, seed = 123,
    warm_start_prior = make_warm_start_prior(stats::setNames(c(-10, 20, 0, 5, -4), ids)))
  expect_identical(warm$warm_start_pairs, cold$warm_start_pairs)
  expect_identical(warm$warm_start_idx, cold$warm_start_idx)
  expect_identical(warm$warm_start_done, cold$warm_start_done)
  select <- function(state) {
    withr::local_seed(77)
    pairwiseLLM:::select_next_pair(state)
  }
  selected_warm <- select(warm)
  selected_cold <- select(cold)
  for (field in c("i", "j", "A", "B", "p", "u0")) {
    expect_identical(selected_warm[[field]], selected_cold[[field]])
  }
})
