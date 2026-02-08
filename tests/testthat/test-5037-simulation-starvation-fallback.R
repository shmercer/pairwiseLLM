test_that("starvation scenario has explicit fallback paths and deterministic distribution", {
  withr::local_seed(1)

  run_1 <- pairwiseLLM:::.adaptive_simulation_run(
    scenario = "starvation_fallback",
    run_seed = 77L,
    judge_seed = 88L
  )
  run_2 <- pairwiseLLM:::.adaptive_simulation_run(
    scenario = "starvation_fallback",
    run_seed = 77L,
    judge_seed = 88L
  )

  dist_1 <- pairwiseLLM:::.adaptive_fallback_distribution(run_1$step_log)
  dist_2 <- pairwiseLLM:::.adaptive_fallback_distribution(run_2$step_log)
  expect_equal(dist_1, dist_2)

  expect_true(all(run_1$step_log$status == "starved"))
  expect_true(all(!is.na(run_1$step_log$fallback_path)))
  expect_true(isTRUE(run_1$state$meta$stop_decision))
  expect_equal(run_1$state$meta$stop_reason, "candidate_starvation")
})
