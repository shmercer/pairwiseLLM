test_that("quota shortfall scenario is measurable and deterministic", {
  withr::local_seed(1)

  run_1 <- pairwiseLLM:::.adaptive_simulation_run(
    scenario = "quota_shortfall",
    run_seed = 7L,
    judge_seed = 9L
  )
  run_2 <- pairwiseLLM:::.adaptive_simulation_run(
    scenario = "quota_shortfall",
    run_seed = 7L,
    judge_seed = 9L
  )

  q_1 <- pairwiseLLM:::.adaptive_stage_quota_summary(run_1$step_log)
  q_2 <- pairwiseLLM:::.adaptive_stage_quota_summary(run_2$step_log)

  expect_equal(q_1, q_2)
  expect_true(nrow(q_1) > 0L)
  expect_true(any(q_1$shortfall > 0L, na.rm = TRUE))
  expect_true(isTRUE(run_1$state$meta$stop_decision))
  expect_equal(run_1$state$meta$stop_reason, "candidate_starvation")
})
