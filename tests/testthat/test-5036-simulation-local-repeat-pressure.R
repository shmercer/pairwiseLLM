test_that("local scenario admits same-stratum or anchor-involving pairs", {
  withr::local_seed(1)

  run <- pairwiseLLM:::.adaptive_simulation_run(
    scenario = "local_repeat_pressure",
    run_seed = 55L,
    judge_seed = 66L,
    n_steps = 3L
  )

  step_log <- run$step_log
  local_rows <- step_log[step_log$round_stage == "local_link" & !is.na(step_log$pair_id), , drop = FALSE]
  expect_true(nrow(local_rows) >= 1L)
  expect_true(any(local_rows$dist_stratum == 0L | local_rows$is_anchor_i | local_rows$is_anchor_j, na.rm = TRUE))

  repeated <- local_rows$used_in_round_i > 0L | local_rows$used_in_round_j > 0L
  expect_true(any(repeated, na.rm = TRUE))
})

test_that("repeat routing blocks when repeated endpoints are not underrepresented", {
  withr::local_seed(1)

  run <- pairwiseLLM:::.adaptive_simulation_run(
    scenario = "local_repeat_blocked",
    run_seed = 55L,
    judge_seed = 66L,
    n_steps = 3L
  )

  expect_true(isTRUE(run$state$meta$stop_decision))
  expect_equal(run$state$meta$stop_reason, "candidate_starvation")
  expect_true(any(run$step_log$candidate_starved))
  expect_true(tail(run$step_log$status, 1L) == "starved")
})
