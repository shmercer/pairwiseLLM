test_that("adaptive_rank_resume restores state and run_live can continue", {
  items <- make_test_items(5)
  judge <- make_deterministic_judge("i_wins")
  session_dir <- withr::local_tempdir()

  state <- adaptive_rank_start(items, session_dir = session_dir)
  withr::local_seed(1)
  state <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 2L,
    session_dir = session_dir,
    progress = "none"
  )

  resumed <- adaptive_rank_resume(session_dir)
  withr::local_seed(2)
  resumed <- adaptive_rank_run_live(
    resumed,
    judge,
    n_steps = 2L,
    session_dir = session_dir,
    progress = "none"
  )

  expect_equal(nrow(resumed$step_log), nrow(state$step_log) + 2L)
  history <- adaptive_results_history(resumed, committed_only = TRUE)
  expect_equal(nrow(history), sum(!is.na(resumed$step_log$pair_id)))
})
