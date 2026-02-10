test_that("adaptive_rank_run_live executes multiple steps with deterministic judge", {
  items <- make_test_items(3)
  state <- adaptive_rank_start(items, seed = 11)
  judge <- make_deterministic_judge("i_wins")

  withr::local_seed(1)
  out <- adaptive_rank_run_live(state, judge, n_steps = 3L, progress = "none")

  expect_true(inherits(out, "adaptive_state"))
  expect_equal(nrow(out$step_log), 3L)
  expect_equal(nrow(out$history_pairs), sum(out$step_log$status == "ok"))
  expect_true(nrow(out$history_pairs) >= 1L)
})
