test_that("print and summarize_adaptive use v2 logs", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  judge <- make_deterministic_judge("i_wins")

  withr::local_seed(1)
  state <- adaptive_rank_run_live(state, judge, n_steps = 1L, progress = "none")

  output <- capture.output(print(state))
  expect_true(any(grepl("items", output)))
  expect_true(any(grepl("steps", output)))
  expect_false(any(grepl("batch_log", output)))

  summary <- summarize_adaptive(state)
  expect_true(tibble::is_tibble(summary))
  expect_true(all(c(
    "n_items",
    "steps_attempted",
    "committed_pairs",
    "n_refits",
    "last_stop_decision",
    "last_stop_reason"
  ) %in% names(summary)))
})
