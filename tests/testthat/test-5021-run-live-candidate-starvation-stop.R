test_that("adaptive_rank_run_live stops on candidate starvation", {
  items <- make_test_items(2)
  state <- pairwiseLLM:::new_adaptive_state(items)
  state$history_pairs <- tibble::tibble(
    A_id = c("1", "1", "1"),
    B_id = c("2", "2", "2")
  )

  calls <- 0L
  judge <- function(A, B, state, ...) {
    calls <<- calls + 1L
    list(is_valid = TRUE, Y = 1L)
  }

  out <- adaptive_rank_run_live(state, judge, n_steps = 5L, progress = "none")

  expect_equal(calls, 0L)
  expect_true(nrow(out$step_log) >= 1L)
  expect_true(all(out$step_log$status == "starved"))
  expect_true(all(is.na(out$step_log$pair_id)))
  expect_equal(tail(out$step_log$round_stage, 1L), "local_link")
  expect_true(isTRUE(out$meta$stop_decision))
  expect_equal(out$meta$stop_reason, "candidate_starvation")
  expect_equal(nrow(out$round_log), 0L)
})
