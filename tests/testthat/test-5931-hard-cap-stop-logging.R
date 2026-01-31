testthat::test_that("hard-cap stop writes batch_log and round_log rows", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N)
  state$phase <- "phase2"
  state$mode <- "adaptive"

  keys <- names(state$pair_count)[1:3]
  state$pair_count[keys] <- 1L
  state$comparisons_observed <- 3L
  state$comparisons_scheduled <- 3L

  out <- pairwiseLLM:::.adaptive_schedule_next_pairs(
    state = state,
    target_pairs = 1L,
    adaptive = list(),
    seed = 1L
  )
  state <- out$state

  testthat::expect_identical(state$mode, "stopped")
  testthat::expect_identical(state$stop_reason, "hard_cap_40pct")
  testthat::expect_true(nrow(state$batch_log) >= 1L)
  testthat::expect_true(nrow(state$config$round_log) >= 1L)

  last_batch <- state$batch_log[nrow(state$batch_log), , drop = FALSE]
  last_round <- state$config$round_log[nrow(state$config$round_log), , drop = FALSE]

  testthat::expect_identical(last_batch$n_pairs_selected[[1L]], 0L)
  testthat::expect_identical(last_round$stop_reason[[1L]], "hard_cap_40pct")
})
