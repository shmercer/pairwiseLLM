testthat::test_that("hard-cap stop writes batch_log and round_log rows", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L, budget_max = 10L))
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N)
  state$phase <- "phase2"
  state$mode <- "adaptive"

  keys <- names(state$pair_count)[1:3]
  state$pair_count[keys] <- 1L
  state$history_pairs <- tibble::tibble(
    pair_uid = c("A:B#1", "A:C#1", "A:D#1"),
    unordered_key = c("A:B", "A:C", "A:D"),
    ordered_key = c("A:B", "A:C", "A:D"),
    A_id = c("A", "A", "A"),
    B_id = c("B", "C", "D"),
    A_text = c("alpha", "alpha", "alpha"),
    B_text = c("bravo", "charlie", "delta"),
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )
  state$history_results <- tibble::tibble(
    pair_uid = c("A:B#1", "A:C#1", "A:D#1"),
    unordered_key = c("A:B", "A:C", "A:D"),
    ordered_key = c("A:B", "A:C", "A:D"),
    A_id = c("A", "A", "A"),
    B_id = c("B", "C", "D"),
    better_id = c("A", "A", "A"),
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    backend = "mock",
    model = "mock"
  )
  state$comparisons_observed <- 3L
  state$comparisons_scheduled <- as.integer(nrow(state$history_pairs))
  state$last_refit_at <- 0L
  state$new_since_refit <- state$comparisons_observed - state$last_refit_at

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
