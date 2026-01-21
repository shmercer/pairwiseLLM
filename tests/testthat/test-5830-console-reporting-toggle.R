testthat::test_that("progress output is off by default", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N)
  state$comparisons_observed <- 1L

  selection <- tibble::tibble(utility = 0.10, is_explore = FALSE)
  candidate_stats <- list(n_candidates_generated = 1L, n_candidates_after_filters = 1L)

  out <- capture.output({
    invisible(pairwiseLLM:::.adaptive_append_batch_log(
      state = state,
      iter = 1L,
      phase = "phase1",
      mode = state$mode,
      created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      batch_size_target = 2L,
      selection = selection,
      candidate_stats = candidate_stats,
      candidate_starved = FALSE,
      fallback_stage = "base",
      W_used = state$config$v3$W,
      config = state$config$v3,
      exploration_only = FALSE,
      utilities = selection
    ))
  })

  testthat::expect_length(out, 0L)
})

testthat::test_that("iteration progress line prints when enabled", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(progress = TRUE, progress_every_iter = 1L, progress_level = "basic")
  )
  state$comparisons_observed <- 2L

  selection <- tibble::tibble(utility = 0.10, is_explore = FALSE)
  candidate_stats <- list(n_candidates_generated = 1L, n_candidates_after_filters = 1L)

  out <- capture.output({
    invisible(pairwiseLLM:::.adaptive_append_batch_log(
      state = state,
      iter = 1L,
      phase = "phase1",
      mode = state$mode,
      created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      batch_size_target = 2L,
      selection = selection,
      candidate_stats = candidate_stats,
      candidate_starved = FALSE,
      fallback_stage = "base",
      W_used = state$config$v3$W,
      config = state$config$v3,
      exploration_only = FALSE,
      utilities = selection
    ))
  })

  testthat::expect_true(any(grepl("phase1", out)))
  testthat::expect_true(any(grepl("iter=", out)))
  testthat::expect_true(any(grepl("selected=", out)))
  testthat::expect_true(any(grepl("completed=", out)))
})
