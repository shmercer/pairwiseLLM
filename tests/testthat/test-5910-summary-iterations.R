testthat::test_that("summarize_iterations returns stable schema with gini metrics", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$deg <- c(A = 1L, B = 2L, C = 3L)
  state$pos1 <- c(A = 1L, B = 1L, C = 0L)

  row <- pairwiseLLM:::build_batch_log_row(
    iter = 1L,
    phase = "phase1",
    mode = "warm_start",
    created_at = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    batch_size_target = 6L,
    n_pairs_selected = 6L,
    n_pairs_completed = 5L,
    n_pairs_failed = 1L,
    backlog_unjudged = 0L,
    n_explore_target = 2L,
    n_explore_selected = 2L,
    n_exploit_target = 4L,
    n_exploit_selected = 4L,
    n_candidates_generated = 10L,
    n_candidates_after_filters = 8L,
    candidate_starved = FALSE,
    reason_short_batch = NA_character_,
    W_used = 5L,
    explore_rate_used = 0.2,
    utility_selected_p50 = 0.55,
    utility_selected_p90 = 0.9,
    utility_candidate_p90 = 0.95
  )
  state$batch_log <- dplyr::bind_rows(state$batch_log, row)

  summary <- pairwiseLLM::summarize_iterations(state, last_n = 1L)

  testthat::expect_s3_class(summary, "tbl_df")
  testthat::expect_true(all(c("gini_degree", "gini_pos_A") %in% names(summary)))
  testthat::expect_true(all(c("n_candidates_generated", "utility_selected_p90", "backlog_unjudged") %in% names(summary)))
  testthat::expect_equal(summary$gini_degree[[1L]], pairwiseLLM:::compute_gini_degree(state$deg))
  testthat::expect_equal(summary$gini_pos_A[[1L]], pairwiseLLM:::compute_gini_posA(state$pos1))
  testthat::expect_true("iter_start_time" %in% names(summary))
})

testthat::test_that("summarize_iterations handles missing logs", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$batch_log <- NULL

  summary <- pairwiseLLM::summarize_iterations(state)

  testthat::expect_s3_class(summary, "tbl_df")
  testthat::expect_equal(nrow(summary), 0L)
  testthat::expect_true(all(c("gini_degree", "gini_pos_A") %in% names(summary)))
  testthat::expect_true(all(c("n_candidates_generated", "utility_selected_p90") %in% names(summary)))
})
