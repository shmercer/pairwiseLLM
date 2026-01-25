testthat::test_that("summaries are views over canonical outputs", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))

  batch_row <- pairwiseLLM:::build_batch_log_row(
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
  state$batch_log <- dplyr::bind_rows(state$batch_log, batch_row)

  round_log <- pairwiseLLM:::round_log_schema()
  round_log <- dplyr::bind_rows(round_log, tibble::tibble(
    round_id = 1L,
    iter_at_refit = 0L,
    stop_decision = TRUE,
    stop_reason = "manual"
  ))
  state$config$round_log <- round_log

  state$config$item_summary <- tibble::tibble(
    ID = state$ids,
    theta_mean = c(0.2, -0.1, 0.0),
    theta_sd = c(0.1, 0.2, 0.3),
    theta_ci90_lo = c(-0.1, -0.2, -0.3),
    theta_ci90_hi = c(0.3, 0.2, 0.1),
    theta_ci95_lo = c(-0.2, -0.3, -0.4),
    theta_ci95_hi = c(0.4, 0.3, 0.2),
    rank_mean = c(1.0, 2.0, 3.0),
    rank_sd = c(0.1, 0.2, 0.3),
    deg = c(1L, 2L, 3L),
    posA_prop = c(1.0, 0.5, 0.0)
  )

  iter_summary <- pairwiseLLM::summarize_iterations(list(state = state), include_optional = FALSE)
  testthat::expect_equal(iter_summary$n_pairs_selected[[1L]], 6L)
  testthat::expect_equal(iter_summary$n_pairs_completed[[1L]], 5L)

  refit_summary <- pairwiseLLM::summarize_refits(state, include_optional = FALSE)
  testthat::expect_true(refit_summary$stop_decision[[1L]])
  testthat::expect_equal(refit_summary$stop_reason[[1L]], "manual")

  item_summary <- pairwiseLLM::summarize_items(state, include_optional = FALSE)
  testthat::expect_equal(item_summary$theta_q05[[1L]], state$config$item_summary$theta_ci90_lo[[1L]])
  testthat::expect_equal(item_summary$theta_q95[[1L]], state$config$item_summary$theta_ci90_hi[[1L]])
  testthat::expect_equal(item_summary$pos_A_rate[[1L]], state$config$item_summary$posA_prop[[1L]])
})

testthat::test_that("summaries handle log lists and warn on non-summary posterior", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))

  batch_row <- pairwiseLLM:::build_batch_log_row(
    iter = 1L,
    phase = "phase1",
    mode = "warm_start",
    created_at = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    batch_size_target = 2L,
    n_pairs_selected = 2L,
    n_pairs_completed = 2L,
    n_pairs_failed = 0L,
    backlog_unjudged = 0L,
    n_explore_target = 1L,
    n_explore_selected = 1L,
    n_exploit_target = 1L,
    n_exploit_selected = 1L,
    n_candidates_generated = 4L,
    n_candidates_after_filters = 4L,
    candidate_starved = FALSE,
    reason_short_batch = NA_character_,
    W_used = 2L,
    explore_rate_used = 0.5,
    utility_selected_p50 = 0.5,
    utility_selected_p90 = 0.6,
    utility_candidate_p90 = 0.7
  )
  state$batch_log <- dplyr::bind_rows(state$batch_log, batch_row)

  iter_summary <- pairwiseLLM::summarize_iterations(
    list(batch_log = state$batch_log),
    include_optional = FALSE
  )
  testthat::expect_equal(iter_summary$n_pairs_selected[[1L]], 2L)
  testthat::expect_equal(iter_summary$n_pairs_completed[[1L]], 2L)

  testthat::expect_warning(
    pairwiseLLM::summarize_items(state, posterior = matrix(0, nrow = 1, ncol = 1)),
    "item summary"
  )
})
