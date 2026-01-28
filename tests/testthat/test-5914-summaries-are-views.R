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

  item_summary <- tibble::tibble(
    ID = state$ids,
    theta_mean = c(0.2, -0.1, 0.0),
    theta_sd = c(0.1, 0.2, 0.3),
    theta_p2.5 = c(-0.2, -0.3, -0.4),
    theta_p5 = c(-0.1, -0.2, -0.3),
    theta_p50 = c(0.1, -0.05, 0.0),
    theta_p95 = c(0.3, 0.2, 0.1),
    theta_p97.5 = c(0.4, 0.3, 0.2),
    rank_mean = c(1.0, 2.0, 3.0),
    rank_p2.5 = c(1.0, 1.8, 2.7),
    rank_p5 = c(1.0, 1.9, 2.8),
    rank_p50 = c(1.0, 2.0, 3.0),
    rank_p95 = c(1.2, 2.1, 3.2),
    rank_p97.5 = c(1.3, 2.2, 3.3),
    rank_sd = c(0.1, 0.2, 0.3),
    deg = c(1L, 2L, 3L),
    posA_prop = c(1.0, 0.5, 0.0)
  )
  state$logs <- list(
    item_log_list = list(
      dplyr::relocate(dplyr::mutate(item_summary, refit_id = 1L), refit_id, .before = 1L)
    )
  )

  iter_summary <- pairwiseLLM::summarize_iterations(list(state = state), include_optional = FALSE)
  expected_iter <- state$batch_log |>
    dplyr::select(dplyr::any_of(c(
      "iter",
      "phase",
      "mode",
      "created_at",
      "batch_size_target",
      "n_pairs_selected",
      "n_pairs_completed",
      "candidate_starved",
      "reason_short_batch",
      "n_explore_selected",
      "n_exploit_selected"
    )))
  testthat::expect_equal(iter_summary, expected_iter)

  refit_summary <- pairwiseLLM::summarize_refits(state, include_optional = FALSE)
  expected_refit <- state$config$round_log |>
    dplyr::select(dplyr::any_of(c(
      "round_id",
      "iter_at_refit",
      "new_pairs",
      "divergences",
      "max_rhat",
      "min_ess_bulk",
      "epsilon_mean",
      "reliability_EAP",
      "theta_sd_eap",
      "rho_theta_lag",
      "delta_sd_theta_lag",
      "rho_rank_lag",
      "hard_cap_threshold",
      "n_unique_pairs_seen",
      "rank_stability_pass",
      "diagnostics_pass",
      "lag_eligible",
      "stop_decision",
      "stop_reason",
      "mode"
    )))
  testthat::expect_equal(refit_summary, expected_refit)

  item_summary <- pairwiseLLM::summarize_items(state, include_optional = FALSE)
  testthat::expect_equal(item_summary, state$logs$item_log_list[[1L]])
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
  testthat::expect_equal(iter_summary, state$batch_log |>
    dplyr::select(dplyr::any_of(c(
      "iter",
      "phase",
      "mode",
      "created_at",
      "batch_size_target",
      "n_pairs_selected",
      "n_pairs_completed",
      "candidate_starved",
      "reason_short_batch",
      "n_explore_selected",
      "n_exploit_selected"
    ))))

  testthat::expect_warning(
    pairwiseLLM::summarize_items(state, posterior = matrix(0, nrow = 1, ncol = 1)),
    "item log list"
  )
})

testthat::test_that("summaries preserve logical and NA values", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))

  batch_log <- tibble::tibble(
    iter = 1L,
    phase = "phase1",
    mode = "warm_start",
    created_at = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    batch_size_target = 2L,
    n_pairs_selected = 2L,
    n_pairs_completed = 1L,
    candidate_starved = NA,
    reason_short_batch = NA_character_,
    n_explore_selected = 1L,
    n_exploit_selected = 1L,
    safe_no_utility = TRUE,
    fallback_exhausted = FALSE
  )
  state$batch_log <- batch_log

  iter_summary <- pairwiseLLM::summarize_iterations(state)
  testthat::expect_identical(iter_summary$candidate_starved, batch_log$candidate_starved)
  testthat::expect_identical(iter_summary$safe_no_utility, batch_log$safe_no_utility)
  testthat::expect_identical(iter_summary$fallback_exhausted, batch_log$fallback_exhausted)

  round_log <- tibble::tibble(
    round_id = 1L,
    iter_at_refit = 0L,
    stop_decision = TRUE,
    lag_eligible = FALSE,
    diagnostics_pass = NA
  )
  state$config$round_log <- round_log

  refit_summary <- pairwiseLLM::summarize_refits(state)
  testthat::expect_identical(refit_summary$stop_decision, round_log$stop_decision)
  testthat::expect_identical(refit_summary$lag_eligible, round_log$lag_eligible)
  testthat::expect_identical(refit_summary$diagnostics_pass, round_log$diagnostics_pass)
})
