make_run_state <- function(budget_max = 6L) {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = as.integer(budget_max))
  )
}

add_scheduled_pair <- function(state, A_id, B_id, iter = 1L, phase = "phase1") {
  created_at <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  unordered_key <- pairwiseLLM:::make_unordered_key(A_id, B_id)
  ordered_key <- pairwiseLLM:::make_ordered_key(A_id, B_id)
  pair_uid <- pairwiseLLM:::pair_uid_from_state(state, unordered_key)

  row <- tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = A_id,
    B_id = B_id,
    A_text = state$texts[[A_id]],
    B_text = state$texts[[B_id]],
    phase = phase,
    iter = as.integer(iter),
    created_at = created_at
  )

  state <- pairwiseLLM:::record_presentation(state, A_id, B_id)
  state$history_pairs <- dplyr::bind_rows(state$history_pairs, row)
  state$comparisons_scheduled <- as.integer(state$comparisons_scheduled + 1L)

  list(state = state, pair_row = row)
}

make_result_tbl <- function(pair_row, better_id = NULL) {
  if (is.null(better_id)) {
    better_id <- pair_row$A_id
  }
  winner_pos <- ifelse(better_id == pair_row$A_id, 1L, 2L)
  tibble::tibble(
    pair_uid = as.character(pair_row$pair_uid),
    unordered_key = as.character(pair_row$unordered_key),
    ordered_key = as.character(pair_row$ordered_key),
    A_id = as.character(pair_row$A_id),
    B_id = as.character(pair_row$B_id),
    better_id = as.character(better_id),
    winner_pos = as.integer(winner_pos),
    phase = as.character(pair_row$phase),
    iter = as.integer(pair_row$iter),
    received_at = as.POSIXct("2026-01-01 00:05:00", tz = "UTC"),
    backend = "mock",
    model = "mock"
  )
}

testthat::test_that("validate_state enforces run-scale invariants", {
  state <- make_run_state()
  scheduled1 <- add_scheduled_pair(state, "A", "B")
  state <- scheduled1$state
  scheduled2 <- add_scheduled_pair(state, "C", "D")
  state <- scheduled2$state

  results_tbl <- make_result_tbl(scheduled1$pair_row)
  ingest <- pairwiseLLM:::.adaptive_ingest_results_incremental(state, results_tbl)
  state <- ingest$state
  state$last_refit_at <- 0L
  state$new_since_refit <- as.integer(state$comparisons_observed)

  testthat::expect_silent(pairwiseLLM:::validate_state(state))

  state_bad <- state
  state_bad$new_since_refit <- as.integer(state_bad$comparisons_observed + 1L)
  testthat::expect_error(
    pairwiseLLM:::validate_state(state_bad),
    "new_since_refit"
  )

  state_bad <- state
  state_bad$comparisons_scheduled <- -1L
  testthat::expect_error(
    pairwiseLLM:::validate_state(state_bad),
    "comparisons_scheduled"
  )

  state_bad <- state
  state_bad$comparisons_observed <- -1L
  testthat::expect_error(
    pairwiseLLM:::validate_state(state_bad),
    "comparisons_observed"
  )

  state_bad <- state
  state_bad$last_refit_at <- as.integer(state_bad$comparisons_observed + 1L)
  testthat::expect_error(
    pairwiseLLM:::validate_state(state_bad),
    "last_refit_at"
  )

  state_bad <- state
  state_bad$new_since_refit <- 0L
  state_bad$last_refit_at <- 0L
  testthat::expect_error(
    pairwiseLLM:::validate_state(state_bad),
    "comparisons_observed - last_refit_at"
  )

  state_bad <- state
  pair_key <- names(state_bad$pair_count)[[1L]]
  dup_counts <- rep(1L, 7L)
  names(dup_counts) <- rep(pair_key, 7L)
  state_bad$pair_count <- dup_counts
  testthat::expect_error(
    pairwiseLLM:::validate_state(state_bad),
    "more unique pairs"
  )
})

testthat::test_that("batch_log captures per-iteration deltas and backlog", {
  state <- make_run_state()
  scheduled1 <- add_scheduled_pair(state, "A", "B")
  state <- scheduled1$state
  state <- add_scheduled_pair(state, "C", "D")$state

  results_tbl <- make_result_tbl(scheduled1$pair_row)
  ingest <- pairwiseLLM:::.adaptive_ingest_results_incremental(state, results_tbl)
  state <- ingest$state

  state$log_counters <- list(comparisons_observed = 0L, failed_attempts = 0L)
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  selection <- tibble::tibble(
    unordered_key = "A:B",
    utility = 0.2
  )
  utilities <- tibble::tibble(
    unordered_key = "A:B",
    utility = 0.2
  )

  state <- pairwiseLLM:::.adaptive_append_batch_log(
    state = state,
    iter = 1L,
    phase = "phase1",
    mode = "adaptive",
    created_at = as.POSIXct("2026-01-01 00:10:00", tz = "UTC"),
    batch_size_target = 2L,
    selection = selection,
    candidate_stats = list(n_candidates_generated = 2L, n_candidates_after_filters = 1L),
    candidate_starved = TRUE,
    fallback_stage = "base_window",
    fallback_used = "base_window",
    fallback_path = NULL,
    stage_attempts = NULL,
    W_used = config$W,
    config = config,
    exploration_only = FALSE,
    utilities = utilities,
    iter_exit_path = "test"
  )

  batch_row <- state$batch_log[nrow(state$batch_log), , drop = FALSE]
  testthat::expect_equal(batch_row$n_pairs_selected[[1L]], 1L)
  testthat::expect_equal(batch_row$n_pairs_completed[[1L]], 1L)
  testthat::expect_equal(batch_row$n_pairs_failed[[1L]], 0L)
  testthat::expect_equal(
    batch_row$backlog_unjudged[[1L]],
    state$comparisons_scheduled - state$comparisons_observed
  )
  testthat::expect_equal(
    state$log_counters$comparisons_observed,
    state$comparisons_observed
  )
})

testthat::test_that("stop metrics and round_log rows align with run-scale counts", {
  state <- make_run_state()
  scheduled1 <- add_scheduled_pair(state, "A", "B")
  state <- scheduled1$state
  state <- add_scheduled_pair(state, "C", "D")$state

  results_tbl <- make_result_tbl(scheduled1$pair_row)
  ingest <- pairwiseLLM:::.adaptive_ingest_results_incremental(state, results_tbl)
  state <- ingest$state
  state$last_refit_at <- 0L
  state$new_since_refit <- as.integer(state$comparisons_observed)

  fit <- make_v3_fit_contract(state$ids)
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  candidates_with_utility <- tibble::tibble(
    unordered_key = "A:B",
    utility = 0.1
  )

  metrics <- pairwiseLLM:::compute_stop_metrics(
    state = state,
    fit = fit,
    candidates_with_utility = candidates_with_utility,
    config = config
  )
  testthat::expect_equal(metrics$scheduled_pairs, state$comparisons_scheduled)
  testthat::expect_equal(metrics$completed_pairs, state$comparisons_observed)
  testthat::expect_equal(metrics$proposed_pairs, nrow(candidates_with_utility))
  testthat::expect_equal(
    metrics$n_unique_pairs_seen,
    sum(state$pair_count >= 1L)
  )

  round_row <- pairwiseLLM:::build_round_log_row(
    state = state,
    fit = fit,
    metrics = metrics,
    config = config,
    new_pairs = state$new_since_refit
  )
  testthat::expect_equal(
    round_row$backlog_unjudged[[1L]],
    metrics$scheduled_pairs - metrics$completed_pairs
  )
})

testthat::test_that("resume preserves run-scale counts after partial results", {
  state <- make_run_state(budget_max = 1L)
  scheduled1 <- add_scheduled_pair(state, "A", "B")
  state <- scheduled1$state
  scheduled2 <- add_scheduled_pair(state, "C", "D")
  state <- scheduled2$state

  results_tbl <- make_result_tbl(scheduled1$pair_row)
  submission_info <- list(
    backend = "openai",
    model = "gpt-test",
    trait_name = "quality",
    trait_description = "Which is better?",
    prompt_template = "test",
    pairs_submitted = dplyr::bind_rows(scheduled1$pair_row, scheduled2$pair_row),
    results = results_tbl
  )

  resume_out <- pairwiseLLM::adaptive_rank_resume(
    state = state,
    mode = "live",
    submission_info = submission_info,
    adaptive = list(),
    seed = 123
  )

  resumed <- resume_out$state
  testthat::expect_equal(resumed$comparisons_scheduled, 1L)
  testthat::expect_equal(resumed$comparisons_observed, 1L)
  testthat::expect_equal(nrow(resumed$history_pairs), 1L)
  testthat::expect_equal(nrow(resumed$history_results), 1L)
  testthat::expect_equal(resumed$new_since_refit, 1L)
})
