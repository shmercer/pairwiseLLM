testthat::test_that("adaptive run rollback helpers validate inputs", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L)
  )

  testthat::expect_error(
    pairwiseLLM:::.adaptive_remove_history_pair(state, ""),
    "pair_uid"
  )

  testthat::expect_error(
    pairwiseLLM:::.adaptive_remove_history_pair(state, "A:B#1"),
    "history_pairs"
  )

  pair_row <- tibble::tibble(
    pair_uid = c("A:B#1", "A:B#1"),
    unordered_key = c("A:B", "A:B"),
    ordered_key = c("A:B", "A:B"),
    A_id = c("A", "A"),
    B_id = c("B", "B"),
    A_text = c("alpha", "alpha"),
    B_text = c("bravo", "bravo"),
    phase = c("phase1", "phase1"),
    iter = c(0L, 0L),
    created_at = as.POSIXct(c("2026-01-01 00:00:00", "2026-01-01 00:00:00"), tz = "UTC")
  )
  state$history_pairs <- pair_row
  state$comparisons_scheduled <- 2L
  testthat::expect_error(
    pairwiseLLM:::.adaptive_remove_history_pair(state, "A:B#1"),
    "exactly one"
  )

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L)
  )
  state <- pairwiseLLM:::record_presentation(state, "A", "B")
  state$history_pairs <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "bravo",
    phase = "phase1",
    iter = 0L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )
  state$comparisons_scheduled <- 0L
  testthat::expect_error(
    pairwiseLLM:::.adaptive_remove_history_pair(state, "A:B#1"),
    "comparisons_scheduled"
  )
})

testthat::test_that("adaptive rollback and starvation helpers cover branches", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L)
  )
  state <- pairwiseLLM:::record_presentation(state, "A", "B")
  state$history_pairs <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "bravo",
    phase = "phase1",
    iter = 0L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )
  state$comparisons_scheduled <- 1L

  pairs_submitted <- state$history_pairs
  state <- pairwiseLLM:::.adaptive_rollback_presentations(state, pairs_submitted)
  testthat::expect_equal(nrow(state$history_pairs), 0L)
  testthat::expect_equal(state$comparisons_scheduled, 0L)

  attempts <- tibble::tibble(
    stage = c("stage1", "stage2", "stage3", "stage4"),
    n_generated = c(3L, 4L, 5L, 6L),
    n_survive = c(2L, 2L, 2L, 2L),
    n_selected = c(1L, 1L, 1L, 1L),
    W_used = c(3L, 3L, 3L, 3L),
    anchor_pool = c("pool", "pool", "pool", "pool")
  )
  reason <- pairwiseLLM:::.adaptive_starvation_reason_from_attempts(
    attempts,
    batch_size = 2L,
    config = list(C_max = 200L)
  )
  testthat::expect_identical(reason, "few_candidates_generated")

  reason <- pairwiseLLM:::.adaptive_starvation_reason_from_attempts(
    stage_attempts = list(),
    batch_size = 2L,
    config = list(C_max = 200L)
  )
  testthat::expect_true(is.na(reason))

  reason <- pairwiseLLM:::.adaptive_starvation_reason_from_attempts(
    stage_attempts = tibble::tibble(n_selected = 1L),
    batch_size = -1L,
    config = list(C_max = 200L)
  )
  testthat::expect_true(is.na(reason))

  reason <- pairwiseLLM:::.adaptive_starvation_reason_from_attempts(
    attempts,
    batch_size = 1L,
    config = list(C_max = 50L)
  )
  testthat::expect_true(is.na(reason))

  fields <- pairwiseLLM:::.adaptive_stage_attempts_fields(
    stage_attempts = list(attempts),
    batch_size = 2L,
    config = list(C_max = 200L),
    candidate_starved = TRUE
  )
  testthat::expect_equal(fields$a1_stage, "stage1")
  testthat::expect_equal(fields$aN_tried, 1L)
  testthat::expect_equal(fields$aN_best_stage, "stage4")
  testthat::expect_equal(fields$starvation_reason, "few_candidates_generated")

  fields <- pairwiseLLM:::.adaptive_stage_attempts_fields(
    stage_attempts = list(),
    batch_size = 2L,
    config = list(C_max = 200L),
    candidate_starved = FALSE
  )
  testthat::expect_true(is.na(fields$a1_stage))
  testthat::expect_true(is.na(fields$starvation_reason))
})

testthat::test_that("adaptive schedule replacement pairs validates inputs", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 1L)
  )

  out <- pairwiseLLM:::.adaptive_schedule_replacement_pairs(
    state,
    target_pairs = 0L,
    adaptive = list(),
    seed = 1L,
    replacement_phase = "phase2"
  )
  testthat::expect_equal(nrow(out$pairs), 0L)

  state <- pairwiseLLM:::record_presentation(state, "A", "B")
  state$history_pairs <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "bravo",
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )
  state$comparisons_scheduled <- as.integer(nrow(state$history_pairs))
  out <- pairwiseLLM:::.adaptive_schedule_replacement_pairs(
    state,
    target_pairs = 1L,
    adaptive = list(),
    seed = 1L,
    replacement_phase = "phase2"
  )
  testthat::expect_equal(nrow(out$pairs), 0L)

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 1L)
  )
  testthat::expect_error(
    pairwiseLLM:::.adaptive_schedule_replacement_pairs(
      state,
      target_pairs = 1L,
      adaptive = list(),
      seed = 1L,
      replacement_phase = ""
    ),
    "replacement_phase"
  )

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 1L)
  )
  scheduled_pairs <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "bravo",
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )

  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_schedule_replacement_pairs(
      state,
      target_pairs = 1L,
      adaptive = list(),
      seed = 1L,
      replacement_phase = "phase2"
    ),
    .adaptive_schedule_next_pairs = function(state, target_pairs, adaptive, seed, near_stop = FALSE) {
      state$config$skip_stop_checks <- TRUE
      list(state = state, pairs = scheduled_pairs)
    }
  )
  testthat::expect_true(is.null(out$state$config$skip_stop_checks))
  testthat::expect_equal(nrow(out$pairs), 1L)
})
