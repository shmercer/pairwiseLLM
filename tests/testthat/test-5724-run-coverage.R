testthat::test_that("adaptive_rollback_presentations exits when rollback table empties", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$results_seen <- c("p1" = TRUE)

  pairs_submitted <- tibble::tibble(
    pair_uid = "p1",
    A_id = "A",
    B_id = "B"
  )

  failed_attempts <- tibble::tibble(
    pair_uid = "p1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    phase = "phase2",
    iter = 1L,
    attempted_at = as.POSIXct("2020-01-01", tz = "UTC"),
    backend = "openai",
    model = "gpt",
    error_code = "timeout",
    error_detail = "retry"
  )

  out <- pairwiseLLM:::.adaptive_rollback_presentations(state, pairs_submitted, failed_attempts)
  testthat::expect_identical(out, state)
})

testthat::test_that("adaptive_starvation_reason_from_attempts handles invalid batch size", {
  attempts <- tibble::tibble(n_selected = 0L, n_generated = 5L)
  reason <- pairwiseLLM:::.adaptive_starvation_reason_from_attempts(
    attempts,
    batch_size = -1L,
    config = list(C_max = 20L)
  )
  testthat::expect_true(is.na(reason))
})

testthat::test_that("adaptive_stage_attempts_fields uses defaults for NULL values", {
  attempts <- list(tibble::tibble(
    stage = list(NULL),
    n_generated = 1L,
    n_survive = 1L,
    n_selected = 1L
  ))

  fields <- pairwiseLLM:::.adaptive_stage_attempts_fields(
    stage_attempts = attempts,
    batch_size = 1L,
    config = list(C_max = 20L),
    candidate_starved = FALSE
  )

  testthat::expect_true(is.na(fields$a1_stage))
})
