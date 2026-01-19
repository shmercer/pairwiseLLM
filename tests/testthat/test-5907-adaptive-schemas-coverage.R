make_schema_state <- function() {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  pairwiseLLM:::adaptive_state_new(samples, config = list())
}

testthat::test_that("validate_pairs_tbl checks optional columns", {
  pairs <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "bravo",
    phase = "phase1",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    backend = "openai"
  )

  expect_silent(pairwiseLLM:::validate_pairs_tbl(pairs))
})

testthat::test_that("validate_state rejects invalid pair_count configurations", {
  base_state <- make_schema_state()

  state <- base_state
  state$pair_count <- 0.5
  expect_error(pairwiseLLM:::validate_state(state), "pair_count")

  state <- base_state
  state$pair_count <- c(1L, 0L)
  expect_error(pairwiseLLM:::validate_state(state), "named")

  state <- base_state
  state$pair_count <- stats::setNames(c(1L, -1L), c("A:B", "A:C"))
  expect_error(pairwiseLLM:::validate_state(state), "non-negative")
})

testthat::test_that("validate_state enforces ordered count and refit invariants", {
  base_state <- make_schema_state()

  state <- base_state
  state$pair_ordered_count <- 0.5
  expect_error(pairwiseLLM:::validate_state(state), "pair_ordered_count")

  state <- base_state
  state$pair_ordered_count <- c(1L, 0L)
  expect_error(pairwiseLLM:::validate_state(state), "named")

  state <- base_state
  state$pair_ordered_count <- stats::setNames(c(-1L), "A:B")
  expect_error(pairwiseLLM:::validate_state(state), "non-negative")

  state <- base_state
  state$new_since_refit <- 1.5
  expect_error(pairwiseLLM:::validate_state(state), "new_since_refit")

  state <- base_state
  state$new_since_refit <- -1L
  expect_error(pairwiseLLM:::validate_state(state), "non-negative")

  state <- base_state
  state$last_refit_at <- 1.5
  expect_error(pairwiseLLM:::validate_state(state), "last_refit_at")

  state <- base_state
  state$last_refit_at <- -1L
  expect_error(pairwiseLLM:::validate_state(state), "non-negative")
})

testthat::test_that("validate_state enforces posterior and mode fields", {
  base_state <- make_schema_state()

  state <- base_state
  state$posterior <- "bad"
  expect_error(pairwiseLLM:::validate_state(state), "posterior")

  state <- base_state
  state$posterior <- list()
  expect_error(pairwiseLLM:::validate_state(state), "U_dup_threshold")

  state <- base_state
  state$posterior <- list(U_dup_threshold = c(0.1, 0.2))
  expect_error(pairwiseLLM:::validate_state(state), "numeric length 1")

  state <- base_state
  state$mode <- 1
  expect_error(pairwiseLLM:::validate_state(state), "mode")

  state <- base_state
  state$repair_attempts <- 1.5
  expect_error(pairwiseLLM:::validate_state(state), "repair_attempts")

  state <- base_state
  state$repair_attempts <- -1L
  expect_error(pairwiseLLM:::validate_state(state), "non-negative")

  state <- base_state
  state$stop_reason <- c("a", "b")
  expect_error(pairwiseLLM:::validate_state(state), "stop_reason")
})
