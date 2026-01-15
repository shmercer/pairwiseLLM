test_that("adaptive_state_new creates a valid initial state", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "charlie")
  )

  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  expect_silent(pairwiseLLM:::validate_state(state))
  expect_s3_class(state, "adaptive_state")
  expect_equal(length(state$unordered_count), 3L)
  expect_true(all(state$unordered_count == 0L))
})

test_that("record_exposure updates counts consistently", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  state2 <- pairwiseLLM:::record_exposure(state, "A", "B")

  expect_equal(state2$pos1[["A"]], 1L)
  expect_equal(state2$pos2[["B"]], 1L)
  expect_equal(state2$deg[["A"]], 1L)
  expect_equal(state2$deg[["B"]], 1L)
  expect_equal(state2$imb[["A"]], 1L)
  expect_equal(state2$imb[["B"]], -1L)
})

test_that("adaptive_state_save/load roundtrip preserves class and fields", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state <- pairwiseLLM:::record_exposure(state, "A", "B")

  tmp_dir <- withr::local_tempdir()
  path <- file.path(tmp_dir, "state.rds")

  pairwiseLLM:::adaptive_state_save(state, path)
  loaded <- pairwiseLLM:::adaptive_state_load(path)

  expect_s3_class(loaded, "adaptive_state")
  expect_equal(loaded$deg, state$deg)
  expect_equal(loaded$ordered_seen, state$ordered_seen)
})

test_that("failed_attempts does not affect comparisons_observed", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  failed_attempts <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    phase = "phase1",
    iter = 1L,
    attempted_at = as.POSIXct("2026-01-03 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test",
    error_code = "timeout",
    error_detail = NA_character_
  )

  state$failed_attempts <- failed_attempts

  expect_equal(state$comparisons_observed, 0L)
  expect_silent(pairwiseLLM:::validate_state(state))
})

test_that("validate_state rejects non-adaptive_state and bad results_seen", {
  expect_error(pairwiseLLM:::validate_state(list()), "adaptive_state")

  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  state$results_seen <- list(TRUE)
  expect_error(pairwiseLLM:::validate_state(state), "results_seen")

  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$results_seen <- c(TRUE, FALSE)
  expect_error(pairwiseLLM:::validate_state(state), "named")
})
