test_that("validate_pairs_tbl accepts minimal required columns", {
  pairs <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "beta",
    phase = "phase1",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )

  expect_silent(validate_pairs_tbl(pairs))
})

test_that("validate_pairs_tbl fails on missing required columns", {
  pairs <- tibble::tibble(
    pair_uid = "A:B#1",
    A_id = "A"
  )

  expect_error(validate_pairs_tbl(pairs), "missing required columns")
})

test_that("validate_pairs_tbl rejects wrong types", {
  pairs <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "beta",
    phase = "phase1",
    iter = 1,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )

  expect_error(validate_pairs_tbl(pairs), "iter")
})

test_that("validate_results_tbl accepts observed-only rows", {
  results <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase1",
    iter = 1L,
    received_at = as.POSIXct("2026-01-02 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test"
  )

  expect_silent(validate_results_tbl(results))
})

test_that("validate_results_tbl rejects invalid better_id", {
  results <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    better_id = "C",
    winner_pos = 1L,
    phase = "phase1",
    iter = 1L,
    received_at = as.POSIXct("2026-01-02 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test"
  )

  expect_error(validate_results_tbl(results), "better_id")
})

test_that("validate_failed_attempts_tbl accepts minimal required columns", {
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

  expect_silent(validate_failed_attempts_tbl(failed_attempts))
})

test_that("validate_failed_attempts_tbl rejects unsupported error_code", {
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
    error_code = "bad_code",
    error_detail = "oops"
  )

  expect_error(validate_failed_attempts_tbl(failed_attempts), "error_code")
})
