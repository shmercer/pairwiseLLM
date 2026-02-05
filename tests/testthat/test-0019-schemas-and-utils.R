test_that("schemas validate pair tables and results", {
  out <- pairwiseLLM:::as_pairs_tbl(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "beta",
    phase = "phase1",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    utility = 1.5,
    extra_col = "extra"
  )

  required <- c(
    "pair_uid", "unordered_key", "ordered_key",
    "A_id", "B_id", "A_text", "B_text",
    "phase", "iter", "created_at"
  )
  expect_equal(names(out)[seq_along(required)], required)
  expect_true("extra_col" %in% names(out))

  expect_error(pairwiseLLM:::validate_pairs_tbl("bad"), "data frame")

  bad_phase <- out
  bad_phase$phase <- "phaseX"
  expect_error(pairwiseLLM:::validate_pairs_tbl(bad_phase), "phase")

  bad_type <- out
  bad_type$utility <- "nope"
  expect_error(pairwiseLLM:::validate_pairs_tbl(bad_type), "utility")
})

test_that("schemas reject invalid results and failed attempts", {
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

  results_bad <- results
  results_bad$better_id <- NA_character_
  expect_error(pairwiseLLM:::validate_results_tbl(results_bad), "better_id")

  results_bad <- results
  results_bad$winner_pos <- 3L
  expect_error(pairwiseLLM:::validate_results_tbl(results_bad), "winner_pos")

  results_bad <- results
  results_bad$winner_pos <- 2L
  expect_error(pairwiseLLM:::validate_results_tbl(results_bad), "winner_pos")

  expect_error(pairwiseLLM:::validate_results_tbl("bad"), "data frame")

  failed <- tibble::tibble(
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
  expect_error(pairwiseLLM:::validate_failed_attempts_tbl(failed), "error_code")
  expect_error(pairwiseLLM:::validate_failed_attempts_tbl("bad"), "data frame")
})

test_that("adaptive_with_seed preserves RNG state", {
  withr::local_seed(1)
  baseline_pre <- stats::runif(3)
  baseline_post <- stats::runif(3)

  withr::local_seed(1)
  pre <- stats::runif(3)
  pairwiseLLM:::.adaptive_with_seed(42, stats::runif(1))
  post <- stats::runif(3)

  expect_equal(pre, baseline_pre)
  expect_equal(post, baseline_post)
})
