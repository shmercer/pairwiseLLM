test_that("normalize_llm_results returns canonical observed rows with keys", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta",
    pair_uid = "pair-001"
  )

  raw <- tibble::tibble(
    custom_id = "pair-001",
    ID1 = "A",
    ID2 = "B",
    better_id = "A",
    status_code = 200L
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_equal(nrow(out$results), 1L)
  expect_equal(out$results$A_id, "A")
  expect_equal(out$results$B_id, "B")
  expect_equal(out$results$better_id, "A")
  expect_equal(out$results$winner_pos, 1L)
  expect_equal(out$results$unordered_key, "A:B")
  expect_equal(out$results$ordered_key, "A:B")
  expect_equal(out$results$pair_uid, "A:B#1")
  expect_equal(out$results$custom_id, "pair-001")
  expect_equal(out$results$backend, "openai")
  expect_equal(out$results$model, "gpt-test")
})

test_that("normalize_llm_results routes invalid winners to failed_attempts", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta"
  )

  raw <- tibble::tibble(
    ID1 = "A",
    ID2 = "B",
    better_id = "C",
    error_message = NA_character_
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "gemini",
    model = "gem-test",
    include_raw = FALSE
  )

  expect_equal(nrow(out$results), 0L)
  expect_equal(nrow(out$failed_attempts), 1L)
  expect_equal(out$failed_attempts$error_code, "invalid_winner")
})

test_that("normalize_llm_results honors include_raw and row-order alignment", {
  pairs <- tibble::tibble(
    ID1 = c("A", "C"),
    text1 = c("alpha", "charlie"),
    ID2 = c("B", "D"),
    text2 = c("beta", "delta")
  )

  raw <- tibble::tibble(
    better_id = c("A", "D"),
    raw_response = list(list(foo = "bar"), list(foo = "baz"))
  )

  out_keep <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "together",
    model = "together-test",
    include_raw = TRUE
  )

  out_drop <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "together",
    model = "together-test",
    include_raw = FALSE
  )

  expect_true("raw_response" %in% names(out_keep$results))
  expect_false("raw_response" %in% names(out_drop$results))
  expect_equal(nrow(out_keep$results), 2L)
})

test_that("normalize_llm_results classifies parse errors", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta"
  )

  raw <- tibble::tibble(
    ID1 = "A",
    ID2 = "B",
    better_id = NA_character_,
    error_message = "Failed to parse JSON response."
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "anthropic",
    model = "claude-test",
    include_raw = FALSE
  )

  expect_equal(nrow(out$results), 0L)
  expect_equal(out$failed_attempts$error_code, "parse_error")
})
