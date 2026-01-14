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

test_that("normalize_llm_results reads pairs from list column and pairs_path", {
  base_pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta"
  )
  raw <- tibble::tibble(
    ID1 = "A",
    ID2 = "B",
    better_id = "A"
  )

  list_pairs <- tibble::tibble(pairs = list(base_pairs))
  out_list <- .normalize_llm_results(
    raw = raw,
    pairs = list_pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )
  expect_equal(nrow(out_list$results), 1L)

  tmp_dir <- withr::local_tempdir()
  pairs_path <- file.path(tmp_dir, "pairs.rds")
  saveRDS(base_pairs, pairs_path)
  path_pairs <- tibble::tibble(pairs_path = pairs_path)
  out_path <- .normalize_llm_results(
    raw = raw,
    pairs = path_pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )
  expect_equal(nrow(out_path$results), 1L)
})

test_that("normalize_llm_results supports A/B pair schema and legacy custom_id parsing", {
  pairs <- tibble::tibble(
    A_id = "A",
    A_text = "alpha",
    B_id = "B",
    B_text = "beta"
  )
  raw <- tibble::tibble(
    custom_id = "LIVE_A_vs_B",
    better_id = "A"
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_equal(out$results$A_id, "A")
  expect_equal(out$results$B_id, "B")
  expect_equal(out$results$custom_id, "LIVE_A_vs_B")
})

test_that("normalize_llm_results coerces duplicate attempted_at dates", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta",
    pair_uid = "pair-1"
  )
  raw <- tibble::tibble(
    custom_id = c("pair-1", "pair-1"),
    ID1 = c("A", "A"),
    ID2 = c("B", "B"),
    better_id = c("A", "A"),
    attempted_at = as.Date("2024-01-01")
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_equal(nrow(out$failed_attempts), 1L)
  expect_s3_class(out$failed_attempts$attempted_at, "POSIXct")
})

test_that("normalize_llm_results coerces duplicate attempted_at numeric timestamps", {
  pairs <- tibble::tibble(
    ID1 = "C",
    text1 = "charlie",
    ID2 = "D",
    text2 = "delta",
    pair_uid = "pair-2"
  )
  raw <- tibble::tibble(
    custom_id = c("pair-2", "pair-2"),
    ID1 = c("C", "C"),
    ID2 = c("D", "D"),
    better_id = c("D", "D"),
    attempted_at = 1700000000
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_equal(nrow(out$failed_attempts), 1L)
  expect_s3_class(out$failed_attempts$attempted_at, "POSIXct")
})

test_that("normalize_llm_results coerces duplicate attempted_at character timestamps", {
  pairs <- tibble::tibble(
    ID1 = "E",
    text1 = "echo",
    ID2 = "F",
    text2 = "foxtrot",
    pair_uid = "pair-3"
  )
  raw <- tibble::tibble(
    custom_id = c("pair-3", "pair-3"),
    ID1 = c("E", "E"),
    ID2 = c("F", "F"),
    better_id = c("E", "E"),
    attempted_at = "2024-01-03 00:00:00"
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_equal(nrow(out$failed_attempts), 1L)
  expect_s3_class(out$failed_attempts$attempted_at, "POSIXct")
})

test_that("normalize_llm_results merges backend failed_attempts payloads", {
  pairs <- tibble::tibble(
    ID1 = c("A", "C"),
    text1 = c("alpha", "charlie"),
    ID2 = c("B", "D"),
    text2 = c("beta", "delta"),
    pair_uid = c("pair-1", "pair-2")
  )
  raw <- list(
    results = tibble::tibble(
      custom_id = "pair-1",
      ID1 = "A",
      ID2 = "B",
      better_id = "A"
    ),
    failed_attempts = tibble::tibble(
      custom_id = "pair-2",
      error_message = "timeout while waiting"
    )
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_equal(nrow(out$results), 1L)
  expect_true(nrow(out$failed_attempts) >= 1L)
  expect_true("timeout" %in% out$failed_attempts$error_code)
})
