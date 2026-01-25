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
  expect_equal(out$results$pair_uid, "pair-001")
  expect_equal(out$results$pair_uid_provided, "pair-001")
  expect_equal(out$results$custom_id, "pair-001")
  expect_equal(out$results$backend, "openai")
  expect_equal(out$results$model, "gpt-test")
  expect_type(out$results$ID1, "character")
  expect_type(out$results$ID2, "character")
})

test_that("normalize_llm_results assigns pair_uid per row when missing in pairs", {
  pairs <- tibble::tibble(
    ID1 = c("S17", "S19"),
    text1 = c("alpha", "bravo"),
    ID2 = c("S12", "S15"),
    text2 = c("charlie", "delta")
  )

  raw <- tibble::tibble(
    ID1 = c("S17", "S19"),
    ID2 = c("S12", "S15"),
    better_id = c("S17", "S15")
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_equal(out$results$unordered_key, c("S12:S17", "S15:S19"))
  expect_equal(out$results$pair_uid, c("S12:S17#1", "S15:S19#1"))
  expect_true(all(is.na(out$results$pair_uid_provided)))
})

test_that("failed_attempts_from_pairs computes pair_uid per row when missing", {
  pairs <- tibble::tibble(
    ID1 = c("S17", "S19"),
    text1 = c("alpha", "bravo"),
    ID2 = c("S12", "S15"),
    text2 = c("charlie", "delta")
  )

  out <- .pairwiseLLM_failed_attempts_from_pairs(
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    error_code = "http_error",
    error_detail = "unit test"
  )

  expect_equal(out$unordered_key, c("S12:S17", "S15:S19"))
  expect_equal(out$pair_uid, c("S12:S17#1", "S15:S19#1"))
  expect_true(all(is.na(out$pair_uid_provided)))
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

test_that("normalize_llm_results errors when pairs lack required identifiers", {
  bad_pairs <- tibble::tibble(foo = "A", bar = "B")
  raw <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")

  expect_error(
    .normalize_llm_results(
      raw = raw,
      pairs = bad_pairs,
      backend = "openai",
      model = "gpt-test",
      include_raw = FALSE
    ),
    "must contain columns"
  )
})

test_that("normalize_llm_results falls back to row-order alignment with raw IDs", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta"
  )

  raw <- tibble::tibble(
    ID1 = "X",
    ID2 = "Y",
    better_id = "X",
    status_code = 200L,
    error_message = NA_character_
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_equal(out$alignment, "row_order")
  expect_equal(out$results$A_id, "A")
  expect_equal(out$results$B_id, "B")
  expect_equal(out$results$better_id, "A")
})

test_that("normalize_llm_results classifies timeout/http/result_type errors", {
  pairs <- tibble::tibble(
    ID1 = c("A", "C", "E"),
    text1 = c("alpha", "charlie", "echo"),
    ID2 = c("B", "D", "F"),
    text2 = c("beta", "delta", "foxtrot")
  )

  raw <- tibble::tibble(
    ID1 = c("A", "C", "E"),
    ID2 = c("B", "D", "F"),
    better_id = c(NA_character_, NA_character_, NA_character_),
    status_code = c(500L, NA_integer_, NA_integer_),
    error_message = c(NA_character_, "Request timed out", NA_character_),
    result_type = c(NA_character_, NA_character_, "errored")
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_equal(nrow(out$results), 0L)
  expect_equal(out$failed_attempts$error_code, c("http_error", "timeout", "http_error"))
})

test_that("normalize_llm_results appends retry_failures to failed_attempts", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta"
  )

  raw <- tibble::tibble(
    ID1 = "A",
    ID2 = "B",
    better_id = "A",
    retry_failures = list(tibble::tibble(
      error_code = "http_error",
      error_detail = "HTTP 429",
      attempted_at = as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
    ))
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_equal(nrow(out$results), 1L)
  expect_true(any(out$failed_attempts$error_code == "http_error"))
  expect_true(any(grepl("HTTP 429", out$failed_attempts$error_detail)))
})

test_that("normalize_llm_results merges backend failed_attempts by custom_id", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta",
    pair_uid = "pair-42"
  )

  raw <- list(
    results = tibble::tibble(
      custom_id = "pair-42",
      ID1 = "A",
      ID2 = "B",
      better_id = "A"
    ),
    failed_attempts = tibble::tibble(
      custom_id = "pair-42",
      error_message = "timeout waiting"
    )
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_true(any(out$failed_attempts$error_code == "timeout"))
  expect_true(any(grepl("timeout", out$failed_attempts$error_detail)))
})

test_that("normalize_llm_results supports alternate ID column names in pairs", {
  raw <- tibble::tibble(
    ID1 = "A",
    ID2 = "B",
    better_id = "A"
  )

  variants <- list(
    tibble::tibble(A_id = "A", B_id = "B", A_text = "a", B_text = "b"),
    tibble::tibble(A = "A", B = "B"),
    tibble::tibble(idA = "A", idB = "B"),
    tibble::tibble(ID_A = "A", ID_B = "B"),
    tibble::tibble(id1 = "A", id2 = "B")
  )

  for (pairs in variants) {
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
  }
})

test_that("normalize_llm_results treats missing better_id as failure", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta"
  )

  raw <- tibble::tibble(
    custom_id = "MALFORMED_ID",
    better_id = NA_character_,
    status_code = 200L,
    error_message = NA_character_
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_equal(out$alignment, "row_order")
  expect_equal(nrow(out$results), 0L)
  expect_true(any(out$failed_attempts$error_code == "backend_missing_fields"))
})

test_that("normalize_llm_results supplies defaults for retry_failures fields", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta"
  )

  raw <- tibble::tibble(
    ID1 = "A",
    ID2 = "B",
    better_id = "A",
    retry_failures = list(tibble::tibble(error_detail = "HTTP 429"))
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_true(any(out$failed_attempts$error_code == "http_error"))
  expect_true(any(grepl("HTTP 429", out$failed_attempts$error_detail)))
  expect_s3_class(out$failed_attempts$attempted_at, "POSIXct")
})

test_that("normalize_llm_results errors on mismatched raw and pairs sizes", {
  pairs <- tibble::tibble(
    ID1 = c("A", "C"),
    text1 = c("alpha", "charlie"),
    ID2 = c("B", "D"),
    text2 = c("beta", "delta")
  )

  raw <- tibble::tibble(
    better_id = "A"
  )

  expect_error(
    .normalize_llm_results(
      raw = raw,
      pairs = pairs,
      backend = "openai",
      model = "gpt-test",
      include_raw = FALSE
    ),
    "Unable to align"
  )
})

test_that("normalize_llm_results records duplicate custom_id attempts with defaults", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta",
    pair_uid = "pair-dup"
  )

  raw <- tibble::tibble(
    custom_id = c("pair-dup", "pair-dup"),
    ordered_occurrence_index = c(1L, 2L),
    better_id = c("A", "A")
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_true(nrow(out$failed_attempts) >= 1L)
  expect_true(any(out$failed_attempts$error_code == "http_error"))
  expect_s3_class(out$failed_attempts$attempted_at, "POSIXct")
})

test_that("normalize_llm_results handles unsupported attempted_at types", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta"
  )

  raw <- tibble::tibble(
    ID1 = c("A", "A"),
    ID2 = c("B", "B"),
    better_id = c("A", "A"),
    attempted_at = list(list("weird"), list("weird"))
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_true(nrow(out$failed_attempts) >= 1L)
  expect_s3_class(out$failed_attempts$attempted_at, "POSIXct")
  expect_true(any(!is.na(out$failed_attempts$attempted_at)))
})

test_that("normalize_llm_results supplies retry_failures error_detail default", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta"
  )

  raw <- tibble::tibble(
    ID1 = "A",
    ID2 = "B",
    better_id = "A",
    retry_failures = list(tibble::tibble(error_code = "http_error"))
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_true(any(is.na(out$failed_attempts$error_detail)))
})

test_that("normalize_llm_results parses custom_id edge cases", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta"
  )

  raw_na <- tibble::tibble(
    custom_id = NA_character_,
    better_id = NA_character_,
    status_code = 200L
  )

  out_na <- .normalize_llm_results(
    raw = raw_na,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )
  expect_equal(out_na$alignment, "row_order")

  raw_no_underscore <- tibble::tibble(
    custom_id = "LIVEA_vs_B",
    better_id = NA_character_,
    status_code = 200L
  )

  out_no <- .normalize_llm_results(
    raw = raw_no_underscore,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )
  expect_equal(out_no$alignment, "row_order")
})

test_that("normalize_llm_results retains cost for failed attempts", {
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
    cost = 0.01
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_true(any(!is.na(out$failed_attempts$cost)))
})

test_that("normalize_llm_results merges raw_failed by ID1/ID2", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta"
  )

  raw <- list(
    results = tibble::tibble(
      ID1 = "A",
      ID2 = "B",
      better_id = "A"
    ),
    failed_attempts = tibble::tibble(
      ID1 = "A",
      ID2 = "B",
      error_message = "HTTP 500"
    )
  )

  out <- suppressWarnings(.normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  ))

  expect_true(any(out$failed_attempts$error_code == "http_error"))
})

test_that("normalize_llm_results coalesces suffixed columns from row-order alignment", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta"
  )

  raw <- tibble::tibble(
    better_id = "A",
    `ID1.x` = "shadow"
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_equal(out$results$ID1, "A")
})

test_that("normalize_llm_results drops .x/.y columns after ID join", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta"
  )

  raw <- tibble::tibble(
    ID1 = "A",
    ID2 = "B",
    better_id = "A",
    unordered_key = "A:B",
    ordered_key = "A:B"
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_false(any(grepl("\\\\.x$|\\\\.y$", names(out$results))))
})

test_that("normalize_llm_results outputs satisfy adaptive schema validators", {
  pairs <- tibble::tibble(
    ID1 = c("A", "C"),
    text1 = c("alpha", "charlie"),
    ID2 = c("B", "D"),
    text2 = c("beta", "delta"),
    pair_uid = c("pair-1", "pair-2")
  )

  raw <- tibble::tibble(
    custom_id = c("pair-1", "pair-2"),
    ID1 = c("A", "C"),
    ID2 = c("B", "D"),
    better_id = c("A", "E"),
    error_message = c(NA_character_, "invalid winner")
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_silent(validate_results_tbl(out$results))
  expect_silent(validate_failed_attempts_tbl(out$failed_attempts))
})

test_that("normalize_llm_results records duplicate custom_id attempts with extra metadata", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta",
    pair_uid = "pair-meta"
  )

  raw <- tibble::tibble(
    custom_id = c("pair-meta", "pair-meta"),
    ID1 = c("A", "A"),
    ID2 = c("B", "B"),
    better_id = c("A", "A"),
    ordered_occurrence_index = c(1L, 2L),
    status_code = c(500L, 200L),
    error_message = c("HTTP 500", NA_character_),
    prompt_tokens = c(10, 11),
    completion_tokens = c(20, 21),
    total_tokens = c(30, 32),
    cost = c(0.1, 0.2)
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_true(nrow(out$failed_attempts) >= 1L)
  expect_true(is.integer(out$failed_attempts$status_code))
  expect_true(is.numeric(out$failed_attempts$total_tokens))
})

test_that("normalize_llm_results records duplicate ordered_key attempts with totals", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta"
  )

  raw <- tibble::tibble(
    ID1 = c("A", "A"),
    ID2 = c("B", "B"),
    better_id = c("A", "A"),
    total_tokens = c(40, 41)
  )

  out <- .normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )

  expect_true(any(out$failed_attempts$error_code == "http_error"))
  expect_true(is.numeric(out$failed_attempts$total_tokens))
})
