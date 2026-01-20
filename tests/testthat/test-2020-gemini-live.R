# tests/testthat/test-gemini_live.R

trait_description <- pairwiseLLM:::trait_description
set_prompt_template <- pairwiseLLM:::set_prompt_template
gemini_compare_pair_live <- pairwiseLLM::gemini_compare_pair_live
submit_gemini_pairs_live <- pairwiseLLM::submit_gemini_pairs_live

test_that("gemini_compare_pair_live parses a successful response without
          thoughts", {
  skip_if_not_installed("httr2")

  ns <- asNamespace("pairwiseLLM")

  fake_resp <- structure(list(), class = "httr2_response")

  fake_body <- list(
    model = "models/gemini-3-pro-preview",
    candidates = list(
      list(
        content = list(
          parts = list(
            list(text = "Some internal text that we treat as content. "),
            list(text = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")
          )
        )
      )
    ),
    usageMetadata = list(
      promptTokenCount     = 42L,
      candidatesTokenCount = 7L,
      totalTokenCount      = 49L
    )
  )

  testthat::local_mocked_bindings(
    # Avoid hitting .get_api_key() / env
    .gemini_api_key = function(api_key = NULL) "TEST_GEMINI_KEY",
    .gemini_req_perform = function(req) fake_resp,
    .gemini_resp_status = function(resp) 200L,
    .gemini_resp_body_json = function(resp, ...) fake_body,
    .env = ns
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  res <- gemini_compare_pair_live(
    ID1               = "S01",
    text1             = "Sample 1 text.",
    ID2               = "S02",
    text2             = "Sample 2 text.",
    model             = "gemini-3-pro-preview",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    thinking_level    = "low",
    include_thoughts  = FALSE,
    include_raw       = TRUE
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)

  expect_equal(res$ID1, "S01")
  expect_equal(res$ID2, "S02")
  expect_equal(res$model, "models/gemini-3-pro-preview")
  expect_equal(res$object_type, "generateContent")
  expect_equal(res$status_code, 200L)
  expect_true(is.na(res$error_message) || identical(res$error_message, ""))

  # Without include_thoughts, everything is collapsed into content
  expect_true(is.na(res$thoughts))
  expect_true(grepl("Some internal text", res$content, fixed = TRUE))
  expect_true(grepl("<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>", res$content,
    fixed = TRUE
  ))

  expect_equal(res$better_sample, "SAMPLE_1")
  expect_equal(res$better_id, "S01")

  expect_equal(res$prompt_tokens, 42)
  expect_equal(res$completion_tokens, 7)
  expect_equal(res$total_tokens, 49)

  expect_true("raw_response" %in% names(res))
  expect_type(res$raw_response, "list")
  expect_identical(res$raw_response[[1]], fake_body)
})

test_that("gemini_compare_pair_live parses thoughts and content when
          include_thoughts = TRUE", {
  skip_if_not_installed("httr2")

  ns <- asNamespace("pairwiseLLM")

  fake_resp <- structure(list(), class = "httr2_response")

  fake_body <- list(
    model = "models/gemini-3-pro-preview",
    candidates = list(
      list(
        content = list(
          parts = list(
            list(text = "These are my detailed thoughts..."),
            list(text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>")
          )
        )
      )
    ),
    usageMetadata = list()
  )

  testthat::local_mocked_bindings(
    .gemini_api_key = function(api_key = NULL) "TEST_GEMINI_KEY",
    .gemini_req_perform = function(req) fake_resp,
    .gemini_resp_status = function(resp) 200L,
    .gemini_resp_body_json = function(resp, ...) fake_body,
    .env = ns
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  res <- gemini_compare_pair_live(
    ID1               = "S01",
    text1             = "Sample 1 text.",
    ID2               = "S02",
    text2             = "Sample 2 text.",
    model             = "gemini-3-pro-preview",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    thinking_level    = "high",
    include_thoughts  = TRUE,
    include_raw       = FALSE
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)

  # thoughts should come from first part, content from subsequent parts
  expect_equal(res$thoughts, "These are my detailed thoughts...")
  expect_equal(res$content, "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>")

  expect_equal(res$better_sample, "SAMPLE_2")
  expect_equal(res$better_id, "S02")

  # usageMetadata missing -> NAs
  expect_true(is.na(res$prompt_tokens))
  expect_true(is.na(res$completion_tokens))
  expect_true(is.na(res$total_tokens))
})

test_that("gemini_compare_pair_live handles responses without
          <BETTER_SAMPLE> tag", {
  skip_if_not_installed("httr2")

  ns <- asNamespace("pairwiseLLM")

  fake_resp <- structure(list(), class = "httr2_response")

  fake_body <- list(
    model = "models/gemini-3-pro-preview",
    candidates = list(
      list(
        content = list(
          parts = list(
            list(text = "I forgot to include the tag, sorry.")
          )
        )
      )
    )
  )

  testthat::local_mocked_bindings(
    .gemini_api_key = function(api_key = NULL) "TEST_GEMINI_KEY",
    .gemini_req_perform = function(req) fake_resp,
    .gemini_resp_status = function(resp) 200L,
    .gemini_resp_body_json = function(resp, ...) fake_body,
    .env = ns
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  res <- gemini_compare_pair_live(
    ID1               = "S01",
    text1             = "Sample 1 text.",
    ID2               = "S02",
    text2             = "Sample 2 text.",
    model             = "gemini-3-pro-preview",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    thinking_level    = "low",
    include_thoughts  = FALSE
  )

  expect_true(is.na(res$better_sample))
  expect_true(is.na(res$better_id))
})

test_that("gemini_compare_pair_live returns an error row when
          request fails", {
  skip_if_not_installed("httr2")

  ns <- asNamespace("pairwiseLLM")

  # Simulate a generic error thrown by .gemini_req_perform
  testthat::local_mocked_bindings(
    .gemini_api_key = function(api_key = NULL) "TEST_GEMINI_KEY",
    .gemini_req_perform = function(req) stop("HTTP 500 Internal Server Error"),
    .gemini_resp_status = function(resp) 500L,
    .gemini_resp_body_json = function(resp, ...) NULL,
    .env = ns
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  res <- gemini_compare_pair_live(
    ID1               = "S01",
    text1             = "Sample 1 text.",
    ID2               = "S02",
    text2             = "Sample 2 text.",
    model             = "gemini-3-pro-preview",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl
  )

  expect_equal(nrow(res), 1L)
  expect_equal(res$ID1, "S01")
  expect_equal(res$ID2, "S02")

  expect_true(is.na(res$object_type))
  expect_true(is.na(res$content))
  expect_true(is.na(res$thoughts))
  expect_true(is.na(res$better_sample))
  expect_true(is.na(res$better_id))

  expect_match(res$error_message, "HTTP 500", fixed = FALSE)
})

test_that("gemini_compare_pair_live handles httr2_http errors and extracts body", {
  skip_if_not_installed("httr2")
  ns <- asNamespace("pairwiseLLM")

  # Create a real httr2 response with a body
  # Note: httr2::response() requires httr2 >= 0.2.0
  err_body_text <- "{\"error\": \"Invalid argument details\"}"
  fake_err_resp <- httr2::response(
    status_code = 400,
    body = charToRaw(err_body_text),
    headers = list("Content-Type" = "application/json")
  )

  # Create the condition object that httr2 throws
  cnd <- structure(
    list(message = "HTTP 400 Bad Request", resp = fake_err_resp),
    class = c("httr2_http", "error", "condition")
  )

  testthat::local_mocked_bindings(
    .gemini_api_key = function(...) "TEST_KEY",
    .gemini_request = function(...) structure(list(), class = "httr2_request"),
    .gemini_req_body_json = function(req, body) req,
    .gemini_req_perform = function(...) stop(cnd),
    .env = ns
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  res <- gemini_compare_pair_live(
    ID1 = "A", text1 = "A", ID2 = "B", text2 = "B",
    model = "gemini-3-pro-preview",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl
  )

  expect_equal(res$status_code, 400L)
  # The error message should contain the original message AND the body
  expect_match(res$error_message, "HTTP 400 Bad Request", fixed = TRUE)
  expect_match(res$error_message, err_body_text, fixed = TRUE)
})

test_that("gemini_compare_pair_live validates model and maps thinking_level
          medium to High", {
  skip_if_not_installed("httr2")

  ns <- asNamespace("pairwiseLLM")

  fake_resp <- structure(list(), class = "httr2_response")
  fake_body <- list(candidates = list())

  testthat::local_mocked_bindings(
    .gemini_api_key = function(api_key = NULL) "TEST_GEMINI_KEY",
    .gemini_req_perform = function(req) fake_resp,
    .gemini_resp_status = function(resp) 200L,
    .gemini_resp_body_json = function(resp, ...) fake_body,
    .env = ns
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # Invalid model
  expect_error(
    gemini_compare_pair_live(
      ID1               = "S01",
      text1             = "Sample 1 text.",
      ID2               = "S02",
      text2             = "Sample 2 text.",
      model             = "",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl
    ),
    "`model` must be a non-empty character scalar"
  )

  # thinking_level = "medium" should warn but still work
  expect_warning(
    {
      res <- gemini_compare_pair_live(
        ID1               = "S01",
        text1             = "Sample 1 text.",
        ID2               = "S02",
        text2             = "Sample 2 text.",
        model             = "gemini-3-pro-preview",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        thinking_level    = "medium"
      )
      # fake_body has empty candidates, so NA results
      expect_true(is.na(res$better_sample))
    },
    "mapping to \"High\" internally",
    fixed = FALSE
  )

  # thinking_budget in ... should be ignored (no error, but warning)
  expect_warning(
    {
      res <- gemini_compare_pair_live(
        ID1               = "S01",
        text1             = "Sample 1 text.",
        ID2               = "S02",
        text2             = "Sample 2 text.",
        model             = "gemini-3-pro-preview",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        thinking_level    = "low",
        thinking_budget   = 2048 # should be ignored
      )
    },
    "`thinking_budget` is ignored for Gemini 3",
    fixed = FALSE
  )
})

test_that("gemini_compare_pair_live correctly constructs request body with config", {
  skip_if_not_installed("httr2")
  ns <- asNamespace("pairwiseLLM")

  captured_body <- NULL

  # Mock internal helpers to capture request body
  testthat::local_mocked_bindings(
    .gemini_api_key = function(...) "TEST_KEY",
    .gemini_request = function(...) structure(list(), class = "httr2_request"),
    .gemini_req_body_json = function(req, body) {
      captured_body <<- body
      req
    },
    .gemini_req_perform = function(...) structure(list(), class = "httr2_response"),
    .gemini_resp_status = function(...) 200L,
    .gemini_resp_body_json = function(...) list(candidates = list()),
    .env = ns
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # Call with all config options
  res <- gemini_compare_pair_live(
    ID1 = "A", text1 = "A", ID2 = "B", text2 = "B",
    model = "gemini-3-pro-preview",
    trait_name = td$name, trait_description = td$description,
    prompt_template = tmpl,
    temperature = 0.7,
    top_p = 0.9,
    top_k = 40,
    max_output_tokens = 100,
    thinking_level = "low",
    include_thoughts = TRUE
  )

  expect_type(captured_body, "list")
  gc <- captured_body$generationConfig

  expect_equal(gc$temperature, 0.7)
  expect_equal(gc$topP, 0.9)
  expect_equal(gc$topK, 40)
  expect_equal(gc$maxOutputTokens, 100)

  # Check thinking config
  expect_equal(gc$thinkingConfig$thinkingLevel, "Low")
  expect_true(gc$thinkingConfig$includeThoughts)
})

test_that("gemini_compare_pair_live handles empty/malformed candidates gracefully", {
  skip_if_not_installed("httr2")
  ns <- asNamespace("pairwiseLLM")

  # Scenario 1: candidates list is empty
  empty_candidates <- list(candidates = list())

  # Scenario 2: parts list is empty (content exists but has no parts)
  empty_parts <- list(candidates = list(list(content = list(parts = list()))))

  # Scenario 3: include_thoughts=TRUE but only 1 part (fallback)
  one_part <- list(candidates = list(list(content = list(parts = list(
    list(text = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")
  )))))

  # 1. Empty candidates
  testthat::local_mocked_bindings(
    .gemini_api_key = function(...) "K",
    .gemini_req_perform = function(...) structure(list(), class = "httr2_response"),
    .gemini_resp_status = function(...) 200L,
    .gemini_resp_body_json = function(...) empty_candidates,
    .gemini_request = function(...) structure(list(), class = "httr2_request"),
    .gemini_req_body_json = function(req, ...) req,
    .env = ns
  )

  res1 <- gemini_compare_pair_live("A", "a", "B", "b", "m", "t", "d")
  expect_true(is.na(res1$content))
  expect_true(is.na(res1$thoughts))

  # 2. Empty parts (update mock)
  testthat::local_mocked_bindings(
    .gemini_resp_body_json = function(...) empty_parts,
    .env = ns
  )
  res2 <- gemini_compare_pair_live("A", "a", "B", "b", "m", "t", "d")
  expect_true(is.na(res2$content))

  # 3. One part + include_thoughts=TRUE
  testthat::local_mocked_bindings(
    .gemini_resp_body_json = function(...) one_part,
    .env = ns
  )
  res3 <- gemini_compare_pair_live("A", "a", "B", "b", "m", "t", "d", include_thoughts = TRUE)

  # Should fallback: content gets the text, thoughts remains NA
  expect_true(is.na(res3$thoughts))
  expect_equal(res3$content, "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")
  expect_equal(res3$better_sample, "SAMPLE_1")
})

# =====================================================================
# NEW TESTS for submit_gemini_pairs_live (List output & Features)
# =====================================================================

testthat::test_that("submit_gemini_pairs_live returns list structure for zero rows", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  empty_pairs <- tibble::tibble(
    ID1 = character(0), text1 = character(0),
    ID2 = character(0), text2 = character(0)
  )

  res <- submit_gemini_pairs_live(
    pairs = empty_pairs,
    model = "gemini-3-pro-preview",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl
  )

  testthat::expect_type(res, "list")
  testthat::expect_named(res, c("results", "failed_pairs", "failed_attempts"))
  testthat::expect_s3_class(res$results, "tbl_df")
  testthat::expect_s3_class(res$failed_pairs, "tbl_df")
  testthat::expect_equal(nrow(res$results), 0L)
  testthat::expect_equal(nrow(res$failed_pairs), 0L)

  # Check include_raw logic
  res_raw <- submit_gemini_pairs_live(
    pairs = empty_pairs,
    model = "gemini-3-pro-preview",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    include_raw = TRUE
  )
  testthat::expect_true("raw_response" %in% names(res_raw$results))
})

testthat::test_that("submit_gemini_pairs_live runs correctly and returns list", {
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  pairs <- tibble::tibble(
    ID1 = c("S01", "S02"),
    text1 = c("T1", "T2"),
    ID2 = c("S03", "S04"),
    text2 = c("T3", "T4")
  )

  # Mock success response
  testthat::local_mocked_bindings(
    gemini_compare_pair_live = function(ID1, ID2, ...) {
      tibble::tibble(
        custom_id = sprintf("LIVE_%s_vs_%s", ID1, ID2),
        ID1 = ID1,
        ID2 = ID2,
        model = "m",
        status_code = 200L,
        error_message = NA_character_,
        better_sample = "SAMPLE_1",
        better_id = ID1
      )
    },
    .env = pll_ns
  )

  res <- submit_gemini_pairs_live(
    pairs = pairs,
    model = "gemini-3-pro-preview",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    verbose = FALSE
  )

  testthat::expect_type(res, "list")
  testthat::expect_equal(nrow(res$results), 2L)
  testthat::expect_equal(nrow(res$failed_pairs), 0L)
})

testthat::test_that("submit_gemini_pairs_live separates failed pairs", {
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")

  pairs <- tibble::tibble(
    ID1 = c("S01", "FailMe"),
    text1 = "A", ID2 = "B", text2 = "C"
  )

  testthat::with_mocked_bindings(
    gemini_compare_pair_live = function(ID1, ...) {
      if (ID1 == "FailMe") {
        return(tibble::tibble(
          custom_id = "LIVE_FailMe_vs_B", ID1 = ID1, ID2 = "B",
          model = "gemini", status_code = 500L,
          error_message = "Mock API Error",
          better_id = NA_character_
        ))
      }
      tibble::tibble(
        custom_id = "LIVE_S01_vs_B", ID1 = ID1, ID2 = "B",
        model = "gemini", status_code = 200L,
        error_message = NA_character_,
        better_id = ID1
      )
    },
    .env = pll_ns,
    {
      res <- submit_gemini_pairs_live(
        pairs, "gemini", td$name, td$description,
        verbose = FALSE
      )

      testthat::expect_equal(nrow(res$results), 1L)
      testthat::expect_equal(nrow(res$failed_pairs), 1L)
      testthat::expect_equal(nrow(res$failed_attempts), 1L)
      testthat::expect_equal(res$failed_pairs$ID1, "FailMe")
      testthat::expect_equal(res$failed_pairs$error_message, "Mock API Error")
    }
  )
})

testthat::test_that("submit_gemini_pairs_live respects save_path (Resume Logic)", {
  testthat::skip_if_not_installed("readr")
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")
  tmp_csv <- tempfile(fileext = ".csv")

  # 1. Fake existing file
  existing_data <- tibble::tibble(
    custom_id = "LIVE_S01_vs_S02",
    ID1 = "S01", ID2 = "S02",
    model = "gemini", status_code = 200L, error_message = NA_character_,
    better_id = "S01"
  )
  readr::write_csv(existing_data, tmp_csv)

  # 2. Input pairs: one old, one new
  pairs <- tibble::tibble(
    ID1 = c("S01", "S03"),
    text1 = c("A", "B"),
    ID2 = c("S02", "S04"),
    text2 = c("C", "D")
  )

  call_count <- 0

  testthat::with_mocked_bindings(
    gemini_compare_pair_live = function(...) {
      call_count <<- call_count + 1
      tibble::tibble(
        custom_id = "LIVE_S03_vs_S04",
        ID1 = "S03", ID2 = "S04",
        model = "gemini", status_code = 200L, error_message = NA_character_,
        better_id = "S03"
      )
    },
    .env = pll_ns,
    {
      res <- submit_gemini_pairs_live(
        pairs = pairs,
        model = "gemini",
        trait_name = td$name,
        trait_description = td$description,
        save_path = tmp_csv,
        verbose = FALSE
      )

      # Should call API only ONCE
      testthat::expect_equal(call_count, 1L)
      # Result should contain BOTH
      testthat::expect_equal(nrow(res$results), 2L)
      testthat::expect_setequal(res$results$ID1, c("S01", "S03"))
    }
  )
  unlink(tmp_csv)
})

testthat::test_that("submit_gemini_pairs_live validates inputs", {
  td <- trait_description("overall_quality")

  # 1. Missing columns
  bad_pairs <- tibble::tibble(ID1 = "A", text1 = "t")
  testthat::expect_error(
    submit_gemini_pairs_live(bad_pairs, "gemini", td$name, td$description),
    "must contain columns"
  )

  # 2. Invalid status_every
  good_pairs <- tibble::tibble(ID1 = "A", text1 = "t", ID2 = "B", text2 = "t")
  testthat::expect_error(
    submit_gemini_pairs_live(
      good_pairs, "gemini", td$name, td$description,
      status_every = 0
    ),
    "positive integer"
  )
})

# =============================================================================
# Additional tests for submit_gemini_pairs_live coverage
# =============================================================================

test_that("submit_gemini_pairs_live creates output directory if missing", {
  skip_if_not_installed("readr")
  ns <- asNamespace("pairwiseLLM")

  # Setup unique temp directory
  temp_dir <- tempfile("gemini_test_dir")
  on.exit(unlink(temp_dir, recursive = TRUE))

  save_path <- file.path(temp_dir, "results.csv")

  pairs <- tibble::tibble(
    ID1 = "S1", text1 = "A",
    ID2 = "S2", text2 = "B"
  )

  # Mock successful comparison
  local_mocked_bindings(
    gemini_compare_pair_live = function(...) {
      tibble::tibble(
        custom_id = "cid", ID1 = "S1", ID2 = "S2", model = "m",
        status_code = 200L, error_message = NA_character_,
        better_id = "S1"
      )
    },
    .env = ns
  )

  # Run
  res <- submit_gemini_pairs_live(
    pairs, "model", "trait", "desc",
    save_path = save_path, verbose = FALSE
  )

  # Assert directory was created
  expect_true(dir.exists(temp_dir))
  expect_true(file.exists(save_path))
})

test_that("submit_gemini_pairs_live resume logic skips existing pairs", {
  skip_if_not_installed("readr")
  ns <- asNamespace("pairwiseLLM")

  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file))

  # Create "existing" results for S1 vs S2
  existing <- tibble::tibble(
    custom_id = "LIVE_S1_vs_S2",
    ID1 = "S1", ID2 = "S2", model = "m", status_code = 200,
    better_id = "S1"
  )
  readr::write_csv(existing, csv_file)

  # Input pairs: S1 vs S2 (done) and S3 vs S4 (new)
  pairs <- tibble::tibble(
    ID1 = c("S1", "S3"), text1 = c("A", "C"),
    ID2 = c("S2", "S4"), text2 = c("B", "D")
  )

  calls <- 0

  local_mocked_bindings(
    gemini_compare_pair_live = function(ID1, ...) {
      calls <<- calls + 1
      tibble::tibble(
        custom_id = sprintf("LIVE_%s_vs_S4", ID1),
        ID1 = ID1, ID2 = "S4", model = "m", status_code = 200L,
        error_message = NA_character_,
        better_id = ID1
      )
    },
    .env = ns
  )

  res <- submit_gemini_pairs_live(
    pairs, "model", "trait", "desc",
    save_path = csv_file, verbose = TRUE
  )

  # Should only process the second pair
  expect_equal(calls, 1)
  # Result should combine existing (disk) + new (memory)
  expect_equal(nrow(res$results), 2)
})

test_that("submit_gemini_pairs_live runs parallel logic (coverage test)", {
  pairs <- tibble::tibble(
    ID1 = c("A", "B", "C"),
    ID2 = c("D", "E", "F"),
    text1 = "X",
    text2 = "Y"
  )

  gemini_env <- environment(submit_gemini_pairs_live)

  testthat::local_mocked_bindings(
    `future::plan` = function(...) NULL,
    `future.apply::future_lapply` = function(X, FUN, ...) lapply(X, FUN, ...),
    gemini_compare_pair_live = function(ID1, ID2, include_raw, ...) {
      tibble::tibble(
        custom_id = sprintf("LIVE_%s_vs_%s", ID1, ID2),
        ID1 = ID1,
        ID2 = ID2,
        model = "m",
        status_code = 500L,
        error_message = "Error: Mock fail",
        raw_response = if (isTRUE(include_raw)) list(list(mock = TRUE)) else list(NULL),
        better_sample = NA_character_,
        better_id = NA_character_
      )
    },
    .env = gemini_env
  )

  out_csv <- tempfile(fileext = ".csv")
  res <- submit_gemini_pairs_live(
    pairs = pairs,
    model = "m",
    parallel = TRUE,
    workers = 2,
    include_raw = TRUE,
    save_path = out_csv
  )

  expect_equal(nrow(res$results), 0L)
  expect_equal(nrow(res$failed_pairs), 3L)
  expect_equal(nrow(res$failed_attempts), 3L)
  expect_true("raw_response" %in% names(res$failed_pairs))
  expect_true(file.exists(out_csv))
})


test_that("submit_gemini_pairs_live sequential saves and catches errors", {
  skip_if_not_installed("readr")
  ns <- asNamespace("pairwiseLLM")
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file))

  pairs <- tibble::tibble(
    ID1 = c("S1", "S2"), text1 = "A",
    ID2 = c("T1", "T2"), text2 = "B"
  )

  local_mocked_bindings(
    gemini_compare_pair_live = function(ID1, include_raw, ...) {
      if (ID1 == "S2") stop("Seq Error")
      tibble::tibble(
        custom_id = "cid", ID1 = ID1, ID2 = "T", model = "m",
        status_code = 200L, error_message = NA_character_,
        better_id = ID1,
        raw_response = if (include_raw) list(list(a = 1)) else NULL
      )
    },
    .env = ns
  )

  res <- submit_gemini_pairs_live(
    pairs, "m", "t", "d",
    save_path = csv_file,
    include_raw = TRUE,
    verbose = FALSE,
    parallel = FALSE # Explicit sequential
  )

  # 1. Check Results count (observed outcomes only)
  expect_equal(nrow(res$results), 1)

  # $failed_pairs contains only the failure
  expect_equal(nrow(res$failed_pairs), 1)
  expect_match(res$failed_pairs$error_message, "Seq Error")
  expect_equal(nrow(res$failed_attempts), 1)

  # 2. Check CSV
  expect_true(file.exists(csv_file))

  # Suppress "parsing issues" warning from read_csv
  csv_data <- suppressWarnings(readr::read_csv(csv_file, show_col_types = FALSE))

  expect_equal(nrow(csv_data), 2)
  expect_false("raw_response" %in% names(csv_data))
})

test_that("submit_gemini_pairs_live warns on CSV write failure", {
  skip_if_not_installed("readr")
  ns <- asNamespace("pairwiseLLM")

  # Provide an invalid path (directory as file) to trigger write failure
  bad_path <- tempdir()

  pairs <- tibble::tibble(ID1 = "S1", text1 = "A", ID2 = "T1", text2 = "B")

  local_mocked_bindings(
    gemini_compare_pair_live = function(...) {
      tibble::tibble(
        custom_id = "cid", ID1 = "S1", ID2 = "T1", model = "m",
        status_code = 200L, error_message = NA_character_,
        better_id = "S1"
      )
    },
    .env = ns
  )

  # We suppress warnings to mute the "Could not read existing save file" warning
  # that occurs because bad_path exists (as a dir) but fails to read.
  # expect_warning will still capture the specific "Failed to save" warning we care about.
  suppressWarnings({
    expect_warning(
      submit_gemini_pairs_live(
        pairs, "m", "t", "d",
        save_path = bad_path,
        verbose = FALSE
      ),
      "Failed to save incremental result"
    )
  })
})

test_that("submit_gemini_pairs_live handles resume read error and parallel write error", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  skip_if_not_installed("readr")

  ns <- asNamespace("pairwiseLLM")

  # Use a directory as the save_path to trigger read/write errors.
  bad_path <- tempdir()

  pairs <- tibble::tibble(ID1 = "S1", text1 = "A", ID2 = "S2", text2 = "B")

  # Mock internal function
  local_mocked_bindings(
    gemini_compare_pair_live = function(...) {
      tibble::tibble(
        custom_id = "id", ID1 = "S1", ID2 = "S2", model = "m",
        status_code = 200L, error_message = NA_character_,
        better_id = "S1"
      )
    },
    .env = ns
  )

  # Wrap in suppressWarnings so that system-level warnings (like "Permission denied"
  # from trying to read a directory) are silenced, while expect_warning still
  # captures and verifies the specific package warnings we care about.
  suppressWarnings({
    expect_warning(
      expect_warning(
        submit_gemini_pairs_live(
          pairs, "m", "t", "d",
          save_path = bad_path,
          parallel = TRUE, workers = 2,
          verbose = FALSE
        ),
        "Could not read existing save file"
      ),
      "Failed to save incremental results"
    )
  })
})

test_that("submit_gemini_pairs_live outputs verbose messages", {
  ns <- asNamespace("pairwiseLLM")
  pairs <- tibble::tibble(ID1 = "S1", text1 = "A", ID2 = "S2", text2 = "B")

  local_mocked_bindings(
    gemini_compare_pair_live = function(...) {
      tibble::tibble(
        custom_id = "id", ID1 = "S1", ID2 = "S2", model = "m",
        status_code = 200L, error_message = NA_character_,
        better_id = "S1"
      )
    },
    .env = ns
  )

  # Capture all messages generated during execution
  msgs <- capture_messages(
    submit_gemini_pairs_live(
      pairs, "m", "t", "d",
      verbose = TRUE, status_every = 1, parallel = FALSE
    )
  )

  # Expect at least two messages (Start + Per-pair + Completion)
  expect_true(length(msgs) >= 2)

  # Check startup message content
  expect_true(any(grepl("Submitting 1 live pair", msgs)))

  # Check per-pair status message content
  expect_true(any(grepl("Comparing S1 vs S2", msgs)))
})

test_that("gemini live wrappers call future backends", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan("sequential")

  testthat::expect_error(
    pairwiseLLM:::`future::plan`("sequential"),
    NA
  )

  out <- pairwiseLLM:::`future.apply::future_lapply`(list(1, 2), identity)
  testthat::expect_identical(out, list(1, 2))
})

test_that("gemini live httr2 wrapper helpers delegate correctly", {
  skip_if_not_installed("httr2")
  testthat::expect_true(is.function(pairwiseLLM:::.gemini_req_perform))
  testthat::expect_true(is.function(pairwiseLLM:::.gemini_resp_body_json))
  testthat::expect_true(is.function(pairwiseLLM:::.gemini_resp_status))
})

test_that("submit_gemini_pairs_live errors when readr is missing for save_path", {
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")

  testthat::with_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base",
    {
      testthat::expect_error(
        submit_gemini_pairs_live(
          pairs = pairs,
          model = "m",
          trait_name = "t",
          trait_description = "d",
          save_path = "out.csv"
        ),
        "readr"
      )
    }
  )
})

test_that("submit_gemini_pairs_live creates directory with verbose message", {
  skip_if_not_installed("readr")
  ns <- asNamespace("pairwiseLLM")
  temp_dir <- file.path(tempdir(), "gemini_nested_dir")
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  save_path <- file.path(temp_dir, "out.csv")
  pairs <- tibble::tibble(ID1 = "S1", text1 = "A", ID2 = "S2", text2 = "B")

  testthat::local_mocked_bindings(
    gemini_compare_pair_live = function(...) {
      tibble::tibble(
        custom_id = "cid",
        ID1 = "S1",
        ID2 = "S2",
        model = "m",
        status_code = 200L,
        error_message = NA_character_,
        better_id = "S1"
      )
    },
    .env = ns
  )

  msgs <- capture_messages(
    submit_gemini_pairs_live(
      pairs = pairs,
      model = "m",
      trait_name = "t",
      trait_description = "d",
      save_path = save_path,
      verbose = TRUE
    )
  )

  testthat::expect_true(any(grepl("Creating output directory", msgs)))
  testthat::expect_true(dir.exists(temp_dir))
})

test_that("submit_gemini_pairs_live uses pair_uid for resume logic", {
  skip_if_not_installed("readr")
  ns <- asNamespace("pairwiseLLM")
  save_path <- tempfile(fileext = ".csv")
  on.exit(unlink(save_path), add = TRUE)

  existing <- tibble::tibble(
    pair_uid = "pair-1",
    ID1 = "A",
    ID2 = "B",
    better_id = "A",
    status_code = 200L
  )
  readr::write_csv(existing, save_path)

  pairs <- tibble::tibble(
    ID1 = c("A", "C"),
    text1 = c("a", "c"),
    ID2 = c("B", "D"),
    text2 = c("b", "d"),
    pair_uid = c("pair-1", "pair-2")
  )

  calls <- 0L
  testthat::local_mocked_bindings(
    gemini_compare_pair_live = function(...) {
      calls <<- calls + 1L
      tibble::tibble(
        custom_id = "pair-2",
        ID1 = "C",
        ID2 = "D",
        better_id = "C",
        model = "m",
        status_code = 200L,
        error_message = NA_character_
      )
    },
    .env = ns
  )

  res <- submit_gemini_pairs_live(
    pairs = pairs,
    model = "m",
    trait_name = "t",
    trait_description = "d",
    save_path = save_path,
    verbose = FALSE
  )

  testthat::expect_equal(calls, 1L)
  testthat::expect_true(nrow(res$results) >= 1L)
})

test_that("submit_gemini_pairs_live handles resume files without custom IDs", {
  skip_if_not_installed("readr")
  ns <- asNamespace("pairwiseLLM")
  save_path <- tempfile(fileext = ".csv")
  on.exit(unlink(save_path), add = TRUE)

  existing <- tibble::tibble(
    ID1 = "A",
    ID2 = "B",
    status_code = 200L,
    better_id = "A"
  )
  readr::write_csv(existing, save_path)

  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")

  testthat::local_mocked_bindings(
    gemini_compare_pair_live = function(...) {
      tibble::tibble(
        custom_id = "cid",
        ID1 = "A",
        ID2 = "B",
        model = "m",
        status_code = 200L,
        error_message = NA_character_,
        better_id = "A"
      )
    },
    .env = ns
  )

  res <- submit_gemini_pairs_live(
    pairs = pairs,
    model = "m",
    trait_name = "t",
    trait_description = "d",
    save_path = save_path,
    verbose = FALSE
  )

  testthat::expect_equal(nrow(res$results), 1L)
})

test_that("submit_gemini_pairs_live reports parallel worker errors", {
  pairs <- tibble::tibble(
    ID1 = c("A", "B"),
    text1 = "a",
    ID2 = c("C", "D"),
    text2 = "b"
  )

  gemini_env <- environment(submit_gemini_pairs_live)

  testthat::local_mocked_bindings(
    `future::plan` = function(...) NULL,
    `future.apply::future_lapply` = function(X, FUN, ...) lapply(X, FUN, ...),
    gemini_compare_pair_live = function(...) stop("parallel fail"),
    .env = gemini_env
  )

  res <- submit_gemini_pairs_live(
    pairs = pairs,
    model = "m",
    trait_name = "t",
    trait_description = "d",
    parallel = TRUE,
    workers = 2,
    include_raw = TRUE,
    verbose = FALSE
  )

  testthat::expect_equal(nrow(res$results), 0L)
  testthat::expect_equal(nrow(res$failed_pairs), 2L)
  testthat::expect_true(all(grepl("parallel fail", res$failed_pairs$error_message)))
})
