# =====================================================================
#   test-together_live.R
#   Tests for together_compare_pair_live() and submit_together_pairs_live()
# =====================================================================

testthat::test_that(
  "together_compare_pair_live parses a successful response without thoughts and respects explicit temperature",
  {
    pll_ns <- asNamespace("pairwiseLLM")

    fake_body <- list(
      id = "chatcmpl-123",
      object = "chat.completion",
      model = "moonshotai/Kimi-K2-Instruct-0905",
      choices = list(
        list(
          index = 0L,
          message = list(
            role = "assistant",
            content = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> Explanation."
          ),
          finish_reason = "stop"
        )
      ),
      usage = list(
        prompt_tokens = 42L,
        completion_tokens = 7L,
        total_tokens = 49L
      )
    )

    captured_bodies <- list()

    testthat::local_mocked_bindings(
      .together_api_key = function(api_key = NULL) "TEST_TOGETHER_KEY",
      .together_req_body_json = function(req, body) {
        captured_bodies <<- append(captured_bodies, list(body))
        req
      },
      .together_req_perform = function(req) "FAKE_RESP",
      .together_resp_status = function(resp) 200L,
      .together_resp_body_json = function(resp, simplifyVector = FALSE) fake_body,
      .env = pll_ns
    )

    td <- trait_description("overall_quality")
    tmpl <- set_prompt_template()

    res <- together_compare_pair_live(
      ID1               = "S01",
      text1             = "Sample 1 text.",
      ID2               = "S02",
      text2             = "Sample 2 text.",
      model             = "moonshotai/Kimi-K2-Instruct-0905",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl,
      include_raw       = TRUE,
      temperature       = 0
    )

    # Basic structure
    testthat::expect_s3_class(res, "tbl_df")
    testthat::expect_equal(nrow(res), 1L)

    testthat::expect_equal(res$ID1, "S01")
    testthat::expect_equal(res$ID2, "S02")
    testthat::expect_equal(res$model, "moonshotai/Kimi-K2-Instruct-0905")
    testthat::expect_equal(res$object_type, "chat.completion")
    testthat::expect_equal(res$status_code, 200L)
    testthat::expect_true(is.na(res$error_message) || identical(res$error_message, ""))

    # No <think> in content, no thoughts extracted
    testthat::expect_true(is.na(res$thoughts))
    testthat::expect_true(grepl(
      "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
      res$content,
      fixed = TRUE
    ))

    testthat::expect_equal(res$better_sample, "SAMPLE_1")
    testthat::expect_equal(res$better_id, "S01")

    testthat::expect_equal(res$prompt_tokens, 42)
    testthat::expect_equal(res$completion_tokens, 7)
    testthat::expect_equal(res$total_tokens, 49)

    # raw_response list-column present and correct
    testthat::expect_true("raw_response" %in% names(res))
    testthat::expect_type(res$raw_response, "list")
    testthat::expect_identical(res$raw_response[[1]], fake_body)

    # Outgoing request body captured once, with explicit temperature = 0
    testthat::expect_equal(length(captured_bodies), 1L)
    b <- captured_bodies[[1]]
    testthat::expect_equal(b$model, "moonshotai/Kimi-K2-Instruct-0905")
    testthat::expect_equal(b$temperature, 0)
    testthat::expect_true(is.list(b$messages))
    testthat::expect_true(length(b$messages) == 1L)
  }
)

# ---------------------------------------------------------------------

testthat::test_that(
  "together_compare_pair_live applies default temperatures when not supplied",
  {
    pll_ns <- asNamespace("pairwiseLLM")

    fake_body <- list(
      id = "chatcmpl-any",
      object = "chat.completion",
      model = "dummy",
      choices = list(
        list(
          index = 0L,
          message = list(
            role = "assistant",
            content = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>"
          ),
          finish_reason = "stop"
        )
      )
    )

    captured_bodies <- list()

    testthat::local_mocked_bindings(
      .together_api_key = function(api_key = NULL) "TEST_TOGETHER_KEY",
      .together_req_body_json = function(req, body) {
        captured_bodies <<- append(captured_bodies, list(body))
        req
      },
      .together_req_perform = function(req) "FAKE_RESP",
      .together_resp_status = function(resp) 200L,
      .together_resp_body_json = function(resp, simplifyVector = FALSE) fake_body,
      .env = pll_ns
    )

    td <- trait_description("overall_quality")
    tmpl <- set_prompt_template()

    # 1) Non-thinking model (Kimi) with no temperature -> default 0
    together_compare_pair_live(
      ID1               = "S01",
      text1             = "Text 1",
      ID2               = "S02",
      text2             = "Text 2",
      model             = "moonshotai/Kimi-K2-Instruct-0905",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl
    )

    # 2) DeepSeek-R1 with no temperature -> default 0.6
    together_compare_pair_live(
      ID1               = "S03",
      text1             = "Text 3",
      ID2               = "S04",
      text2             = "Text 4",
      model             = "deepseek-ai/DeepSeek-R1",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl
    )

    testthat::expect_equal(length(captured_bodies), 2L)

    b1 <- captured_bodies[[1]]
    b2 <- captured_bodies[[2]]

    testthat::expect_equal(b1$model, "moonshotai/Kimi-K2-Instruct-0905")
    testthat::expect_equal(b1$temperature, 0)

    testthat::expect_equal(b2$model, "deepseek-ai/DeepSeek-R1")
    testthat::expect_equal(b2$temperature, 0.6)
  }
)

# ---------------------------------------------------------------------

testthat::test_that(
  "together_compare_pair_live parses DeepSeek-R1 <think> thoughts correctly",
  {
    pll_ns <- asNamespace("pairwiseLLM")

    raw_content <- paste0(
      "<think>This is internal chain-of-thought.</think>\n",
      "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> Visible explanation."
    )

    fake_body <- list(
      id = "chatcmpl-456",
      object = "chat.completion",
      model = "deepseek-ai/DeepSeek-R1",
      choices = list(
        list(
          index = 0L,
          message = list(
            role = "assistant",
            content = raw_content
          ),
          finish_reason = "stop"
        )
      ),
      usage = list(
        prompt_tokens = 100L,
        completion_tokens = 20L,
        total_tokens = 120L
      )
    )

    testthat::local_mocked_bindings(
      .together_api_key = function(api_key = NULL) "TEST_TOGETHER_KEY",
      .together_req_body_json = function(req, body) req,
      .together_req_perform = function(req) "FAKE_RESP",
      .together_resp_status = function(resp) 200L,
      .together_resp_body_json = function(resp, simplifyVector = FALSE) fake_body,
      .env = pll_ns
    )

    td <- trait_description("overall_quality")
    tmpl <- set_prompt_template()

    res <- together_compare_pair_live(
      ID1               = "S01",
      text1             = "Sample 1 text.",
      ID2               = "S02",
      text2             = "Sample 2 text.",
      model             = "deepseek-ai/DeepSeek-R1",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl
    )

    testthat::expect_s3_class(res, "tbl_df")
    testthat::expect_equal(nrow(res), 1L)

    # Thoughts should be extracted from inside <think>...</think>
    testthat::expect_false(is.na(res$thoughts))
    testthat::expect_true(grepl(
      "internal chain-of-thought",
      res$thoughts,
      fixed = TRUE
    ))

    # Content should no longer contain <think> tags, only the visible answer
    testthat::expect_false(grepl("<think>", res$content, fixed = TRUE))
    testthat::expect_true(grepl(
      "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>",
      res$content,
      fixed = TRUE
    ))

    testthat::expect_equal(res$better_sample, "SAMPLE_2")
    testthat::expect_equal(res$better_id, "S02")

    # Token counts passed through correctly
    testthat::expect_equal(res$prompt_tokens, 100)
    testthat::expect_equal(res$completion_tokens, 20)
    testthat::expect_equal(res$total_tokens, 120)
  }
)

# ---------------------------------------------------------------------

testthat::test_that(
  "together_compare_pair_live handles responses without <BETTER_SAMPLE> tag",
  {
    pll_ns <- asNamespace("pairwiseLLM")

    fake_body <- list(
      id = "chatcmpl-789",
      object = "chat.completion",
      model = "Qwen/Qwen3-235B-A22B-Instruct-2507-tput",
      choices = list(
        list(
          index = 0L,
          message = list(
            role = "assistant",
            content = "I forgot to include the tag, sorry."
          ),
          finish_reason = "stop"
        )
      )
    )

    testthat::local_mocked_bindings(
      .together_api_key = function(api_key = NULL) "TEST_TOGETHER_KEY",
      .together_req_body_json = function(req, body) req,
      .together_req_perform = function(req) "FAKE_RESP",
      .together_resp_status = function(resp) 200L,
      .together_resp_body_json = function(resp, simplifyVector = FALSE) fake_body,
      .env = pll_ns
    )

    td <- trait_description("overall_quality")
    tmpl <- set_prompt_template()

    res <- together_compare_pair_live(
      ID1               = "S01",
      text1             = "Sample 1 text.",
      ID2               = "S02",
      text2             = "Sample 2 text.",
      model             = "Qwen/Qwen3-235B-A22B-Instruct-2507-tput",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl
    )

    testthat::expect_true(is.na(res$better_sample))
    testthat::expect_true(is.na(res$better_id))
  }
)

# ---------------------------------------------------------------------

testthat::test_that(
  "together_compare_pair_live returns an error row when JSON parse fails",
  {
    pll_ns <- asNamespace("pairwiseLLM")

    testthat::local_mocked_bindings(
      .together_api_key = function(api_key = NULL) "TEST_TOGETHER_KEY",
      .together_req_body_json = function(req, body) req,
      .together_req_perform = function(req) "FAKE_RESP",
      .together_resp_status = function(resp) 500L,
      .together_resp_body_json = function(resp, simplifyVector = FALSE) {
        stop("boom")
      },
      .env = pll_ns
    )

    td <- trait_description("overall_quality")
    tmpl <- set_prompt_template()

    res <- together_compare_pair_live(
      ID1               = "S01",
      text1             = "Sample 1 text.",
      ID2               = "S02",
      text2             = "Sample 2 text.",
      model             = "deepseek-ai/DeepSeek-V3",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl,
      include_raw       = TRUE
    )

    testthat::expect_equal(nrow(res), 1L)
    testthat::expect_equal(res$ID1, "S01")
    testthat::expect_equal(res$ID2, "S02")

    testthat::expect_true(is.na(res$model))
    testthat::expect_true(is.na(res$object_type))
    testthat::expect_equal(res$status_code, 500L)

    testthat::expect_true(is.na(res$content))
    testthat::expect_true(is.na(res$thoughts))
    testthat::expect_true(is.na(res$better_sample))
    testthat::expect_true(is.na(res$better_id))

    testthat::expect_match(
      res$error_message,
      "Failed to parse Together.ai response body as JSON",
      fixed = FALSE
    )

    # When parse fails and include_raw = TRUE we expect NULL raw_response[[1]]
    testthat::expect_true("raw_response" %in% names(res))
    testthat::expect_true(is.null(res$raw_response[[1]]))
  }
)

# ---------------------------------------------------------------------

testthat::test_that("together_compare_pair_live validates input types", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # ID1 must be character
  testthat::expect_error(
    together_compare_pair_live(
      ID1 = 123, text1 = "t", ID2 = "B", text2 = "t",
      model = "model", trait_name = td$name, trait_description = td$description
    ),
    "`ID1` must be a single character"
  )

  # text1 must be character
  testthat::expect_error(
    together_compare_pair_live(
      ID1 = "A", text1 = list(), ID2 = "B", text2 = "t",
      model = "model", trait_name = td$name, trait_description = td$description
    ),
    "`text1` must be a single character"
  )

  # model must be character
  testthat::expect_error(
    together_compare_pair_live(
      ID1 = "A", text1 = "t", ID2 = "B", text2 = "t",
      model = 1, trait_name = td$name, trait_description = td$description
    ),
    "`model` must be a single character"
  )
})

testthat::test_that("together_compare_pair_live handles network/HTTP errors gracefully", {
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")

  # Mock internals to simulate a connection error
  testthat::with_mocked_bindings(
    .together_api_key = function(...) "KEY",
    .together_req_body_json = function(req, ...) req,
    .together_req_perform = function(...) {
      stop("Simulated connection timeout")
    },
    .env = pll_ns,
    {
      res <- together_compare_pair_live(
        ID1 = "S1", text1 = "A", ID2 = "S2", text2 = "B",
        model = "model", trait_name = td$name, trait_description = td$description,
        include_raw = TRUE
      )

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res$ID1, "S1")

      # The catch-all error handler sets status_code to NA
      testthat::expect_true(is.na(res$status_code))

      # Check error message is captured
      testthat::expect_match(res$error_message, "Together.ai request error: Simulated connection timeout")

      # Check include_raw behavior on error (should be NULL)
      testthat::expect_true("raw_response" %in% names(res))
      testthat::expect_true(is.null(res$raw_response[[1]]))
    }
  )
})

testthat::test_that("together_compare_pair_live handles API-level errors (valid JSON, bad status)", {
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")

  fake_error_body <- list(
    error = list(
      message = "Rate limit exceeded",
      type = "rate_limit_error"
    )
  )

  testthat::with_mocked_bindings(
    .together_api_key = function(...) "KEY",
    .together_req_body_json = function(req, ...) req,
    .together_req_perform = function(...) "RESP",
    .together_resp_status = function(...) 429L,
    .together_resp_body_json = function(...) fake_error_body,
    .env = pll_ns,
    {
      res <- together_compare_pair_live(
        ID1 = "S1", text1 = "A", ID2 = "S2", text2 = "B",
        model = "model", trait_name = td$name, trait_description = td$description
      )

      testthat::expect_equal(res$status_code, 429L)
      testthat::expect_match(res$error_message, "Rate limit exceeded")
      testthat::expect_true(is.na(res$content))
    }
  )
})

testthat::test_that("together_compare_pair_live handles incomplete <think> tags", {
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")

  # Case: <think> is started but never closed (e.g. max tokens reached)
  raw_text <- "<think> I am thinking... [cut off]"

  fake_body <- list(
    choices = list(list(message = list(content = raw_text)))
  )

  testthat::with_mocked_bindings(
    .together_api_key = function(...) "KEY",
    .together_req_body_json = function(req, ...) req,
    .together_req_perform = function(...) "RESP",
    .together_resp_status = function(...) 200L,
    .together_resp_body_json = function(...) fake_body,
    .env = pll_ns,
    {
      res <- together_compare_pair_live(
        ID1 = "S1", text1 = "A", ID2 = "S2", text2 = "B",
        model = "deepseek-ai/DeepSeek-R1", trait_name = td$name, trait_description = td$description
      )

      # If regex tags aren't both present, it treats the whole thing as visible content
      testthat::expect_true(is.na(res$thoughts))
      testthat::expect_equal(res$content, raw_text)
    }
  )
})

# =====================================================================
# NEW TESTS for submit_together_pairs_live (List output & Features)
# =====================================================================

testthat::test_that("submit_together_pairs_live returns list structure for zero rows", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  empty_pairs <- tibble::tibble(
    ID1 = character(0), text1 = character(0),
    ID2 = character(0), text2 = character(0)
  )

  res <- submit_together_pairs_live(
    pairs = empty_pairs,
    model = "deepseek-ai/DeepSeek-R1",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl
  )

  # Expect a list with two tibbles
  testthat::expect_type(res, "list")
  testthat::expect_named(res, c("results", "failed_pairs", "failed_attempts"))
  testthat::expect_s3_class(res$results, "tbl_df")
  testthat::expect_s3_class(res$failed_pairs, "tbl_df")
  testthat::expect_equal(nrow(res$results), 0L)
  testthat::expect_equal(nrow(res$failed_pairs), 0L)

  # Check include_raw behavior
  res_raw <- submit_together_pairs_live(
    pairs = empty_pairs,
    model = "deepseek-ai/DeepSeek-R1",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    include_raw = TRUE
  )
  testthat::expect_true("raw_response" %in% names(res_raw$results))
})

testthat::test_that("submit_together_pairs_live runs correctly and returns list", {
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
    together_compare_pair_live = function(ID1, ID2, ...) {
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

  res <- submit_together_pairs_live(
    pairs = pairs,
    model = "deepseek-ai/DeepSeek-R1",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    verbose = FALSE
  )

  testthat::expect_type(res, "list")
  testthat::expect_equal(nrow(res$results), 2L)
  testthat::expect_equal(nrow(res$failed_pairs), 0L)
})

testthat::test_that("submit_together_pairs_live separates failed pairs", {
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")

  pairs <- tibble::tibble(
    ID1 = c("S01", "FailMe"),
    text1 = "A", ID2 = "B", text2 = "C"
  )

  testthat::with_mocked_bindings(
    together_compare_pair_live = function(ID1, ...) {
      if (ID1 == "FailMe") {
        # Simulate an error result returned by the lower-level function
        return(tibble::tibble(
          custom_id = "LIVE_FailMe_vs_B", ID1 = ID1, ID2 = "B",
          model = "mod", status_code = 500L,
          error_message = "Mock API Error",
          better_id = NA_character_
        ))
      }
      # Success case
      tibble::tibble(
        custom_id = "LIVE_S01_vs_B", ID1 = ID1, ID2 = "B",
        model = "mod", status_code = 200L,
        error_message = NA_character_,
        better_id = ID1
      )
    },
    .env = pll_ns,
    {
      res <- submit_together_pairs_live(
        pairs, "model", td$name, td$description,
        verbose = FALSE
      )

      # Results should contain BOTH rows (full log)
      testthat::expect_equal(nrow(res$results), 1L)

      # Failed pairs should contain ONLY the failure
      testthat::expect_equal(nrow(res$failed_pairs), 1L)
      testthat::expect_equal(res$failed_pairs$ID1, "FailMe")
      testthat::expect_equal(res$failed_pairs$error_message, "Mock API Error")
    }
  )
})

testthat::test_that("submit_together_pairs_live respects save_path (Resume Logic)", {
  testthat::skip_if_not_installed("readr")
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")
  tmp_csv <- tempfile(fileext = ".csv")

  # 1. Create a "fake" existing result file
  # Pair S01 vs S02 is "already done"
  existing_data <- tibble::tibble(
    custom_id = "LIVE_S01_vs_S02",
    ID1 = "S01", ID2 = "S02",
    model = "mod", status_code = 200L, error_message = NA_character_
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
    together_compare_pair_live = function(...) {
      call_count <<- call_count + 1
      tibble::tibble(
        custom_id = "LIVE_S03_vs_S04",
        ID1 = "S03", ID2 = "S04",
        model = "mod", status_code = 200L, error_message = NA_character_
      )
    },
    .env = pll_ns,
    {
      res <- submit_together_pairs_live(
        pairs = pairs,
        model = "mod",
        trait_name = td$name,
        trait_description = td$description,
        save_path = tmp_csv,
        verbose = FALSE
      )

      # Should call API only ONCE (for S03)
      testthat::expect_equal(call_count, 1L)

      # Result should contain BOTH (one from disk, one from new run)
      testthat::expect_equal(nrow(res$results), 2L)
      testthat::expect_setequal(res$results$ID1, c("S01", "S03"))
    }
  )

  unlink(tmp_csv)
})

testthat::test_that("submit_together_pairs_live validates inputs", {
  td <- trait_description("overall_quality")

  # 1. Missing columns
  bad_pairs <- tibble::tibble(ID1 = "A", text1 = "t")
  testthat::expect_error(
    submit_together_pairs_live(bad_pairs, "mod", td$name, td$description),
    "must contain columns"
  )

  # 2. Invalid status_every (0 is not positive)
  good_pairs <- tibble::tibble(ID1 = "A", text1 = "t", ID2 = "B", text2 = "t")
  testthat::expect_error(
    submit_together_pairs_live(
      good_pairs, "mod", td$name, td$description,
      status_every = 0
    ),
    "positive integer"
  )
})

testthat::test_that("submit_together_pairs_live: Directory creation & Raw response cleanup", {
  testthat::skip_if_not_installed("readr")
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")

  tmp_dir <- tempfile()
  tmp_file <- file.path(tmp_dir, "out.csv")

  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")

  testthat::with_mocked_bindings(
    together_compare_pair_live = function(...) {
      res <- tibble::tibble(
        custom_id = "LIVE_A_vs_B", ID1 = "A", ID2 = "B",
        model = "m", status_code = 200, error_message = NA,
        thoughts = NA_character_, content = "content",
        better_sample = NA_character_, better_id = NA_character_,
        prompt_tokens = 1, completion_tokens = 1, total_tokens = 2
      )
      res$raw_response <- list(list(foo = "bar"))
      res
    },
    .env = pll_ns,
    {
      out <- capture.output(
        {
          res <- submit_together_pairs_live(
            pairs, "model", td$name, td$description,
            save_path = tmp_file, verbose = TRUE, include_raw = TRUE
          )
        },
        type = "message"
      )

      # FIX 1: Updated regex to match "Completed 1 pairs" (removed "Together.ai")
      testthat::expect_true(any(grepl("Completed 1 pairs", out)))

      testthat::expect_true(dir.exists(tmp_dir))
      saved <- readr::read_csv(tmp_file, show_col_types = FALSE)
      testthat::expect_false("raw_response" %in% names(saved))
      testthat::expect_true("raw_response" %in% names(res$results))
    }
  )
  unlink(tmp_dir, recursive = TRUE)
})

testthat::test_that("submit_together_pairs_live: Resume logic (Read Error Handling)", {
  testthat::skip_if_not_installed("readr")
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")
  tmp <- tempfile(fileext = ".csv")
  file.create(tmp)

  # Mock read_csv to throw an error, forcing the tryCatch error handler (Line 569)
  testthat::with_mocked_bindings(
    read_csv = function(...) stop("Mock Read Error"),
    .package = "readr",
    {
      testthat::with_mocked_bindings(
        together_compare_pair_live = function(...) {
          tibble::tibble(
            custom_id = "LIVE_A_vs_B", ID1 = "A", ID2 = "B",
            model = "m", status_code = 200, error_message = NA
          )
        },
        .env = pll_ns,
        {
          testthat::expect_warning(
            submit_together_pairs_live(
              pairs, "model", td$name, td$description,
              save_path = tmp, verbose = FALSE
            ),
            "Could not read existing save file"
          )
        }
      )
    }
  )
  unlink(tmp)
})

testthat::test_that("submit_together_pairs_live: Sequential Save Error Handling", {
  testthat::skip_if_not_installed("readr")
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")
  tmp_file <- tempfile(fileext = ".csv")

  # Mock write_csv to throw an error (triggering Line 728 warning)
  testthat::with_mocked_bindings(
    write_csv = function(...) stop("Disk full"),
    .package = "readr",
    {
      testthat::with_mocked_bindings(
        together_compare_pair_live = function(...) {
          tibble::tibble(
            custom_id = "LIVE_A_vs_B", ID1 = "A", ID2 = "B",
            model = "m", status_code = 200, error_message = NA
          )
        },
        .env = pll_ns,
        {
          testthat::expect_warning(
            submit_together_pairs_live(
              pairs, "model", td$name, td$description,
              save_path = tmp_file, verbose = FALSE
            ),
            "Failed to save incremental result"
          )
        }
      )
    }
  )
})

testthat::test_that("submit_together_pairs_live: Parallel Execution & Save Error", {
  testthat::skip_if_not_installed("future")
  testthat::skip_if_not_installed("future.apply")
  testthat::skip_if_not_installed("readr")

  td <- trait_description("overall_quality")
  pairs <- tibble::tibble(
    ID1 = c("A", "B"), text1 = c("a", "b"),
    ID2 = c("C", "D"), text2 = c("c", "d")
  )
  tmp_file <- tempfile(fileext = ".csv")

  # Mock write_csv to throw an error (triggering the save warning)
  testthat::with_mocked_bindings(
    write_csv = function(...) stop("Parallel Disk full"),
    .package = "readr",
    {
      capture.output({
        testthat::expect_warning(
          res <- submit_together_pairs_live(
            pairs,
            model = 123, # FIX 2: Pass invalid model type to force stop() inside worker
            trait_name = td$name,
            trait_description = td$description,
            parallel = TRUE, workers = 2,
            save_path = tmp_file, verbose = TRUE
          ),
          "Failed to save incremental results"
        )
      })

      # Now we expect failures with "Error: " because together_compare_pair_live threw an error
      testthat::expect_equal(nrow(res$failed_pairs), 2L)
      testthat::expect_true(all(grepl("Error", res$failed_pairs$error_message)))
      testthat::expect_true(all(grepl("must be a single character", res$failed_pairs$error_message)))
    }
  )
})

testthat::test_that("submit_together_pairs_live: Sequential Internal Error Handling", {
  # Covers lines 706-715 (tryCatch error handler in sequential loop)
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")

  testthat::with_mocked_bindings(
    together_compare_pair_live = function(...) stop("Sequential Internal Crash"),
    .env = pll_ns,
    {
      res <- submit_together_pairs_live(
        pairs, "model", td$name, td$description,
        verbose = FALSE
      )

      testthat::expect_equal(nrow(res$failed_pairs), 1L)
      testthat::expect_match(res$failed_pairs$error_message, "Sequential Internal Crash")
    }
  )
})

testthat::test_that("submit_together_pairs_live: Parallel Save Strips raw_response", {
  testthat::skip_if_not_installed("future")
  testthat::skip_if_not_installed("future.apply")
  testthat::skip_if_not_installed("readr")

  td <- trait_description("overall_quality")
  # Use a pair to generate a result
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")
  tmp_file <- tempfile(fileext = ".csv")

  # We run with parallel = TRUE and include_raw = TRUE.
  # We provide a fake API key, so the worker will fail (caught by internal tryCatch).
  # The worker returns an error tibble which includes a 'raw_response' column (Line 655).
  # The main process aggregates this and should strip 'raw_response' before saving (Line 667).

  # No warning expected (save should succeed)
  testthat::expect_warning(
    submit_together_pairs_live(
      pairs, "model", td$name, td$description,
      parallel = TRUE, workers = 2,
      save_path = tmp_file, verbose = FALSE,
      include_raw = TRUE,
      api_key = "FAKE_KEY"
    ),
    regexp = NA
  )

  testthat::expect_true(file.exists(tmp_file))
  saved <- readr::read_csv(tmp_file, show_col_types = FALSE)

  # Verification:
  # 1. We processed the pair (failed)
  testthat::expect_equal(nrow(saved), 1L)
  testthat::expect_true(!is.na(saved$error_message[1]))

  # 2. The raw_response column was STRIPPED from the CSV
  testthat::expect_false("raw_response" %in% names(saved))
})
