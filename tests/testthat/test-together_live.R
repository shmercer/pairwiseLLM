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

testthat::test_that(
  "submit_together_pairs_live validates inputs and handles zero-row pairs",
  {
    td <- trait_description("overall_quality")
    tmpl <- set_prompt_template()

    # Missing columns
    bad_pairs <- tibble::tibble(
      ID1   = "S01",
      text1 = "Sample 1"
      # missing ID2/text2
    )

    testthat::expect_error(
      submit_together_pairs_live(
        pairs             = bad_pairs,
        model             = "deepseek-ai/DeepSeek-R1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl
      ),
      "`pairs` must contain columns",
      fixed = FALSE
    )

    # Zero rows: should return empty tibble with expected columns
    empty_pairs <- tibble::tibble(
      ID1   = character(0),
      text1 = character(0),
      ID2   = character(0),
      text2 = character(0)
    )

    res_empty <- submit_together_pairs_live(
      pairs             = empty_pairs,
      model             = "deepseek-ai/DeepSeek-R1",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl
    )

    testthat::expect_s3_class(res_empty, "tbl_df")
    testthat::expect_equal(nrow(res_empty), 0L)
    testthat::expect_setequal(
      names(res_empty),
      c(
        "custom_id", "ID1", "ID2", "model", "object_type",
        "status_code", "error_message", "thoughts", "content",
        "better_sample", "better_id",
        "prompt_tokens", "completion_tokens", "total_tokens"
      )
    )

    # When include_raw = TRUE, raw_response column should be present even for zero rows
    res_empty_raw <- submit_together_pairs_live(
      pairs             = empty_pairs,
      model             = "deepseek-ai/DeepSeek-R1",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl,
      include_raw       = TRUE
    )

    testthat::expect_true("raw_response" %in% names(res_empty_raw))
    testthat::expect_type(res_empty_raw$raw_response, "list")
  }
)

# ---------------------------------------------------------------------

testthat::test_that("submit_together_pairs_live runs correctly (mocking internals)", {
  pll_ns <- asNamespace("pairwiseLLM")

  pairs <- tibble::tibble(
    ID1   = c("S01", "S02"),
    text1 = c("Text 1a", "Text 2a"),
    ID2   = c("S03", "S04"),
    text2 = c("Text 1b", "Text 2b")
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # We capture the bodies sent to the API to verify arguments were passed down
  captured_bodies <- list()

  fake_body_resp <- list(
    id = "chatcmpl-test",
    object = "chat.completion",
    choices = list(
      list(
        message = list(content = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")
      )
    ),
    usage = list(total_tokens = 10)
  )

  testthat::with_mocked_bindings(
    .together_api_key = function(...) "TEST_KEY",
    .together_req_body_json = function(req, body) {
      captured_bodies <<- append(captured_bodies, list(body))
      req
    },
    .together_req_perform = function(req) "FAKE_RESP",
    .together_resp_status = function(resp) 200L,
    .together_resp_body_json = function(resp, simplifyVector = FALSE) fake_body_resp,
    .env = pll_ns,
    {
      res <- submit_together_pairs_live(
        pairs             = pairs,
        model             = "deepseek-ai/DeepSeek-R1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        include_raw       = FALSE,
        verbose           = FALSE,
        progress          = FALSE,
        temperature       = 0.7 # Custom arg to verify passthrough
      )

      # Verify structure
      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(nrow(res), 2L)
      testthat::expect_equal(res$ID1, c("S01", "S02"))

      # Verify results parsed from the fake body
      testthat::expect_true(all(res$better_sample == "SAMPLE_1"))

      # Verify arguments were passed down to internals correctly
      testthat::expect_equal(length(captured_bodies), 2L)
      # Check passthrough of 'temperature' and 'model'
      testthat::expect_equal(captured_bodies[[1]]$model, "deepseek-ai/DeepSeek-R1")
      testthat::expect_equal(captured_bodies[[1]]$temperature, 0.7)
    }
  )
})

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

testthat::test_that("submit_together_pairs_live validates status_every", {
  td <- trait_description("overall_quality")
  pairs <- tibble::tibble(ID1 = "A", text1 = "t", ID2 = "B", text2 = "t")

  testthat::expect_error(
    submit_together_pairs_live(pairs, "model", td$name, td$description, status_every = 0),
    "status_every` must be a single positive integer"
  )
  testthat::expect_error(
    submit_together_pairs_live(pairs, "model", td$name, td$description, status_every = "1"),
    "status_every` must be a single positive integer"
  )
})

testthat::test_that("submit_together_pairs_live handles internal R errors in loop", {
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")

  pairs <- tibble::tibble(
    ID1 = c("S1", "S2"),
    text1 = "A", ID2 = "B", text2 = "C"
  )

  # Here we mock 'together_compare_pair_live' SAFELY because we use with_mocked_bindings
  # and this test is isolated at the end. However, to be ultra-safe and consistent with
  # the strategy, we can simulate the error by mocking the internals to crash for specific ID.

  testthat::with_mocked_bindings(
    .together_api_key = function(...) "KEY",
    .together_req_perform = function(...) "RESP",
    .together_resp_status = function(...) 200L,
    .together_resp_body_json = function(...) list(choices = list(list(message = list(content = "OK")))),
    # We mock the high-level function only for this specific aggregation test
    # to guarantee we trigger the catch block inside the loop without relying on network stack details.
    together_compare_pair_live = function(ID1, ...) {
      if (ID1 == "S1") stop("Unexpected internal crash")
      tibble::tibble(
        custom_id = "LIVE_S2_vs_B", ID1 = "S2", ID2 = "B",
        model = "mod", object_type = "chat", status_code = 200L,
        error_message = NA_character_, thoughts = NA_character_,
        content = "Res", better_sample = "SAMPLE_1", better_id = "S2",
        prompt_tokens = 1, completion_tokens = 1, total_tokens = 2
      )
    },
    .env = pll_ns,
    {
      res <- suppressMessages(
        submit_together_pairs_live(
          pairs, "model", td$name, td$description,
          verbose = FALSE, progress = FALSE, include_raw = TRUE
        )
      )

      testthat::expect_equal(nrow(res), 2L)

      # First row: Error caught by loop tryCatch
      r1 <- res[1, ]
      testthat::expect_equal(r1$ID1, "S1")
      testthat::expect_match(r1$error_message, "Error during Together.ai comparison: Unexpected internal crash")
      testthat::expect_true(is.null(r1$raw_response[[1]]))

      # Second row: Success
      r2 <- res[2, ]
      testthat::expect_equal(r2$ID1, "S2")
      testthat::expect_true(is.na(r2$error_message))
    }
  )
})
