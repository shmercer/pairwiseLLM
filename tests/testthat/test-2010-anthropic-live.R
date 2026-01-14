# =====================================================================
#   test-anthropic_live.R
#   Tests for anthropic_compare_pair_live() and submit_anthropic_pairs_live()
# =====================================================================

testthat::test_that("anthropic_compare_pair_live parses /v1/messages correctly", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_body <- list(
    model = "claude-sonnet-4-5-20250929",
    id = "msg_01XYZ",
    type = "message",
    role = "assistant",
    content = list(
      list(
        type = "text", text =
          "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> Explanation."
      )
    ),
    usage = list(
      input_tokens  = 14L,
      output_tokens = 4L,
      total_tokens  = 18L
    )
  )

  testthat::with_mocked_bindings(
    .anthropic_api_key = function(...) "FAKEKEY",
    .anthropic_req_body_json = function(req, body) req,
    .anthropic_req_perform = function(req) {
      structure(list(), class = "fake_resp")
    },
    .anthropic_resp_body_json = function(...) fake_body,
    .anthropic_resp_status = function(...) 200L,
    {
      res <- anthropic_compare_pair_live(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "claude-sonnet-4-5",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        temperature       = 0,
        max_tokens        = 64,
        reasoning         = "none",
        include_raw       = TRUE
      )

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(nrow(res), 1L)

      testthat::expect_equal(res$custom_id, sprintf("LIVE_%s_vs_%s", ID1, ID2))
      testthat::expect_equal(res$ID1, ID1)
      testthat::expect_equal(res$ID2, ID2)

      testthat::expect_equal(res$model, "claude-sonnet-4-5-20250929")
      testthat::expect_equal(res$object_type, "message")
      testthat::expect_equal(res$status_code, 200L)
      testthat::expect_true(is.na(res$error_message))

      testthat::expect_equal(
        res$content,
        "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> Explanation."
      )

      testthat::expect_equal(res$better_sample, "SAMPLE_1")
      testthat::expect_equal(res$better_id, ID1)

      testthat::expect_equal(res$prompt_tokens, 14)
      testthat::expect_equal(res$completion_tokens, 4)
      testthat::expect_equal(res$total_tokens, 18)

      testthat::expect_true("raw_response" %in% names(res))
      testthat::expect_type(res$raw_response, "list")
      testthat::expect_equal(
        res$raw_response[[1]]$model,
        "claude-sonnet-4-5-20250929"
      )
    }
  )
})

# ---------------------------------------------------------------------

testthat::test_that("anthropic_compare_pair_live returns error row on
                    JSON parse failure", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"

  testthat::with_mocked_bindings(
    .anthropic_api_key = function(...) "FAKEKEY",
    .anthropic_req_body_json = function(req, body) req,
    .anthropic_req_perform = function(req) {
      structure(list(), class = "fake_resp")
    },
    .anthropic_resp_body_json = function(...) stop("boom"),
    .anthropic_resp_status = function(...) 500L,
    {
      res <- anthropic_compare_pair_live(
        ID1               = ID1,
        text1             = "X",
        ID2               = ID2,
        text2             = "Y",
        model             = "claude-sonnet-4-5",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        include_raw       = TRUE
      )

      testthat::expect_equal(res$status_code, 500L)
      testthat::expect_equal(
        res$error_message,
        "Failed to parse response body as JSON."
      )
      testthat::expect_true(is.na(res$better_sample))
      testthat::expect_true(is.null(res$raw_response[[1]]))
    }
  )
})

# ---------------------------------------------------------------------

testthat::test_that("anthropic_compare_pair_live handles HTTP errors (status >= 300)", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # Mock error response body
  error_body <- list(
    type = "error",
    error = list(
      type = "invalid_request_error",
      message = "Overloaded"
    )
  )

  testthat::with_mocked_bindings(
    .anthropic_api_key = function(...) "FAKEKEY",
    .anthropic_req_body_json = function(req, body) req,
    .anthropic_req_perform = function(req) structure(list(), class = "fake_resp"),
    .anthropic_resp_body_json = function(...) error_body,
    .anthropic_resp_status = function(...) 503L,
    {
      res <- anthropic_compare_pair_live(
        ID1 = "S1", text1 = "A", ID2 = "S2", text2 = "B",
        model = "claude-sonnet", trait_name = td$name, trait_description = td$description,
        prompt_template = tmpl
      )

      testthat::expect_equal(res$status_code, 503L)
      testthat::expect_equal(res$error_message, "Overloaded")
      testthat::expect_true(is.na(res$better_sample))
    }
  )
})

# ---------------------------------------------------------------------

testthat::test_that("anthropic_compare_pair_live handles missing tags and empty content", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # Case 1: Text exists but no tags
  body_no_tags <- list(
    model = "claude",
    content = list(list(type = "text", text = "I cannot decide."))
  )

  testthat::with_mocked_bindings(
    .anthropic_api_key = function(...) "FAKEKEY",
    .anthropic_req_body_json = function(req, body) req,
    .anthropic_req_perform = function(req) structure(list(), class = "fake_resp"),
    .anthropic_resp_body_json = function(...) body_no_tags,
    .anthropic_resp_status = function(...) 200L,
    {
      res <- anthropic_compare_pair_live(
        ID1 = "S1", text1 = "A", ID2 = "S2", text2 = "B",
        model = "claude", trait_name = td$name, trait_description = td$description,
        prompt_template = tmpl
      )
      testthat::expect_equal(res$content, "I cannot decide.")
      testthat::expect_true(is.na(res$better_sample))
      testthat::expect_true(is.na(res$better_id))
    }
  )

  # Case 2: Content is empty/null
  body_empty <- list(model = "claude", content = list())

  # FIX: Ensure we mock .anthropic_req_perform and .anthropic_api_key here too,
  # otherwise it tries to make a real network request.
  testthat::with_mocked_bindings(
    .anthropic_api_key = function(...) "FAKEKEY",
    .anthropic_req_body_json = function(req, body) req,
    .anthropic_req_perform = function(req) structure(list(), class = "fake_resp"),
    .anthropic_resp_body_json = function(...) body_empty,
    .anthropic_resp_status = function(...) 200L,
    {
      res <- anthropic_compare_pair_live(
        ID1 = "S1", text1 = "A", ID2 = "S2", text2 = "B",
        model = "claude", trait_name = td$name, trait_description = td$description
      )
      testthat::expect_true(is.na(res$content))
      testthat::expect_true(is.na(res$better_sample))
    }
  )
})

# ---------------------------------------------------------------------

testthat::test_that("anthropic_compare_pair_live applies recommended
                    defaults", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"

  bodies <- list()

  fake_body <- list(
    model = "claude-sonnet-4-5-20250929",
    id = "msg_01XYZ",
    type = "message",
    role = "assistant",
    content = list(
      list(type = "text", text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>")
    ),
    usage = list(
      input_tokens  = 10L,
      output_tokens = 4L,
      total_tokens  = 14L
    )
  )

  testthat::with_mocked_bindings(
    .anthropic_api_key = function(...) "FAKEKEY",
    .anthropic_req_body_json = function(req, body) {
      bodies <<- append(bodies, list(body))
      req
    },
    .anthropic_req_perform = function(req) {
      structure(list(), class = "fake_resp")
    },
    .anthropic_resp_body_json = function(...) fake_body,
    .anthropic_resp_status = function(...) 200L,
    {
      res <- anthropic_compare_pair_live(
        ID1               = ID1,
        text1             = "A",
        ID2               = ID2,
        text2             = "B",
        model             = "claude-sonnet-4-5",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        include_raw       = FALSE
        # note: no temperature or max_tokens supplied
      )

      testthat::expect_equal(res$better_id, ID2)

      # Check that body has our recommended defaults
      testthat::expect_equal(length(bodies), 1L)
      b <- bodies[[1]]

      testthat::expect_equal(b$model, "claude-sonnet-4-5")
      testthat::expect_equal(b$max_tokens, 768)
      testthat::expect_equal(b$temperature, 0)
    }
  )
})

testthat::test_that("anthropic_compare_pair_live passes top_p parameter", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  bodies <- list()

  testthat::with_mocked_bindings(
    .anthropic_api_key = function(...) "FAKEKEY",
    .anthropic_req_body_json = function(req, body) {
      bodies <<- append(bodies, list(body))
      req
    },
    .anthropic_req_perform = function(...) structure(list(), class = "fake_resp"),
    .anthropic_resp_body_json = function(...) list(),
    .anthropic_resp_status = function(...) 200L,
    {
      anthropic_compare_pair_live(
        ID1 = "A", text1 = "a", ID2 = "B", text2 = "b", model = "claude",
        trait_name = td$name, trait_description = td$description,
        top_p = 0.95
      )

      testthat::expect_equal(bodies[[1]]$top_p, 0.95)
    }
  )
})

# ---------------------------------------------------------------------

testthat::test_that("anthropic_compare_pair_live defaults and thinking
                    depend on reasoning", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  bodies <- list()

  fake_body <- list(
    model = "claude-sonnet-4-5-20250929",
    id = "msg_01XYZ",
    type = "message",
    role = "assistant",
    content = list(
      list(type = "text", text = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")
    ),
    usage = list(
      input_tokens  = 10L,
      output_tokens = 4L
      # total_tokens omitted on purpose so we test computed total
    )
  )

  testthat::with_mocked_bindings(
    .anthropic_api_key = function(...) "FAKEKEY",
    .anthropic_req_body_json = function(req, body) {
      bodies <<- append(bodies, list(body))
      req
    },
    .anthropic_req_perform = function(req) {
      structure(list(), class = "fake_resp")
    },
    .anthropic_resp_body_json = function(...) fake_body,
    .anthropic_resp_status = function(...) 200L,
    {
      # Call once with reasoning = "none"
      res_none <- anthropic_compare_pair_live(
        ID1               = "S1",
        text1             = "A",
        ID2               = "S2",
        text2             = "B",
        model             = "claude-sonnet-4-5",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        reasoning         = "none"
      )

      # Call again with reasoning = "enabled"
      res_reason <- anthropic_compare_pair_live(
        ID1               = "S1",
        text1             = "A",
        ID2               = "S2",
        text2             = "B",
        model             = "claude-sonnet-4-5",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        reasoning         = "enabled"
      )

      # Parsed output correctness (same fake response in both cases)
      testthat::expect_equal(res_none$better_id, "S1")
      testthat::expect_equal(res_reason$better_id, "S1")

      # total_tokens should be computed as input + output (10 + 4)
      testthat::expect_equal(res_none$total_tokens, 14)
      testthat::expect_equal(res_reason$total_tokens, 14)

      # We should have captured two bodies with different defaults
      testthat::expect_equal(length(bodies), 2L)
      b_none <- bodies[[1]]
      b_reason <- bodies[[2]]

      # Temperatures differ by reasoning mode
      testthat::expect_equal(b_none$temperature, 0)
      testthat::expect_equal(b_reason$temperature, 1)

      # Different max_tokens by reasoning mode
      testthat::expect_equal(b_none$max_tokens, 768)
      testthat::expect_equal(b_reason$max_tokens, 2048)

      # No thinking block when reasoning = "none"
      testthat::expect_false("thinking" %in% names(b_none))

      # Thinking block present and well-formed when reasoning = "enabled"
      testthat::expect_true("thinking" %in% names(b_reason))
      testthat::expect_equal(b_reason$thinking$type, "enabled")
      # Default matches your function's default; adjust here if you changed it
      testthat::expect_equal(b_reason$thinking$budget_tokens, 1024)
    }
  )
})

testthat::test_that("anthropic_compare_pair_live upgrades reasoning when
                    include_thoughts = TRUE", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  bodies <- list()

  fake_body <- list(
    model = "claude-sonnet-4-5-20250929",
    id = "msg_01XYZ",
    type = "message",
    role = "assistant",
    content = list(
      list(type = "text", text = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")
    ),
    usage = list(
      input_tokens  = 10L,
      output_tokens = 4L
    )
  )

  testthat::with_mocked_bindings(
    .anthropic_api_key = function(...) "FAKEKEY",
    .anthropic_req_body_json = function(req, body) {
      bodies <<- append(bodies, list(body))
      req
    },
    .anthropic_req_perform = function(req) {
      structure(list(), class = "fake_resp")
    },
    .anthropic_resp_body_json = function(...) fake_body,
    .anthropic_resp_status = function(...) 200L,
    {
      res <- anthropic_compare_pair_live(
        ID1               = "S1",
        text1             = "A",
        ID2               = "S2",
        text2             = "B",
        model             = "claude-sonnet-4-5",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        reasoning         = "none",
        include_thoughts  = TRUE
      )

      testthat::expect_equal(res$better_id, "S1")

      testthat::expect_equal(length(bodies), 1L)
      b <- bodies[[1]]

      # When include_thoughts = TRUE and reasoning = "none", we upgrade to
      # extended thinking mode, which implies temperature = 1 and
      # a thinking block
      testthat::expect_equal(b$temperature, 1)
      testthat::expect_equal(b$max_tokens, 2048)
      testthat::expect_true("thinking" %in% names(b))
      testthat::expect_equal(b$thinking$type, "enabled")
      testthat::expect_equal(b$thinking$budget_tokens, 1024)
    }
  )
})

testthat::test_that("anthropic_compare_pair_live parses reasoning-enabled
                    output correctly", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"

  fake_body <- list(
    model = "claude-sonnet-4-5-20250929",
    id = "msg_01XYZ",
    type = "message",
    role = "assistant",
    content = list(
      # Simulate a more verbose answer you'd get with extended thinking:
      list(type = "text", text = "Some hidden reasoning and explanation. "),
      list(
        type = "text", text =
          "Final choice: <BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>."
      )
    ),
    usage = list(
      input_tokens  = 20L,
      output_tokens = 30L,
      total_tokens  = 50L
    )
  )

  bodies <- list()

  testthat::with_mocked_bindings(
    .anthropic_api_key = function(...) "FAKEKEY",
    .anthropic_req_body_json = function(req, body) {
      bodies <<- append(bodies, list(body))
      req
    },
    .anthropic_req_perform = function(req) {
      structure(list(), class = "fake_resp")
    },
    .anthropic_resp_body_json = function(...) fake_body,
    .anthropic_resp_status = function(...) 200L,
    {
      res <- anthropic_compare_pair_live(
        ID1               = ID1,
        text1             = "Text 1",
        ID2               = ID2,
        text2             = "Text 2",
        model             = "claude-sonnet-4-5",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        reasoning         = "enabled",
        include_raw       = TRUE
      )

      # Still a single-row tibble
      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(nrow(res), 1L)

      # We concatenated both blocks correctly
      testthat::expect_true(grepl(
        "Some hidden reasoning and explanation\\.",
        res$content
      ))
      testthat::expect_true(grepl("<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>",
        res$content,
        fixed = TRUE
      ))

      # Tag parsing works even with extra explanation
      testthat::expect_equal(res$better_sample, "SAMPLE_2")
      testthat::expect_equal(res$better_id, ID2)

      # Tokens passed through correctly
      testthat::expect_equal(res$prompt_tokens, 20)
      testthat::expect_equal(res$completion_tokens, 30)
      testthat::expect_equal(res$total_tokens, 50)

      # We did send a thinking block in the request
      testthat::expect_equal(length(bodies), 1L)
      b <- bodies[[1]]
      testthat::expect_true("thinking" %in% names(b))
      testthat::expect_equal(b$thinking$type, "enabled")
    }
  )
})

testthat::test_that("anthropic_compare_pair_live errors if temperature != 1
                    with reasoning = 'enabled'", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # temperature = 0 should be rejected when reasoning = "enabled"
  testthat::expect_error(
    anthropic_compare_pair_live(
      ID1               = "S1",
      text1             = "A",
      ID2               = "S2",
      text2             = "B",
      model             = "claude-sonnet-4-5",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl,
      reasoning         = "enabled",
      temperature       = 0
    ),
    "temperature` must be 1",
    fixed = FALSE
  )
})

testthat::test_that("anthropic_compare_pair_live enforces
                    thinking_budget_tokens constraints", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # budget too small
  testthat::expect_error(
    anthropic_compare_pair_live(
      ID1                     = "S1",
      text1                   = "A",
      ID2                     = "S2",
      text2                   = "B",
      model                   = "claude-sonnet-4-5",
      trait_name              = td$name,
      trait_description       = td$description,
      prompt_template         = tmpl,
      reasoning               = "enabled",
      thinking_budget_tokens  = 512
    ),
    "at least 1024",
    fixed = TRUE
  )

  # budget >= max_tokens should also error
  testthat::expect_error(
    anthropic_compare_pair_live(
      ID1                     = "S1",
      text1                   = "A",
      ID2                     = "S2",
      text2                   = "B",
      model                   = "claude-sonnet-4-5",
      trait_name              = td$name,
      trait_description       = td$description,
      prompt_template         = tmpl,
      reasoning               = "enabled",
      max_tokens              = 1500,
      thinking_budget_tokens  = 1500
    ),
    "thinking_budget_tokens.*must be smaller than.*max_tokens",
    fixed = FALSE
  )
})

testthat::test_that("anthropic_compare_pair_live validates input types", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # Test non-character inputs
  testthat::expect_error(
    anthropic_compare_pair_live(
      ID1 = 123, text1 = "t", ID2 = "B", text2 = "t",
      model = "claude", trait_name = td$name, trait_description = td$description
    ),
    "`ID1` must be a single character"
  )

  testthat::expect_error(
    anthropic_compare_pair_live(
      ID1 = "A", text1 = "t", ID2 = "B", text2 = "t",
      model = 123, trait_name = td$name, trait_description = td$description
    ),
    "`model` must be a single character"
  )
})

testthat::test_that("anthropic_compare_pair_live warns on conflicting thoughts settings", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # If user says reasoning="enabled" but include_thoughts=FALSE, it should warn
  # but keep reasoning="enabled"
  testthat::with_mocked_bindings(
    # Mock internals to avoid actual API call failure
    .anthropic_api_key = function(...) "FAKE",
    .anthropic_request = function(...) list(),
    .anthropic_req_body_json = function(req, ...) req,
    .anthropic_req_perform = function(...) structure(list(), class = "fake"),
    .anthropic_resp_status = function(...) 200L,
    .anthropic_resp_body_json = function(...) list(type = "message"),
    {
      testthat::expect_warning(
        anthropic_compare_pair_live(
          ID1 = "A", text1 = "t", ID2 = "B", text2 = "t",
          model = "claude", trait_name = td$name, trait_description = td$description,
          reasoning = "enabled",
          include_thoughts = FALSE
        ),
        "include_thoughts = FALSE but reasoning = 'enabled'"
      )
    }
  )
})

testthat::test_that("anthropic_compare_pair_live handles complex blocks (redacted thinking, multi-text)", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # Complex body: Thinking + Redacted Thinking + Split Text
  complex_body <- list(
    id = "msg_complex",
    type = "message",
    model = "claude-test",
    role = "assistant",
    content = list(
      list(type = "thinking", thinking = "I am thinking... "),
      list(type = "redacted_thinking", data = "HIDDEN"),
      list(type = "text", text = "Here is part 1. "),
      list(type = "text", text = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")
    ),
    usage = list(input_tokens = 10, output_tokens = 10, total_tokens = 20)
  )

  testthat::with_mocked_bindings(
    .anthropic_api_key = function(...) "FAKE",
    .anthropic_req_body_json = function(req, ...) req,
    .anthropic_req_perform = function(...) structure(list(), class = "fake"),
    .anthropic_resp_status = function(...) 200L,
    .anthropic_resp_body_json = function(...) complex_body,
    {
      res <- anthropic_compare_pair_live(
        ID1 = "S1", text1 = "A", ID2 = "S2", text2 = "B",
        model = "claude", trait_name = td$name, trait_description = td$description
      )

      # Check thoughts concatenation including redaction tag
      testthat::expect_match(res$thoughts, "I am thinking... ", fixed = TRUE)
      testthat::expect_match(res$thoughts, "[REDACTED_THINKING]HIDDEN", fixed = TRUE)

      # Check content concatenation
      testthat::expect_match(res$content, "Here is part 1. <BETTER_SAMPLE>", fixed = TRUE)
      testthat::expect_equal(res$better_sample, "SAMPLE_1")
    }
  )
})

testthat::test_that("anthropic_compare_pair_live calculates total_tokens fallback", {
  td <- trait_description("overall_quality")

  # Usage body missing total_tokens
  body_no_total <- list(
    type = "message",
    content = list(list(type = "text", text = "HI")),
    usage = list(input_tokens = 50, output_tokens = 50) # Sum = 100
  )

  testthat::with_mocked_bindings(
    .anthropic_api_key = function(...) "FAKE",
    .anthropic_req_body_json = function(req, ...) req,
    .anthropic_req_perform = function(...) structure(list(), class = "fake"),
    .anthropic_resp_status = function(...) 200L,
    .anthropic_resp_body_json = function(...) body_no_total,
    {
      res <- anthropic_compare_pair_live(
        ID1 = "A", text1 = "t", ID2 = "B", text2 = "t",
        model = "claude", trait_name = td$name, trait_description = td$description
      )
      testthat::expect_equal(res$total_tokens, 100)
    }
  )
})

# =====================================================================
# NEW TESTS for submit_anthropic_pairs_live (List output & Features)
# =====================================================================

testthat::test_that("submit_anthropic_pairs_live returns list structure for zero rows", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  empty_pairs <- tibble::tibble(
    ID1 = character(0), text1 = character(0),
    ID2 = character(0), text2 = character(0)
  )

  res <- submit_anthropic_pairs_live(
    pairs = empty_pairs,
    model = "claude-sonnet-4-5",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl
  )

  # Check list structure
  testthat::expect_type(res, "list")
  testthat::expect_named(res, c("results", "failed_pairs", "failed_attempts"))
  testthat::expect_s3_class(res$results, "tbl_df")
  testthat::expect_s3_class(res$failed_pairs, "tbl_df")
  testthat::expect_equal(nrow(res$results), 0L)
  testthat::expect_equal(nrow(res$failed_pairs), 0L)

  # Check include_raw logic
  res_raw <- submit_anthropic_pairs_live(
    pairs = empty_pairs,
    model = "claude-sonnet-4-5",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    include_raw = TRUE
  )
  testthat::expect_true("raw_response" %in% names(res_raw$results))
})

testthat::test_that("submit_anthropic_pairs_live runs correctly and returns list", {
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
    anthropic_compare_pair_live = function(ID1, ID2, ...) {
      tibble::tibble(
        custom_id = sprintf("LIVE_%s_vs_%s", ID1, ID2),
        ID1 = ID1,
        ID2 = ID2,
        model = "mod",
        status_code = 200L,
        error_message = NA_character_,
        better_sample = "SAMPLE_1",
        better_id = ID1
      )
    },
    .env = pll_ns
  )

  res <- submit_anthropic_pairs_live(
    pairs = pairs,
    model = "claude-sonnet-4-5",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    verbose = FALSE
  )

  testthat::expect_type(res, "list")
  testthat::expect_equal(nrow(res$results), 2L)
  testthat::expect_equal(nrow(res$failed_pairs), 0L)
})

testthat::test_that("submit_anthropic_pairs_live separates failed pairs", {
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")

  pairs <- tibble::tibble(
    ID1 = c("S01", "FailMe"),
    text1 = "A", ID2 = "B", text2 = "C"
  )

  testthat::with_mocked_bindings(
    anthropic_compare_pair_live = function(ID1, ...) {
      if (ID1 == "FailMe") {
        # Simulate error return from lower-level function
        return(tibble::tibble(
          custom_id = "LIVE_FailMe_vs_B", ID1 = ID1, ID2 = "B",
          model = "claude", status_code = 500L,
          error_message = "Mock API Error",
          better_id = NA_character_
        ))
      }
      # Success case
      tibble::tibble(
        custom_id = "LIVE_S01_vs_B", ID1 = ID1, ID2 = "B",
        model = "claude", status_code = 200L,
        error_message = NA_character_,
        better_id = ID1
      )
    },
    .env = pll_ns,
    {
      res <- submit_anthropic_pairs_live(
        pairs, "claude", td$name, td$description,
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

testthat::test_that("submit_anthropic_pairs_live respects save_path (Resume Logic)", {
  testthat::skip_if_not_installed("readr")
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")
  tmp_csv <- tempfile(fileext = ".csv")

  # 1. Create a "fake" existing result file
  # Pair S01 vs S02 is "already done"
  existing_data <- tibble::tibble(
    custom_id = "LIVE_S01_vs_S02",
    ID1 = "S01", ID2 = "S02",
    model = "claude", status_code = 200L, error_message = NA_character_
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
    anthropic_compare_pair_live = function(...) {
      call_count <<- call_count + 1
      tibble::tibble(
        custom_id = "LIVE_S03_vs_S04",
        ID1 = "S03", ID2 = "S04",
        model = "claude", status_code = 200L, error_message = NA_character_
      )
    },
    .env = pll_ns,
    {
      res <- submit_anthropic_pairs_live(
        pairs = pairs,
        model = "claude",
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

testthat::test_that("submit_anthropic_pairs_live validates inputs", {
  td <- trait_description("overall_quality")

  # 1. Missing columns
  bad_pairs <- tibble::tibble(ID1 = "A", text1 = "t")
  testthat::expect_error(
    submit_anthropic_pairs_live(bad_pairs, "claude", td$name, td$description),
    "must contain columns"
  )

  # 2. Invalid status_every (0 is not positive)
  good_pairs <- tibble::tibble(ID1 = "A", text1 = "t", ID2 = "B", text2 = "t")
  testthat::expect_error(
    submit_anthropic_pairs_live(
      good_pairs, "claude", td$name, td$description,
      status_every = 0
    ),
    "positive integer"
  )
})

testthat::test_that("submit_anthropic_pairs_live: Sequential error handling (catch block)", {
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")

  # Force error in internal function to hit lines 931-940
  testthat::with_mocked_bindings(
    anthropic_compare_pair_live = function(...) stop("Sequential Error"),
    .env = pll_ns,
    {
      res <- submit_anthropic_pairs_live(
        pairs, "model", td$name, td$description,
        verbose = FALSE
      )

      # Should be in failed_pairs with the caught error message
      testthat::expect_equal(nrow(res$failed_pairs), 1L)
      testthat::expect_match(res$failed_pairs$error_message, "Sequential Error")
      testthat::expect_equal(res$failed_pairs$ID1, "A")
    }
  )
})


testthat::test_that("submit_anthropic_pairs_live: Directory creation & Raw response cleanup", {
  testthat::skip_if_not_installed("readr")
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")

  # Use a path in a new subdirectory to test dir.create (Lines 750-751)
  tmp_dir <- tempfile()
  tmp_file <- file.path(tmp_dir, "out.csv")

  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")

  testthat::with_mocked_bindings(
    anthropic_compare_pair_live = function(...) {
      # Return result with raw_response to test removal logic (Line 947)
      res <- tibble::tibble(
        custom_id = "LIVE_A_vs_B", ID1 = "A", ID2 = "B",
        model = "m", status_code = 200, error_message = NA
      )
      res$raw_response <- list(list(foo = "bar"))
      res
    },
    .env = pll_ns,
    {
      out <- capture.output(
        {
          res <- submit_anthropic_pairs_live(
            pairs, "model", td$name, td$description,
            save_path = tmp_file, verbose = TRUE, include_raw = TRUE
          )
        },
        type = "message"
      )

      # Check directory creation message
      testthat::expect_true(any(grepl("Creating output directory", out)))
      testthat::expect_true(dir.exists(tmp_dir))

      # Check that raw_response was removed from the saved file
      saved <- readr::read_csv(tmp_file, show_col_types = FALSE)
      testthat::expect_false("raw_response" %in% names(saved))

      # But present in the returned object
      testthat::expect_true("raw_response" %in% names(res$results))
    }
  )
  unlink(tmp_dir, recursive = TRUE)
})

testthat::test_that("submit_anthropic_pairs_live: Parallel execution & Parallel error handling", {
  testthat::skip_if_not_installed("future")
  testthat::skip_if_not_installed("future.apply")
  testthat::skip_if_not_installed("readr") # We will use save_path here too

  td <- trait_description("overall_quality")

  # Define enough pairs to potentially trigger chunking if chunk_size was small,
  # but mainly to test the parallel loop mechanics.
  pairs_par <- tibble::tibble(
    ID1 = c("A", "B"), text1 = c("a", "b"),
    ID2 = c("C", "D"), text2 = c("c", "d")
  )

  tmp_par <- tempfile(fileext = ".csv")

  # We run with parallel = TRUE and workers = 2.
  # We do NOT mock anthropic_compare_pair_live here. This means the workers
  # will execute the real function, fail (due to fake API key / no network),
  # and trigger the tryCatch inside 'work_fn' (Lines 868-881).
  # This covers the parallel block, the chunk loop, and the parallel error handling.

  out_par <- capture.output(
    {
      res_par <- submit_anthropic_pairs_live(
        pairs_par, "model", td$name, td$description,
        parallel = TRUE, workers = 2, verbose = TRUE,
        api_key = "fake_key", # Ensure failure
        save_path = tmp_par
      )
    },
    type = "message"
  )

  # Check parallel message (Line 834)
  testthat::expect_true(any(grepl("Processing 2 pairs in PARALLEL", out_par)))

  # Check that we captured the failures from workers
  testthat::expect_equal(nrow(res_par$failed_pairs), 2L)
  testthat::expect_true(all(!is.na(res_par$failed_pairs$error_message)))

  # Check incremental save worked in parallel (Lines 887-897)
  # Even though they failed, the error tibble is saved.
  testthat::expect_true(file.exists(tmp_par))
  saved_par <- readr::read_csv(tmp_par, show_col_types = FALSE)
  testthat::expect_equal(nrow(saved_par), 2L)

  unlink(tmp_par)
})

testthat::test_that("submit_anthropic_pairs_live: Parallel Save Failure", {
  testthat::skip_if_not_installed("future")
  testthat::skip_if_not_installed("readr")

  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")
  tmp_file <- tempfile(fileext = ".csv")

  # 3. Test File Save Failure (Parallel) (Line 896)
  # Tricky part: We need to allow the parallel worker to run (requireNamespace = TRUE),
  # but fail the write_csv in the main process.

  testthat::with_mocked_bindings(
    # We must ensure we don't block the parallel workers from starting
    # so we don't mock requireNamespace here.

    # Mock write_csv to fail
    write_csv = function(...) stop("Parallel Disk full"),
    .package = "readr",
    {
      # We force parallel execution.
      # Since we aren't mocking the internal worker function here, it might fail or succeed
      # depending on the environment (likely fail due to missing API key),
      # but it WILL attempt to save the result (even an error result) to CSV.

      testthat::expect_warning(
        submit_anthropic_pairs_live(
          pairs, "model", td$name, td$description,
          parallel = TRUE, workers = 2, save_path = tmp_file, verbose = FALSE
        ),
        "Failed to save incremental results"
      )
    }
  )
})

testthat::test_that("submit_anthropic_pairs_live: Resume logic (Read Error Handling)", {
  testthat::skip_if_not_installed("readr")
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")
  tmp <- tempfile(fileext = ".csv")

  # Create a dummy file so the function attempts to read it
  file.create(tmp)

  # Mock read_csv to throw an error, forcing the tryCatch error handler (Line 787)
  testthat::with_mocked_bindings(
    read_csv = function(...) stop("Mock Read Error"),
    .package = "readr",
    {
      # Mock the internal API call to return success so the function proceeds
      testthat::with_mocked_bindings(
        anthropic_compare_pair_live = function(...) {
          tibble::tibble(
            custom_id = "LIVE_A_vs_B", ID1 = "A", ID2 = "B",
            model = "m", status_code = 200, error_message = NA
          )
        },
        .env = pll_ns,
        {
          testthat::expect_warning(
            submit_anthropic_pairs_live(
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

testthat::test_that("submit_anthropic_pairs_live: Sequential Save Error Handling", {
  testthat::skip_if_not_installed("readr")
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")
  tmp_file <- tempfile(fileext = ".csv")

  # Mock write_csv to throw an error (triggering Line 953 warning)
  testthat::with_mocked_bindings(
    write_csv = function(...) stop("Disk full"),
    .package = "readr",
    {
      testthat::with_mocked_bindings(
        anthropic_compare_pair_live = function(...) {
          tibble::tibble(
            custom_id = "LIVE_A_vs_B", ID1 = "A", ID2 = "B",
            model = "m", status_code = 200, error_message = NA
          )
        },
        .env = pll_ns,
        {
          testthat::expect_warning(
            submit_anthropic_pairs_live(
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

testthat::test_that("submit_anthropic_pairs_live: Parallel Save Error Handling", {
  testthat::skip_if_not_installed("future")
  testthat::skip_if_not_installed("readr")

  td <- trait_description("overall_quality")
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")
  tmp_file <- tempfile(fileext = ".csv")

  # Mock write_csv to throw an error (triggering Line 896 warning)
  testthat::with_mocked_bindings(
    write_csv = function(...) stop("Parallel Disk full"),
    .package = "readr",
    {
      # We force parallel execution. The worker function is NOT mocked, so it will
      # execute the real code, likely fail (missing API key), and return an error tibble.
      # The main process then attempts to save this error tibble and hits our mock error.

      testthat::expect_warning(
        submit_anthropic_pairs_live(
          pairs, "model", td$name, td$description,
          parallel = TRUE, workers = 2, save_path = tmp_file, verbose = FALSE
        ),
        "Failed to save incremental results"
      )
    }
  )
})
