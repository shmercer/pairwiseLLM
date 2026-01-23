# =====================================================================
# test-openai_live.R
# Tests for openai_compare_pair_live() and submit_openai_pairs_live()
# =====================================================================

trait_description <- pairwiseLLM:::trait_description
set_prompt_template <- pairwiseLLM:::set_prompt_template
openai_compare_pair_live <- pairwiseLLM::openai_compare_pair_live
submit_openai_pairs_live <- pairwiseLLM::submit_openai_pairs_live
openai_compare_pair_live_orig <- openai_compare_pair_live

testthat::test_that("openai_compare_pair_live parses chat.completions correctly", {
  data("example_writing_samples", package = "pairwiseLLM")
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_body <- list(
    object = "chat.completion",
    model = "gpt-4.1",
    choices = list(list(
      message = list(
        role = "assistant",
        content = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> Some explanation."
      )
    )),
    usage = list(
      prompt_tokens = 10L,
      completion_tokens = 5L,
      total_tokens = 15L
    )
  )

  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "FAKEKEY",
    .openai_req_body_json = function(req, body) req,
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) fake_body,
    .openai_resp_status = function(...) 200L,
    {
      res <- openai_compare_pair_live(
        ID1 = ID1,
        text1 = text1,
        ID2 = ID2,
        text2 = text2,
        model = "gpt-4.1",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        endpoint = "chat.completions",
        temperature = 0,
        include_raw = TRUE
      )

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(nrow(res), 1L)
      testthat::expect_equal(res$custom_id, sprintf("LIVE_%s_vs_%s", ID1, ID2))
      testthat::expect_equal(res$ID1, ID1)
      testthat::expect_equal(res$ID2, ID2)
      testthat::expect_equal(res$model, "gpt-4.1")
      testthat::expect_equal(res$object_type, "chat.completion")
      testthat::expect_equal(res$status_code, 200L)
      testthat::expect_true(is.na(res$error_message))
      testthat::expect_equal(
        res$content,
        "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> Some explanation."
      )
      testthat::expect_equal(res$better_sample, "SAMPLE_1")
      testthat::expect_equal(res$better_id, ID1)
      testthat::expect_equal(res$prompt_tokens, 10)
      testthat::expect_equal(res$completion_tokens, 5)
      testthat::expect_equal(res$total_tokens, 15)

      # raw_response
      testthat::expect_true("raw_response" %in% names(res))
      testthat::expect_type(res$raw_response, "list")
      testthat::expect_equal(res$raw_response[[1]]$object, "chat.completion")
      testthat::expect_equal(res$raw_response[[1]]$model, "gpt-4.1")
    }
  )
})

# ---------------------------------------------------------------------

testthat::test_that("openai_compare_pair_live parses responses endpoint correctly", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text A"
  text2 <- "Text B"

  fake_body <- list(
    object = "response",
    model = "gpt-5.1",
    output = list(list(
      content = list(
        list(
          type = "output_text", text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> A "
        ),
        list(type = "output_text", text = "B")
      )
    )),
    usage = list(
      input_tokens = 7L,
      output_tokens = 3L,
      total_tokens = 10L
    )
  )

  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "FAKEKEY",
    .openai_req_body_json = function(req, body) req,
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) fake_body,
    .openai_resp_status = function(...) 200L,
    {
      res <- openai_compare_pair_live(
        ID1 = ID1,
        text1 = text1,
        ID2 = ID2,
        text2 = text2,
        model = "gpt-5.1",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        endpoint = "responses",
        reasoning = "none",
        include_raw = TRUE
      )

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res$object_type, "response")
      testthat::expect_equal(res$model, "gpt-5.1")
      testthat::expect_equal(
        res$content,
        "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> A B"
      )
      testthat::expect_equal(res$better_sample, "SAMPLE_2")
      testthat::expect_equal(res$better_id, ID2)
      testthat::expect_equal(res$prompt_tokens, 7)
      testthat::expect_equal(res$completion_tokens, 3)
      testthat::expect_equal(res$total_tokens, 10)
      testthat::expect_true("raw_response" %in% names(res))
      testthat::expect_equal(res$raw_response[[1]]$model, "gpt-5.1")
    }
  )
})

# ---------------------------------------------------------------------

testthat::test_that("openai_compare_pair_live returns error row on JSON parse failure", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  ID1 <- "S01"
  ID2 <- "S02"

  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "FAKEKEY",
    .openai_req_body_json = function(req, body) req,
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) stop("boom"),
    .openai_resp_status = function(...) 500L,
    {
      res <- openai_compare_pair_live(
        ID1 = ID1,
        text1 = "X",
        ID2 = ID2,
        text2 = "Y",
        model = "gpt-4.1",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        endpoint = "chat.completions",
        include_raw = TRUE
      )

      testthat::expect_equal(res$status_code, 500L)
      testthat::expect_equal(
        res$error_message,
        "Failed to parse JSON."
      )
      testthat::expect_true(is.na(res$better_sample))
      testthat::expect_true(is.null(res$raw_response[[1]]))
    }
  )
})

# ---------------------------------------------------------------------

testthat::test_that("openai_compare_pair_live enforces gpt-5.1/5.2 + reasoning constraints", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # 1. GPT-5.1 Should error
  testthat::expect_error(
    openai_compare_pair_live(
      ID1 = "A", text1 = "x", ID2 = "B", text2 = "y",
      model = "gpt-5.1",
      trait_name = td$name,
      trait_description = td$description,
      prompt_template = tmpl,
      endpoint = "responses",
      reasoning = "low",
      temperature = 0
    ),
    regexp = "gpt-5.1/5.2"
  )

  # 2. GPT-5.2 date-stamped should error
  testthat::expect_error(
    openai_compare_pair_live(
      ID1 = "A", text1 = "x", ID2 = "B", text2 = "y",
      model = "gpt-5.2-2025-12-11",
      trait_name = td$name,
      trait_description = td$description,
      prompt_template = tmpl,
      endpoint = "responses",
      reasoning = "medium",
      top_p = 0.5
    ),
    regexp = "gpt-5.1/5.2"
  )

  # Allowed case
  fake_body <- list(
    object = "response",
    model = "gpt-5.1",
    output = list(list(
      content = list(list(
        type = "output_text", text = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>"
      ))
    )),
    usage = list(input_tokens = 1L, output_tokens = 1L, total_tokens = 2L)
  )

  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "FAKEKEY",
    .openai_req_body_json = function(req, body) req,
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) fake_body,
    .openai_resp_status = function(...) 200L,
    {
      res <- openai_compare_pair_live(
        ID1 = "A", text1 = "x", ID2 = "B", text2 = "y",
        model = "gpt-5.1",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        endpoint = "responses",
        reasoning = "none",
        include_raw = TRUE
      )
      testthat::expect_equal(res$better_id, "A")
    }
  )
})

# ---------------------------------------------------------------------

testthat::test_that("openai_compare_pair_live allows other gpt-5* models with temp=0", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # Mock success response
  fake_body <- list(
    object = "response",
    model = "gpt-5-mini",
    output = list(list(
      content = list(list(
        type = "output_text", text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>"
      ))
    )),
    usage = list(input_tokens = 1L, output_tokens = 1L, total_tokens = 2L)
  )

  # Capture request body to check temperature
  captured_body <- NULL

  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "FAKEKEY",
    .openai_req_body_json = function(req, body) {
      captured_body <<- body
      req
    },
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) fake_body,
    .openai_resp_status = function(...) 200L,
    {
      res <- openai_compare_pair_live(
        ID1 = "A", text1 = "x", ID2 = "B", text2 = "y",
        model = "gpt-5-mini",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        endpoint = "responses",
        include_raw = TRUE
      )
      testthat::expect_equal(res$better_id, "B")
      # Check that temperature was defaulted to 0
      testthat::expect_equal(captured_body$temperature, 0)
    }
  )
})

# ---------------------------------------------------------------------

testthat::test_that("openai_compare_pair_live collects thoughts and message text separately for responses", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text A"
  text2 <- "Text B"

  fake_body <- list(
    object = "response",
    model = "gpt-5.1",
    reasoning = list(
      effort = "low",
      summary = list(text = "Reasoning summary. ")
    ),
    output = list(
      list(
        id = "rs_x",
        type = "reasoning",
        summary = list()
      ),
      list(
        id = "msg_x",
        type = "message",
        status = "completed",
        content = list(
          list(
            type = "output_text",
            text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> Final answer."
          )
        ),
        role = "assistant"
      )
    ),
    usage = list(
      input_tokens = 10L,
      output_tokens = 5L,
      total_tokens = 15L
    )
  )

  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "FAKEKEY",
    .openai_req_body_json = function(req, body) req,
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) fake_body,
    .openai_resp_status = function(...) 200L,
    {
      res <- openai_compare_pair_live(
        ID1 = ID1,
        text1 = text1,
        ID2 = ID2,
        text2 = text2,
        model = "gpt-5.1",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        endpoint = "responses",
        reasoning = "low",
        include_thoughts = TRUE,
        include_raw = TRUE
      )

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res$object_type, "response")

      # Reasoning summary should go to thoughts
      testthat::expect_equal(res$thoughts, "Reasoning summary. ")

      # Content should be assistant message only
      testthat::expect_equal(
        res$content,
        "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> Final answer."
      )
      testthat::expect_equal(res$better_sample, "SAMPLE_2")
      testthat::expect_equal(res$better_id, ID2)
    }
  )
})

# ---------------------------------------------------------------------

testthat::test_that("openai_compare_pair_live picks up reasoning summary from output items", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text A"
  text2 <- "Text B"

  fake_body <- list(
    object = "response",
    model = "gpt-5.1",
    # No top-level reasoning$summary here
    reasoning = list(
      effort = "low"
    ),
    output = list(
      list(
        id = "rs_x",
        type = "reasoning",
        summary = list(
          list(type = "summary_text", text = "Reasoning sentence 1."),
          list(type = "summary_text", text = "Reasoning sentence 2.")
        )
      ),
      list(
        id = "msg_x",
        type = "message",
        status = "completed",
        content = list(
          list(
            type = "output_text",
            text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> Final answer."
          )
        ),
        role = "assistant"
      )
    ),
    usage = list(
      input_tokens = 5L,
      output_tokens = 5L,
      total_tokens = 10L
    )
  )

  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "FAKEKEY",
    .openai_req_body_json = function(req, body) req,
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) fake_body,
    .openai_resp_status = function(...) 200L,
    {
      res <- openai_compare_pair_live(
        ID1 = ID1,
        text1 = text1,
        ID2 = ID2,
        text2 = text2,
        model = "gpt-5.1",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        endpoint = "responses",
        reasoning = "low",
        include_thoughts = TRUE,
        include_raw = TRUE
      )

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res$object_type, "response")

      # Thoughts should be both summary_text entries present
      testthat::expect_match(res$thoughts, "Reasoning sentence 1.", fixed = TRUE)
      testthat::expect_match(res$thoughts, "Reasoning sentence 2.", fixed = TRUE)

      # Content should be assistant message only
      testthat::expect_equal(
        res$content,
        "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> Final answer."
      )
      testthat::expect_equal(res$better_sample, "SAMPLE_2")
      testthat::expect_equal(res$better_id, ID2)
    }
  )
})

testthat::test_that("openai_compare_pair_live validates input types", {
  td <- trait_description("overall_quality")
  testthat::expect_error(
    openai_compare_pair_live(
      ID1 = 123, text1 = "t", ID2 = "B", text2 = "t",
      model = "gpt-4", trait_name = td$name, trait_description = td$description
    ),
    "ID1 invalid"
  )
  testthat::expect_error(
    openai_compare_pair_live(
      ID1 = "A", text1 = "t", ID2 = "B", text2 = list(),
      model = "gpt-4", trait_name = td$name, trait_description = td$description
    ),
    "text2 invalid"
  )
})

testthat::test_that("openai_compare_pair_live handles HTTP errors gracefully", {
  td <- trait_description("overall_quality")
  # Simulate 400 Bad Request
  fake_error_body <- list(
    error = list(message = "Invalid parameter")
  )
  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "KEY",
    .openai_req_body_json = function(req, ...) req,
    .openai_req_perform = function(...) "RESP",
    .openai_resp_status = function(...) 400L,
    .openai_resp_body_json = function(...) fake_error_body,
    {
      res <- openai_compare_pair_live(
        ID1 = "A", text1 = "t", ID2 = "B", text2 = "t",
        model = "gpt-4", trait_name = td$name, trait_description = td$description
      )
      testthat::expect_equal(res$status_code, 400L)
      testthat::expect_equal(res$error_message, "Invalid parameter")
      testthat::expect_true(is.na(res$content))
    }
  )
})

testthat::test_that("openai_compare_pair_live parses legacy reasoning summary location", {
  td <- trait_description("overall_quality")
  # Old structure where summary was at body$reasoning$summary$text
  fake_body <- list(
    object = "response",
    model = "gpt-5.1",
    reasoning = list(
      effort = "low",
      summary = list(text = "Legacy summary.")
    ),
    output = list(
      list(
        type = "message",
        content = list(list(type = "output_text", text = "Content"))
      )
    )
  )
  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "KEY",
    .openai_req_body_json = function(req, ...) req,
    .openai_req_perform = function(...) "RESP",
    .openai_resp_status = function(...) 200L,
    .openai_resp_body_json = function(...) fake_body,
    {
      res <- openai_compare_pair_live(
        ID1 = "A", text1 = "t", ID2 = "B", text2 = "t",
        model = "gpt-5.1", trait_name = td$name, trait_description = td$description,
        endpoint = "responses"
      )
      testthat::expect_equal(res$thoughts, "Legacy summary.")
      testthat::expect_equal(res$content, "Content")
    }
  )
})

testthat::test_that("openai_compare_pair_live validates ID2, text1, and model", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # Invalid ID2
  testthat::expect_error(
    openai_compare_pair_live(
      ID1 = "A", text1 = "A", ID2 = 123, text2 = "B",
      model = "gpt-4", trait_name = td$name, trait_description = td$description,
      prompt_template = tmpl
    ),
    "ID2 invalid"
  )

  # Invalid text1
  testthat::expect_error(
    openai_compare_pair_live(
      ID1 = "A", text1 = list(), ID2 = "B", text2 = "B",
      model = "gpt-4", trait_name = td$name, trait_description = td$description,
      prompt_template = tmpl
    ),
    "text1 invalid"
  )

  # Invalid model
  testthat::expect_error(
    openai_compare_pair_live(
      ID1 = "A", text1 = "A", ID2 = "B", text2 = "B",
      model = 123, trait_name = td$name, trait_description = td$description,
      prompt_template = tmpl
    ),
    "model invalid"
  )
})

testthat::test_that("openai_compare_pair_live passes optional parameters (top_p, logprobs)", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  captured_body <- NULL

  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "KEY",
    .openai_req_body_json = function(req, body) {
      captured_body <<- body
      req
    },
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) list(),
    .openai_resp_status = function(...) 200L,
    {
      # 1. Chat Completions
      openai_compare_pair_live(
        "A", "t", "B", "t", "gpt-4", td$name, td$description, tmpl,
        endpoint = "chat.completions",
        top_p = 0.9,
        logprobs = TRUE
      )
      testthat::expect_equal(captured_body$top_p, 0.9)
      testthat::expect_equal(captured_body$logprobs, TRUE)

      # 2. Responses
      openai_compare_pair_live(
        "A", "t", "B", "t", "gpt-5.1", td$name, td$description, tmpl,
        endpoint = "responses",
        top_p = 0.8,
        logprobs = FALSE
      )
      testthat::expect_equal(captured_body$top_p, 0.8)
      testthat::expect_equal(captured_body$logprobs, FALSE)
    }
  )
})

testthat::test_that("openai_compare_pair_live constructs generic HTTP error message", {
  td <- trait_description("overall_quality")
  # Response with error status but no body$error object (triggering the else if status >= 400 block)
  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "KEY",
    .openai_req_body_json = function(req, body) req,
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) list(), # Empty body
    .openai_resp_status = function(...) 418L,
    {
      res <- openai_compare_pair_live("A", "t", "B", "t", "gpt-4", td$name, td$description)
      testthat::expect_equal(res$status_code, 418L)
      testthat::expect_equal(res$error_message, "HTTP 418")
    }
  )
})

testthat::test_that("openai_compare_pair_live parses dataframe reasoning summaries", {
  td <- trait_description("overall_quality")

  # output structure where summary is a data.frame
  fake_body <- list(
    object = "response",
    model = "gpt-5.1",
    output = list(
      list(
        type = "reasoning",
        summary = data.frame(text = "DF Summary", stringsAsFactors = FALSE)
      )
    )
  )

  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "KEY",
    .openai_req_body_json = function(req, body) req,
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) fake_body,
    .openai_resp_status = function(...) 200L,
    {
      res <- openai_compare_pair_live("A", "t", "B", "t", "gpt-5.1", td$name, td$description, endpoint = "responses")
      testthat::expect_equal(res$thoughts, "DF Summary")
    }
  )
})

testthat::test_that("openai_compare_pair_live validates ID2, text1, and model", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # Invalid ID2
  testthat::expect_error(
    openai_compare_pair_live(
      ID1 = "A", text1 = "A", ID2 = 123, text2 = "B",
      model = "gpt-4", trait_name = td$name, trait_description = td$description,
      prompt_template = tmpl
    ),
    "ID2 invalid"
  )

  # Invalid text1
  testthat::expect_error(
    openai_compare_pair_live(
      ID1 = "A", text1 = list(), ID2 = "B", text2 = "B",
      model = "gpt-4", trait_name = td$name, trait_description = td$description,
      prompt_template = tmpl
    ),
    "text1 invalid"
  )

  # Invalid model
  testthat::expect_error(
    openai_compare_pair_live(
      ID1 = "A", text1 = "A", ID2 = "B", text2 = "B",
      model = 123, trait_name = td$name, trait_description = td$description,
      prompt_template = tmpl
    ),
    "model invalid"
  )
})

testthat::test_that("openai_compare_pair_live passes optional parameters (top_p, logprobs)", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  captured_body <- NULL

  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "KEY",
    .openai_req_body_json = function(req, body) {
      captured_body <<- body
      req
    },
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) list(),
    .openai_resp_status = function(...) 200L,
    {
      # 1. Chat Completions
      openai_compare_pair_live(
        "A", "t", "B", "t", "gpt-4", td$name, td$description, tmpl,
        endpoint = "chat.completions",
        top_p = 0.9,
        logprobs = TRUE
      )
      testthat::expect_equal(captured_body$top_p, 0.9)
      testthat::expect_equal(captured_body$logprobs, TRUE)

      # 2. Responses
      openai_compare_pair_live(
        "A", "t", "B", "t", "gpt-5.1", td$name, td$description, tmpl,
        endpoint = "responses",
        top_p = 0.8,
        logprobs = FALSE
      )
      testthat::expect_equal(captured_body$top_p, 0.8)
      testthat::expect_equal(captured_body$logprobs, FALSE)
    }
  )
})

testthat::test_that("openai_compare_pair_live constructs generic HTTP error message", {
  td <- trait_description("overall_quality")
  # Response with error status but no body$error object (triggering the else if status >= 400 block)
  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "KEY",
    .openai_req_body_json = function(req, body) req,
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) list(), # Empty body
    .openai_resp_status = function(...) 418L,
    {
      res <- openai_compare_pair_live("A", "t", "B", "t", "gpt-4", td$name, td$description)
      testthat::expect_equal(res$status_code, 418L)
      testthat::expect_equal(res$error_message, "HTTP 418")
    }
  )
})

testthat::test_that("openai_compare_pair_live parses dataframe reasoning summaries", {
  td <- trait_description("overall_quality")

  # output structure where summary is a data.frame
  fake_body <- list(
    object = "response",
    model = "gpt-5.1",
    output = list(
      list(
        type = "reasoning",
        summary = data.frame(text = "DF Summary", stringsAsFactors = FALSE)
      )
    )
  )

  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "KEY",
    .openai_req_body_json = function(req, body) req,
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) fake_body,
    .openai_resp_status = function(...) 200L,
    {
      res <- openai_compare_pair_live("A", "t", "B", "t", "gpt-5.1", td$name, td$description, endpoint = "responses")
      testthat::expect_equal(res$thoughts, "DF Summary")
    }
  )
})

testthat::test_that("openai_compare_pair_live validates ID2, text1, and model", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # Invalid ID2
  testthat::expect_error(
    openai_compare_pair_live(
      ID1 = "A", text1 = "A", ID2 = 123, text2 = "B",
      model = "gpt-4", trait_name = td$name, trait_description = td$description,
      prompt_template = tmpl
    ),
    "ID2 invalid"
  )

  # Invalid text1
  testthat::expect_error(
    openai_compare_pair_live(
      ID1 = "A", text1 = list(), ID2 = "B", text2 = "B",
      model = "gpt-4", trait_name = td$name, trait_description = td$description,
      prompt_template = tmpl
    ),
    "text1 invalid"
  )

  # Invalid model
  testthat::expect_error(
    openai_compare_pair_live(
      ID1 = "A", text1 = "A", ID2 = "B", text2 = "B",
      model = 123, trait_name = td$name, trait_description = td$description,
      prompt_template = tmpl
    ),
    "model invalid"
  )
})

testthat::test_that("openai_compare_pair_live passes optional parameters (top_p, logprobs)", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  captured_body <- NULL

  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "KEY",
    .openai_req_body_json = function(req, body) {
      captured_body <<- body
      req
    },
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) list(),
    .openai_resp_status = function(...) 200L,
    {
      # 1. Chat Completions
      openai_compare_pair_live(
        "A", "t", "B", "t", "gpt-4", td$name, td$description, tmpl,
        endpoint = "chat.completions",
        top_p = 0.9,
        logprobs = TRUE
      )
      testthat::expect_equal(captured_body$top_p, 0.9)
      testthat::expect_equal(captured_body$logprobs, TRUE)

      # 2. Responses
      openai_compare_pair_live(
        "A", "t", "B", "t", "gpt-5.1", td$name, td$description, tmpl,
        endpoint = "responses",
        top_p = 0.8,
        logprobs = FALSE
      )
      testthat::expect_equal(captured_body$top_p, 0.8)
      testthat::expect_equal(captured_body$logprobs, FALSE)
    }
  )
})

testthat::test_that("openai_compare_pair_live constructs generic HTTP error message", {
  td <- trait_description("overall_quality")
  # Response with error status but no body$error object (triggering the else if status >= 400 block)
  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "KEY",
    .openai_req_body_json = function(req, body) req,
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) list(), # Empty body
    .openai_resp_status = function(...) 418L,
    {
      res <- openai_compare_pair_live("A", "t", "B", "t", "gpt-4", td$name, td$description)
      testthat::expect_equal(res$status_code, 418L)
      testthat::expect_equal(res$error_message, "HTTP 418")
    }
  )
})

testthat::test_that("openai_compare_pair_live parses dataframe reasoning summaries", {
  td <- trait_description("overall_quality")

  # output structure where summary is a data.frame
  fake_body <- list(
    object = "response",
    model = "gpt-5.1",
    output = list(
      list(
        type = "reasoning",
        summary = data.frame(text = "DF Summary", stringsAsFactors = FALSE)
      )
    )
  )

  testthat::with_mocked_bindings(
    .openai_api_key = function(...) "KEY",
    .openai_req_body_json = function(req, body) req,
    .openai_req_perform = function(req) structure(list(), class = "fake_resp"),
    .openai_resp_body_json = function(...) fake_body,
    .openai_resp_status = function(...) 200L,
    {
      res <- openai_compare_pair_live("A", "t", "B", "t", "gpt-5.1", td$name, td$description, endpoint = "responses")
      testthat::expect_equal(res$thoughts, "DF Summary")
    }
  )
})

# =====================================================================
# NEW TESTS for submit_openai_pairs_live (List output & Features)
# =====================================================================

testthat::test_that("submit_openai_pairs_live returns valid list structure for zero rows", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  empty_pairs <- tibble::tibble(
    ID1 = character(0), text1 = character(0),
    ID2 = character(0), text2 = character(0)
  )

  res <- submit_openai_pairs_live(
    pairs = empty_pairs,
    model = "gpt-4.1",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl
  )

  # output must be a list with results and failed_pairs
  testthat::expect_type(res, "list")
  testthat::expect_named(res, c("results", "failed_pairs", "failed_attempts"))
  testthat::expect_s3_class(res$results, "tbl_df")
  testthat::expect_s3_class(res$failed_pairs, "tbl_df")
  testthat::expect_equal(nrow(res$results), 0L)
  testthat::expect_equal(nrow(res$failed_pairs), 0L)
})

testthat::test_that("submit_openai_pairs_live handles row-wise execution and returns list", {
  pairs <- tibble::tibble(
    ID1 = c("S01", "S03"),
    text1 = c("Text 1", "Text 3"),
    ID2 = c("S02", "S04"),
    text2 = c("Text 2", "Text 4")
  )
  td <- trait_description("overall_quality")
  pll_ns <- asNamespace("pairwiseLLM")
  on.exit(assign("openai_compare_pair_live", openai_compare_pair_live_orig, envir = pll_ns), add = TRUE)

  # Mock the single-pair function
  fake_result_fn <- function(ID1, ID2, ...) {
    tibble::tibble(
      custom_id = sprintf("LIVE_%s_vs_%s", ID1, ID2),
      ID1 = ID1, ID2 = ID2, model = "gpt-4.1",
      object_type = "chat.completion", status_code = 200L,
      error_message = NA_character_, better_sample = "SAMPLE_1", better_id = ID1,
      prompt_tokens = 10, completion_tokens = 5, total_tokens = 15
    )
  }

  testthat::with_mocked_bindings(
    openai_compare_pair_live = function(ID1, ID2, ...) fake_result_fn(ID1, ID2),
    .env = pll_ns,
    {
      res <- submit_openai_pairs_live(
        pairs = pairs,
        model = "gpt-4.1",
        trait_name = td$name,
        trait_description = td$description,
        verbose = FALSE
      )

      # Check structure
      testthat::expect_type(res, "list")
      testthat::expect_equal(nrow(res$results), 2L)
      testthat::expect_equal(nrow(res$failed_pairs), 0L)
      testthat::expect_equal(res$results$better_id, c("S01", "S03"))
    }
  )
})

testthat::test_that("submit_openai_pairs_live separates failed pairs", {
  pairs <- tibble::tibble(
    ID1 = c("S01", "FailMe"),
    text1 = c("A", "B"),
    ID2 = c("S02", "C"),
    text2 = c("D", "E")
  )
  td <- trait_description("overall_quality")
  pll_ns <- asNamespace("pairwiseLLM")
  on.exit(assign("openai_compare_pair_live", openai_compare_pair_live_orig, envir = pll_ns), add = TRUE)

  # Mock function that fails for the second pair
  testthat::with_mocked_bindings(
    openai_compare_pair_live = function(ID1, ...) {
      if (ID1 == "FailMe") stop("API Error")
      tibble::tibble(
        custom_id = sprintf("LIVE_%s_vs_S02", ID1),
        ID1 = ID1, ID2 = "S02", model = "gpt-4.1",
        status_code = 200L, error_message = NA_character_,
        better_id = ID1
      )
    },
    .env = pll_ns,
    {
      # Run quietly
      res <- submit_openai_pairs_live(
        pairs = pairs, model = "gpt-4.1",
        trait_name = td$name, trait_description = td$description,
        verbose = FALSE
      )

      # Should have 2 results total in the main table (one success, one fail row)
      # BUT verify the failed_pairs extraction
      testthat::expect_equal(nrow(res$results), 1L)
      testthat::expect_equal(nrow(res$failed_pairs), 1L)
      testthat::expect_equal(res$failed_pairs$ID1, "FailMe")
      testthat::expect_match(res$failed_pairs$error_message, "API Error")
    }
  )
})

testthat::test_that("submit_openai_pairs_live respects save_path (Resume Logic)", {
  testthat::skip_if_not_installed("readr")

  td <- trait_description("overall_quality")
  pll_ns <- asNamespace("pairwiseLLM")
  on.exit(assign("openai_compare_pair_live", openai_compare_pair_live_orig, envir = pll_ns), add = TRUE)
  tmp_csv <- tempfile(fileext = ".csv")

  # 1. Create a "fake" existing result file
  # Pair S01 vs S02 is "already done"
  existing_data <- tibble::tibble(
    custom_id = "LIVE_S01_vs_S02",
    ID1 = "S01",
    ID2 = "S02",
    model = "gpt-4.1",
    status_code = 200L,
    error_message = NA_character_,
    better_sample = "SAMPLE_1",
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

  # We count how many times openai_compare_pair_live is called
  counter <- new.env(parent = emptyenv())
  counter$n <- 0L

  testthat::with_mocked_bindings(
    openai_compare_pair_live = function(...) {
      counter$n <- counter$n + 1L
      tibble::tibble(
        custom_id = "LIVE_S03_vs_S04", # Mock return for the new pair
        ID1 = "S03",
        ID2 = "S04",
        model = "gpt-4.1",
        status_code = 200L,
        error_message = NA_character_,
        better_sample = "SAMPLE_1",
        better_id = "S03"
      )
    },
    .env = pll_ns,
    {
      res <- submit_openai_pairs_live(
        pairs = pairs,
        model = "gpt-4.1",
        trait_name = td$name,
        trait_description = td$description,
        save_path = tmp_csv,
        verbose = FALSE
      )

      # 3. Validation
      # Should call API only ONCE (for S03), skipping S01
      testthat::expect_equal(counter$n, 1L)

      # Result should contain BOTH (one from disk, one from new run)
      testthat::expect_equal(nrow(res$results), 2L)
      testthat::expect_setequal(res$results$ID1, c("S01", "S03"))
    }
  )

  unlink(tmp_csv)
})

testthat::test_that("submit_openai_pairs_live validates inputs", {
  td <- trait_description("overall_quality")

  # 1. Missing columns
  bad_pairs <- tibble::tibble(ID1 = "A", text1 = "t")
  testthat::expect_error(
    submit_openai_pairs_live(bad_pairs, "gpt-4", td$name, td$description),
    "must contain columns"
  )

  # 2. Invalid status_every (0 is not positive)
  good_pairs <- tibble::tibble(ID1 = "A", text1 = "t", ID2 = "B", text2 = "t")

  # We expect the error we just added back to the function
  testthat::expect_error(
    submit_openai_pairs_live(
      good_pairs, "gpt-4", td$name, td$description,
      status_every = 0
    ),
    "positive integer"
  )
})

testthat::test_that("submit_openai_pairs_live: Directory creation & Raw response cleanup", {
  testthat::skip_if_not_installed("readr")
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")

  # Use a path in a new subdirectory to test dir.create (Lines 441-442)
  tmp_dir <- tempfile()
  tmp_file <- file.path(tmp_dir, "out.csv")

  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")

  # Mock the comparison to return a result with raw_response
  testthat::with_mocked_bindings(
    openai_compare_pair_live = function(...) {
      res <- tibble::tibble(
        custom_id = "LIVE_A_vs_B", ID1 = "A", ID2 = "B",
        model = "gpt-4.1", status_code = 200, error_message = NA,
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
          res <- submit_openai_pairs_live(
            pairs, "gpt-4.1", td$name, td$description,
            save_path = tmp_file, verbose = TRUE, include_raw = TRUE
          )
        },
        type = "message"
      )

      # Check directory creation message (Line 441)
      testthat::expect_true(any(grepl("Creating output directory", out)))
      testthat::expect_true(dir.exists(tmp_dir))

      # Check verbose timing messages
      testthat::expect_true(any(grepl("Completed 1 pairs", out)))

      # Check that raw_response was removed from the saved file (Line 615 check)
      saved <- readr::read_csv(tmp_file, show_col_types = FALSE)
      testthat::expect_false("raw_response" %in% names(saved))

      # But present in the returned object
      testthat::expect_true("raw_response" %in% names(res$results))
    }
  )
  unlink(tmp_dir, recursive = TRUE)
})

testthat::test_that("submit_openai_pairs_live: Resume logic (Read Error Handling)", {
  testthat::skip_if_not_installed("readr")
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")
  tmp <- tempfile(fileext = ".csv")
  file.create(tmp)

  # Mock read_csv to throw an error, forcing the tryCatch error handler (Line 481)
  testthat::with_mocked_bindings(
    read_csv = function(...) stop("Mock Read Error"),
    .package = "readr",
    {
      testthat::with_mocked_bindings(
        openai_compare_pair_live = function(...) {
          tibble::tibble(
            custom_id = "LIVE_A_vs_B", ID1 = "A", ID2 = "B",
            model = "gpt-4.1", status_code = 200, error_message = NA
          )
        },
        .env = pll_ns,
        {
          testthat::expect_warning(
            submit_openai_pairs_live(
              pairs, "gpt-4.1", td$name, td$description,
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

testthat::test_that("submit_openai_pairs_live: Sequential Save Error Handling", {
  testthat::skip_if_not_installed("readr")
  pll_ns <- asNamespace("pairwiseLLM")
  td <- trait_description("overall_quality")
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")
  tmp_file <- tempfile(fileext = ".csv")

  # Mock write_csv to throw an error (triggering Lines 617-619 warning)
  testthat::with_mocked_bindings(
    write_csv = function(...) stop("Disk full"),
    .package = "readr",
    {
      testthat::with_mocked_bindings(
        openai_compare_pair_live = function(...) {
          tibble::tibble(
            custom_id = "LIVE_A_vs_B", ID1 = "A", ID2 = "B",
            model = "gpt-4.1", status_code = 200, error_message = NA
          )
        },
        .env = pll_ns,
        {
          testthat::expect_warning(
            submit_openai_pairs_live(
              pairs, "gpt-4.1", td$name, td$description,
              save_path = tmp_file, verbose = FALSE
            ),
            "Failed to save incremental result"
          )
        }
      )
    }
  )
})

testthat::test_that("submit_openai_pairs_live: Parallel Execution & Save Error", {
  testthat::skip_if_not_installed("future")
  testthat::skip_if_not_installed("future.apply")
  testthat::skip_if_not_installed("readr")

  td <- trait_description("overall_quality")
  # Use 2 pairs to ensure the loop runs
  pairs <- tibble::tibble(
    ID1 = c("A", "B"), text1 = c("a", "b"),
    ID2 = c("C", "D"), text2 = c("c", "d")
  )
  tmp_file <- tempfile(fileext = ".csv")

  # We force parallel execution.
  # We do NOT mock openai_compare_pair_live, so the workers will execute real code.
  # Since there is no API key/network, they will fail and return error tibbles.
  # This covers the parallel worker tryCatch (Lines 535-555).

  # We mock write_csv in the main process to throw an error (Lines 573-574).
  testthat::with_mocked_bindings(
    write_csv = function(...) stop("Parallel Disk full"),
    .package = "readr",
    {
      capture.output({
        testthat::expect_warning(
          res <- submit_openai_pairs_live(
            pairs,
            model = "gpt-4.1",
            trait_name = td$name,
            trait_description = td$description,
            parallel = TRUE, workers = 2,
            save_path = tmp_file, verbose = TRUE,
            api_key = "FAKE_KEY"
          ),
          "Failed to save incremental results"
        )
      })

      # Verify we got failures from the workers
      testthat::expect_equal(nrow(res$failed_pairs), 2L)
      # The error message comes from the worker tryCatch
      testthat::expect_true(all(grepl("Error", res$failed_pairs$error_message)))
    }
  )
})

testthat::test_that("submit_openai_pairs_live: Parallel Save Strips raw_response", {
  testthat::skip_if_not_installed("future")
  testthat::skip_if_not_installed("future.apply")
  testthat::skip_if_not_installed("readr")

  td <- trait_description("overall_quality")
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")
  tmp_file <- tempfile(fileext = ".csv")

  # Parallel execution with include_raw = TRUE.
  # The workers will fail (fake key), returning a tibble WITH `raw_response`.
  # The main process must strip this column before saving (Line 566).

  testthat::expect_warning(
    submit_openai_pairs_live(
      pairs, "gpt-4.1", td$name, td$description,
      parallel = TRUE, workers = 2,
      save_path = tmp_file, verbose = FALSE,
      include_raw = TRUE,
      api_key = "FAKE_KEY"
    ),
    regexp = NA # Should NOT warn about save failure
  )

  testthat::expect_true(file.exists(tmp_file))
  saved <- readr::read_csv(tmp_file, show_col_types = FALSE)

  # Verify processed (failed) row exists but raw_response is gone
  testthat::expect_equal(nrow(saved), 1L)
  testthat::expect_false("raw_response" %in% names(saved))
})

testthat::test_that("submit_openai_pairs_live: Sequential Internal Error Handling", {
  # Covers the sequential loop tryCatch error handler (Lines 598-609)
  pll_ns <- asNamespace("pairwiseLLM")
  on.exit(assign("openai_compare_pair_live", openai_compare_pair_live_orig, envir = pll_ns), add = TRUE)
  td <- trait_description("overall_quality")
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")

  testthat::with_mocked_bindings(
    openai_compare_pair_live = function(...) stop("Sequential Internal Crash"),
    .env = pll_ns,
    {
      res <- submit_openai_pairs_live(
        pairs, "gpt-4.1", td$name, td$description,
        verbose = FALSE
      )

      testthat::expect_equal(nrow(res$failed_pairs), 1L)
      testthat::expect_match(res$failed_pairs$error_message, "Sequential Internal Crash")
    }
  )
})

testthat::test_that("submit_openai_pairs_live: Resume Verbose Message", {
  testthat::skip_if_not_installed("readr")
  td <- trait_description("overall_quality")
  tmp <- tempfile(fileext = ".csv")

  # Create an existing result file
  existing <- tibble::tibble(
    custom_id = "LIVE_A_vs_B", ID1 = "A", ID2 = "B",
    model = "m", status_code = 200, error_message = NA
  )
  readr::write_csv(existing, tmp)

  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")

  # Run with verbose = TRUE to trigger the messages
  # We do not mock the API here because the function should skip the pair before calling the API
  out <- capture.output({
    res <- submit_openai_pairs_live(
      pairs, "model", td$name, td$description,
      save_path = tmp, verbose = TRUE
    )
  }, type = "message")

  testthat::expect_true(any(grepl("Found existing file", out)))
  testthat::expect_true(any(grepl("Skipping 1 pairs already present", out)))

  # Ensure the result contains the skipped row
  testthat::expect_equal(nrow(res$results), 1L)
  testthat::expect_equal(res$results$custom_id, "LIVE_A_vs_B")

  unlink(tmp)
})
