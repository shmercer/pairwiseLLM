# =====================================================================
# test-openai_live.R
# Tests for openai_compare_pair_live() and submit_openai_pairs_live()
# =====================================================================

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

testthat::test_that("submit_openai_pairs_live returns empty tibble for zero rows", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  empty_pairs <- tibble::tibble(
    ID1 = character(0),
    text1 = character(0),
    ID2 = character(0),
    text2 = character(0)
  )
  res <- submit_openai_pairs_live(
    pairs = empty_pairs,
    model = "gpt-4.1",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    endpoint = "chat.completions"
  )
  testthat::expect_equal(nrow(res), 0L)
  testthat::expect_true("thoughts" %in% names(res))
  testthat::expect_false("raw_response" %in% names(res))
})

# ---------------------------------------------------------------------

testthat::test_that("submit_openai_pairs_live with include_raw=TRUE returns raw_response column", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  empty_pairs <- tibble::tibble(
    ID1 = character(0),
    text1 = character(0),
    ID2 = character(0),
    text2 = character(0)
  )
  res <- submit_openai_pairs_live(
    pairs = empty_pairs,
    model = "gpt-4.1",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    endpoint = "chat.completions",
    include_raw = TRUE
  )
  testthat::expect_equal(nrow(res), 0L)
  testthat::expect_true("thoughts" %in% names(res))
  testthat::expect_true("raw_response" %in% names(res))
  testthat::expect_type(res$raw_response, "list")
})

# ---------------------------------------------------------------------

testthat::test_that("submit_openai_pairs_live calls openai_compare_pair_live row-wise", {
  pairs <- tibble::tibble(
    ID1 = c("S01", "S03"),
    text1 = c("Text 1", "Text 3"),
    ID2 = c("S02", "S04"),
    text2 = c("Text 2", "Text 4")
  )
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  calls <- list()

  fake_result <- function(ID1, ID2, chosen) {
    tibble::tibble(
      custom_id = sprintf("LIVE_%s_vs_%s", ID1, ID2),
      ID1 = ID1,
      ID2 = ID2,
      model = "gpt-4.1",
      object_type = "chat.completion",
      status_code = 200L,
      error_message = NA_character_,
      content = sprintf("<BETTER_SAMPLE>%s</BETTER_SAMPLE>", chosen),
      better_sample = chosen,
      better_id = if (chosen == "SAMPLE_1") ID1 else ID2,
      prompt_tokens = 10,
      completion_tokens = 5,
      total_tokens = 15
    )
  }

  testthat::with_mocked_bindings(
    openai_compare_pair_live = function(ID1, text1, ID2, text2, model, trait_name,
                                        trait_description, prompt_template, endpoint, api_key,
                                        include_raw, ...) {
      calls <<- append(calls, list(list(ID1 = ID1, ID2 = ID2)))
      if (ID1 == "S01") {
        fake_result(ID1, ID2, "SAMPLE_1")
      } else {
        fake_result(ID1, ID2, "SAMPLE_2")
      }
    },
    {
      res <- submit_openai_pairs_live(
        pairs = pairs,
        model = "gpt-4.1",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        endpoint = "chat.completions",
        include_raw = FALSE,
        verbose = FALSE,
        progress = FALSE
      )
      testthat::expect_equal(length(calls), 2L)
      testthat::expect_equal(res$better_id, c("S01", "S04"))
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

testthat::test_that("submit_openai_pairs_live validates inputs", {
  td <- trait_description("overall_quality")
  # Missing columns
  bad_pairs <- tibble::tibble(ID1 = "A", text1 = "t")
  testthat::expect_error(
    submit_openai_pairs_live(bad_pairs, "gpt-4", td$name, td$description),
    "must contain columns"
  )
  # Invalid status_every
  good_pairs <- tibble::tibble(ID1 = "A", text1 = "t", ID2 = "B", text2 = "t")
  testthat::expect_error(
    submit_openai_pairs_live(
      good_pairs, "gpt-4", td$name, td$description,
      status_every = 0
    ),
    "status_every` must be a single positive integer"
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
      res <- openai_compare_pair_live("A", "t", "B", "t", "gpt-5.1", td$name, td$description, endpoint="responses")
      testthat::expect_equal(res$thoughts, "DF Summary")
    }
  )
})
