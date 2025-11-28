# tests/testthat/test-openai_live.R

testthat::test_that("openai_compare_pair_live parses chat.completions correctly", {
  data("example_writing_samples", package = "pairwiseLLM")

  td   <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1   <- "S01"
  ID2   <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  # Fake OpenAI chat.completions-style JSON body
  fake_body <- list(
    object = "chat.completion",
    model  = "gpt-4.1",
    choices = list(list(
      message = list(
        role    = "assistant",
        content = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> Some explanation."
      )
    )),
    usage = list(
      prompt_tokens     = 10L,
      completion_tokens = 5L,
      total_tokens      = 15L
    )
  )

  testthat::with_mocked_bindings(
    .openai_api_key = function(api_key = Sys.getenv("OPENAI_API_KEY")) "FAKEKEY",
    req_body_json   = function(req, body) req,        # passthrough
    req_perform     = function(req) structure(list(), class = "fake_resp"),
    resp_body_json  = function(resp, simplifyVector = FALSE) fake_body,
    resp_status     = function(resp) 200L,
    {
      res <- openai_compare_pair_live(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "gpt-4.1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        endpoint          = "chat.completions",
        temperature       = 0
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

      testthat::expect_equal(res$prompt_tokens,     10)
      testthat::expect_equal(res$completion_tokens, 5)
      testthat::expect_equal(res$total_tokens,      15)
    }
  )
})

testthat::test_that("openai_compare_pair_live parses responses endpoint correctly", {
  td   <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1   <- "S01"
  ID2   <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  # Fake OpenAI /v1/responses-style body with output_text blocks
  fake_body <- list(
    object = "response",
    model  = "gpt-5.1",
    output = list(list(
      content = list(
        list(type = "output_text",
             text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> A "),
        list(type = "output_text",
             text = "B")
      )
    )),
    usage = list(
      input_tokens  = 7L,
      output_tokens = 3L,
      total_tokens  = 10L
    )
  )

  testthat::with_mocked_bindings(
    .openai_api_key = function(api_key = Sys.getenv("OPENAI_API_KEY")) "FAKEKEY",
    req_body_json   = function(req, body) req,
    req_perform     = function(req) structure(list(), class = "fake_resp"),
    resp_body_json  = function(resp, simplifyVector = FALSE) fake_body,
    resp_status     = function(resp) 200L,
    {
      res <- openai_compare_pair_live(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "gpt-5.1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        endpoint          = "responses",
        reasoning         = "none"
      )

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(nrow(res), 1L)

      testthat::expect_equal(res$model, "gpt-5.1")
      testthat::expect_equal(res$object_type, "response")

      # Content concatenates all output_text segments
      testthat::expect_equal(
        res$content,
        "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> A B"
      )

      testthat::expect_equal(res$better_sample, "SAMPLE_2")
      testthat::expect_equal(res$better_id, ID2)

      # usage fields map input/output → prompt/completion
      testthat::expect_equal(res$prompt_tokens,     7)
      testthat::expect_equal(res$completion_tokens, 3)
      testthat::expect_equal(res$total_tokens,      10)
    }
  )
})

testthat::test_that("openai_compare_pair_live returns error row on JSON parse failure", {
  td   <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1   <- "S01"
  ID2   <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  testthat::with_mocked_bindings(
    .openai_api_key = function(api_key = Sys.getenv("OPENAI_API_KEY")) "FAKEKEY",
    req_body_json   = function(req, body) req,
    req_perform     = function(req) structure(list(), class = "fake_resp"),
    # Simulate resp_body_json throwing
    resp_body_json  = function(resp, simplifyVector = FALSE) stop("boom"),
    resp_status     = function(resp) 500L,
    {
      res <- openai_compare_pair_live(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "gpt-4.1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        endpoint          = "chat.completions"
      )

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(nrow(res), 1L)

      testthat::expect_equal(res$status_code, 500L)
      testthat::expect_equal(
        res$error_message,
        "Failed to parse response body as JSON."
      )

      testthat::expect_true(is.na(res$better_sample))
      testthat::expect_true(is.na(res$better_id))
      testthat::expect_true(is.na(res$content))
    }
  )
})

testthat::test_that("openai_compare_pair_live enforces gpt-5.1 + reasoning constraints", {
  td   <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1   <- "S01"
  ID2   <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  # Should error: gpt-5.1 + reasoning != "none" + non-NULL temp/top_p/logprobs
  testthat::expect_error(
    openai_compare_pair_live(
      ID1               = ID1,
      text1             = text1,
      ID2               = ID2,
      text2             = text2,
      model             = "gpt-5.1",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl,
      endpoint          = "responses",
      reasoning         = "low",
      temperature       = 0,
      top_p             = 1,
      logprobs          = NULL
    ),
    regexp = "For gpt-5.1 with reasoning effort not equal to 'none'"
  )

  # Allowed: gpt-5.1 + reasoning = "none" + temp/top_p/logprobs
  # (Mock network so we don't hit the API.)
  fake_body <- list(
    object = "response",
    model  = "gpt-5.1",
    output = list(list(content = list(
      list(type = "output_text", text = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")
    )))
  )

  testthat::with_mocked_bindings(
    .openai_api_key = function(api_key = Sys.getenv("OPENAI_API_KEY")) "FAKEKEY",
    req_body_json   = function(req, body) req,
    req_perform     = function(req) structure(list(), class = "fake_resp"),
    resp_body_json  = function(resp, simplifyVector = FALSE) fake_body,
    resp_status     = function(resp) 200L,
    {
      res <- openai_compare_pair_live(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "gpt-5.1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        endpoint          = "responses",
        reasoning         = "none",
        temperature       = 0,
        top_p             = 1,
        logprobs          = NULL
      )

      testthat::expect_equal(res$better_sample, "SAMPLE_1")
      testthat::expect_equal(res$better_id, ID1)
    }
  )
})

testthat::test_that("openai_compare_pair_live enforces other gpt-5* constraints", {
  td   <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1   <- "S01"
  ID2   <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  # Error: other gpt-5* model with non-NULL temp/top_p/logprobs
  testthat::expect_error(
    openai_compare_pair_live(
      ID1               = ID1,
      text1             = text1,
      ID2               = ID2,
      text2             = text2,
      model             = "gpt-5-mini",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl,
      endpoint          = "responses",
      reasoning         = "low",
      temperature       = 0,
      top_p             = 1,
      logprobs          = NULL
    ),
    regexp = "For gpt-5\\* models other than gpt-5.1"
  )

  # Allowed: other gpt-5* model with temp/top_p/logprobs = NULL
  fake_body <- list(
    object = "response",
    model  = "gpt-5-mini",
    output = list(list(content = list(
      list(type = "output_text", text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>")
    )))
  )

  testthat::with_mocked_bindings(
    .openai_api_key = function(api_key = Sys.getenv("OPENAI_API_KEY")) "FAKEKEY",
    req_body_json   = function(req, body) req,
    req_perform     = function(req) structure(list(), class = "fake_resp"),
    resp_body_json  = function(resp, simplifyVector = FALSE) fake_body,
    resp_status     = function(resp) 200L,
    {
      res <- openai_compare_pair_live(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "gpt-5-mini",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        endpoint          = "responses",
        reasoning         = "low",
        temperature       = NULL,
        top_p             = NULL,
        logprobs          = NULL
      )

      testthat::expect_equal(res$model, "gpt-5-mini")
      testthat::expect_equal(res$better_sample, "SAMPLE_2")
      testthat::expect_equal(res$better_id, ID2)
    }
  )
})

testthat::test_that("submit_openai_pairs_live returns empty tibble for zero rows", {
  td   <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  empty_pairs <- tibble::tibble(
    ID1   = character(0),
    text1 = character(0),
    ID2   = character(0),
    text2 = character(0)
  )

  res <- submit_openai_pairs_live(
    pairs             = empty_pairs,
    model             = "gpt-4.1",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    endpoint          = "chat.completions"
  )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(nrow(res), 0L)
  testthat::expect_true(all(
    c(
      "custom_id", "ID1", "ID2", "model", "object_type",
      "status_code", "error_message", "content",
      "better_sample", "better_id",
      "prompt_tokens", "completion_tokens", "total_tokens"
    ) %in% names(res)
  ))
})

testthat::test_that("submit_openai_pairs_live calls openai_compare_pair_live row-wise", {
  pairs <- tibble::tibble(
    ID1   = c("S01", "S03"),
    text1 = c("Text 1", "Text 3"),
    ID2   = c("S02", "S04"),
    text2 = c("Text 2", "Text 4")
  )

  td   <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  calls <- list()

  fake_result <- function(ID1, ID2, chosen) {
    tibble::tibble(
      custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
      ID1               = ID1,
      ID2               = ID2,
      model             = "gpt-4.1",
      object_type       = "chat.completion",
      status_code       = 200L,
      error_message     = NA_character_,
      content           = sprintf("<BETTER_SAMPLE>%s</BETTER_SAMPLE>", chosen),
      better_sample     = if (chosen == "SAMPLE_1") "SAMPLE_1" else "SAMPLE_2",
      better_id         = if (chosen == "SAMPLE_1") ID1 else ID2,
      prompt_tokens     = 10,
      completion_tokens = 5,
      total_tokens      = 15
    )
  }

  testthat::with_mocked_bindings(
    openai_compare_pair_live = function(
    ID1, text1, ID2, text2, model, trait_name,
    trait_description, prompt_template, endpoint, ...
    ) {
      # record the call
      calls <<- append(calls, list(
        list(ID1 = ID1, ID2 = ID2, endpoint = endpoint, model = model)
      ))
      # alternately pick SAMPLE_1 / SAMPLE_2
      if (ID1 == "S01") {
        fake_result(ID1, ID2, "SAMPLE_1")
      } else {
        fake_result(ID1, ID2, "SAMPLE_2")
      }
    },
    {
      res <- submit_openai_pairs_live(
        pairs             = pairs,
        model             = "gpt-4.1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        endpoint          = "chat.completions"
      )

      testthat::expect_equal(length(calls), 2L)
      testthat::expect_equal(calls[[1]]$ID1, "S01")
      testthat::expect_equal(calls[[1]]$ID2, "S02")
      testthat::expect_equal(calls[[1]]$endpoint, "chat.completions")

      testthat::expect_equal(calls[[2]]$ID1, "S03")
      testthat::expect_equal(calls[[2]]$ID2, "S04")

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(nrow(res), 2L)

      testthat::expect_equal(res$better_id, c("S01", "S04"))
    }
  )
})

testthat::test_that("submit_openai_pairs_live errors on missing columns", {
  td   <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  bad_pairs <- tibble::tibble(
    ID1 = "S01",
    ID2 = "S02"
    # missing text1/text2
  )

  testthat::expect_error(
    submit_openai_pairs_live(
      pairs             = bad_pairs,
      model             = "gpt-4.1",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl,
      endpoint          = "chat.completions"
    ),
    regexp = "`pairs` must contain columns"
  )
})
