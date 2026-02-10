# =====================================================================
# test-openai_batch.R
# Tests for build_openai_batch_requests() and related batch helpers
# =====================================================================

trait_description <- pairwiseLLM:::trait_description
set_prompt_template <- pairwiseLLM:::set_prompt_template
make_pairs <- pairwiseLLM::make_pairs
build_openai_batch_requests <- pairwiseLLM:::build_openai_batch_requests
write_openai_batch_file <- pairwiseLLM:::write_openai_batch_file
parse_openai_batch_output <- pairwiseLLM:::parse_openai_batch_output
build_prompt <- pairwiseLLM:::build_prompt

testthat::test_that("build_openai_batch_requests builds valid chat.completions JSONL objects", {
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)
  pairs <- pairs[1:2, ]
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  batch <- build_openai_batch_requests(
    pairs = pairs,
    model = "gpt-4.1",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    endpoint = "chat.completions",
    temperature = 0,
    top_p = 1,
    logprobs = NULL
  )
  testthat::expect_s3_class(batch, "tbl_df")
  testthat::expect_equal(nrow(batch), 2L)
  testthat::expect_true(all(c("custom_id", "method", "url", "body") %in% names(batch)))
  # Body structure check
  b1 <- batch$body[[1]]
  testthat::expect_equal(b1$model, "gpt-4.1")
  testthat::expect_true(is.list(b1$messages))
  roles <- vapply(b1$messages, function(m) m[["role"]], character(1))
  testthat::expect_true(any(roles == "user"))
})

testthat::test_that("write_openai_batch_file writes JSONL file", {
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)
  pairs <- pairs[1:2, ]
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  batch <- build_openai_batch_requests(
    pairs = pairs,
    model = "gpt-4.1",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    endpoint = "chat.completions"
  )
  tmp <- tempfile("openai-batch-", fileext = ".jsonl")
  write_openai_batch_file(batch, tmp)
  testthat::expect_true(file.exists(tmp))
  lines <- readLines(tmp, warn = FALSE)
  testthat::expect_equal(length(lines), nrow(batch))
  # Each line should be valid JSON with required top-level keys
  objs <- lapply(lines, jsonlite::fromJSON)
  keys <- lapply(objs, names)
  testthat::expect_true(all(vapply(keys, function(k) {
    all(c("custom_id", "method", "url", "body") %in% k)
  }, logical(1))))
})

testthat::test_that("build_openai_batch_requests supports gpt-5.1 with reasoning = 'none' on responses", {
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)
  pairs <- pairs[1:1, ]
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  # For gpt-5.1 + reasoning = "none", temperature/top_p/logprobs are allowed
  batch <- build_openai_batch_requests(
    pairs = pairs,
    model = "gpt-5.1",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    endpoint = "responses",
    reasoning = "none",
    temperature = 0,
    top_p = 1,
    logprobs = NULL
  )
  testthat::expect_s3_class(batch, "tbl_df")
  testthat::expect_equal(nrow(batch), 1L)
  b1 <- batch$body[[1]]
  testthat::expect_equal(b1$model, "gpt-5.1")
  testthat::expect_equal(b1$input, build_prompt(
    template = tmpl,
    trait_name = td$name,
    trait_desc = td$description,
    text1 = pairs$text1[1],
    text2 = pairs$text2[1]
  ))
  # reasoning should be present with effort = "none"
  testthat::expect_true("reasoning" %in% names(b1) || is.null(b1$reasoning) ||
    identical(b1$reasoning$effort, "none"))
})

testthat::test_that("build_openai_batch_requests errors for gpt-5.1 + reasoning != 'none' with temp/top_p/logprobs", {
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)
  pairs <- pairs[1:1, ]
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  testthat::expect_error(
    build_openai_batch_requests(
      pairs = pairs,
      model = "gpt-5.1",
      trait_name = td$name,
      trait_description = td$description,
      prompt_template = tmpl,
      endpoint = "responses",
      reasoning = "low", # <- not 'none'
      temperature = 0,
      top_p = 1,
      logprobs = NULL
    ),
    regexp = "For gpt-5.1/5.2 with reasoning"
  )
})

testthat::test_that("build_openai_batch_requests drops temp for gpt-5 base with reasoning", {
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)
  pairs <- pairs[1:1, ]
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  batch <- build_openai_batch_requests(
    pairs = pairs,
    model = "gpt-5-mini",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    endpoint = "responses",
    reasoning = "low",
    temperature = NULL,
    top_p = NULL,
    logprobs = NULL
  )
  testthat::expect_s3_class(batch, "tbl_df")
  testthat::expect_equal(nrow(batch), 1L)
  # Verify temperature is omitted
  testthat::expect_false("temperature" %in% names(batch$body[[1]]))
})

testthat::test_that("parse_openai_batch_output collects thoughts and message text separately for responses", {
  tmp <- tempfile(fileext = ".jsonl")
  on.exit(unlink(tmp), add = TRUE)
  # Construct a fake batch output line similar to gpt-5.1 responses
  line_obj <- list(
    custom_id = "LIVE_S01_vs_S02",
    response = list(
      status_code = 200L,
      body = list(
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
    ),
    error = NULL
  )
  json_line <- jsonlite::toJSON(line_obj, auto_unbox = TRUE)
  writeLines(json_line, con = tmp, useBytes = TRUE)
  res <- parse_openai_batch_output(tmp)
  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(nrow(res), 1L)
  # IDs from custom_id
  testthat::expect_equal(res$custom_id, "LIVE_S01_vs_S02")
  testthat::expect_equal(res$ID1, "S01")
  testthat::expect_equal(res$ID2, "S02")
  # Basic metadata
  testthat::expect_equal(res$model, "gpt-5.1")
  testthat::expect_equal(res$object_type, "response")
  testthat::expect_equal(res$status_code, 200L)
  testthat::expect_true(is.na(res$error_message))
  # Reasoning summary should go to thoughts
  testthat::expect_equal(res$thoughts, "Reasoning summary. ")
  # Content should be assistant message only
  testthat::expect_equal(
    res$content,
    "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> Final answer."
  )
  # Tag parsing and better_id mapping
  testthat::expect_equal(res$better_sample, "SAMPLE_2")
  testthat::expect_equal(res$better_id, "S02")
  # Token usage
  testthat::expect_equal(res$prompt_tokens, 10)
  testthat::expect_equal(res$completion_tokens, 5)
  testthat::expect_equal(res$total_tokens, 15)
})

testthat::test_that("build_openai_batch_requests adds reasoning summary when include_thoughts = TRUE", {
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)
  pairs <- pairs[1:1, ]
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  # include_thoughts = TRUE, reasoning != "none" -> summary = "auto"
  batch <- build_openai_batch_requests(
    pairs = pairs,
    model = "gpt-5.1",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    endpoint = "responses",
    reasoning = "low",
    include_thoughts = TRUE
  )
  testthat::expect_s3_class(batch, "tbl_df")
  testthat::expect_equal(nrow(batch), 1L)
  b1 <- batch$body[[1]]
  testthat::expect_equal(b1$model, "gpt-5.1")
  testthat::expect_true("reasoning" %in% names(b1))
  testthat::expect_equal(b1$reasoning$effort, "low")
  testthat::expect_equal(b1$reasoning$summary, "auto")
  # include_thoughts = TRUE but reasoning = "none" -> summary included
  batch_none <- build_openai_batch_requests(
    pairs = pairs,
    model = "gpt-5.1",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    endpoint = "responses",
    reasoning = "none",
    include_thoughts = TRUE
  )
  b2 <- batch_none$body[[1]]
  testthat::expect_true("reasoning" %in% names(b2))
  testthat::expect_equal(b2$reasoning$effort, "none")
  testthat::expect_equal(b2$reasoning$summary, "auto")
})

testthat::test_that("build_openai_batch_requests handles empty pairs tibble", {
  # Covers the n == 0L check
  empty_pairs <- tibble::tibble(
    ID1 = character(), text1 = character(),
    ID2 = character(), text2 = character()
  )
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  batch <- build_openai_batch_requests(
    pairs = empty_pairs,
    model = "gpt-4.1",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl
  )

  testthat::expect_s3_class(batch, "tbl_df")
  testthat::expect_equal(nrow(batch), 0L)
  testthat::expect_named(batch, c("custom_id", "method", "url", "body"))
})

testthat::test_that("build_openai_batch_requests warns if include_thoughts=TRUE for non-reasoning model", {
  # Covers the warning block when is_reasoning_model is FALSE but include_thoughts is TRUE
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)[1:1, ]
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  testthat::expect_warning(
    build_openai_batch_requests(
      pairs = pairs,
      model = "gpt-4o", # Not a reasoning model
      trait_name = td$name,
      trait_description = td$description,
      prompt_template = tmpl,
      endpoint = "responses",
      include_thoughts = TRUE
    ),
    "include_thoughts requested for non-reasoning model"
  )
})

testthat::test_that("build_openai_batch_requests passes top_p and logprobs to body", {
  # Covers the lines adding optional parameters to the body list for both endpoints
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)[1:1, ]
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # 1. Chat Completions
  batch_chat <- build_openai_batch_requests(
    pairs = pairs,
    model = "gpt-4.1",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    endpoint = "chat.completions",
    temperature = 0.5,
    top_p = 0.9,
    logprobs = TRUE
  )
  body_chat <- batch_chat$body[[1]]
  testthat::expect_equal(body_chat$temperature, 0.5)
  testthat::expect_equal(body_chat$top_p, 0.9)
  testthat::expect_equal(body_chat$logprobs, TRUE)

  # 2. Responses
  batch_resp <- build_openai_batch_requests(
    pairs = pairs,
    model = "gpt-4.1",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    endpoint = "responses",
    temperature = 0.7,
    top_p = 0.8,
    logprobs = FALSE
  )
  body_resp <- batch_resp$body[[1]]
  testthat::expect_equal(body_resp$temperature, 0.7)
  testthat::expect_equal(body_resp$top_p, 0.8)
  testthat::expect_equal(body_resp$logprobs, FALSE)
})

# ---------------------------------------------------------------------
# Coverage improvements
# ---------------------------------------------------------------------

testthat::test_that("build_openai_batch_requests handles empty pairs tibble", {
  # Covers the n == 0L check
  empty_pairs <- tibble::tibble(
    ID1 = character(), text1 = character(),
    ID2 = character(), text2 = character()
  )
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  batch <- build_openai_batch_requests(
    pairs = empty_pairs,
    model = "gpt-4.1",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl
  )

  testthat::expect_s3_class(batch, "tbl_df")
  testthat::expect_equal(nrow(batch), 0L)
  testthat::expect_named(batch, c("custom_id", "method", "url", "body"))
})

testthat::test_that("build_openai_batch_requests warns if include_thoughts=TRUE for non-reasoning model", {
  # Covers the warning block when is_reasoning_model is FALSE but include_thoughts is TRUE
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)[1:1, ]
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  testthat::expect_warning(
    build_openai_batch_requests(
      pairs = pairs,
      model = "gpt-4o", # Not a reasoning model
      trait_name = td$name,
      trait_description = td$description,
      prompt_template = tmpl,
      endpoint = "responses",
      include_thoughts = TRUE
    ),
    "include_thoughts requested for non-reasoning model"
  )
})

testthat::test_that("build_openai_batch_requests passes top_p and logprobs to body", {
  # Covers the lines adding optional parameters to the body list for both endpoints
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)[1:1, ]
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # 1. Chat Completions
  batch_chat <- build_openai_batch_requests(
    pairs = pairs,
    model = "gpt-4.1",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    endpoint = "chat.completions",
    temperature = 0.5,
    top_p = 0.9,
    logprobs = TRUE
  )
  body_chat <- batch_chat$body[[1]]
  testthat::expect_equal(body_chat$temperature, 0.5)
  testthat::expect_equal(body_chat$top_p, 0.9)
  testthat::expect_equal(body_chat$logprobs, TRUE)

  # 2. Responses
  batch_resp <- build_openai_batch_requests(
    pairs = pairs,
    model = "gpt-4.1",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    endpoint = "responses",
    temperature = 0.7,
    top_p = 0.8,
    logprobs = FALSE
  )
  body_resp <- batch_resp$body[[1]]
  testthat::expect_equal(body_resp$temperature, 0.7)
  testthat::expect_equal(body_resp$top_p, 0.8)
  testthat::expect_equal(body_resp$logprobs, FALSE)
})
