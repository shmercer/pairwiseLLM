test_that("build_openai_batch_requests builds valid chat.completions JSONL objects", {
  data("example_writing_samples", package = "pairwiseLLM")

  pairs <- make_pairs(example_writing_samples)
  pairs <- pairs[1:2, ]

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  batch <- build_openai_batch_requests(
    pairs             = pairs,
    model             = "gpt-4.1",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    endpoint          = "chat.completions",
    temperature       = 0,
    top_p             = 1,
    logprobs          = NULL
  )

  expect_s3_class(batch, "tbl_df")
  expect_equal(nrow(batch), 2L)
  expect_true(all(c("custom_id", "method", "url", "body") %in% names(batch)))

  # Body structure check
  b1 <- batch$body[[1]]
  expect_equal(b1$model, "gpt-4.1")
  expect_true(is.list(b1$messages))
  roles <- vapply(b1$messages, function(m) m[["role"]], character(1))
  expect_true(any(roles == "user"))
})

test_that("write_openai_batch_file writes JSONL file", {
  data("example_writing_samples", package = "pairwiseLLM")

  pairs <- make_pairs(example_writing_samples)
  pairs <- pairs[1:2, ]

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  batch <- build_openai_batch_requests(
    pairs             = pairs,
    model             = "gpt-4.1",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    endpoint          = "chat.completions"
  )

  tmp <- tempfile("openai-batch-", fileext = ".jsonl")
  write_openai_batch_file(batch, tmp)

  expect_true(file.exists(tmp))

  lines <- readLines(tmp, warn = FALSE)
  expect_equal(length(lines), nrow(batch))

  # Each line should be valid JSON with required top-level keys
  objs <- lapply(lines, jsonlite::fromJSON)
  keys <- lapply(objs, names)
  expect_true(all(vapply(keys, function(k) all(c("custom_id", "method", "url", "body") %in% k), logical(1))))
})

test_that("build_openai_batch_requests supports gpt-5.1 with reasoning = 'none' on responses", {
  data("example_writing_samples", package = "pairwiseLLM")

  pairs <- make_pairs(example_writing_samples)
  pairs <- pairs[1:1, ]

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # For gpt-5.1 + reasoning = "none", temperature/top_p/logprobs are allowed
  batch <- build_openai_batch_requests(
    pairs             = pairs,
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

  expect_s3_class(batch, "tbl_df")
  expect_equal(nrow(batch), 1L)

  b1 <- batch$body[[1]]
  expect_equal(b1$model, "gpt-5.1")
  expect_equal(b1$input, build_prompt(
    template   = tmpl,
    trait_name = td$name,
    trait_desc = td$description,
    text1      = pairs$text1[1],
    text2      = pairs$text2[1]
  ))
  # reasoning should be present with effort = "none"
  expect_true("reasoning" %in% names(b1) || is.null(b1$reasoning) || identical(b1$reasoning$effort, "none"))
})

test_that("build_openai_batch_requests errors for gpt-5.1 + reasoning != 'none' with temp/top_p/logprobs", {
  data("example_writing_samples", package = "pairwiseLLM")

  pairs <- make_pairs(example_writing_samples)
  pairs <- pairs[1:1, ]

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  expect_error(
    build_openai_batch_requests(
      pairs             = pairs,
      model             = "gpt-5.1",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl,
      endpoint          = "responses",
      reasoning         = "low", # <- not 'none'
      temperature       = 0,
      top_p             = 1,
      logprobs          = NULL
    ),
    regexp = "For gpt-5.1 with reasoning effort not equal to 'none'"
  )
})

test_that("build_openai_batch_requests errors for other gpt-5* models when temp/top_p/logprobs are non-NULL", {
  data("example_writing_samples", package = "pairwiseLLM")

  pairs <- make_pairs(example_writing_samples)
  pairs <- pairs[1:1, ]

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # For other gpt-5* models (e.g., gpt-5-mini), temp/top_p/logprobs must be NULL
  expect_error(
    build_openai_batch_requests(
      pairs             = pairs,
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
})

test_that("build_openai_batch_requests allows other gpt-5* models with temp/top_p/logprobs = NULL", {
  data("example_writing_samples", package = "pairwiseLLM")

  pairs <- make_pairs(example_writing_samples)
  pairs <- pairs[1:1, ]

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  batch <- build_openai_batch_requests(
    pairs             = pairs,
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

  expect_s3_class(batch, "tbl_df")
  expect_equal(nrow(batch), 1L)
  expect_equal(batch$body[[1]]$model, "gpt-5-mini")
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
          effort  = "low",
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
          input_tokens  = 10L,
          output_tokens = 5L,
          total_tokens  = 15L
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

test_that("build_openai_batch_requests adds reasoning summary when include_thoughts = TRUE", {
  data("example_writing_samples", package = "pairwiseLLM")

  pairs <- make_pairs(example_writing_samples)
  pairs <- pairs[1:1, ]

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # include_thoughts = TRUE, reasoning != "none" -> summary = "auto"
  batch <- build_openai_batch_requests(
    pairs             = pairs,
    model             = "gpt-5.1",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    endpoint          = "responses",
    reasoning         = "low",
    include_thoughts  = TRUE
  )

  expect_s3_class(batch, "tbl_df")
  expect_equal(nrow(batch), 1L)

  b1 <- batch$body[[1]]
  expect_equal(b1$model, "gpt-5.1")
  expect_true("reasoning" %in% names(b1))
  expect_equal(b1$reasoning$effort, "low")
  expect_equal(b1$reasoning$summary, "auto")

  # include_thoughts = TRUE but reasoning = "none" -> no summary field
  batch_none <- build_openai_batch_requests(
    pairs             = pairs,
    model             = "gpt-5.1",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    endpoint          = "responses",
    reasoning         = "none",
    include_thoughts  = TRUE
  )

  b2 <- batch_none$body[[1]]
  expect_true("reasoning" %in% names(b2))
  expect_equal(b2$reasoning$effort, "none")
  expect_false("summary" %in% names(b2$reasoning))
})
