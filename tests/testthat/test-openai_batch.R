test_that("build_openai_batch_requests builds valid chat.completions JSONL objects", {
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)

  # Simple template with required placeholders
  tmpl  <- "Evaluate {TRAIT_DESCRIPTION}.\n\nSample 1:\n{SAMPLE_1}\n\nSample 2:\n{SAMPLE_2}"
  trait <- "overall writing quality"

  batch_tbl <- build_openai_batch_requests(
    pairs             = pairs[1:2, ],
    model             = "gpt-4.1",
    trait_description = trait,
    prompt_template   = tmpl,
    endpoint          = "chat.completions",
    temperature       = 0,
    top_p             = 1,
    logprobs          = NULL
  )

  expect_s3_class(batch_tbl, "tbl_df")
  expect_true(all(c("ID1", "ID2", "text1", "text2", "custom_id", "jsonl") %in% names(batch_tbl)))
  expect_equal(nrow(batch_tbl), 2L)

  # JSON parses and has required fields
  obj1 <- jsonlite::fromJSON(batch_tbl$jsonl[1], simplifyVector = TRUE)
  expect_equal(obj1$method, "POST")
  expect_equal(obj1$url, "/v1/chat/completions")
  expect_equal(obj1$body$model, "gpt-4.1")

  # messages structure
  expect_true("messages" %in% names(obj1$body))
  msgs <- obj1$body$messages

  # Handle both data.frame and list-of-lists cases
  if (is.data.frame(msgs)) {
    roles <- msgs$role
  } else {
    roles <- vapply(msgs, function(m) m$role, character(1))
  }

  expect_true("user" %in% roles)

})

test_that("write_openai_batch_file writes JSONL file", {
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)

  tmpl  <- "Evaluate {TRAIT_DESCRIPTION}.\n\nSample 1:\n{SAMPLE_1}\n\nSample 2:\n{SAMPLE_2}"
  trait <- "overall writing quality"

  batch_tbl <- build_openai_batch_requests(
    pairs             = pairs[1:2, ],
    model             = "gpt-4.1",
    trait_description = trait,
    prompt_template   = tmpl,
    endpoint          = "chat.completions"
  )

  tmp <- tempfile(fileext = ".jsonl")
  write_openai_batch_file(batch_tbl, tmp)

  lines <- readLines(tmp, warn = FALSE)
  expect_equal(length(lines), nrow(batch_tbl))

  # Each line should parse as JSON
  obj <- jsonlite::fromJSON(lines[1], simplifyVector = TRUE)
  expect_true(is.list(obj))
})

test_that("build_openai_batch_requests supports gpt-5.1 with reasoning = 'none' on responses", {
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)

  tmpl  <- "Evaluate {TRAIT_DESCRIPTION}.\n\nSample 1:\n{SAMPLE_1}\n\nSample 2:\n{SAMPLE_2}"
  trait <- "overall writing quality"

  batch_tbl <- build_openai_batch_requests(
    pairs             = pairs[1:1, ],
    model             = "gpt-5.1",
    trait_description = trait,
    prompt_template   = tmpl,
    endpoint          = "responses",
    reasoning_effort  = "none",
    temperature       = 0,
    top_p             = 1,
    logprobs          = NULL
  )

  obj <- jsonlite::fromJSON(batch_tbl$jsonl[1], simplifyVector = TRUE)

  expect_equal(obj$url, "/v1/responses")
  expect_equal(obj$body$model, "gpt-5.1")
  expect_equal(obj$body$reasoning$effort, "none")
  expect_equal(obj$body$temperature, 0)
  expect_equal(obj$body$top_p, 1)
})

test_that("build_openai_batch_requests errors for gpt-5.1 + reasoning != 'none' with temp/top_p/logprobs", {
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)

  tmpl  <- "Evaluate {TRAIT_DESCRIPTION}.\n\nSample 1:\n{SAMPLE_1}\n\nSample 2:\n{SAMPLE_2}"
  trait <- "overall writing quality"

  expect_error(
    build_openai_batch_requests(
      pairs             = pairs[1:1, ],
      model             = "gpt-5.1",
      trait_description = trait,
      prompt_template   = tmpl,
      endpoint          = "responses",
      reasoning_effort  = "low",
      temperature       = 0,      # illegal combo
      top_p             = 1,
      logprobs          = NULL
    ),
    "are not supported",
    ignore.case = TRUE
  )
})

test_that("build_openai_batch_requests errors for other gpt-5* models when temp/top_p/logprobs are non-NULL", {
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)

  tmpl  <- "Evaluate {TRAIT_DESCRIPTION}.\n\nSample 1:\n{SAMPLE_1}\n\nSample 2:\n{SAMPLE_2}"
  trait <- "overall writing quality"

  # e.g., gpt-5-mini
  expect_error(
    build_openai_batch_requests(
      pairs             = pairs[1:1, ],
      model             = "gpt-5-mini",
      trait_description = trait,
      prompt_template   = tmpl,
      endpoint          = "responses",
      reasoning_effort  = "low",
      temperature       = 0,      # not allowed
      top_p             = 1,
      logprobs          = NULL
    ),
    "not supported",
    ignore.case = TRUE
  )
})

test_that("build_openai_batch_requests allows other gpt-5* models with temp/top_p/logprobs = NULL", {
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)

  tmpl  <- "Evaluate {TRAIT_DESCRIPTION}.\n\nSample 1:\n{SAMPLE_1}\n\nSample 2:\n{SAMPLE_2}"
  trait <- "overall writing quality"

  batch_tbl <- build_openai_batch_requests(
    pairs             = pairs[1:1, ],
    model             = "gpt-5-mini",
    trait_description = trait,
    prompt_template   = tmpl,
    endpoint          = "responses",
    reasoning_effort  = "low",
    temperature       = NULL,
    top_p             = NULL,
    logprobs          = NULL
  )

  obj <- jsonlite::fromJSON(batch_tbl$jsonl[1], simplifyVector = TRUE)

  expect_equal(obj$url, "/v1/responses")
  expect_equal(obj$body$model, "gpt-5-mini")

  # No temperature/top_p/logprobs fields in body
  expect_false("temperature" %in% names(obj$body))
  expect_false("top_p" %in% names(obj$body))
  expect_false("logprobs" %in% names(obj$body))
})
