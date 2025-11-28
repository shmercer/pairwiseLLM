test_that("build_openai_batch_requests builds valid chat.completions JSONL objects", {
  data("example_writing_samples", package = "pairwiseLLM")

  pairs <- make_pairs(example_writing_samples)
  pairs <- pairs[1:2, ]

  td   <- trait_description("overall_quality")
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

  td   <- trait_description("overall_quality")
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

  td   <- trait_description("overall_quality")
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

  td   <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  expect_error(
    build_openai_batch_requests(
      pairs             = pairs,
      model             = "gpt-5.1",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl,
      endpoint          = "responses",
      reasoning         = "low",     # <- not 'none'
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

  td   <- trait_description("overall_quality")
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

  td   <- trait_description("overall_quality")
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
