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

testthat::test_that("Batch storage controls preserve booleans in every serialized request", {
  pairs <- tibble::tibble(ID1 = c("A", "C"), text1 = "First", ID2 = c("B", "D"), text2 = "Second")
  path <- file.path(withr::local_tempdir(), "requests.jsonl")
  for (endpoint in c("chat.completions", "responses")) {
    for (model in c("gpt-4.1", "gpt-5.6-terra")) {
      args <- list(pairs = pairs, model = model, trait_name = "Cohesion",
                   trait_description = "Connections", endpoint = endpoint)
      baseline <- do.call(build_openai_batch_requests, args)
      for (extra in list(list(), list(store = NULL), list(store = FALSE), list(store = TRUE))) {
        batch <- do.call(build_openai_batch_requests, c(args, extra))
        write_openai_batch_file(batch, path)
        lines <- readLines(path)
        testthat::expect_length(lines, 2L)
        for (i in seq_along(lines)) {
          obj <- jsonlite::fromJSON(lines[i], simplifyVector = FALSE)
          testthat::expect_identical(obj$custom_id, baseline$custom_id[i])
          testthat::expect_identical(obj$body$store, extra$store)
          testthat::expect_identical("store" %in% names(obj$body), !is.null(extra$store))
          body <- batch$body[[i]]
          body$store <- NULL
          testthat::expect_identical(body, baseline$body[[i]])
        }
      }
    }
  }
})

testthat::test_that("Batch output limits serialize with reasoning and sampling intact", {
  pairs <- tibble::tibble(ID1 = c("A", "C"), text1 = "First", ID2 = c("B", "D"), text2 = "Second")
  path <- file.path(withr::local_tempdir(), "requests.jsonl")
  args <- list(pairs = pairs, model = "gpt-5.6-terra", trait_name = "Cohesion",
               trait_description = "Connections", endpoint = "responses", reasoning = "none",
               temperature = 0, top_p = 1, logprobs = TRUE, store = FALSE)
  baseline <- do.call(build_openai_batch_requests, args)
  testthat::expect_identical(
    do.call(build_openai_batch_requests, c(args, list(max_output_tokens = NULL))), baseline
  )
  for (limit in c(1, 64, .Machine$integer.max)) {
    batch <- do.call(build_openai_batch_requests, c(args, list(max_output_tokens = limit)))
    write_openai_batch_file(batch, path)
    objects <- lapply(readLines(path), jsonlite::fromJSON)
    for (i in seq_along(objects)) {
      testthat::expect_identical(objects[[i]]$body$max_output_tokens, as.integer(limit))
      testthat::expect_identical(objects[[i]]$body$reasoning$effort, "none")
      testthat::expect_identical(objects[[i]]$body$store, FALSE)
      body <- batch$body[[i]]
      body$max_output_tokens <- NULL
      testthat::expect_identical(body, baseline$body[[i]])
    }
  }
  positional <- build_openai_batch_requests(
    pairs, "gpt-4.1", "Cohesion", "Connections", set_prompt_template(),
    "responses", NULL, NULL, NULL, NULL, FALSE, "OLD"
  )
  testthat::expect_identical(positional$custom_id, c("OLD_A_vs_B", "OLD_C_vs_D"))
  testthat::expect_false(any(c("store", "max_output_tokens") %in% names(positional$body[[1]])))
})

testthat::test_that("Batch controls reject invalid values before empty-pair handling", {
  pairs <- tibble::tibble(ID1 = "A", text1 = "First", ID2 = "B", text2 = "Second")
  for (n in 0:1) {
    args <- list(pairs = pairs[seq_len(n), ], model = "gpt-4.1", trait_name = "Cohesion",
                 trait_description = "Connections", endpoint = "responses")
    for (value in list(NA, 0, "false", logical(), c(TRUE, FALSE), matrix(TRUE), list(FALSE))) {
      testthat::expect_error(do.call(build_openai_batch_requests, c(args, list(store = value))),
                            "`store` must be TRUE, FALSE, or NULL.", fixed = TRUE)
    }
    for (value in list(NA_real_, NaN, Inf, -Inf, 0, -1, 1.5, "64", TRUE, numeric(),
                       c(1, 2), matrix(64), list(64), .Machine$integer.max + 1)) {
      testthat::expect_error(
        do.call(build_openai_batch_requests, c(args, list(max_output_tokens = value))),
        "`max_output_tokens` must be a positive integer", fixed = TRUE
      )
    }
    args$endpoint <- "chat.completions"
    testthat::expect_error(
      do.call(build_openai_batch_requests, c(args, list(max_output_tokens = 64))),
      "supported only by the OpenAI Responses endpoint", fixed = TRUE
    )
  }
  empty <- build_openai_batch_requests(pairs[0, ], "gpt-4.1", "Cohesion", "Connections",
                                       endpoint = "responses", store = FALSE, max_output_tokens = 64)
  testthat::expect_identical(nrow(empty), 0L)
  testthat::expect_named(empty, c("custom_id", "method", "url", "body"))
})

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
  bodies <- lapply(objs, `[[`, "body")
  testthat::expect_true(all(vapply(
    bodies,
    function(body) !any(c("temperature", "top_p") %in% names(body)),
    logical(1)
  )))
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
    regexp = "For GPT-5.x reasoning models"
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
