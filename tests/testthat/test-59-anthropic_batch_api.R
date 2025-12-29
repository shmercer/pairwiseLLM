testthat::test_that("build_anthropic_batch_requests builds valid requests", {
  data("example_writing_samples", package = "pairwiseLLM")

  pairs <- make_pairs(example_writing_samples)
  pairs <- pairs[1:2, ]

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  batch <- build_anthropic_batch_requests(
    pairs             = pairs,
    model             = "claude-sonnet-4-5",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    reasoning         = "none"
  )

  testthat::expect_s3_class(batch, "tbl_df")
  testthat::expect_equal(nrow(batch), 2L)
  testthat::expect_true(all(c("custom_id", "params") %in% names(batch)))

  # Basic structure checks on first params
  p1 <- batch$params[[1]]
  testthat::expect_equal(p1$model, "claude-sonnet-4-5")
  testthat::expect_true(is.list(p1$messages))

  # NEW: Batch should match anthropic live formatting => no system field
  testthat::expect_false("system" %in% names(p1))

  # With reasoning = "none", temperature should default to 0 and there
  # should be no thinking block; max_tokens should default to 768.
  testthat::expect_equal(p1$temperature, 0)
  testthat::expect_equal(p1$max_tokens, 768)
  testthat::expect_false("thinking" %in% names(p1))

  # NEW: User message contains the full build_prompt() output
  msg1 <- p1$messages[[1]]
  testthat::expect_equal(msg1$role, "user")
  testthat::expect_true(is.list(msg1$content))
  testthat::expect_equal(msg1$content[[1]]$type, "text")

  text_block <- msg1$content[[1]]$text

  expected_prompt <- build_prompt(
    template   = tmpl,
    trait_name = td$name,
    trait_desc = td$description,
    text1      = pairs$text1[1],
    text2      = pairs$text2[1]
  )

  # Strongest parity check: exact string match to build_prompt()
  testthat::expect_identical(text_block, expected_prompt)
})

testthat::test_that("build_anthropic_batch_requests enforces reasoning
                    constraints", {
  data("example_writing_samples", package = "pairwiseLLM")

  pairs <- make_pairs(example_writing_samples)
  pairs <- pairs[1:1, ]

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # reasoning = "enabled" with default temperature and budgets should work
  batch <- build_anthropic_batch_requests(
    pairs             = pairs,
    model             = "claude-sonnet-4-5",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    reasoning         = "enabled"
  )

  p1 <- batch$params[[1]]
  testthat::expect_equal(p1$temperature, 1)
  testthat::expect_true("thinking" %in% names(p1))
  testthat::expect_gte(p1$thinking$budget_tokens, 1024)
  testthat::expect_gt(p1$max_tokens, p1$thinking$budget_tokens)

  # temperature != 1 with reasoning = "enabled" should error
  testthat::expect_error(
    build_anthropic_batch_requests(
      pairs             = pairs,
      model             = "claude-sonnet-4-5",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl,
      reasoning         = "enabled",
      temperature       = 0
    ),
    regexp = "temperature"
  )

  # thinking_budget_tokens < 1024 should error
  testthat::expect_error(
    build_anthropic_batch_requests(
      pairs = pairs,
      model = "claude-sonnet-4-5",
      trait_name = td$name,
      trait_description = td$description,
      prompt_template = tmpl,
      reasoning = "enabled",
      thinking_budget_tokens = 512
    ),
    regexp = "thinking_budget_tokens"
  )
})

testthat::test_that("run_anthropic_batch_pipeline upgrades reasoning for
                    include_thoughts = TRUE", {
  pairs <- tibble::tibble(
    ID1   = "S01",
    text1 = "Text 1",
    ID2   = "S02",
    text2 = "Text 2"
  )

  fake_req_tbl <- tibble::tibble(
    custom_id = "ANTH_S01_vs_S02",
    params    = list(list(dummy = TRUE))
  )

  captured_reasoning <- NULL

  fake_batch_initial <- list(
    id = "msgbatch_123",
    type = "message_batch",
    processing_status = "in_progress",
    request_counts = list(
      processing = 1L,
      succeeded  = 0L,
      errored    = 0L,
      canceled   = 0L,
      expired    = 0L
    ),
    results_url = NULL
  )

  testthat::with_mocked_bindings(
    build_anthropic_batch_requests = function(pairs, model, trait_name,
                                              trait_description,
                                              prompt_template, reasoning, ...) {
      captured_reasoning <<- reasoning
      fake_req_tbl
    },
    anthropic_create_batch = function(requests, api_key, anthropic_version) {
      fake_batch_initial
    },
    anthropic_poll_batch_until_complete = function(batch_id, interval_seconds,
                                                   timeout_seconds, api_key,
                                                   anthropic_version, verbose) {
      stop("Polling should not be called in this test")
    },
    anthropic_download_batch_results = function(batch_id, output_path, api_key,
                                                anthropic_version) {
      stop("Download should not be called in this test")
    },
    parse_anthropic_batch_output = function(jsonl_path, tag_prefix,
                                            tag_suffix) {
      stop("Parse should not be called in this test")
    },
    {
      td <- list(name = "Overall quality", description = "Quality")
      tmpl <- set_prompt_template()

      # Start with reasoning = "none" but include_thoughts = TRUE; this should
      # cause run_anthropic_batch_pipeline() to upgrade reasoning to "enabled".
      res <- run_anthropic_batch_pipeline(
        pairs             = pairs,
        model             = "claude-sonnet-4-5",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        reasoning         = "none",
        include_thoughts  = TRUE,
        poll              = FALSE,
        verbose           = FALSE
      )

      testthat::expect_type(res, "list")
      testthat::expect_equal(captured_reasoning, "enabled")
    }
  )
})

testthat::test_that("parse_anthropic_batch_output handles succeeded and
                    errored results", {
  tmp <- tempfile(fileext = ".jsonl")
  on.exit(unlink(tmp), add = TRUE)

  # Succeeded result line, similar to docs
  line_ok <- list(
    custom_id = "ANTH_S01_vs_S02",
    result = list(
      type = "succeeded",
      message = list(
        id = "msg_014VwiXbi91y3JMjcpyGBHX5",
        type = "message",
        role = "assistant",
        model = "claude-sonnet-4-5-20250929",
        content = list(
          list(
            type = "text",
            text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> Hello!"
          )
        ),
        stop_reason = "end_turn",
        stop_sequence = NULL,
        usage = list(
          input_tokens  = 10L,
          output_tokens = 5L,
          total_tokens  = 15L
        )
      )
    )
  )

  # Errored result line
  line_err <- list(
    custom_id = "ANTH_S03_vs_S04",
    result = list(
      type = "errored",
      error = list(
        type    = "invalid_request",
        message = "Validation error"
      )
    )
  )

  json_lines <- c(
    jsonlite::toJSON(line_ok, auto_unbox = TRUE),
    jsonlite::toJSON(line_err, auto_unbox = TRUE)
  )
  writeLines(json_lines, con = tmp, useBytes = TRUE)

  res <- parse_anthropic_batch_output(tmp)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(nrow(res), 2L)

  # First row: succeeded
  r1 <- res[1, ]
  testthat::expect_equal(r1$custom_id, "ANTH_S01_vs_S02")
  testthat::expect_equal(r1$ID1, "S01")
  testthat::expect_equal(r1$ID2, "S02")
  testthat::expect_equal(r1$result_type, "succeeded")
  testthat::expect_equal(r1$status_code, 200L)
  testthat::expect_true(is.na(r1$error_message))
  testthat::expect_equal(r1$model, "claude-sonnet-4-5-20250929")
  testthat::expect_equal(r1$better_sample, "SAMPLE_2")
  testthat::expect_equal(r1$better_id, "S02")
  testthat::expect_equal(r1$prompt_tokens, 10)
  testthat::expect_equal(r1$completion_tokens, 5)
  testthat::expect_equal(r1$total_tokens, 15)

  # Second row: errored
  r2 <- res[2, ]
  testthat::expect_equal(r2$custom_id, "ANTH_S03_vs_S04")
  testthat::expect_equal(r2$ID1, "S03")
  testthat::expect_equal(r2$ID2, "S04")
  testthat::expect_equal(r2$result_type, "errored")
  testthat::expect_true(is.na(r2$status_code))
  testthat::expect_match(r2$error_message, "Validation error")
  testthat::expect_true(is.na(r2$content))
  testthat::expect_true(is.na(r2$better_sample))
  testthat::expect_true(is.na(r2$better_id))
})

testthat::test_that("run_anthropic_batch_pipeline works with polling
                    and parsing", {
  pairs <- tibble::tibble(
    ID1   = "S01",
    text1 = "Text 1",
    ID2   = "S02",
    text2 = "Text 2"
  )

  fake_req_tbl <- tibble::tibble(
    custom_id = "ANTH_S01_vs_S02",
    params    = list(list(dummy = TRUE))
  )

  fake_batch_initial <- list(
    id = "msgbatch_123",
    type = "message_batch",
    processing_status = "in_progress",
    request_counts = list(
      processing = 1L,
      succeeded  = 0L,
      errored    = 0L,
      canceled   = 0L,
      expired    = 0L
    ),
    results_url = NULL
  )

  fake_batch_final <- list(
    id = "msgbatch_123",
    type = "message_batch",
    processing_status = "ended",
    request_counts = list(
      processing = 0L,
      succeeded  = 1L,
      errored    = 0L,
      canceled   = 0L,
      expired    = 0L
    ),
    results_url = "https://example.com/results.jsonl"
  )

  fake_results <- tibble::tibble(
    custom_id = "ANTH_S01_vs_S02",
    ID1 = "S01",
    ID2 = "S02",
    better_id = "S01",
    result_type = "succeeded",
    status_code = 200L,
    content = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    prompt_tokens = 10,
    completion_tokens = 5,
    total_tokens = 15
  )

  # Flags to ensure each helper is called
  created_batch_id <- NULL
  polled_batch_id <- NULL
  downloaded_batch <- NULL
  parsed_path <- NULL

  testthat::with_mocked_bindings(
    build_anthropic_batch_requests = function(pairs, model, trait_name,
                                              trait_description,
                                              prompt_template, reasoning, ...) {
      fake_req_tbl
    },
    anthropic_create_batch = function(requests, api_key, anthropic_version) {
      created_batch_id <<- "msgbatch_123"
      fake_batch_initial
    },
    anthropic_poll_batch_until_complete = function(batch_id, interval_seconds,
                                                   timeout_seconds, api_key,
                                                   anthropic_version, verbose) {
      polled_batch_id <<- batch_id
      fake_batch_final
    },
    anthropic_download_batch_results = function(batch_id, output_path, api_key,
                                                anthropic_version) {
      downloaded_batch <<- batch_id
      # Write a dummy .jsonl file so that parse_* can read it
      writeLines('{"dummy": true}', con = output_path)
      invisible(output_path)
    },
    parse_anthropic_batch_output = function(jsonl_path, tag_prefix,
                                            tag_suffix) {
      parsed_path <<- jsonl_path
      fake_results
    },
    {
      td <- list(name = "Overall quality", description = "Quality")
      tmpl <- set_prompt_template()

      res <- run_anthropic_batch_pipeline(
        pairs             = pairs,
        model             = "claude-sonnet-4-5",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        reasoning         = "none",
        interval_seconds  = 0,
        timeout_seconds   = 10,
        verbose           = FALSE
      )

      testthat::expect_equal(created_batch_id, "msgbatch_123")
      testthat::expect_equal(polled_batch_id, "msgbatch_123")
      testthat::expect_equal(downloaded_batch, "msgbatch_123")
      testthat::expect_true(file.exists(res$batch_input_path))
      testthat::expect_true(file.exists(res$batch_output_path))
      testthat::expect_true(file.exists(parsed_path))

      # Return structure should mirror run_openai_batch_pipeline()
      testthat::expect_true(all(c(
        "batch_input_path", "batch_output_path", "file", "batch", "results"
      ) %in% names(res)))

      testthat::expect_null(res$file)
      testthat::expect_equal(res$batch$processing_status, "ended")
      testthat::expect_equal(res$results$better_id, "S01")
    }
  )
})

testthat::test_that("run_anthropic_batch_pipeline does not poll or parse when
                    poll = FALSE", {
  pairs <- tibble::tibble(
    ID1   = "S01",
    text1 = "Text 1",
    ID2   = "S02",
    text2 = "Text 2"
  )

  fake_req_tbl <- tibble::tibble(
    custom_id = "ANTH_S01_vs_S02",
    params    = list(list(dummy = TRUE))
  )

  fake_batch_initial <- list(
    id = "msgbatch_123",
    type = "message_batch",
    processing_status = "in_progress",
    request_counts = list(
      processing = 1L,
      succeeded  = 0L,
      errored    = 0L,
      canceled   = 0L,
      expired    = 0L
    ),
    results_url = NULL
  )

  poll_called <- FALSE
  download_called <- FALSE
  parse_called <- FALSE

  testthat::with_mocked_bindings(
    build_anthropic_batch_requests = function(pairs, model, trait_name,
                                              trait_description,
                                              prompt_template, reasoning, ...) {
      fake_req_tbl
    },
    anthropic_create_batch = function(requests, api_key, anthropic_version) {
      fake_batch_initial
    },
    anthropic_poll_batch_until_complete = function(batch_id, interval_seconds,
                                                   timeout_seconds, api_key,
                                                   anthropic_version, verbose) {
      poll_called <<- TRUE
      stop("Polling should not be called when poll = FALSE")
    },
    anthropic_download_batch_results = function(batch_id, output_path, api_key,
                                                anthropic_version) {
      download_called <<- TRUE
      stop("Download should not be called when poll = FALSE")
    },
    parse_anthropic_batch_output = function(jsonl_path, tag_prefix,
                                            tag_suffix) {
      parse_called <<- TRUE
      stop("Parse should not be called when poll = FALSE")
    },
    {
      td <- list(name = "Overall quality", description = "Quality")
      tmpl <- set_prompt_template()

      res <- run_anthropic_batch_pipeline(
        pairs             = pairs,
        model             = "claude-sonnet-4-5",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        reasoning         = "none",
        poll              = FALSE
      )

      testthat::expect_false(poll_called)
      testthat::expect_false(download_called)
      testthat::expect_false(parse_called)

      testthat::expect_true(file.exists(res$batch_input_path))
      testthat::expect_null(res$batch_output_path)
      testthat::expect_null(res$results)

      # Return structure still standardized
      testthat::expect_true(all(c(
        "batch_input_path", "batch_output_path", "file", "batch", "results"
      ) %in% names(res)))
      testthat::expect_null(res$file)
      testthat::expect_equal(res$batch$processing_status, "in_progress")
    }
  )
})

testthat::test_that("internal helper .parse_ids_from_custom_id handles edge cases", {
  # Internal function is available directly in the test environment
  res <- .parse_ids_from_custom_id("ANTH_S01_vs_S02")
  testthat::expect_equal(res$ID1, "S01")
  testthat::expect_equal(res$ID2, "S02")

  # Different prefix
  res <- .parse_ids_from_custom_id("CUSTOM_123_vs_456")
  testthat::expect_equal(res$ID1, "123")
  testthat::expect_equal(res$ID2, "456")

  # Malformed: No underscores
  res <- .parse_ids_from_custom_id("ANTHS01vsS02")
  testthat::expect_true(is.na(res$ID1))
  testthat::expect_true(is.na(res$ID2))

  # Malformed: Missing IDs parts
  res <- .parse_ids_from_custom_id("ANTH_vs_")
  testthat::expect_true(is.na(res$ID1))
  testthat::expect_true(is.na(res$ID2))

  # Input validation
  res <- .parse_ids_from_custom_id(NULL)
  testthat::expect_true(is.na(res$ID1))
  res <- .parse_ids_from_custom_id("")
  testthat::expect_true(is.na(res$ID1))
})

testthat::test_that("internal helper .parse_anthropic_pair_message extracts thoughts and handles no-choice", {
  # Internal function is available directly in the test environment
  parse_msg <- .parse_anthropic_pair_message

  # Case 1: Complex content with extended thinking block and text
  body_thinking <- list(
    type = "message",
    model = "claude-test",
    content = list(
      list(type = "thinking", thinking = "I am thinking deeply."),
      list(type = "text", text = "Here is the result.")
    ),
    usage = list(input_tokens = 10, output_tokens = 20)
  )

  res <- parse_msg(body_thinking, "A", "B")
  testthat::expect_equal(res$thoughts, "I am thinking deeply.")
  testthat::expect_equal(res$content, "Here is the result.")
  # No tags present, so better_sample should be NA
  testthat::expect_true(is.na(res$better_sample))

  # Case 2: No content (e.g., weird API response)
  body_empty <- list(type = "message", model = "claude-test", content = list())
  res_empty <- parse_msg(body_empty, "A", "B")
  testthat::expect_true(is.na(res_empty$content))
  testthat::expect_true(is.na(res_empty$thoughts))

  # Case 3: Text block exists but is NULL inside (defensive coding)
  body_null_text <- list(
    type = "message",
    content = list(list(type = "text", text = NULL))
  )
  res_null <- parse_msg(body_null_text, "A", "B")
  testthat::expect_equal(res_null$content, "")
})

testthat::test_that("build_anthropic_batch_requests validates input columns and types", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # Missing columns in pairs
  bad_pairs <- tibble::tibble(ID1 = "A", text1 = "txt")
  testthat::expect_error(
    build_anthropic_batch_requests(
      pairs = bad_pairs,
      model = "claude-sonnet",
      trait_name = td$name,
      trait_description = td$description
    ),
    regexp = "must contain columns"
  )

  # Invalid model type
  good_pairs <- tibble::tibble(ID1 = "A", text1 = "t", ID2 = "B", text2 = "t")
  testthat::expect_error(
    build_anthropic_batch_requests(
      pairs = good_pairs,
      model = 123, # Not character
      trait_name = td$name,
      trait_description = td$description
    ),
    regexp = "model.*must be a single character"
  )
})

testthat::test_that("build_anthropic_batch_requests passes ... arguments (e.g. top_p)", {
  pairs <- tibble::tibble(ID1 = "A", text1 = "t", ID2 = "B", text2 = "t")

  # Fix: Use a valid trait name supported by trait_description()
  td <- trait_description("overall_quality")

  batch <- build_anthropic_batch_requests(
    pairs = pairs,
    model = "claude-test",
    trait_name = td$name,
    trait_description = td$description,
    top_p = 0.9,
    temperature = 0.5 # Overriding default 0
  )

  params <- batch$params[[1]]
  testthat::expect_equal(params$top_p, 0.9)
  testthat::expect_equal(params$temperature, 0.5)
})

testthat::test_that("anthropic_create_batch and get_batch validate inputs", {
  # Create batch: empty list check
  testthat::expect_error(
    anthropic_create_batch(list()),
    regexp = "must be a non-empty list"
  )

  # Get batch: empty ID check
  testthat::expect_error(
    anthropic_get_batch(""),
    regexp = "must be a non-empty character"
  )
})

testthat::test_that("anthropic_poll_batch_until_complete handles timeout", {
  # Mock get_batch to always return "in_progress"
  mock_batch_progress <- list(
    id = "batch_123",
    processing_status = "in_progress",
    request_counts = list()
  )

  testthat::with_mocked_bindings(
    anthropic_get_batch = function(...) mock_batch_progress,
    {
      start_t <- Sys.time()
      # Set a very short timeout
      res <- anthropic_poll_batch_until_complete(
        batch_id = "batch_123",
        interval_seconds = 0.1,
        timeout_seconds = 0.5,
        verbose = FALSE
      )
      end_t <- Sys.time()

      # It should return the last batch object (which is still in_progress)
      testthat::expect_equal(res$processing_status, "in_progress")

      # It should have taken at least 0.5 seconds
      testthat::expect_gte(as.numeric(difftime(end_t, start_t, units = "secs")), 0.4)
    }
  )
})

testthat::test_that("anthropic_download_batch_results validates results_url", {
  # If the batch object doesn't have a results_url (e.g. not finished), it should error
  mock_batch_not_ready <- list(
    id = "batch_123",
    processing_status = "in_progress",
    results_url = ""
  )

  testthat::with_mocked_bindings(
    anthropic_get_batch = function(...) mock_batch_not_ready,
    {
      testthat::expect_error(
        anthropic_download_batch_results("batch_123", "out.jsonl"),
        regexp = "Batch has no `results_url` yet"
      )
    }
  )
})

testthat::test_that("parse_anthropic_batch_output handles empty files and bad JSON", {
  # Case 1: Empty file
  empty_file <- tempfile()
  file.create(empty_file)
  on.exit(unlink(empty_file), add = TRUE)

  res <- parse_anthropic_batch_output(empty_file)
  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(nrow(res), 0L)
  testthat::expect_true("custom_id" %in% names(res))

  # Case 2: File with one valid line and one garbage line
  bad_json_file <- tempfile()
  lines <- c(
    jsonlite::toJSON(list(custom_id = "ID_A_vs_B", result = list(type = "canceled")), auto_unbox = TRUE),
    "{ broken_json: true " # Malformed
  )
  writeLines(lines, bad_json_file)
  on.exit(unlink(bad_json_file), add = TRUE)

  # Expect a warning about the failed parse
  testthat::expect_warning(
    res_bad <- parse_anthropic_batch_output(bad_json_file),
    regexp = "Failed to parse JSON"
  )

  # It should contain a row for the bad line indicating failure
  testthat::expect_equal(nrow(res_bad), 2L)

  # Check the valid row
  r1 <- res_bad[1, ]
  testthat::expect_equal(r1$result_type, "canceled")

  # Check the broken row
  r2 <- res_bad[2, ]
  testthat::expect_equal(r2$error_message, "Failed to parse JSON line.")
  testthat::expect_true(is.na(r2$custom_id))
})

testthat::test_that("internal helper .parse_ids_from_custom_id handles edge cases", {
  # Internal function is available directly in the test environment
  parse_ids <- .parse_ids_from_custom_id

  # Happy path
  res <- parse_ids("ANTH_S01_vs_S02")
  testthat::expect_equal(res$ID1, "S01")
  testthat::expect_equal(res$ID2, "S02")

  # Different prefix
  res <- parse_ids("CUSTOM_123_vs_456")
  testthat::expect_equal(res$ID1, "123")
  testthat::expect_equal(res$ID2, "456")

  # Malformed: No underscores
  res <- parse_ids("ANTHS01vsS02")
  testthat::expect_true(is.na(res$ID1))
  testthat::expect_true(is.na(res$ID2))

  # Malformed: Missing IDs parts
  res <- parse_ids("ANTH_vs_")
  testthat::expect_true(is.na(res$ID1))
  testthat::expect_true(is.na(res$ID2))

  # Input validation
  res <- parse_ids(NULL)
  testthat::expect_true(is.na(res$ID1))
  res <- parse_ids("")
  testthat::expect_true(is.na(res$ID1))
})

testthat::test_that("internal helper .parse_anthropic_pair_message handles missing content", {
  # Internal function is available directly
  # Case: Empty content list (e.g. pre-fill response or error upstream)
  body_empty <- list(type = "message", model = "claude-test", content = list())
  res <- .parse_anthropic_pair_message(body_empty, "A", "B")

  testthat::expect_true(is.na(res$content))
  testthat::expect_true(is.na(res$thoughts))
  testthat::expect_true(is.na(res$better_sample))
})

test_that("build_anthropic_batch_requests validates required pair columns", {
  bad_pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B") # missing text2
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  expect_error(
    build_anthropic_batch_requests(
      pairs             = bad_pairs,
      model             = "claude-test",
      trait_name        = td$name,
      trait_description = td$description,
      prompt_template   = tmpl
    ),
    "pairs"
  )
})

test_that("anthropic_create_batch validates requests input", {
  expect_error(
    anthropic_create_batch(requests = list(), api_key = "x"),
    "requests"
  )
})

test_that("anthropic_download_batch_results errors if results_url missing", {
  with_mocked_bindings(
    anthropic_get_batch = function(...) list(results_url = ""),
    {
      expect_error(
        anthropic_download_batch_results(
          batch_id = "b1",
          output_path = tempfile(fileext = ".jsonl"),
          api_key = "x"
        ),
        "results_url"
      )
    }
  )
})

test_that("parse_anthropic_batch_output errors on missing jsonl_path", {
  expect_error(
    parse_anthropic_batch_output(jsonl_path = tempfile(fileext = ".jsonl_missing")),
    "does not exist"
  )
})
test_that(".parse_ids_from_custom_id returns NA IDs on malformed custom_id", {
  res <- pairwiseLLM:::.parse_ids_from_custom_id("ANTH_S01_vs_S02_extra_vs_S03")
  expect_true(is.na(res$ID1))
  expect_true(is.na(res$ID2))
})

test_that(".parse_anthropic_pair_message detects SAMPLE_1 as better", {
  body <- list(
    type = "message",
    model = "claude-test",
    content = list(
      list(type = "text", text = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>")
    ),
    usage = list(input_tokens = 1L, output_tokens = 1L)
  )

  res <- pairwiseLLM:::.parse_anthropic_pair_message(body, ID1 = "X", ID2 = "Y")
  expect_equal(res$better_sample, "SAMPLE_1")
  expect_equal(res$better_id, "X")
})

test_that("build_anthropic_batch_requests supports reasoning='enabled' and validates thinking budget", {
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)[1:1, ]
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  # Provide temperature=1 so the code takes the `dots$temperature` branch
  reqs <- build_anthropic_batch_requests(
    pairs = pairs,
    model = "claude-sonnet-4-5",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    reasoning = "enabled",
    temperature = 1,
    max_tokens = 2048,
    thinking_budget_tokens = 1024
  )
  expect_equal(nrow(reqs), 1L)
  expect_true(is.list(reqs$params[[1]]$thinking))

  # thinking_budget_tokens must be < max_tokens
  expect_error(
    build_anthropic_batch_requests(
      pairs = pairs,
      model = "claude-sonnet-4-5",
      trait_name = td$name,
      trait_description = td$description,
      prompt_template = tmpl,
      reasoning = "enabled",
      temperature = 1,
      max_tokens = 1024,
      thinking_budget_tokens = 1024
    ),
    "must be smaller than `max_tokens`",
    fixed = TRUE
  )
})

test_that("anthropic_create_batch and anthropic_get_batch build correct request paths", {
  seen <- new.env(parent = emptyenv())

  fake_request <- function(path, api_key, anthropic_version) {
    seen$last_path <- path
    structure(list(path = path), class = "fake_req")
  }
  fake_req_body_json <- function(req, body) {
    seen$last_body <- body
    req
  }
  fake_req_perform <- function(req) {
    seen$perform_path <- req$path
    structure(list(ok = TRUE), class = "fake_resp")
  }
  fake_resp_body_json <- function(resp, simplifyVector = TRUE) {
    # Return something that the caller expects as a parsed response
    list(id = "msgbatch_123", processing_status = "in_progress", request_counts = list())
  }

  testthat::local_mocked_bindings(
    .anthropic_request = fake_request,
    .anthropic_req_body_json = fake_req_body_json,
    .anthropic_req_perform = fake_req_perform,
    .anthropic_resp_body_json = fake_resp_body_json,
    .package = "pairwiseLLM"
  )

  out <- pairwiseLLM::anthropic_create_batch(requests = list(list(custom_id = "X", params = list())))

  expect_equal(seen$perform_path, "/v1/messages/batches")
  expect_true(is.list(seen$last_body$requests))
  expect_equal(out$id, "msgbatch_123")

  # reset capture for get
  rm(list = c("last_path", "last_body", "perform_path"), envir = seen)

  out2 <- pairwiseLLM::anthropic_get_batch(batch_id = "msgbatch_123")

  expect_equal(seen$perform_path, "/v1/messages/batches/msgbatch_123")
  expect_equal(out2$id, "msgbatch_123")
})

test_that("anthropic_poll_batch_until_complete logs status when verbose and warns on timeout", {
  calls <- 0L
  fake_get <- function(batch_id, ...) {
    calls <<- calls + 1L
    list(
      id = batch_id,
      processing_status = if (calls == 1L) "processing" else "ended",
      request_counts = list(processing = 1L, succeeded = 0L, errored = 0L, canceled = 0L, expired = 0L)
    )
  }

  testthat::local_mocked_bindings(
    anthropic_get_batch = fake_get,
    .package = "pairwiseLLM"
  )

  msgs <- testthat::capture_messages({
    out <- pairwiseLLM::anthropic_poll_batch_until_complete(
      batch_id = "msgbatch_123",
      interval_seconds = 0,
      timeout_seconds = Inf,
      verbose = TRUE
    )
    expect_equal(out$processing_status, "ended")
  })
  expect_true(any(grepl("status:", msgs, fixed = TRUE)))

  # Timeout warning path
  calls <- 0L
  testthat::local_mocked_bindings(
    anthropic_get_batch = function(batch_id, ...) {
      calls <<- calls + 1L
      list(
        id = batch_id,
        processing_status = "processing",
        request_counts = list(processing = 1L, succeeded = 0L, errored = 0L, canceled = 0L, expired = 0L)
      )
    },
    .package = "pairwiseLLM"
  )

  testthat::expect_warning(
    pairwiseLLM::anthropic_poll_batch_until_complete(
      batch_id = "msgbatch_123",
      interval_seconds = 0,
      timeout_seconds = 0,
      verbose = TRUE
    ),
    "Timeout reached"
  )
})

test_that("anthropic_download_batch_results errors without results_url and writes jsonl when present", {
  tmp <- tempfile(fileext = ".jsonl")

  # Ensure CI has a key so header construction doesn't fail
  withr::local_envvar(c(ANTHROPIC_API_KEY = "test-key"))

  # Missing results_url branch
  testthat::local_mocked_bindings(
    anthropic_get_batch = function(...) {
      list(
        id = "msgbatch_123",
        processing_status = "ended",
        results_url = ""
      )
    },
    .package = "pairwiseLLM"
  )

  expect_error(
    pairwiseLLM::anthropic_download_batch_results(
      batch_id = "msgbatch_123",
      output_path = tmp,
      api_key = "test-key"
    ),
    "no `results_url`",
    fixed = TRUE
  )

  # Present results_url branch, avoid network:
  testthat::local_mocked_bindings(
    anthropic_get_batch = function(...) {
      list(
        id = "msgbatch_123",
        processing_status = "ended",
        results_url = "https://example.com/results.jsonl"
      )
    },
    .package = "pairwiseLLM"
  )

  testthat::local_mocked_bindings(
    .anthropic_req_perform = function(req) {
      httr2::response(
        url = req$url,
        status_code = 200L,
        headers = list(`content-type` = "application/jsonl"),
        body = charToRaw('{"custom_id":"X","result":{"type":"succeeded"}}\n')
      )
    },
    .package = "pairwiseLLM"
  )

  pairwiseLLM::anthropic_download_batch_results(
    batch_id = "msgbatch_123",
    output_path = tmp,
    api_key = "test-key"
  )

  expect_true(file.exists(tmp))
  txt <- readLines(tmp, warn = FALSE)
  expect_true(length(txt) >= 1L)
  expect_true(grepl('"custom_id"', txt[[1]], fixed = TRUE))
})

test_that("parse_anthropic_batch_output skips blank lines", {
  # One blank line and one valid line
  line_ok <- jsonlite::toJSON(
    list(
      custom_id = "ANTH_S01_vs_S02",
      result = list(
        type = "succeeded",
        message = list(
          type = "message",
          model = "claude-test",
          content = list(list(type = "text", text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>"))
        )
      )
    ),
    auto_unbox = TRUE
  )

  tmp <- tempfile(fileext = ".jsonl")
  writeLines(c("", "   ", line_ok), tmp)

  out <- parse_anthropic_batch_output(tmp)
  expect_equal(nrow(out), 1L)
  expect_equal(out$ID1[[1]], "S01")
  expect_equal(out$ID2[[1]], "S02")
})
