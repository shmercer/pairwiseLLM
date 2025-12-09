test_that("build_openai_batch_requests builds valid chat.completions JSONL
          objects", {
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
  expect_true(all(vapply(keys, function(k) {
    all(c(
      "custom_id", "method",
      "url", "body"
    ) %in% k)
  }, logical(1))))
})

test_that("build_openai_batch_requests supports gpt-5.1 with reasoning =
          'none' on responses", {
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
  expect_true("reasoning" %in% names(b1) || is.null(b1$reasoning) ||
    identical(b1$reasoning$effort, "none"))
})

test_that("build_openai_batch_requests errors for gpt-5.1 + reasoning !=
          'none' with temp/top_p/logprobs", {
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

test_that("build_openai_batch_requests errors for other gpt-5* models when
          temp/top_p/logprobs are non-NULL", {
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

test_that("build_openai_batch_requests allows other gpt-5* models with
          temp/top_p/logprobs = NULL", {
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

testthat::test_that("parse_openai_batch_output collects thoughts and message
                    text separately for responses", {
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

test_that("build_openai_batch_requests adds reasoning summary when
          include_thoughts = TRUE", {
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

testthat::test_that("run_openai_batch_pipeline works with polling and
                    parsing", {
  pairs <- tibble::tibble(
    ID1   = "S01",
    text1 = "Text 1",
    ID2   = "S02",
    text2 = "Text 2"
  )

  fake_batch_tbl <- tibble::tibble(jsonl = '{"dummy": true}')
  fake_file <- list(id = "file_123")
  fake_batch <- list(
    id             = "batch_123",
    status         = "completed",
    output_file_id = "file_out_123"
  )
  fake_results <- tibble::tibble(ID1 = "S01", ID2 = "S02", better_id = "S01")

  # capture the endpoint used for openai_create_batch
  used_endpoint <- NULL

  testthat::with_mocked_bindings(
    build_openai_batch_requests = function(pairs, model, trait_name,
                                           trait_description, prompt_template,
                                           endpoint, ...) {
      testthat::expect_equal(endpoint, "chat.completions")
      fake_batch_tbl
    },
    write_openai_batch_file = function(batch_tbl, path) {
      writeLines(batch_tbl$jsonl, path)
      invisible(path)
    },
    openai_upload_batch_file = function(path, api_key) {
      testthat::expect_true(file.exists(path))
      fake_file
    },
    openai_create_batch = function(input_file_id, endpoint, completion_window,
                                   metadata, api_key) {
      used_endpoint <<- endpoint
      list(id = "batch_123", status = "in_progress")
    },
    openai_poll_batch_until_complete = function(batch_id, interval_seconds,
                                                timeout_seconds, max_attempts,
                                                api_key, verbose) {
      testthat::expect_equal(batch_id, "batch_123")
      fake_batch
    },
    openai_download_batch_output = function(batch_id, path, api_key) {
      writeLines('{"dummy": true}', path)
      invisible(path)
    },
    parse_openai_batch_output = function(path) {
      testthat::expect_true(file.exists(path))
      fake_results
    },
    {
      td <- list(name = "Overall quality", description = "Quality")
      tmpl <- set_prompt_template()

      res <- run_openai_batch_pipeline(
        pairs             = pairs,
        model             = "gpt-4.1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        endpoint          = "chat.completions",
        interval_seconds  = 0,
        timeout_seconds   = 10,
        max_attempts      = 5
      )

      testthat::expect_equal(used_endpoint, "/v1/chat/completions")
      testthat::expect_true(file.exists(res$batch_input_path))
      testthat::expect_true(file.exists(res$batch_output_path))
      testthat::expect_equal(res$results$better_id, "S01")
      testthat::expect_equal(res$batch$status, "completed")
    }
  )
})

testthat::test_that("run_openai_batch_pipeline does not poll or parse when
                    poll = FALSE", {
  pairs <- tibble::tibble(
    ID1   = "S01",
    text1 = "Text 1",
    ID2   = "S02",
    text2 = "Text 2"
  )

  fake_batch_tbl <- tibble::tibble(jsonl = '{"dummy": true}')
  fake_file <- list(id = "file_123")
  fake_batch <- list(id = "batch_123", status = "queued")

  poll_called <- FALSE
  download_called <- FALSE
  parse_called <- FALSE

  testthat::with_mocked_bindings(
    build_openai_batch_requests = function(pairs, model, trait_name,
                                           trait_description, prompt_template,
                                           endpoint, ...) {
      fake_batch_tbl
    },
    write_openai_batch_file = function(batch_tbl, path) {
      writeLines(batch_tbl$jsonl, path)
      invisible(path)
    },
    openai_upload_batch_file = function(path, api_key) fake_file,
    openai_create_batch = function(input_file_id, endpoint, completion_window,
                                   metadata, api_key) {
      fake_batch
    },
    openai_poll_batch_until_complete = function(batch_id, interval_seconds,
                                                timeout_seconds, max_attempts,
                                                api_key, verbose) {
      poll_called <<- TRUE
      stop("polling should not be called when poll = FALSE")
    },
    openai_download_batch_output = function(batch_id, path, api_key) {
      download_called <<- TRUE
      stop("download should not be called when poll = FALSE")
    },
    parse_openai_batch_output = function(path) {
      parse_called <<- TRUE
      stop("parse should not be called when poll = FALSE")
    },
    {
      td <- list(name = "Overall quality", description = "Quality")
      tmpl <- set_prompt_template()

      res <- run_openai_batch_pipeline(
        pairs             = pairs,
        model             = "gpt-4.1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        endpoint          = "responses",
        poll              = FALSE
      )

      testthat::expect_false(poll_called)
      testthat::expect_false(download_called)
      testthat::expect_false(parse_called)

      testthat::expect_true(file.exists(res$batch_input_path))
      testthat::expect_null(res$batch_output_path)
      testthat::expect_null(res$results)
      testthat::expect_equal(res$batch$status, "queued")
    }
  )
})

testthat::test_that("openai_upload_batch_file errors on missing file", {
  nonexistent <- tempfile(fileext = ".jsonl")
  testthat::expect_false(file.exists(nonexistent))

  testthat::expect_error(
    openai_upload_batch_file(nonexistent),
    "File does not exist"
  )
})

testthat::test_that("openai_download_batch_output errors if no
                    output_file_id", {
  fake_batch <- list(
    id             = "batch_123",
    status         = "completed",
    output_file_id = NULL
  )

  testthat::with_mocked_bindings(
    openai_get_batch = function(batch_id, api_key) fake_batch,
    {
      tf <- tempfile(fileext = ".jsonl")
      testthat::expect_error(
        openai_download_batch_output("batch_123", tf),
        "has no output_file_id"
      )
    }
  )
})

testthat::test_that("openai_poll_batch_until_complete succeeds after
                    several polls", {
  fake_batches <- list(
    list(id = "batch_123", status = "in_progress"),
    list(id = "batch_123", status = "in_progress"),
    list(
      id = "batch_123", status = "completed", output_file_id =
        "file_out_123"
    )
  )
  i <- 0L

  testthat::with_mocked_bindings(
    openai_get_batch = function(batch_id, api_key) {
      i <<- i + 1L
      fake_batches[[i]]
    },
    {
      res <- openai_poll_batch_until_complete(
        batch_id         = "batch_123",
        interval_seconds = 0, # no sleep in tests
        timeout_seconds  = 60,
        max_attempts     = 5,
        verbose          = FALSE
      )
      testthat::expect_equal(res$status, "completed")
      testthat::expect_equal(i, 3L)
    }
  )
})

testthat::test_that("openai_poll_batch_until_complete stops at max_attempts", {
  fake_batch <- list(id = "batch_123", status = "in_progress")
  i <- 0L

  testthat::with_mocked_bindings(
    openai_get_batch = function(batch_id, api_key) {
      i <<- i + 1L
      fake_batch
    },
    {
      testthat::expect_error(
        openai_poll_batch_until_complete(
          batch_id         = "batch_123",
          interval_seconds = 0, # avoid sleeping in tests
          timeout_seconds  = 60,
          max_attempts     = 3,
          verbose          = FALSE
        ),
        "Reached max_attempts"
      )
      testthat::expect_equal(i, 3L)
    }
  )
})

# -------------------------------------------------------------------
# Internal helper: .openai_api_key
# -------------------------------------------------------------------

testthat::test_that(".openai_api_key prefers explicit api_key over env", {
  old <- Sys.getenv("OPENAI_API_KEY", unset = "")
  on.exit(Sys.setenv(OPENAI_API_KEY = old), add = TRUE)

  Sys.setenv(OPENAI_API_KEY = "FROM_ENV")

  # Explicit argument should win
  res <- .openai_api_key("EXPLICIT_KEY")
  testthat::expect_equal(res, "EXPLICIT_KEY")
})

testthat::test_that(".openai_api_key falls back to OPENAI_API_KEY env var", {
  old <- Sys.getenv("OPENAI_API_KEY", unset = "")
  on.exit(Sys.setenv(OPENAI_API_KEY = old), add = TRUE)

  Sys.setenv(OPENAI_API_KEY = "FROM_ENV")

  res <- .openai_api_key(NULL)
  testthat::expect_equal(res, "FROM_ENV")

  # Empty string should also trigger env fallback (via .get_api_key)
  res2 <- .openai_api_key("")
  testthat::expect_equal(res2, "FROM_ENV")
})

# -------------------------------------------------------------------
# openai_upload_batch_file: happy path
# -------------------------------------------------------------------

testthat::test_that("openai_upload_batch_file uploads file and returns id", {
  tf <- tempfile(fileext = ".jsonl")
  on.exit(unlink(tf), add = TRUE)
  writeLines(c('{"a":1}', '{"b":2}'), tf)

  captured <- list()

  testthat::with_mocked_bindings(
    .openai_request = function(path, api_key) {
      captured$path <<- path
      captured$api_key <<- api_key
      "REQ"
    },
    req_body_multipart = function(req, file, purpose) {
      captured$multipart_req <<- req
      captured$file <<- file # this is a form_file object
      captured$purpose <<- purpose
      list(req = req, file = file, purpose = purpose)
    },
    req_perform = function(req) {
      captured$performed <<- TRUE
      "RESP"
    },
    resp_body_json = function(resp, simplifyVector = TRUE) {
      captured$resp <<- resp
      list(id = "file_123")
    },
    {
      out <- openai_upload_batch_file(tf, purpose = "batch")

      testthat::expect_equal(out$id, "file_123")
      testthat::expect_equal(captured$path, "/files")
      testthat::expect_true(captured$performed)

      # file is an httr2::form_file object; check its fields instead of
      # raw equality with the path string.
      testthat::expect_s3_class(captured$file, "form_file")
      testthat::expect_equal(captured$file$path, tf)
      testthat::expect_equal(captured$purpose, "batch")
    }
  )
})

# -------------------------------------------------------------------
# openai_create_batch / openai_get_batch
# -------------------------------------------------------------------

testthat::test_that("openai_create_batch sends correct body and returns batch", {
  captured <- list()

  testthat::with_mocked_bindings(
    .openai_request = function(path, api_key) {
      captured$path <<- path
      captured$api_key <<- api_key
      "REQ"
    },
    req_body_json = function(req, body) {
      captured$body <<- body
      "REQ_WITH_BODY"
    },
    req_perform = function(req) {
      captured$performed <<- TRUE
      "RESP"
    },
    resp_body_json = function(resp, simplifyVector = TRUE) {
      captured$resp <<- resp
      list(id = "batch_123", status = "queued")
    },
    {
      batch <- openai_create_batch(
        input_file_id     = "file_123",
        endpoint          = "responses",
        completion_window = "24h",
        metadata          = list(foo = "bar"),
        api_key           = "TEST_KEY"
      )

      testthat::expect_equal(batch$id, "batch_123")
      testthat::expect_equal(batch$status, "queued")

      # Focus on body correctness and the fact we performed the request.
      testthat::expect_equal(captured$body$input_file_id, "file_123")
      testthat::expect_equal(captured$body$endpoint, "responses")
      testthat::expect_equal(captured$body$completion_window, "24h")
      testthat::expect_equal(captured$body$metadata$foo, "bar")
      testthat::expect_true(captured$performed)
    }
  )
})

testthat::test_that("openai_get_batch calls batches endpoint and returns response", {
  captured <- list()

  testthat::with_mocked_bindings(
    .openai_request = function(path, api_key) {
      captured$path <<- path
      captured$api_key <<- api_key
      "REQ"
    },
    req_perform = function(req) {
      captured$performed <<- TRUE
      "RESP"
    },
    resp_body_json = function(resp, simplifyVector = TRUE) {
      captured$resp <<- resp
      list(id = "batch_123", status = "completed")
    },
    {
      batch <- openai_get_batch("batch_123", api_key = "TEST_KEY")

      testthat::expect_equal(batch$id, "batch_123")
      testthat::expect_equal(batch$status, "completed")
      testthat::expect_equal(captured$path, "/batches/batch_123")
      testthat::expect_true(captured$performed)
    }
  )
})

# -------------------------------------------------------------------
# openai_download_batch_output: happy path
# -------------------------------------------------------------------

testthat::test_that("openai_download_batch_output downloads to path when output_file_id present", {
  fake_batch <- list(
    id             = "batch_123",
    status         = "completed",
    output_file_id = "file_out_123"
  )

  captured <- list()
  tf <- tempfile(fileext = ".jsonl")
  on.exit(unlink(tf), add = TRUE)

  testthat::with_mocked_bindings(
    openai_get_batch = function(batch_id, api_key) fake_batch,
    .openai_request = function(path, api_key) {
      captured$path <<- path
      captured$api_key <<- api_key
      "REQ"
    },
    req_perform = function(req) {
      captured$performed <<- TRUE
      "RESP"
    },
    resp_body_raw = function(resp) {
      captured$resp <<- resp
      charToRaw('{"ok":true}\n')
    },
    {
      out_path <- openai_download_batch_output("batch_123", tf, api_key = "TEST_KEY")

      testthat::expect_equal(out_path, tf)
      testthat::expect_true(file.exists(tf))
      testthat::expect_equal(captured$path, "/files/file_out_123/content")
      testthat::expect_true(captured$performed)

      # File should contain exactly the raw we wrote
      txt <- readLines(tf, warn = FALSE)
      testthat::expect_equal(txt, '{"ok":true}')
    }
  )
})

# -------------------------------------------------------------------
# openai_poll_batch_until_complete: timeout_seconds branch
# -------------------------------------------------------------------

testthat::test_that("openai_poll_batch_until_complete errors on timeout_seconds", {
  fake_batch <- list(id = "batch_123", status = "in_progress")
  calls <- 0L

  testthat::with_mocked_bindings(
    openai_get_batch = function(batch_id, api_key) {
      calls <<- calls + 1L
      fake_batch
    },
    {
      testthat::expect_error(
        openai_poll_batch_until_complete(
          batch_id         = "batch_123",
          interval_seconds = 0, # no sleep for tests
          timeout_seconds  = 0, # immediately exceed timeout
          max_attempts     = 100,
          verbose          = FALSE
        ),
        "Timeout \\(0 seconds\\) waiting for batch",
        fixed = FALSE
      )

      # Should have polled at least once
      testthat::expect_gte(calls, 1L)
    }
  )
})
