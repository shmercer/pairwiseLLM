testthat::test_that("build_gemini_batch_requests builds valid requests", {
  data("example_writing_samples", package = "pairwiseLLM")

  pairs <- make_pairs(example_writing_samples)
  pairs <- pairs[1:2, ]

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  batch <- build_gemini_batch_requests(
    pairs             = pairs,
    model             = "gemini-3-pro-preview",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    thinking_level    = "low"
  )

  testthat::expect_s3_class(batch, "tbl_df")
  testthat::expect_equal(nrow(batch), 2L)
  testthat::expect_true(all(c("custom_id", "request") %in% names(batch)))

  # Basic structure checks on first request
  r1 <- batch$request[[1]]
  testthat::expect_true(is.list(r1$contents))
  testthat::expect_true(is.list(r1$generationConfig))

  # User message should contain SAMPLE_1 / SAMPLE_2 labels in the text
  msg1 <- r1$contents[[1]]
  testthat::expect_equal(msg1$role, "user")
  parts <- msg1$parts
  testthat::expect_true(is.list(parts))
  text_block <- parts[[1]]$text

  # We now just require that the labels SAMPLE_1 / SAMPLE_2 appear somewhere,
  # not necessarily wrapped in angle brackets.
  testthat::expect_true(grepl("SAMPLE_1", text_block, fixed = TRUE))
  testthat::expect_true(grepl("SAMPLE_2", text_block, fixed = TRUE))
})

testthat::test_that("parse_gemini_batch_output handles succeeded and errored
                    results", {
  tmp <- tempfile(fileext = ".jsonl")
  on.exit(unlink(tmp), add = TRUE)

  # Succeeded result line, similar in spirit to live responses
  succ_resp <- list(
    model = "gemini-3-pro-preview",
    candidates = list(
      list(
        content = list(
          parts = list(
            list(
              text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> Hello!"
            )
          )
        )
      )
    ),
    usageMetadata = list(
      promptTokenCount     = 10L,
      candidatesTokenCount = 5L,
      totalTokenCount      = 15L
    )
  )

  line_ok <- list(
    custom_id = "GEM_S01_vs_S02",
    result = list(
      type     = "succeeded",
      response = succ_resp
    )
  )

  # Errored result line
  line_err <- list(
    custom_id = "GEM_S03_vs_S04",
    result = list(
      type = "errored",
      error = list(
        code    = 400L,
        message = "Validation error",
        status  = "INVALID_ARGUMENT"
      )
    )
  )

  json_lines <- c(
    jsonlite::toJSON(line_ok, auto_unbox = TRUE),
    jsonlite::toJSON(line_err, auto_unbox = TRUE)
  )
  writeLines(json_lines, con = tmp, useBytes = TRUE)

  # New API: parse_gemini_batch_output() expects a requests_tbl with
  # custom_id / ID1 / ID2 in the same order as the requests.
  requests_tbl <- tibble::tibble(
    custom_id = c("GEM_S01_vs_S02", "GEM_S03_vs_S04"),
    ID1       = c("S01", "S03"),
    ID2       = c("S02", "S04")
  )

  res <- parse_gemini_batch_output(
    results_path = tmp,
    requests_tbl = requests_tbl
  )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(nrow(res), 2L)

  # First row: succeeded
  r1 <- res[1, ]
  testthat::expect_equal(r1$custom_id, "GEM_S01_vs_S02")
  testthat::expect_equal(r1$ID1, "S01")
  testthat::expect_equal(r1$ID2, "S02")
  testthat::expect_equal(r1$result_type, "succeeded")
  testthat::expect_equal(r1$status_code, 200L)
  testthat::expect_true(is.na(r1$error_message))
  testthat::expect_equal(r1$model, "gemini-3-pro-preview")
  testthat::expect_equal(r1$better_sample, "SAMPLE_2")
  testthat::expect_equal(r1$better_id, "S02")
  testthat::expect_equal(r1$prompt_tokens, 10)
  testthat::expect_equal(r1$completion_tokens, 5)
  testthat::expect_equal(r1$total_tokens, 15)

  # Second row: errored
  r2 <- res[2, ]
  testthat::expect_equal(r2$custom_id, "GEM_S03_vs_S04")
  testthat::expect_equal(r2$ID1, "S03")
  testthat::expect_equal(r2$ID2, "S04")
  testthat::expect_equal(r2$result_type, "errored")
  testthat::expect_true(is.na(r2$status_code))
  testthat::expect_match(r2$error_message, "Validation error")
  testthat::expect_true(is.na(r2$content))
  testthat::expect_true(is.na(r2$better_sample))
  testthat::expect_true(is.na(r2$better_id))
})

testthat::test_that("parse_gemini_batch_output handles invalid JSON lines
                    gracefully", {
  tmp <- tempfile(fileext = ".jsonl")
  on.exit(unlink(tmp), add = TRUE)

  writeLines("not-json", con = tmp, useBytes = TRUE)

  # Even for invalid JSON, we now must pass a requests_tbl; IDs here are dummies.
  requests_tbl <- tibble::tibble(
    custom_id = "GEM_S01_vs_S02",
    ID1       = "S01",
    ID2       = "S02"
  )

  res <- parse_gemini_batch_output(
    results_path = tmp,
    requests_tbl = requests_tbl
  )

  testthat::expect_equal(nrow(res), 1L)
  testthat::expect_true(is.na(res$custom_id))
  testthat::expect_match(res$error_message, "Failed to parse JSON line")
})

testthat::test_that("run_gemini_batch_pipeline works with polling and parsing
                    (mocked)", {
  pairs <- tibble::tibble(
    ID1   = "S01",
    text1 = "Text 1",
    ID2   = "S02",
    text2 = "Text 2"
  )

  # New requests_tbl shape: custom_id + ID1 + ID2 + request
  fake_req_tbl <- tibble::tibble(
    custom_id = "GEM_S01_vs_S02",
    ID1       = "S01",
    ID2       = "S02",
    request   = list(list(dummy = TRUE))
  )

  fake_batch_initial <- list(
    name     = "batches/123",
    metadata = list(state = "JOB_STATE_RUNNING")
  )

  fake_batch_final <- list(
    name     = "batches/123",
    metadata = list(state = "JOB_STATE_SUCCEEDED")
  )

  fake_results <- tibble::tibble(
    custom_id         = "GEM_S01_vs_S02",
    ID1               = "S01",
    ID2               = "S02",
    model             = "gemini-3-pro-preview",
    object_type       = "generateContent",
    status_code       = 200L,
    result_type       = "succeeded",
    error_message     = NA_character_,
    content           = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_1",
    better_id         = "S01",
    prompt_tokens     = 10,
    completion_tokens = 5,
    total_tokens      = 15
  )

  created_batch_name <- NULL
  polled_batch_name <- NULL
  download_batch_obj <- NULL
  parsed_path <- NULL

  td <- list(name = "Overall quality", description = "Quality")
  tmpl <- set_prompt_template()

  testthat::with_mocked_bindings(
    build_gemini_batch_requests = function(pairs, model, trait_name,
                                           trait_description,
                                           prompt_template, thinking_level,
                                           ...) {
      fake_req_tbl
    },
    gemini_create_batch = function(requests, model, api_key, api_version,
                                   display_name = NULL) {
      created_batch_name <<- "batches/123"
      fake_batch_initial
    },
    gemini_poll_batch_until_complete = function(batch_name, interval_seconds,
                                                timeout_seconds, api_key,
                                                api_version, verbose) {
      polled_batch_name <<- batch_name
      fake_batch_final
    },
    gemini_download_batch_results = function(batch, requests_tbl, output_path,
                                             api_key, api_version) {
      download_batch_obj <<- batch
      # Write a dummy .jsonl file so that parse_* can read it
      writeLines('{"dummy": true}', con = output_path)
      invisible(output_path)
    },
    # New signature: parse_gemini_batch_output(results_path, requests_tbl)
    parse_gemini_batch_output = function(results_path, requests_tbl) {
      parsed_path <<- results_path
      fake_results
    },
    {
      res <- run_gemini_batch_pipeline(
        pairs             = pairs,
        model             = "gemini-3-pro-preview",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        thinking_level    = "low",
        interval_seconds  = 0,
        timeout_seconds   = 10,
        verbose           = FALSE
      )

      testthat::expect_equal(created_batch_name, "batches/123")
      testthat::expect_equal(polled_batch_name, "batches/123")
      testthat::expect_identical(download_batch_obj, fake_batch_final)
      testthat::expect_true(file.exists(res$batch_input_path))
      testthat::expect_true(file.exists(res$batch_output_path))
      testthat::expect_true(file.exists(parsed_path))

      # Return structure should mirror other batch pipelines
      testthat::expect_true(all(c(
        "batch_input_path", "batch_output_path", "file", "batch", "results"
      ) %in% names(res)))

      testthat::expect_null(res$file)
      testthat::expect_equal(res$batch$metadata$state, "JOB_STATE_SUCCEEDED")
      testthat::expect_equal(res$results$better_id, "S01")
    }
  )
})

testthat::test_that("run_gemini_batch_pipeline does not poll or parse when
                    poll = FALSE", {
  pairs <- tibble::tibble(
    ID1   = "S01",
    text1 = "Text 1",
    ID2   = "S02",
    text2 = "Text 2"
  )

  fake_req_tbl <- tibble::tibble(
    custom_id = "GEM_S01_vs_S02",
    ID1       = "S01",
    ID2       = "S02",
    request   = list(list(dummy = TRUE))
  )

  fake_batch_initial <- list(
    name     = "batches/123",
    metadata = list(state = "JOB_STATE_RUNNING")
  )

  poll_called <- FALSE
  download_called <- FALSE
  parse_called <- FALSE

  td <- list(name = "Overall quality", description = "Quality")
  tmpl <- set_prompt_template()

  testthat::with_mocked_bindings(
    build_gemini_batch_requests = function(pairs, model, trait_name,
                                           trait_description,
                                           prompt_template, thinking_level,
                                           ...) {
      fake_req_tbl
    },
    gemini_create_batch = function(requests, model, api_key, api_version,
                                   display_name = NULL) {
      fake_batch_initial
    },
    gemini_poll_batch_until_complete = function(batch_name, interval_seconds,
                                                timeout_seconds, api_key,
                                                api_version, verbose) {
      poll_called <<- TRUE
      stop("Polling should not be called when poll = FALSE")
    },
    gemini_download_batch_results = function(batch, requests_tbl, output_path,
                                             api_key, api_version) {
      download_called <<- TRUE
      stop("Download should not be called when poll = FALSE")
    },
    # New signature: parse_gemini_batch_output(results_path, requests_tbl)
    parse_gemini_batch_output = function(results_path, requests_tbl) {
      parse_called <<- TRUE
      stop("Parse should not be called when poll = FALSE")
    },
    {
      res <- run_gemini_batch_pipeline(
        pairs             = pairs,
        model             = "gemini-3-pro-preview",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        thinking_level    = "low",
        poll              = FALSE
      )

      testthat::expect_false(poll_called)
      testthat::expect_false(download_called)
      testthat::expect_false(parse_called)

      testthat::expect_true(file.exists(res$batch_input_path))
      testthat::expect_null(res$batch_output_path)
      testthat::expect_null(res$results)

      # Standardised return structure
      testthat::expect_true(all(c(
        "batch_input_path", "batch_output_path", "file", "batch", "results"
      ) %in% names(res)))
      testthat::expect_null(res$file)
      testthat::expect_equal(res$batch$metadata$state, "JOB_STATE_RUNNING")
    }
  )
})
