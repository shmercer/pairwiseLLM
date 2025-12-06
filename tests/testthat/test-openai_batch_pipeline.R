testthat::test_that("run_openai_batch_pipeline works with polling and parsing", {
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

testthat::test_that("run_openai_batch_pipeline does not poll or parse when poll = FALSE", {
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
