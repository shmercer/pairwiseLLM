library(tibble)

test_that("llm_submit_pairs_multi_batch splits pairs correctly and writes registry", {
  # Prepare a small set of pairs
  pairs <- tibble::tibble(
    ID1 = c("A", "B", "C", "D"),
    text1 = c("a", "b", "c", "d"),
    ID2 = c("E", "F", "G", "H"),
    text2 = c("e", "f", "g", "h")
  )
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  out_dir <- tempfile("test_multi_batch_")
  # Stub provider pipelines to avoid network and return predictable IDs
  fake_pipeline_openai <- function(..., poll) {
    list(
      batch_input_path  = list(...)[["batch_input_path"]],
      batch_output_path = list(...)[["batch_output_path"]],
      file              = NULL,
      batch             = list(id = sprintf("openai-%04d", sample(1:9999, 1))),
      results           = NULL
    )
  }
  # Use the same fake pipeline for all providers in this test
  with_mocked_bindings(
    run_openai_batch_pipeline     = fake_pipeline_openai,
    run_anthropic_batch_pipeline  = fake_pipeline_openai,
    run_gemini_batch_pipeline     = fake_pipeline_openai,
    {
      res <- llm_submit_pairs_multi_batch(
        pairs             = pairs,
        model             = "fake-model",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "openai",
        n_segments        = 2L,
        output_dir        = out_dir,
        write_registry    = TRUE,
        verbose           = TRUE
      )
      # Expect two segments of two pairs each
      expect_equal(length(res$jobs), 2L)
      # Registry file should exist
      expect_true(file.exists(file.path(out_dir, "jobs_registry.csv")))
      # Registry tibble has expected columns
      expect_true(all(c(
        "segment_index", "provider", "model", "batch_id",
        "batch_input_path", "batch_output_path", "csv_path", "done"
      ) %in% names(res$registry)))
    }
  )
})

# Additional validation tests ---------------------------------------------------

test_that("llm_submit_pairs_multi_batch validates batch_size and n_segments inputs", {
  pairs <- tibble::tibble(
    ID1 = c("A", "B"),
    text1 = c("a", "b"),
    ID2 = c("C", "D"),
    text2 = c("c", "d")
  )
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  # Use fake pipeline to avoid network
  fake_pipeline <- function(..., poll) {
    list(
      batch_input_path  = list(...)[["batch_input_path"]],
      batch_output_path = list(...)[["batch_output_path"]],
      file              = NULL,
      batch             = list(id = "fake-batch-id"),
      results           = NULL
    )
  }
  with_mocked_bindings(
    run_openai_batch_pipeline    = fake_pipeline,
    run_anthropic_batch_pipeline = fake_pipeline,
    run_gemini_batch_pipeline    = fake_pipeline,
    {
      # Error when both batch_size and n_segments are supplied
      expect_error(
        llm_submit_pairs_multi_batch(
          pairs             = pairs,
          model             = "fake-model",
          trait_name        = td$name,
          trait_description = td$description,
          prompt_template   = tmpl,
          backend           = "openai",
          batch_size        = 1L,
          n_segments        = 1L,
          output_dir        = tempdir(),
          verbose           = FALSE
        ),
        "Specify only one of 'batch_size' or 'n_segments'"
      )
      # Error when neither batch_size nor n_segments is supplied
      expect_error(
        llm_submit_pairs_multi_batch(
          pairs             = pairs,
          model             = "fake-model",
          trait_name        = td$name,
          trait_description = td$description,
          prompt_template   = tmpl,
          backend           = "openai",
          output_dir        = tempdir(),
          verbose           = FALSE
        ),
        "Either 'batch_size' or 'n_segments' must be supplied"
      )
      # Error when unsupported backend provided
      # The first argument of match.arg throws an error like
      # "'arg' should be one of \"openai\", \"anthropic\", \"gemini\""
      expect_error(
        llm_submit_pairs_multi_batch(
          pairs             = pairs,
          model             = "fake-model",
          trait_name        = td$name,
          trait_description = td$description,
          prompt_template   = tmpl,
          backend           = "invalid-provider",
          n_segments        = 1L,
          output_dir        = tempdir(),
          verbose           = FALSE
        ),
        "should be one of"
      )
    }
  )
})

test_that("llm_resume_multi_batches processes OpenAI jobs and cleans up JSON files", {
  # Create a dummy job list for OpenAI
  input_path  <- tempfile(fileext = ".jsonl")
  output_path <- tempfile(fileext = ".jsonl")
  csv_path    <- tempfile(fileext = ".csv")
  jobs <- list(list(
    segment_index     = 1L,
    provider          = "openai",
    model             = "fake-model",
    batch_id          = "openai-001",
    batch_input_path  = input_path,
    batch_output_path = output_path,
    csv_path          = csv_path,
    done              = FALSE,
    results           = NULL
  ))
  # Create dummy input JSONL for completeness
  writeLines("{}", con = input_path)
  with_mocked_bindings(
    openai_get_batch = function(id) list(status = "completed"),
    openai_download_batch_output = function(batch_id, path) {
      # Write a simple JSON object to the path
      writeLines('[{"custom_id":"c1","ID1":"A","ID2":"B"}]', path)
      invisible(NULL)
    },
    parse_openai_batch_output = function(path) {
      tibble::tibble(
        custom_id = "c1",
        ID1 = "A",
        ID2 = "B",
        result_type = "succeeded",
        error_message = NA_character_
      )
    },
    {
      res <- llm_resume_multi_batches(
        jobs              = jobs,
        interval_seconds  = 0,
        per_job_delay     = 0,
        write_results_csv = FALSE,
        keep_jsonl        = FALSE,
        verbose           = TRUE
      )
      # Expect the job to be marked done
      expect_true(res$jobs[[1]]$done)
      # Combined results should have one row
      expect_s3_class(res$combined, "tbl_df")
      expect_equal(nrow(res$combined), 1L)
      # JSON files should be removed when keep_jsonl = FALSE
      expect_false(file.exists(input_path))
      expect_false(file.exists(output_path))
    }
  )
})

test_that("llm_resume_multi_batches processes Anthropic jobs and writes results CSV", {
  # Create a dummy job list for Anthropic
  input_path  <- tempfile(fileext = ".jsonl")
  output_path <- tempfile(fileext = ".jsonl")
  csv_path    <- tempfile(fileext = ".csv")
  jobs <- list(list(
    segment_index     = 1L,
    provider          = "anthropic",
    model             = "fake-model",
    batch_id          = "anthropic-001",
    batch_input_path  = input_path,
    batch_output_path = output_path,
    csv_path          = csv_path,
    done              = FALSE,
    results           = NULL
  ))
  writeLines("{}", con = input_path)
  with_mocked_bindings(
    anthropic_get_batch = function(id) list(processing_status = "ended"),
    anthropic_download_batch_results = function(batch_id, output_path) {
      # Write dummy JSONL line
      writeLines('[{"custom_id":"c1","ID1":"X","ID2":"Y"}]', output_path)
      return(output_path)
    },
    parse_anthropic_batch_output = function(jsonl_path, tag_prefix, tag_suffix) {
      tibble::tibble(
        custom_id = "c1",
        ID1 = "X",
        ID2 = "Y",
        result_type = "succeeded",
        error_message = NA_character_
      )
    },
    {
      res <- llm_resume_multi_batches(
        jobs              = jobs,
        interval_seconds  = 0,
        per_job_delay     = 0,
        write_results_csv = TRUE,
        keep_jsonl        = TRUE,
        verbose           = FALSE
      )
      # Job should be done
      expect_true(res$jobs[[1]]$done)
      # CSV file should be created
      expect_true(file.exists(csv_path))
      # Combined results should have one row
      expect_equal(nrow(res$combined), 1L)
    }
  )
})

test_that("llm_resume_multi_batches processes Gemini jobs and reconstructs requests", {
  # Create a temporary directory for batch files
  tmpdir <- tempfile("gemini_multi_batch_test_")
  dir.create(tmpdir, recursive = TRUE)
  input_path  <- file.path(tmpdir, "batch_01_input.jsonl")
  output_path <- file.path(tmpdir, "batch_01_output.jsonl")
  csv_path    <- file.path(tmpdir, "batch_01_results.csv")
  # Prepare input JSON with a requests list
  req_payload <- list(
    requests = list(
      list(custom_id = "c1", ID1 = "ID_A", ID2 = "ID_B", request = list())
    )
  )
  jsonlite::write_json(req_payload, path = input_path, auto_unbox = TRUE)
  jobs <- list(list(
    segment_index     = 1L,
    provider          = "gemini",
    model             = "fake-model",
    batch_id          = "batches/test-gem-001",
    batch_input_path  = input_path,
    batch_output_path = output_path,
    csv_path          = csv_path,
    done              = FALSE,
    results           = NULL
  ))
  with_mocked_bindings(
    gemini_get_batch = function(batch_name) {
      list(metadata = list(state = "BATCH_STATE_SUCCEEDED"))
    },
    gemini_download_batch_results = function(batch, requests_tbl, output_path) {
      # Write a JSONL line for parse function
      line <- jsonlite::toJSON(
        list(
          custom_id = "c1",
          result = list(
            type     = "succeeded",
            response = list() # simplified response
          )
        ),
        auto_unbox = TRUE, null = "null"
      )
      writeLines(line, output_path)
      invisible(output_path)
    },
    parse_gemini_batch_output = function(results_path, requests_tbl) {
      tibble::tibble(
        custom_id     = "c1",
        ID1           = "ID_A",
        ID2           = "ID_B",
        result_type   = "succeeded",
        error_message = NA_character_
      )
    },
    {
      res <- llm_resume_multi_batches(
        jobs              = jobs,
        interval_seconds  = 0,
        per_job_delay     = 0,
        write_results_csv = FALSE,
        keep_jsonl        = TRUE,
        verbose           = FALSE
      )
      # Job done
      expect_true(res$jobs[[1]]$done)
      # Combined results row
      expect_equal(nrow(res$combined), 1L)
      expect_identical(res$combined$custom_id[1], "c1")
    }
  )
})

test_that("llm_resume_multi_batches loads jobs from registry when jobs is NULL", {
  # Construct a jobs tibble and write registry
  tmpdir <- tempfile("multi_batch_registry_")
  dir.create(tmpdir, recursive = TRUE)
  registry <- tibble::tibble(
    segment_index     = 1L,
    provider          = "openai",
    model             = "fake-model",
    batch_id          = "openai-xyz",
    batch_input_path  = tempfile(fileext = ".jsonl"),
    batch_output_path = tempfile(fileext = ".jsonl"),
    csv_path          = tempfile(fileext = ".csv"),
    done              = FALSE
  )
  readr::write_csv(registry, file.path(tmpdir, "jobs_registry.csv"))
  # Stub functions to complete immediately
  with_mocked_bindings(
    openai_get_batch = function(id) list(status = "completed"),
    openai_download_batch_output = function(batch_id, path) {
      writeLines('[{"custom_id":"c1","ID1":"A","ID2":"B"}]', path)
    },
    parse_openai_batch_output = function(path) {
      tibble::tibble(
        custom_id = "c1",
        ID1       = "A",
        ID2       = "B",
        result_type   = "succeeded",
        error_message = NA_character_
      )
    },
    {
      res <- llm_resume_multi_batches(
        jobs              = NULL,
        output_dir        = tmpdir,
        interval_seconds  = 0,
        per_job_delay     = 0,
        write_results_csv = FALSE,
        keep_jsonl        = TRUE,
        verbose           = FALSE
      )
      expect_equal(length(res$jobs), 1L)
      expect_true(res$jobs[[1]]$done)
      expect_equal(nrow(res$combined), 1L)
    }
  )
})

test_that("llm_resume_multi_batches writes combined results CSV when requested", {
  # Prepare a dummy job list with a single OpenAI job that immediately completes
  input_path  <- tempfile(fileext = ".jsonl")
  output_path <- tempfile(fileext = ".jsonl")
  jobs <- list(list(
    segment_index     = 1L,
    provider          = "openai",
    model             = "fake-model",
    batch_id          = "openai-100",
    batch_input_path  = input_path,
    batch_output_path = output_path,
    csv_path          = tempfile(fileext = ".csv"),
    done              = FALSE,
    results           = NULL
  ))
  # Create dummy input file
  writeLines("{}", con = input_path)
  # Destination directory and combined CSV path
  out_dir <- tempfile("multi_batch_combined_")
  dir.create(out_dir, recursive = TRUE)
  combined_name <- "final_results.csv"
  comb_path <- file.path(out_dir, combined_name)
  # Stub OpenAI functions
  with_mocked_bindings(
    openai_get_batch = function(id) list(status = "completed"),
    openai_download_batch_output = function(batch_id, path) {
      writeLines('[{"custom_id":"c1","ID1":"A","ID2":"B"}]', path)
    },
    parse_openai_batch_output = function(path) {
      tibble::tibble(
        custom_id = "c1",
        ID1 = "A",
        ID2 = "B",
        result_type = "succeeded",
        error_message = NA_character_
      )
    },
    {
      res <- llm_resume_multi_batches(
        jobs               = jobs,
        output_dir         = out_dir,
        interval_seconds   = 0,
        per_job_delay      = 0,
        write_results_csv  = FALSE,
        keep_jsonl         = TRUE,
        verbose            = FALSE,
        write_combined_csv = TRUE,
        combined_csv_path  = combined_name
      )
      # Combined results CSV should exist
      expect_true(file.exists(comb_path))
      # Read the file and check number of rows
      df <- suppressMessages(readr::read_csv(comb_path))
      expect_equal(nrow(df), 1L)
      expect_identical(df$custom_id[1], "c1")
      # Combined tibble still returned in res$combined
      expect_equal(nrow(res$combined), 1L)
    }
  )
})

# Additional tests to improve coverage of retry logic and file handling

test_that("openai batch submission retries on 5xx errors and eventually succeeds", {
  # Prepare minimal pairs for a single segment
  pairs <- tibble::tibble(
    ID1 = "X", text1 = "x", ID2 = "Y", text2 = "y"
  )
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  out_dir <- tempfile("openai_retry_success_")
  # Create a stub that fails once with a 500 error, then succeeds
  attempt_counter <- 0L
  fake_pipeline <- function(..., poll) {
    attempt_counter <<- attempt_counter + 1L
    if (attempt_counter == 1L) {
      # simulate an httr2 500 error
      err <- structure(list(message = "Internal Server Error"), class = c("httr2_http_500", "error", "condition"))
      stop(err)
    }
    list(
      batch_input_path  = list(...)[["batch_input_path"]],
      batch_output_path = list(...)[["batch_output_path"]],
      file              = NULL,
      batch             = list(id = "openai-retry-success"),
      results           = NULL
    )
  }
  with_mocked_bindings(
    run_openai_batch_pipeline     = fake_pipeline,
    run_anthropic_batch_pipeline  = fake_pipeline,
    run_gemini_batch_pipeline     = fake_pipeline,
    {
      res <- llm_submit_pairs_multi_batch(
        pairs             = pairs,
        model             = "fake-model",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "openai",
        n_segments        = 1L,
        output_dir        = out_dir,
        write_registry    = FALSE,
        verbose           = FALSE,
        openai_max_retries = 2L
      )
      # Should have succeeded on the second attempt
      expect_equal(length(res$jobs), 1L)
      expect_identical(res$jobs[[1]]$batch_id, "openai-retry-success")
    }
  )
})

test_that("openai batch submission fails after maximum retries", {
  # Minimal pairs
  pairs <- tibble::tibble(
    ID1 = "X", text1 = "x", ID2 = "Y", text2 = "y"
  )
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  out_dir <- tempfile("openai_retry_fail_")
  # Stub always throwing 5xx error
  always_fail <- function(..., poll) {
    err <- structure(list(message = "Internal Server Error"), class = c("httr2_http_500", "error", "condition"))
    stop(err)
  }
  with_mocked_bindings(
    run_openai_batch_pipeline    = always_fail,
    run_anthropic_batch_pipeline = always_fail,
    run_gemini_batch_pipeline    = always_fail,
    {
      expect_error(
        llm_submit_pairs_multi_batch(
          pairs             = pairs,
          model             = "fake-model",
          trait_name        = td$name,
          trait_description = td$description,
          prompt_template   = tmpl,
          backend           = "openai",
          n_segments        = 1L,
          output_dir        = out_dir,
          openai_max_retries = 2L,
          verbose           = FALSE
        ),
        "Failed to create OpenAI batch"
      )
    }
  )
})

test_that("openai download retries on 5xx errors and succeeds", {
  # Setup job list for a single openai batch
  input_path  <- tempfile(fileext = ".jsonl")
  output_path <- tempfile(fileext = ".jsonl")
  csv_path    <- tempfile(fileext = ".csv")
  jobs <- list(list(
    segment_index     = 1L,
    provider          = "openai",
    model             = "fake-model",
    batch_id          = "openai-download-retry",
    batch_input_path  = input_path,
    batch_output_path = output_path,
    csv_path          = csv_path,
    done              = FALSE,
    results           = NULL
  ))
  # Create dummy input JSONL file
  writeLines("{}", con = input_path)
  # Stub batch status and download with one failure then success
  download_attempt <- 0L
  with_mocked_bindings(
    openai_get_batch = function(id) list(status = "completed"),
    openai_download_batch_output = function(batch_id, path) {
      download_attempt <<- download_attempt + 1L
      if (download_attempt == 1L) {
        err <- structure(list(message = "Download failed"), class = c("httr2_http_500", "error", "condition"))
        stop(err)
      } else {
        writeLines('[{"custom_id":"x","ID1":"A","ID2":"B"}]', path)
      }
      invisible(NULL)
    },
    parse_openai_batch_output = function(path) {
      tibble::tibble(
        custom_id = "x",
        ID1 = "A",
        ID2 = "B",
        result_type   = "succeeded",
        error_message = NA_character_
      )
    },
    {
      res <- llm_resume_multi_batches(
        jobs              = jobs,
        interval_seconds  = 0,
        per_job_delay     = 0,
        write_results_csv = FALSE,
        keep_jsonl        = FALSE,
        verbose           = FALSE,
        openai_max_retries = 2L
      )
      # Should be marked done and results captured
      expect_true(res$jobs[[1]]$done)
      expect_equal(nrow(res$combined), 1L)
      expect_identical(res$combined$custom_id[1], "x")
      # Input and output JSONL files should be deleted due to keep_jsonl=FALSE
      expect_false(file.exists(input_path))
      expect_false(file.exists(output_path))
    }
  )
})

test_that("llm_resume_multi_batches writes combined results CSV to absolute path", {
  # Prepare a dummy openai job
  input_path  <- tempfile(fileext = ".jsonl")
  output_path <- tempfile(fileext = ".jsonl")
  csv_path    <- tempfile(fileext = ".csv")
  jobs <- list(list(
    segment_index     = 1L,
    provider          = "openai",
    model             = "fake-model",
    batch_id          = "openai-combined",
    batch_input_path  = input_path,
    batch_output_path = output_path,
    csv_path          = csv_path,
    done              = FALSE,
    results           = NULL
  ))
  writeLines("{}", con = input_path)
  # Create a temporary directory and subdirectory for combined output
  tmp <- tempfile("combined_abs_dir_")
  dir.create(tmp, recursive = TRUE)
  subdir <- file.path(tmp, "sub")
  dir.create(subdir, recursive = TRUE)
  abs_path <- file.path(subdir, "combined.csv")
  # Stub functions
  with_mocked_bindings(
    openai_get_batch = function(id) list(status = "completed"),
    openai_download_batch_output = function(batch_id, path) {
      writeLines('[{"custom_id":"x","ID1":"A","ID2":"B"}]', path)
      invisible(NULL)
    },
    parse_openai_batch_output = function(path) {
      tibble::tibble(
        custom_id = "x",
        ID1 = "A",
        ID2 = "B",
        result_type = "succeeded",
        error_message = NA_character_
      )
    },
    {
      res <- llm_resume_multi_batches(
        jobs               = jobs,
        output_dir         = tmp,
        interval_seconds   = 0,
        per_job_delay      = 0,
        write_results_csv  = FALSE,
        keep_jsonl         = TRUE,
        verbose            = FALSE,
        write_combined_csv = TRUE,
        combined_csv_path  = abs_path
      )
      # Combined CSV should exist at the absolute path
      expect_true(file.exists(abs_path))
      # Combined tibble should be returned
      expect_equal(nrow(res$combined), 1L)
    }
  )
})

test_that("llm_resume_multi_batches updates registry when write_registry=TRUE", {
  # Create initial registry and jobs list
  tmpdir <- tempfile("registry_update_")
  dir.create(tmpdir, recursive = TRUE)
  pairs <- tibble::tibble(
    ID1 = "p", text1 = "a", ID2 = "q", text2 = "b"
  )
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  # Create single job via submit (fake pipeline to avoid network)
  fake_pipeline <- function(..., poll) {
    list(
      batch_input_path  = list(...)[["batch_input_path"]],
      batch_output_path = list(...)[["batch_output_path"]],
      file              = NULL,
      batch             = list(id = "openai-registry-update"),
      results           = NULL
    )
  }
  with_mocked_bindings(
    run_openai_batch_pipeline     = fake_pipeline,
    run_anthropic_batch_pipeline  = fake_pipeline,
    run_gemini_batch_pipeline     = fake_pipeline,
    {
      sub_res <- llm_submit_pairs_multi_batch(
        pairs             = pairs,
        model             = "fake",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "openai",
        n_segments        = 1L,
        output_dir        = tmpdir,
        write_registry    = TRUE,
        verbose           = FALSE
      )
    }
  )
  # Now stub OpenAI functions for resume
  with_mocked_bindings(
    openai_get_batch = function(id) list(status = "completed"),
    openai_download_batch_output = function(batch_id, path) {
      writeLines('[{"custom_id":"z","ID1":"1","ID2":"2"}]', path)
      invisible(NULL)
    },
    parse_openai_batch_output = function(path) {
      tibble::tibble(
        custom_id = "z",
        ID1 = "1",
        ID2 = "2",
        result_type = "succeeded",
        error_message = NA_character_
      )
    },
    {
      res <- llm_resume_multi_batches(
        jobs              = NULL,
        output_dir        = tmpdir,
        interval_seconds  = 0,
        per_job_delay     = 0,
        write_results_csv = FALSE,
        keep_jsonl        = TRUE,
        write_registry    = TRUE,
        verbose           = FALSE
      )
      # After resume, registry should reflect done = TRUE
      reg_path <- file.path(tmpdir, "jobs_registry.csv")
      tbl <- suppressMessages(readr::read_csv(reg_path))
      expect_true(all(tbl$done))
      # Combined results should have one row
      expect_equal(nrow(res$combined), 1L)
      # Jobs list should be updated
      expect_true(res$jobs[[1]]$done)
    }
  )
})

test_that("custom tag_prefix and tag_suffix are forwarded to parse_anthropic_batch_output", {
  # Prepare an anthropic job
  input_path  <- tempfile(fileext = ".jsonl")
  output_path <- tempfile(fileext = ".jsonl")
  csv_path    <- tempfile(fileext = ".csv")
  jobs <- list(list(
    segment_index     = 1L,
    provider          = "anthropic",
    model             = "fake",
    batch_id          = "anthropic-tag-test",
    batch_input_path  = input_path,
    batch_output_path = output_path,
    csv_path          = csv_path,
    done              = FALSE,
    results           = NULL
  ))
  writeLines("{}", con = input_path)
  # Capture the tags passed into parse
  captured_args <- list()
  with_mocked_bindings(
    anthropic_get_batch = function(id) list(processing_status = "ended"),
    anthropic_download_batch_results = function(batch_id, output_path) {
      writeLines('[{"custom_id":"c","ID1":"i","ID2":"j"}]', output_path)
      return(output_path)
    },
    parse_anthropic_batch_output = function(jsonl_path, tag_prefix, tag_suffix) {
      captured_args$prefix <<- tag_prefix
      captured_args$suffix <<- tag_suffix
      tibble::tibble(
        custom_id = "c",
        ID1 = "i",
        ID2 = "j",
        result_type = "succeeded",
        error_message = NA_character_
      )
    },
    {
      res <- llm_resume_multi_batches(
        jobs              = jobs,
        interval_seconds  = 0,
        per_job_delay     = 0,
        write_results_csv = FALSE,
        keep_jsonl        = TRUE,
        tag_prefix        = "<<",
        tag_suffix        = ">>",
        verbose           = FALSE
      )
      # Ensure the custom tags were forwarded
      expect_identical(captured_args$prefix, "<<")
      expect_identical(captured_args$suffix, ">>")
      expect_true(res$jobs[[1]]$done)
      expect_equal(nrow(res$combined), 1L)
    }
  )
})

test_that("llm_resume_multi_batches validates inputs when jobs and output_dir missing", {
  # Expect error if both jobs and output_dir are NULL
  expect_error(
    llm_resume_multi_batches(
      jobs              = NULL,
      output_dir        = NULL,
      interval_seconds  = 0,
      per_job_delay     = 0,
      write_results_csv = FALSE,
      keep_jsonl        = TRUE
    ),
    "Either 'jobs' must be supplied or 'output_dir' must be provided"
  )
})
