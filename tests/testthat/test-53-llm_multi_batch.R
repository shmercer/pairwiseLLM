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
    run_openai_batch_pipeline = fake_pipeline_openai,
    run_anthropic_batch_pipeline = fake_pipeline_openai,
    run_gemini_batch_pipeline = fake_pipeline_openai,
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
    run_openai_batch_pipeline = fake_pipeline,
    run_anthropic_batch_pipeline = fake_pipeline,
    run_gemini_batch_pipeline = fake_pipeline,
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
  input_path <- tempfile(fileext = ".jsonl")
  output_path <- tempfile(fileext = ".jsonl")
  csv_path <- tempfile(fileext = ".csv")
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
  input_path <- tempfile(fileext = ".jsonl")
  output_path <- tempfile(fileext = ".jsonl")
  csv_path <- tempfile(fileext = ".csv")
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
  input_path <- file.path(tmpdir, "batch_01_input.jsonl")
  output_path <- file.path(tmpdir, "batch_01_output.jsonl")
  csv_path <- file.path(tmpdir, "batch_01_results.csv")
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
        ID1 = "A",
        ID2 = "B",
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
  input_path <- tempfile(fileext = ".jsonl")
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
    run_openai_batch_pipeline = fake_pipeline,
    run_anthropic_batch_pipeline = fake_pipeline,
    run_gemini_batch_pipeline = fake_pipeline,
    {
      res <- llm_submit_pairs_multi_batch(
        pairs = pairs,
        model = "fake-model",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        backend = "openai",
        n_segments = 1L,
        output_dir = out_dir,
        write_registry = FALSE,
        verbose = FALSE,
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
    run_openai_batch_pipeline = always_fail,
    run_anthropic_batch_pipeline = always_fail,
    run_gemini_batch_pipeline = always_fail,
    {
      expect_error(
        llm_submit_pairs_multi_batch(
          pairs = pairs,
          model = "fake-model",
          trait_name = td$name,
          trait_description = td$description,
          prompt_template = tmpl,
          backend = "openai",
          n_segments = 1L,
          output_dir = out_dir,
          openai_max_retries = 2L,
          verbose = FALSE
        ),
        "Failed to create OpenAI batch"
      )
    }
  )
})

test_that("openai download retries on 5xx errors and succeeds", {
  # Setup job list for a single openai batch
  input_path <- tempfile(fileext = ".jsonl")
  output_path <- tempfile(fileext = ".jsonl")
  csv_path <- tempfile(fileext = ".csv")
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
        result_type = "succeeded",
        error_message = NA_character_
      )
    },
    {
      res <- llm_resume_multi_batches(
        jobs = jobs,
        interval_seconds = 0,
        per_job_delay = 0,
        write_results_csv = FALSE,
        keep_jsonl = FALSE,
        verbose = FALSE,
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
  input_path <- tempfile(fileext = ".jsonl")
  output_path <- tempfile(fileext = ".jsonl")
  csv_path <- tempfile(fileext = ".csv")
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
    run_openai_batch_pipeline = fake_pipeline,
    run_anthropic_batch_pipeline = fake_pipeline,
    run_gemini_batch_pipeline = fake_pipeline,
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
  input_path <- tempfile(fileext = ".jsonl")
  output_path <- tempfile(fileext = ".jsonl")
  csv_path <- tempfile(fileext = ".csv")
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

# Additional coverage tests ----------------------------------------------------

test_that("llm_submit_pairs_multi_batch distributes segments with n_segments", {
  # Create 5 pairs to split into 3 segments
  pairs <- tibble::tibble(
    ID1 = letters[1:5],
    text1 = letters[1:5],
    ID2 = letters[6:10],
    text2 = letters[6:10]
  )
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  # Vector to capture number of pairs per segment submitted to pipeline
  seg_sizes <- integer(0)
  fake_pipeline <- function(..., poll) {
    args <- list(...)
    # Count number of rows in the pairs passed to the pipeline
    seg_pairs <- args$pairs
    seg_sizes <<- c(seg_sizes, nrow(seg_pairs))
    # Write a simple JSON to the input path so the file exists
    input_json <- list(requests = replicate(nrow(seg_pairs), list(), simplify = FALSE))
    jsonlite::write_json(input_json, args$batch_input_path, auto_unbox = TRUE)
    list(
      batch_input_path  = args$batch_input_path,
      batch_output_path = args$batch_output_path,
      file              = NULL,
      batch             = list(id = paste0("id-", sample(1:999, 1))),
      results           = NULL
    )
  }
  with_mocked_bindings(
    run_openai_batch_pipeline = fake_pipeline,
    run_anthropic_batch_pipeline = fake_pipeline,
    run_gemini_batch_pipeline = fake_pipeline,
    {
      out_dir <- tempfile("seg_n_")
      res <- llm_submit_pairs_multi_batch(
        pairs             = pairs,
        model             = "fake-model",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "openai",
        n_segments        = 3L,
        output_dir        = out_dir,
        write_registry    = FALSE,
        verbose           = FALSE
      )
      # Expect three jobs
      expect_equal(length(res$jobs), 3L)
      # The captured sizes should cover all pairs and distribute remainders
      expect_equal(sum(seg_sizes), 5L)
      expect_true(all(sort(seg_sizes) == c(1L, 2L, 2L)))
    }
  )
})

# New tests to improve coverage of additional branches in llm_multi_batch.R ------------

test_that("llm_submit_pairs_multi_batch splits pairs correctly using batch_size", {
  # Five pairs split by batch_size = 2 should produce three segments (2,2,1)
  pairs <- tibble::tibble(
    ID1 = letters[1:5],
    text1 = letters[1:5],
    ID2 = LETTERS[1:5],
    text2 = LETTERS[1:5]
  )
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  # Capture sizes of each segment and write dummy JSON to input paths
  seg_sizes <- integer(0)
  fake_pipeline <- function(..., poll) {
    args <- list(...)
    seg_pairs <- args$pairs
    seg_sizes <<- c(seg_sizes, nrow(seg_pairs))
    # Create a simple requests list of the appropriate length
    input_json <- list(requests = replicate(nrow(seg_pairs), list(), simplify = FALSE))
    jsonlite::write_json(input_json, args$batch_input_path, auto_unbox = TRUE)
    list(
      batch_input_path  = args$batch_input_path,
      batch_output_path = args$batch_output_path,
      file              = NULL,
      batch             = list(id = paste0("id-", sample(1:999, 1))),
      results           = NULL
    )
  }
  with_mocked_bindings(
    run_openai_batch_pipeline = fake_pipeline,
    run_anthropic_batch_pipeline = fake_pipeline,
    run_gemini_batch_pipeline = fake_pipeline,
    {
      out_dir <- tempfile("seg_bs_")
      res <- llm_submit_pairs_multi_batch(
        pairs             = pairs,
        model             = "fake-model",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "openai",
        batch_size        = 2L,
        output_dir        = out_dir,
        write_registry    = FALSE,
        verbose           = FALSE
      )
      # Expect three segments
      expect_equal(length(res$jobs), 3L)
      # The captured sizes reflect segment distribution
      expect_equal(sum(seg_sizes), 5L)
      expect_true(all(sort(seg_sizes) == c(1L, 2L, 2L)))
    }
  )
})

test_that("llm_submit_pairs_multi_batch routes to anthropic and gemini backends", {
  # Simple two-pair data
  pairs <- tibble::tibble(
    ID1 = c("a", "b"),
    text1 = c("a", "b"),
    ID2 = c("c", "d"),
    text2 = c("c", "d")
  )
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()
  # Stub pipelines return predictable ids/names
  anth_id <- NULL
  gem_name <- NULL
  anth_pipeline <- function(..., poll) {
    anth_id <<- "anth-id-123"
    list(
      batch_input_path  = list(...)[["batch_input_path"]],
      batch_output_path = list(...)[["batch_output_path"]],
      file              = NULL,
      batch             = list(id = anth_id),
      results           = NULL
    )
  }
  gem_pipeline <- function(..., poll) {
    gem_name <<- "batches/gem-name-456"
    list(
      batch_input_path  = list(...)[["batch_input_path"]],
      batch_output_path = list(...)[["batch_output_path"]],
      file              = NULL,
      batch             = list(name = gem_name),
      results           = NULL
    )
  }
  fake_openai <- function(..., poll) {
    list(
      batch_input_path  = list(...)[["batch_input_path"]],
      batch_output_path = list(...)[["batch_output_path"]],
      file              = NULL,
      batch             = list(id = "open-id"),
      results           = NULL
    )
  }
  with_mocked_bindings(
    run_openai_batch_pipeline = fake_openai,
    run_anthropic_batch_pipeline = anth_pipeline,
    run_gemini_batch_pipeline = gem_pipeline,
    {
      dir1 <- tempfile("anth_dir_")
      res1 <- llm_submit_pairs_multi_batch(
        pairs             = pairs,
        model             = "fake",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "anthropic",
        n_segments        = 1L,
        output_dir        = dir1,
        verbose           = FALSE
      )
      expect_identical(res1$jobs[[1]]$batch_id, anth_id)
      dir2 <- tempfile("gem_dir_")
      res2 <- llm_submit_pairs_multi_batch(
        pairs             = pairs,
        model             = "fake",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "gemini",
        n_segments        = 1L,
        output_dir        = dir2,
        verbose           = FALSE
      )
      expect_identical(res2$jobs[[1]]$batch_id, gem_name)
    }
  )
})

test_that("llm_resume_multi_batches marks anthropic jobs done on non-ended status", {
  # Prepare anthropic jobs with non-ended statuses
  statuses <- c("errored", "canceled", "expired")
  for (st in statuses) {
    input_path <- tempfile(fileext = ".jsonl")
    output_path <- tempfile(fileext = ".jsonl")
    csv_path <- tempfile(fileext = ".csv")
    jobs <- list(list(
      segment_index     = 1L,
      provider          = "anthropic",
      model             = "fake",
      batch_id          = paste0("anthrop", st),
      batch_input_path  = input_path,
      batch_output_path = output_path,
      csv_path          = csv_path,
      done              = FALSE,
      results           = NULL
    ))
    writeLines("{}", con = input_path)
    with_mocked_bindings(
      anthropic_get_batch = function(id) list(processing_status = st),
      anthropic_download_batch_results = function(...) {
        stop("should not be called on non-ended status")
      },
      parse_anthropic_batch_output = function(...) {
        stop("should not be called on non-ended status")
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
        expect_true(res$jobs[[1]]$done)
        expect_null(res$jobs[[1]]$results)
      }
    )
  }
})

test_that("llm_resume_multi_batches errors when Gemini requests cannot be reconstructed", {
  # Prepare a gemini job with missing requests list
  tmpdir <- tempfile("gem_missing_req_")
  dir.create(tmpdir, recursive = TRUE)
  input_path <- file.path(tmpdir, "batch_01_input.jsonl")
  output_path <- file.path(tmpdir, "batch_01_output.jsonl")
  csv_path <- file.path(tmpdir, "batch_01_results.csv")
  # Write JSON without requests field
  jsonlite::write_json(list(not_requests = list(list())), path = input_path, auto_unbox = TRUE)
  jobs <- list(list(
    segment_index     = 1L,
    provider          = "gemini",
    model             = "fake",
    batch_id          = "batches/missing",
    batch_input_path  = input_path,
    batch_output_path = output_path,
    csv_path          = csv_path,
    done              = FALSE,
    results           = NULL
  ))
  # Stub gemini_get_batch and others
  with_mocked_bindings(
    gemini_get_batch = function(name) list(metadata = list(state = "BATCH_STATE_SUCCEEDED")),
    gemini_download_batch_results = function(batch, requests_tbl, output_path) {
      writeLines("{}", output_path)
      invisible(NULL)
    },
    parse_gemini_batch_output = function(results_path, requests_tbl) {
      tibble::tibble(custom_id = character(), ID1 = character(), ID2 = character(), result_type = character(), error_message = character())
    },
    {
      expect_error(
        llm_resume_multi_batches(
          jobs              = jobs,
          interval_seconds  = 0,
          per_job_delay     = 0,
          write_results_csv = FALSE,
          keep_jsonl        = TRUE,
          verbose           = FALSE
        ),
        "Failed to reconstruct Gemini requests"
      )
    }
  )
})

test_that("llm_resume_multi_batches handles Gemini top-level state field", {
  # Prepare a gemini job with top-level state field
  tmpdir <- tempfile("gem_top_state_")
  dir.create(tmpdir, recursive = TRUE)
  input_path <- file.path(tmpdir, "batch_01_input.jsonl")
  output_path <- file.path(tmpdir, "batch_01_output.jsonl")
  csv_path <- file.path(tmpdir, "batch_01_results.csv")
  # Write JSON with requests list
  req_payload <- list(requests = list(list(custom_id = "c1", ID1 = "u", ID2 = "v", request = list())))
  jsonlite::write_json(req_payload, path = input_path, auto_unbox = TRUE)
  jobs <- list(list(
    segment_index     = 1L,
    provider          = "gemini",
    model             = "fake",
    batch_id          = "batches/top",
    batch_input_path  = input_path,
    batch_output_path = output_path,
    csv_path          = csv_path,
    done              = FALSE,
    results           = NULL
  ))
  with_mocked_bindings(
    gemini_get_batch = function(name) list(state = "SUCCEEDED"),
    gemini_download_batch_results = function(batch, requests_tbl, output_path) {
      # Write JSONL line to output for parse
      line <- jsonlite::toJSON(
        list(custom_id = "c1", result = list(type = "succeeded", response = list())),
        auto_unbox = TRUE, null = "null"
      )
      writeLines(line, output_path)
      invisible(output_path)
    },
    parse_gemini_batch_output = function(results_path, requests_tbl) {
      tibble::tibble(
        custom_id     = "c1",
        ID1           = "u",
        ID2           = "v",
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
      expect_true(res$jobs[[1]]$done)
      expect_equal(nrow(res$combined), 1L)
      expect_identical(res$combined$custom_id[1], "c1")
    }
  )
})

test_that("llm_resume_multi_batches writes combined CSV to nested relative path", {
  # Create a dummy job that finishes immediately
  input_path <- tempfile(fileext = ".jsonl")
  output_path <- tempfile(fileext = ".jsonl")
  csv_path <- tempfile(fileext = ".csv")
  jobs <- list(list(
    segment_index     = 1L,
    provider          = "openai",
    model             = "fake",
    batch_id          = "open-nested",
    batch_input_path  = input_path,
    batch_output_path = output_path,
    csv_path          = csv_path,
    done              = FALSE,
    results           = NULL
  ))
  writeLines("{}", con = input_path)
  out_dir <- tempfile("combined_nested_")
  dir.create(out_dir, recursive = TRUE)
  # Use a combined_csv_path that contains a directory component within out_dir.
  # Since dirname() != ".", llm_resume_multi_batches treats it as absolute and writes exactly here.
  nested_path <- file.path(out_dir, "subdir1", "subdir2", "comb.csv")
  with_mocked_bindings(
    openai_get_batch = function(id) list(status = "completed"),
    openai_download_batch_output = function(batch_id, path) {
      writeLines('[{"custom_id":"n","ID1":"A","ID2":"B"}]', path)
      invisible(NULL)
    },
    parse_openai_batch_output = function(path) {
      tibble::tibble(
        custom_id = "n",
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
        combined_csv_path  = nested_path
      )
      # Combined file should be written exactly to nested_path
      expect_true(file.exists(nested_path))
      # Combined tibble returned
      expect_equal(nrow(res$combined), 1L)
      expect_identical(res$combined$custom_id[1], "n")
    }
  )
})

test_that("llm_resume_multi_batches propagates unexpected OpenAI download errors", {
  # Prepare job list
  input_path <- tempfile(fileext = ".jsonl")
  output_path <- tempfile(fileext = ".jsonl")
  jobs <- list(list(
    segment_index     = 1L,
    provider          = "openai",
    model             = "fake",
    batch_id          = "open-error",
    batch_input_path  = input_path,
    batch_output_path = output_path,
    csv_path          = tempfile(fileext = ".csv"),
    done              = FALSE,
    results           = NULL
  ))
  writeLines("{}", con = input_path)
  with_mocked_bindings(
    openai_get_batch = function(id) list(status = "completed"),
    openai_download_batch_output = function(batch_id, path) {
      # throw non-HTTP error (not retryable)
      stop("unhandled error")
    },
    parse_openai_batch_output = function(path) {
      stop("should not be called")
    },
    {
      expect_error(
        llm_resume_multi_batches(
          jobs = jobs,
          interval_seconds = 0,
          per_job_delay = 0,
          write_results_csv = FALSE,
          keep_jsonl = TRUE,
          verbose = FALSE,
          openai_max_retries = 2L
        ),
        "unhandled error"
      )
    }
  )
})

test_that("llm_resume_multi_batches writes combined CSV to default path", {
  # Create a simple job that completes immediately
  input_path <- tempfile(fileext = ".jsonl")
  output_path <- tempfile(fileext = ".jsonl")
  csv_path <- tempfile(fileext = ".csv")
  jobs <- list(list(
    segment_index     = 1L,
    provider          = "openai",
    model             = "fake",
    batch_id          = "openai-default-combined",
    batch_input_path  = input_path,
    batch_output_path = output_path,
    csv_path          = csv_path,
    done              = FALSE,
    results           = NULL
  ))
  writeLines("{}", con = input_path)
  out_dir <- tempfile("combined_default_")
  dir.create(out_dir, recursive = TRUE)
  with_mocked_bindings(
    openai_get_batch = function(id) list(status = "completed"),
    openai_download_batch_output = function(batch_id, path) {
      writeLines('[{"custom_id":"def","ID1":"A","ID2":"B"}]', path)
      invisible(NULL)
    },
    parse_openai_batch_output = function(path) {
      tibble::tibble(
        custom_id = "def",
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
        write_combined_csv = TRUE
      )
      # Combined file should be at default path
      default_combined <- file.path(out_dir, "combined_results.csv")
      expect_true(file.exists(default_combined))
      df <- suppressMessages(readr::read_csv(default_combined))
      expect_equal(nrow(df), 1L)
      expect_identical(df$custom_id[1], "def")
      # Combined tibble returned
      expect_equal(nrow(res$combined), 1L)
    }
  )
})

test_that("llm_resume_multi_batches errors when registry is missing", {
  # Use a temporary directory without a registry file
  tmpdir <- tempfile("no_registry_")
  dir.create(tmpdir, recursive = TRUE)
  expect_error(
    llm_resume_multi_batches(
      jobs              = NULL,
      output_dir        = tmpdir,
      interval_seconds  = 0,
      per_job_delay     = 0
    ),
    "No registry file found"
  )
})

test_that("llm_resume_multi_batches errors on unsupported provider", {
  # Create a job with an unsupported provider name
  jobs <- list(list(
    segment_index     = 1L,
    provider          = "unknown",
    model             = "fake",
    batch_id          = "bad",
    batch_input_path  = tempfile(fileext = ".jsonl"),
    batch_output_path = tempfile(fileext = ".jsonl"),
    csv_path          = tempfile(fileext = ".csv"),
    done              = FALSE,
    results           = NULL
  ))
  expect_error(
    llm_resume_multi_batches(
      jobs             = jobs,
      interval_seconds = 0,
      per_job_delay    = 0
    ),
    "Unsupported provider"
  )
})

test_that("llm_resume_multi_batches handles OpenAI non‑terminal statuses", {
  # Simulate an OpenAI job that first returns validating and then fails
  input_path <- tempfile(fileext = ".jsonl")
  output_path <- tempfile(fileext = ".jsonl")
  jobs <- list(list(
    segment_index     = 1L,
    provider          = "openai",
    model             = "fake",
    batch_id          = "openai-nonterminal",
    batch_input_path  = input_path,
    batch_output_path = output_path,
    csv_path          = tempfile(fileext = ".csv"),
    done              = FALSE,
    results           = NULL
  ))
  # Write dummy input
  writeLines("{}", con = input_path)
  # Create generator for statuses: validating -> failed
  status_sequence <- c("validating", "failed")
  call_count <- 0L
  with_mocked_bindings(
    openai_get_batch = function(id) {
      call_count <<- call_count + 1L
      list(status = status_sequence[min(call_count, length(status_sequence))])
    },
    openai_download_batch_output = function(batch_id, path) {
      stop("download should not be called when job failed")
    },
    {
      res <- llm_resume_multi_batches(
        jobs = jobs,
        interval_seconds = 0,
        per_job_delay = 0,
        write_results_csv = FALSE,
        keep_jsonl = TRUE,
        verbose = FALSE
      )
      # Job should be marked done after failure
      expect_true(res$jobs[[1]]$done)
      # Results should remain NULL because it failed
      expect_null(res$jobs[[1]]$results)
    }
  )
})

test_that("llm_resume_multi_batches handles Gemini failure states and cleans up", {
  # Prepare a gemini job that returns a failed state
  tmpdir <- tempfile("gemini_fail_")
  dir.create(tmpdir, recursive = TRUE)
  input_path <- file.path(tmpdir, "batch_01_input.jsonl")
  output_path <- file.path(tmpdir, "batch_01_output.jsonl")
  csv_path <- file.path(tmpdir, "batch_01_results.csv")
  # Write a requests list with one request to input
  req_payload <- list(requests = list(list(custom_id = "c", ID1 = "A", ID2 = "B", request = list())))
  jsonlite::write_json(req_payload, path = input_path, auto_unbox = TRUE)
  jobs <- list(list(
    segment_index     = 1L,
    provider          = "gemini",
    model             = "fake",
    batch_id          = "batches/fail",
    batch_input_path  = input_path,
    batch_output_path = output_path,
    csv_path          = csv_path,
    done              = FALSE,
    results           = NULL
  ))
  with_mocked_bindings(
    gemini_get_batch = function(name) list(metadata = list(state = "BATCH_STATE_FAILED")),
    gemini_download_batch_results = function(batch, requests_tbl, output_path) {
      stop("download should not be called on failure")
    },
    parse_gemini_batch_output = function(results_path, requests_tbl) {
      stop("parse should not be called on failure")
    },
    {
      res <- llm_resume_multi_batches(
        jobs = jobs,
        interval_seconds = 0,
        per_job_delay = 0,
        write_results_csv = FALSE,
        keep_jsonl = FALSE,
        verbose = FALSE
      )
      # Job should be done and results should remain NULL
      expect_true(res$jobs[[1]]$done)
      expect_null(res$jobs[[1]]$results)
      # Input JSON should still exist (we wrote it), but output JSON is never created on failure
      expect_true(file.exists(input_path))
      expect_false(file.exists(output_path))
    }
  )
})

library(tibble)

test_that("llm_submit_pairs_multi_batch retries OpenAI submission on transient 5xx errors", {
  pairs <- tibble::tibble(
    ID1 = c("A", "B"),
    text1 = c("a", "b"),
    ID2 = c("C", "D"),
    text2 = c("c", "d")
  )
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  calls <- 0L
  fake_openai <- function(..., poll) {
    calls <<- calls + 1L
    if (calls < 3L) {
      e <- structure(
        list(message = "boom", call = NULL),
        class = c("httr2_http_500", "error", "condition")
      )
      stop(e)
    }
    list(
      batch_input_path  = list(...)[["batch_input_path"]],
      batch_output_path = list(...)[["batch_output_path"]],
      file              = NULL,
      batch             = list(id = "openai-OK"),
      results           = NULL
    )
  }

  with_mocked_bindings(
    run_openai_batch_pipeline = fake_openai,
    {
      res <- llm_submit_pairs_multi_batch(
        pairs              = pairs,
        model              = "fake-model",
        trait_name         = td$name,
        trait_description  = td$description,
        prompt_template    = tmpl,
        backend            = "openai",
        n_segments         = 1L,
        output_dir         = tempfile("mb_retry_"),
        write_registry     = FALSE,
        verbose            = FALSE,
        openai_max_retries = 3L
      )
      expect_equal(calls, 3L)
      expect_equal(res$jobs[[1]]$batch_id, "openai-OK")
    }
  )
})

test_that("llm_resume_multi_batches can remove JSONL files when keep_jsonl = FALSE", {
  input_path <- tempfile(fileext = ".jsonl")
  output_path <- tempfile(fileext = ".jsonl")
  csv_path <- tempfile(fileext = ".csv")

  writeLines("{}", con = input_path)
  writeLines("{}", con = output_path)

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

  with_mocked_bindings(
    openai_get_batch = function(id) list(status = "completed"),
    openai_download_batch_output = function(batch_id, path) {
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
        verbose           = FALSE
      )
      expect_true(res$jobs[[1]]$done)
      expect_false(file.exists(input_path))
      expect_false(file.exists(output_path))
      expect_equal(nrow(res$combined), 1L)
    }
  )
})

test_that("llm_resume_multi_batches errors on unsupported provider type", {
  jobs <- list(list(
    segment_index     = 1L,
    provider          = "weird",
    model             = "m",
    batch_id          = "id",
    batch_input_path  = tempfile(fileext = ".jsonl"),
    batch_output_path = tempfile(fileext = ".jsonl"),
    csv_path          = tempfile(fileext = ".csv"),
    done              = FALSE,
    results           = NULL
  ))

  expect_error(
    llm_resume_multi_batches(
      jobs = jobs,
      interval_seconds = 0,
      per_job_delay = 0
    ),
    "Unsupported provider type"
  )
})

test_that("llm_submit_pairs_multi_batch retries OpenAI 5xx errors and logs when verbose", {
  td <- list(name = "TASK", description = "Task")
  tmpl <- "<PROMPT>{{ID1}} vs {{ID2}}</PROMPT>"
  pairs <- tibble::tibble(ID1 = c("A", "B"), ID2 = c("C", "D"))
  out_dir <- tempfile("llm_mb_")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  attempt <- 0L
  fake_pipeline_openai_retry <- function(...) {
    attempt <<- attempt + 1L
    if (attempt == 1L) {
      rlang::abort("server error", class = "httr2_http_500")
    }
    list(batch = list(id = "openai-retry-ok"))
  }

  testthat::local_mocked_bindings(
    run_openai_batch_pipeline = fake_pipeline_openai_retry,
    .package = "pairwiseLLM"
  )

  res <- NULL
  msgs <- testthat::capture_messages({
    res <- pairwiseLLM::llm_submit_pairs_multi_batch(
      pairs = pairs,
      model = "fake-model",
      trait_name = td$name,
      trait_description = td$description,
      prompt_template = tmpl,
      backend = "openai",
      batch_size = 2,
      output_dir = out_dir,
      verbose = TRUE,
      openai_max_retries = 2
    )
  })

  expect_true(any(grepl("Error creating OpenAI batch", msgs, fixed = TRUE)))
  expect_equal(res$registry$batch_id[[1]], "openai-retry-ok")
})

test_that("llm_submit_pairs_multi_batch logs Gemini creation message when verbose", {
  td <- list(name = "TASK", description = "Task")
  tmpl <- "<PROMPT>{{ID1}} vs {{ID2}}</PROMPT>"
  pairs <- tibble::tibble(ID1 = c("A", "B"), ID2 = c("C", "D"))
  out_dir <- tempfile("llm_mb_")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  fake_pipeline_gemini <- function(...) {
    list(batch = list(name = "gemini-batch-test"))
  }

  testthat::local_mocked_bindings(
    run_gemini_batch_pipeline = fake_pipeline_gemini,
    .package = "pairwiseLLM"
  )

  msgs <- testthat::capture_messages({
    pairwiseLLM::llm_submit_pairs_multi_batch(
      pairs = pairs,
      model = "fake-model",
      trait_name = td$name,
      trait_description = td$description,
      prompt_template = tmpl,
      backend = "gemini",
      batch_size = 2,
      output_dir = out_dir,
      verbose = TRUE
    )
  })

  expect_true(any(grepl("Gemini batch created", msgs, fixed = TRUE)))
  expect_true(any(grepl("gemini-batch-test", msgs, fixed = TRUE)))
})

test_that("llm_resume_multi_batches returns empty validation report when combined results are NULL", {
  # All jobs done but no results present -> combined = NULL
  tmp <- tempfile("test_llm_resume_null_")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

  jobs <- list(list(
    segment_index     = 1L,
    provider          = "openai",
    model             = "fake",
    batch_id          = "b1",
    batch_input_path  = file.path(tmp, "in.jsonl"),
    batch_output_path = file.path(tmp, "out.jsonl"),
    csv_path          = file.path(tmp, "out.csv"),
    done              = TRUE,
    results           = NULL
  ))

  res <- llm_resume_multi_batches(
    jobs = jobs,
    output_dir = tmp,
    validate = TRUE,
    max_rounds = 0,
    interval_seconds = 0,
    per_job_delay = 0
  )

  expect_true(is.list(res$validation_report))
  expect_equal(res$validation_report$n_rows, 0L)
  expect_equal(res$validation_report$n_missing_id, 0L)
  expect_equal(res$validation_report$n_invalid_winner, 0L)
})

test_that("llm_resume_multi_batches writes combined CSV and logs when verbose", {
  tmp <- tempfile("test_llm_resume_combined_")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

  jobs <- list(list(
    segment_index = 1L,
    provider = "openai",
    model = "fake",
    batch_id = "b1",
    batch_input_path = file.path(tmp, "in.jsonl"),
    batch_output_path = file.path(tmp, "out.jsonl"),
    csv_path = file.path(tmp, "out.csv"),
    done = TRUE,
    results = tibble::tibble(
      ID1 = "A", ID2 = "B",
      winner = "ID1",
      result_type = "succeeded"
    )
  ))

  combined_path <- file.path(tmp, "combined.csv")

  msgs <- testthat::capture_messages({
    res <- llm_resume_multi_batches(
      jobs               = jobs,
      output_dir         = tmp,
      write_combined_csv = TRUE,
      combined_csv_path  = combined_path,
      verbose            = TRUE,
      max_rounds         = 0,
      interval_seconds   = 0,
      per_job_delay      = 0
    )
    expect_true(file.exists(combined_path))
    expect_equal(nrow(res$combined), 1L)
  })

  expect_true(any(grepl("Combined results written", msgs, fixed = TRUE)))
})
