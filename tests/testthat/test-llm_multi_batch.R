library(testthat)
library(tibble)

test_that("llm_submit_pairs_multi_batch splits pairs correctly and writes registry", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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

test_that("llm_resume_multi_batches validates inputs when jobs and output_dir missing", {
  skip_on_cran()
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
