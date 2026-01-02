testthat::test_that("llm_submit_pairs_multi_batch prints anthropic segment message (no leakage)", {
  # Use a realistic pairs schema even though the provider pipeline is mocked.
  pairs <- tibble::tibble(
    ID1 = c("A", "B"),
    text1 = c("a", "b"),
    ID2 = c("C", "D"),
    text2 = c("c", "d")
  )
  out_dir <- tempfile("llm_multi_batch_test_")

  res <- NULL
  testthat::with_mocked_bindings(
    run_anthropic_batch_pipeline = function(..., poll) {
      list(
        batch = list(id = "msgbatch_123"),
        results = NULL
      )
    },
    {
      testthat::expect_message(
        {
          res <- pairwiseLLM::llm_submit_pairs_multi_batch(
            pairs = pairs,
            model = "claude-test",
            trait_name = "Trait",
            trait_description = "Desc",
            backend = "anthropic",
            n_segments = 1L,
            output_dir = out_dir,
            verbose = TRUE
          )
        },
        "Submitting segment 1 of 1"
      )
    }
  )

  testthat::expect_true(is.list(res))
  testthat::expect_true(all(c("jobs", "registry") %in% names(res)))
  testthat::expect_equal(length(res$jobs), 1L)
  testthat::expect_true(is.data.frame(res$registry))
  testthat::expect_equal(nrow(res$registry), 1L)

  testthat::expect_equal(res$jobs[[1]]$provider, "anthropic")
  testthat::expect_equal(res$jobs[[1]]$batch_id, "msgbatch_123")
  testthat::expect_equal(res$registry$provider, "anthropic")
  testthat::expect_equal(res$registry$batch_id, "msgbatch_123")
})

testthat::test_that("llm_resume_multi_batches handles OpenAI get_batch errors and stops after max_rounds", {
  job <- list(
    segment_index = 1L,
    provider = "openai",
    model = NA_character_,
    batch_id = "b1",
    batch_input_path = tempfile(fileext = ".jsonl"),
    batch_output_path = tempfile(fileext = ".jsonl"),
    csv_path = tempfile(fileext = ".csv"),
    done = FALSE,
    results = NULL
  )
  writeLines("{}", job$batch_input_path)
  jobs <- list(job)

  testthat::with_mocked_bindings(
    openai_get_batch = function(batch_id, ...) {
      stop("transient")
    },
    {
      testthat::expect_message(
        testthat::expect_error(
          pairwiseLLM::llm_resume_multi_batches(
            jobs = jobs,
            max_rounds = 1L,
            interval_seconds = 0,
            per_job_delay = 0,
            verbose = TRUE
          ),
          "Polling exceeded max_rounds"
        ),
        "Error retrieving OpenAI batch"
      )
    }
  )
})

testthat::test_that("llm_resume_multi_batches retries when OpenAI download fails", {
  job <- list(
    segment_index = 1L,
    provider = "openai",
    model = NA_character_,
    batch_id = "b1",
    batch_input_path = tempfile(fileext = ".jsonl"),
    batch_output_path = tempfile(fileext = ".jsonl"),
    csv_path = tempfile(fileext = ".csv"),
    done = FALSE,
    results = NULL
  )
  writeLines("{}", job$batch_input_path)
  jobs <- list(job)

  fake_batch <- list(
    id = "b1",
    status = "completed",
    output_file_id = "file_123"
  )

  testthat::with_mocked_bindings(
    openai_get_batch = function(batch_id, ...) {
      fake_batch
    },
    openai_download_batch_output = function(batch_id, output_path, ...) {
      cond <- structure(
        list(
          message = "server error",
          response = list(status_code = 500)
        ),
        class = c("httr2_http_500", "error", "condition")
      )
      stop(cond)
    },
    {
      testthat::expect_message(
        testthat::expect_error(
          pairwiseLLM::llm_resume_multi_batches(
            jobs = jobs,
            max_rounds = 1L,
            interval_seconds = 0,
            per_job_delay = 0,
            verbose = TRUE,
            openai_max_retries = 1L
          ),
          "Polling exceeded max_rounds"
        ),
        "Failed to download OpenAI batch"
      )
    }
  )
})
