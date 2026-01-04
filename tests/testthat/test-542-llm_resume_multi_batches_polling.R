testthat::test_that("llm_resume_multi_batches stops after max_rounds when jobs cannot complete", {
  # Minimal job structure: only fields used by the polling loop and provider branch.
  jobs <- list(list(
    provider = "openai",
    model = "gpt-4o-mini",
    batch_id = "batch_x",
    batch_input_path = tempfile(fileext = ".jsonl"),
    batch_output_path = tempfile(fileext = ".jsonl"),
    csv_path = tempfile(fileext = ".csv"),
    done = FALSE
  ))

  # Force openai_get_batch to error so the job never completes. The function should
  # eventually stop with an informative max_rounds error (and not hang).
  boom <- function(...) stop("transient")
  mockery::stub(pairwiseLLM::llm_resume_multi_batches, "openai_get_batch", boom)

  testthat::expect_error(
    pairwiseLLM::llm_resume_multi_batches(
      jobs = jobs,
      interval_seconds = 0,
      per_job_delay = 0,
      max_rounds = 1,
      verbose = TRUE
    ),
    "max_rounds_reached"
  )
})
