testthat::test_that("llm_resume_multi_batches can validate combined results", {
  outdir <- tempfile("pairwiseLLM_mb_")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  # Create a minimal completed job record (done=TRUE => no polling)
  job1 <- list(
    provider = "openai",
    model = "gpt-4.1",
    batch_id = "batch_1",
    batch_input_path = file.path(outdir, "in1.jsonl"),
    batch_output_path = file.path(outdir, "out1.jsonl"),
    csv_path = file.path(outdir, "res1.csv"),
    done = TRUE,
    results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")
  )

  job2 <- list(
    provider = "openai",
    model = "gpt-4.1",
    batch_id = "batch_2",
    batch_input_path = file.path(outdir, "in2.jsonl"),
    batch_output_path = file.path(outdir, "out2.jsonl"),
    csv_path = file.path(outdir, "res2.csv"),
    done = TRUE,
    results = tibble::tibble(ID1 = "B", ID2 = "C", better_id = "C")
  )

  res <- llm_resume_multi_batches(
    jobs = list(job1, job2),
    output_dir = outdir,
    interval_seconds = 0,
    per_job_delay = 0,
    write_results_csv = FALSE,
    keep_jsonl = TRUE,
    write_registry = FALSE,
    verbose = FALSE,
    write_combined_csv = FALSE,
    validate = TRUE
  )

  testthat::expect_true(is.list(res))
  testthat::expect_true(all(c("jobs", "combined", "validation_report") %in% names(res)))
  testthat::expect_true(is.data.frame(res$combined))
  testthat::expect_equal(nrow(res$combined), 2L)

  testthat::expect_true(is.list(res$validation_report))
  testthat::expect_equal(res$validation_report$n_rows, 2L)
  testthat::expect_equal(res$validation_report$n_invalid_winner, 0L)
})
