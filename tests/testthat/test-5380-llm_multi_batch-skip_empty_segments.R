testthat::test_that("llm_submit_pairs_multi_batch skips empty segments when n_segments > n_pairs", {
  pairs <- tibble::tibble(
    ID1 = c("a", "b"),
    text1 = c("ta", "tb"),
    ID2 = c("c", "d"),
    text2 = c("tc", "td")
  )

  calls <- 0L
  local_run_openai_batch_pipeline <- function(pairs, ...) {
    calls <<- calls + 1L
    list(batch = list(id = paste0("batch_", calls)))
  }

  testthat::local_mocked_bindings(
    run_openai_batch_pipeline = local_run_openai_batch_pipeline,
    .package = "pairwiseLLM"
  )

  tmp <- withr::local_tempdir(pattern = "pairwiseLLM-multi-batch-")

  res <- pairwiseLLM::llm_submit_pairs_multi_batch(
    pairs = pairs,
    backend = "openai",
    model = "gpt-4o-mini",
    trait_name = "Quality",
    trait_description = "Test",
    prompt_template = "<text1>{text1}</text1><text2>{text2}</text2>",
    n_segments = 5,
    output_dir = tmp,
    verbose = TRUE
  )

  # Only the non-empty segments should be submitted (2 pairs => 2 segments).
  testthat::expect_equal(calls, 2L)
  testthat::expect_length(res$jobs, 2L)
  testthat::expect_equal(nrow(res$registry), 2L)
  testthat::expect_equal(res$registry$segment_index, c(1L, 2L))
})
