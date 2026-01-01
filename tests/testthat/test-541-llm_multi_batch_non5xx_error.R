testthat::test_that("llm_submit_pairs_multi_batch propagates non-5xx OpenAI errors", {
  pairs <- tibble::tibble(
    ID1 = c("A", "A"),
    ID2 = c("B", "C"),
    text1 = c("a", "a"),
    text2 = c("b", "c")
  )

  boom <- function(...) stop("boom")
  mockery::stub(pairwiseLLM::llm_submit_pairs_multi_batch, "run_openai_batch_pipeline", boom)

  testthat::expect_error(
    pairwiseLLM::llm_submit_pairs_multi_batch(
      pairs = pairs,
      model = "gpt-4o-mini",
      trait_name = "Task",
      trait_description = "...",
      backend = "openai",
      batch_size = 2,
      verbose = TRUE,
      openai_max_retries = 1
    ),
    "boom"
  )
})
