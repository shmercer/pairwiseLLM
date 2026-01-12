test_that("llm_compare_pair defensively errors for unknown backends", {
  testthat::local_mocked_bindings(
    .normalize_backend_arg = function(...) "not_a_backend",
    .env = asNamespace("pairwiseLLM")
  )

  expect_error(
    pairwiseLLM::llm_compare_pair(
      backend = "openai",
      ID1 = "A",
      text1 = "t1",
      ID2 = "B",
      text2 = "t2",
      model = "m",
      trait_name = "t",
      trait_description = "d",
      api_key = "k"
    ),
    "not implemented",
    fixed = FALSE
  )
})
