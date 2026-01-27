testthat::test_that("normalize_model_variant rejects invalid inputs", {
  testthat::expect_error(
    pairwiseLLM:::normalize_model_variant(1),
    "length-1 character"
  )
  testthat::expect_error(
    pairwiseLLM:::normalize_model_variant(NA_character_),
    "length-1 character"
  )
})
