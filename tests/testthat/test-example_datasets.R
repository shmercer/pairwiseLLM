test_that("example_writing_samples has expected structure", {
  data("example_writing_samples", package = "pairwiseLLM")

  expect_s3_class(example_writing_samples, "tbl_df")
  expect_equal(nrow(example_writing_samples), 20)
  expect_true(all(c("ID", "text", "quality_score") %in% names(example_writing_samples)))
})

test_that("example_writing_pairs has expected structure", {
  data("example_writing_pairs", package = "pairwiseLLM")

  expect_s3_class(example_writing_pairs, "tbl_df")
  expect_equal(nrow(example_writing_pairs), choose(20, 2))
  expect_true(all(c("ID1", "ID2", "better_id") %in% names(example_writing_pairs)))
})

test_that("example_openai_batch_output is a character vector of JSONL lines", {
  data("example_openai_batch_output", package = "pairwiseLLM")

  expect_type(example_openai_batch_output, "character")
  expect_equal(length(example_openai_batch_output), 3)

  # Basic sanity: each line starts with '{' and ends with '}'
  expect_true(all(substr(example_openai_batch_output, 1, 1) == "{"))
  expect_true(all(substr(example_openai_batch_output, nchar(example_openai_batch_output), nchar(example_openai_batch_output)) == "}"))
})
