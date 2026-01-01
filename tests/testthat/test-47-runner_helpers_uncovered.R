testthat::test_that("runner helper coercions cover list + error branches", {
  # list input should coerce to tibble
  x_list <- list(
    results = tibble::tibble(
      ID1 = c("A", "B"),
      ID2 = c("B", "C"),
      better_id = c("A", "C")
    )
  )
  out <- pairwiseLLM:::.coerce_judge_output(x_list)
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(all(c("ID1", "ID2", "better_id") %in% names(out)))

  # wrong list shape should error
  bad_list <- list(ID1 = "A", ID2 = "B")
  testthat::expect_error(
    pairwiseLLM:::.coerce_judge_output(bad_list),
    "judge_fun"
  )

  # non-list dots should be treated as empty list
  cleaned <- pairwiseLLM:::.clean_fit_dots(123)
  testthat::expect_true(is.list(cleaned))
  testthat::expect_length(cleaned, 0)
})
