testthat::test_that("validate_backend_results errors on missing required columns", {
  bad <- tibble::tibble(ID1 = "A", ID2 = "B")
  testthat::expect_error(
    pairwiseLLM::validate_backend_results(bad),
    "Missing: better_id"
  )
})

testthat::test_that("validate_backend_results returns a report with missing-winner counts", {
  df <- tibble::tibble(
    ID1 = c("A", "B"),
    ID2 = c("B", "C"),
    better_id = c(NA_character_, "B")
  )

  out <- pairwiseLLM::validate_backend_results(df, strict = FALSE, return_report = TRUE)

  testthat::expect_true(is.list(out))
  testthat::expect_true(all(c("data", "report") %in% names(out)))

  testthat::expect_s3_class(out$data, "tbl_df")
  testthat::expect_true(is.list(out$report))

  testthat::expect_equal(out$report$n_rows, 2L)
  testthat::expect_equal(out$report$n_missing_id, 0L)
  testthat::expect_equal(out$report$n_missing_winner, 1L)
  testthat::expect_equal(out$report$n_invalid_winner, 0L)
})
