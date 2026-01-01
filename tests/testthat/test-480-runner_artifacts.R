testthat::test_that("runner_artifacts helpers handle dirs and csv writing", {
  tmp <- withr::local_tempdir()
  d <- file.path(tmp, "artifacts")

  testthat::expect_false(pairwiseLLM:::.ensure_dir(NULL))
  testthat::expect_true(pairwiseLLM:::.ensure_dir(d))
  testthat::expect_true(dir.exists(d))

  # Existing dir should be fine
  testthat::expect_true(pairwiseLLM:::.ensure_dir(d))

  # write_csv_safe returns FALSE on NULL input
  f1 <- file.path(d, "x.csv")
  testthat::expect_false(pairwiseLLM:::.write_csv_safe(NULL, f1))

  # invalid path should error
  testthat::expect_error(pairwiseLLM:::.write_csv_safe(data.frame(a = 1), ""))

  # matrix input is coerced and written
  f2 <- file.path(d, "m.csv")
  ok <- pairwiseLLM:::.write_csv_safe(matrix(1:4, nrow = 2), f2)
  testthat::expect_true(isTRUE(ok))
  testthat::expect_true(file.exists(f2))
})
