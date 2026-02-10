testthat::test_that("read_samples_df coerces non-character columns", {
  # Integers in ID and Factor in Text
  df <- data.frame(
    id = 1:2,
    txt = factor(c("T1", "T2"))
  )

  res <- read_samples_df(df, id_col = "id", text_col = "txt")

  testthat::expect_type(res$ID, "character")
  testthat::expect_type(res$text, "character")
  testthat::expect_equal(res$ID, c("1", "2"))
})

testthat::test_that("read_samples_dir errors on empty directory match", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # No .txt files
  testthat::expect_error(
    read_samples_dir(tmp, pattern = "\\.txt$"),
    "No files matching pattern"
  )
})
