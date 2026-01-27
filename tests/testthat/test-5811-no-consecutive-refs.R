testthat::test_that("consecutive-pass stopping references are removed", {
  files <- c(
    testthat::test_path("..", "..", "R", "adaptive_stopping.R"),
    testthat::test_path("..", "..", "R", "adaptive_contracts.R"),
    testthat::test_path("..", "..", "R", "adaptive_run.R")
  )
  content <- paste(vapply(files, function(path) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }, character(1L)), collapse = "\n")

  testthat::expect_false(grepl("checks_passed_in_row", content, fixed = TRUE))
  testthat::expect_false(grepl("stability_consecutive", content, fixed = TRUE))
  testthat::expect_false(grepl("stop_passes", content, fixed = TRUE))
})
