test_that("read_samples_df extracts ID and text correctly with column names", {
  df <- data.frame(
    StudentID = c("S1", "S2"),
    Response = c("Text 1", "Text 2"),
    Grade = c(8, 9),
    stringsAsFactors = FALSE
  )

  samples <- read_samples_df(df, id_col = "StudentID", text_col = "Response")

  expect_s3_class(samples, "tbl_df")
  expect_equal(samples$ID, c("S1", "S2"))
  expect_equal(samples$text, c("Text 1", "Text 2"))
  expect_true("Grade" %in% names(samples))
  expect_equal(samples$Grade, c(8, 9))
  expect_identical(names(samples)[1:2], c("ID", "text"))
})

test_that("read_samples_df works with numeric column indices", {
  df <- data.frame(
    ID_col = c("A", "B"),
    Text_col = c("Sample A", "Sample B"),
    stringsAsFactors = FALSE
  )

  samples <- read_samples_df(df, id_col = 1, text_col = 2)

  expect_equal(samples$ID, c("A", "B"))
  expect_equal(samples$text, c("Sample A", "Sample B"))
})

test_that("read_samples_df coerces IDs and text to character", {
  df <- data.frame(
    ID_num = c(101, 102),
    Text_fac = factor(c("Fac 1", "Fac 2")),
    stringsAsFactors = FALSE
  )

  samples <- read_samples_df(df, id_col = "ID_num", text_col = "Text_fac")

  expect_type(samples$ID, "character")
  expect_type(samples$text, "character")
})

test_that("read_samples_df enforces unique IDs", {
  df_dup <- data.frame(
    StudentID = c("S1", "S1"),
    Response = c("Text 1", "Text 2"),
    stringsAsFactors = FALSE
  )

  expect_error(
    read_samples_df(df_dup, id_col = "StudentID", text_col = "Response"),
    "Duplicate IDs detected"
  )
})

test_that("read_samples_df errors when id_col or text_col is invalid", {
  df <- data.frame(
    StudentID = c("S1", "S2"),
    Response = c("Text 1", "Text 2"),
    stringsAsFactors = FALSE
  )

  expect_error(
    read_samples_df(df, id_col = "NotThere", text_col = "Response")
  )

  expect_error(
    read_samples_df(df, id_col = "StudentID", text_col = 99)
  )
})

test_that("read_samples_dir reads txt files and concatenates lines", {
  tmp <- tempfile("pairwiseLLM-test-dir-")
  dir.create(tmp)

  file1 <- file.path(tmp, "S1.txt")
  file2 <- file.path(tmp, "S2.txt")

  writeLines(c("Line 1 of S1", "Line 2 of S1"), con = file1)
  writeLines("Only line of S2", con = file2)

  samples <- read_samples_dir(path = tmp)

  expect_s3_class(samples, "tbl_df")
  expect_equal(sort(samples$ID), c("S1", "S2"))

  s1_text <- samples$text[samples$ID == "S1"]
  expect_true(grepl("Line 1 of S1", s1_text))
  expect_true(grepl("Line 2 of S1", s1_text))
})

test_that("read_samples_dir errors when no files match pattern", {
  tmp <- tempfile("pairwiseLLM-test-empty-")
  dir.create(tmp)

  expect_error(
    read_samples_dir(path = tmp, pattern = "\\.txt$"),
    "No files matching pattern"
  )
})
