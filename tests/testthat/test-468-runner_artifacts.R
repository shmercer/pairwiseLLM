test_that(".ensure_dir() creates nested directories", {
  td <- withr::local_tempdir()
  path <- file.path(td, "a", "b", "c")
  expect_false(dir.exists(path))

  expect_silent(pairwiseLLM:::.ensure_dir(path))
  expect_true(dir.exists(path))
})


test_that(".write_csv_safe() no-ops on NULL", {
  td <- withr::local_tempdir()
  path <- file.path(td, "null.csv")

  expect_false(file.exists(path))
  expect_silent(pairwiseLLM:::.write_csv_safe(NULL, path))
  expect_false(file.exists(path))
})


test_that(".write_csv_safe() writes header-only for an empty tibble", {
  td <- withr::local_tempdir()
  path <- file.path(td, "empty.csv")

  x <- tibble::tibble(a = double(), b = character())
  expect_silent(pairwiseLLM:::.write_csv_safe(x, path))
  expect_true(file.exists(path))

  y <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(nrow(y), 0)
  expect_equal(names(y), c("a", "b"))
})
