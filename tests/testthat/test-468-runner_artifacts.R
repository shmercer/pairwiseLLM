test_that(".ensure_dir creates directories and is safe on NULL", {
  d <- file.path(tempdir(), "pairwiseLLM-test-artifacts", "nested")
  if (dir.exists(d)) unlink(d, recursive = TRUE, force = TRUE)

  expect_false(dir.exists(d))
  expect_false(pairwiseLLM:::.ensure_dir(NULL))

  pairwiseLLM:::.ensure_dir(d)
  expect_true(dir.exists(d))
})

test_that(".write_csv_safe handles NULL and empty tibbles", {
  p_null <- file.path(tempdir(), "pairwiseLLM-test-artifacts-null.csv")
  if (file.exists(p_null)) unlink(p_null)

  expect_false(pairwiseLLM:::.write_csv_safe(NULL, p_null))
  expect_false(file.exists(p_null))

  p_empty <- file.path(tempdir(), "pairwiseLLM-test-artifacts-empty.csv")
  if (file.exists(p_empty)) unlink(p_empty)

  x <- tibble::tibble(ID = character(), value = numeric())
  expect_true(pairwiseLLM:::.write_csv_safe(x, p_empty))
  expect_true(file.exists(p_empty))

  # Should contain header line even with 0 rows
  lines <- readLines(p_empty)
  expect_true(length(lines) >= 1)
  expect_true(grepl("ID", lines[[1]]))
})
