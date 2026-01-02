test_that("validate_pairwise_results: return_report + all-NA IDs branch", {
  res <- tibble::tibble(ID1 = NA_character_, ID2 = NA_character_, better_id = NA_character_)

  out <- validate_pairwise_results(
    res,
    allow_missing_winner = TRUE,
    allow_self = TRUE,
    return_report = TRUE
  )

  expect_true(is.list(out))
  expect_true(all(c("data", "report") %in% names(out)))
  expect_s3_class(out$data, "tbl_df")
  expect_true(is.list(out$report))
  expect_identical(out$report$n_unique_unordered_pairs, 0L)
})

test_that("validate_pairwise_results: judge_col validation + ids validation branches", {
  res <- tibble::tibble(ID1 = c("A", "A"), ID2 = c("B", "C"), better_id = c("A", "C"), judge = c("j1", NA))

  # Missing judge values should error when judge_col provided
  expect_error(
    validate_pairwise_results(res, judge_col = "judge"),
    "missing values in judge"
  )

  # Invalid IDs should error when ids provided
  res2 <- tibble::tibble(ID1 = c("A", "X"), ID2 = c("B", "C"), better_id = c("A", "B"))
  expect_error(
    validate_pairwise_results(res2, ids = c("A", "B", "C")),
    "IDs not present"
  )
})
