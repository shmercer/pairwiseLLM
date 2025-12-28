test_that("validate_pairwise_results validates required columns and basic constraints", {
  res <- tibble::tibble(ID1 = c("A", "A"), ID2 = c("B", "C"), better_id = c("A", NA_character_))

  out <- validate_pairwise_results(res)
  expect_s3_class(out, "tbl_df")
  expect_equal(out$ID1, c("A", "A"))
  expect_true(is.na(out$better_id[2]))

  # Missing columns
  expect_error(
    validate_pairwise_results(tibble::tibble(ID1 = "A")),
    "must contain columns"
  )

  # Self-comparisons error by default
  res_self <- tibble::tibble(ID1 = "A", ID2 = "A", better_id = "A")
  expect_error(validate_pairwise_results(res_self), "self-comparisons")
  expect_silent(validate_pairwise_results(res_self, allow_self = TRUE))
})

test_that("validate_pairwise_results normalizes supported winner tokens and validates winners", {
  res <- tibble::tibble(
    ID1 = c("A", "A", "B"),
    ID2 = c("B", "C", "C"),
    better_id = c("SAMPLE_1", "sample_2", "2")
  )

  out <- validate_pairwise_results(res, normalize_winner = TRUE)

  # SAMPLE_1 -> ID1; SAMPLE_2 and "2" -> ID2
  expect_equal(out$better_id, c("A", "C", "C"))

  # Unsupported tokens should still error even with normalize_winner=TRUE
  res_bad <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "winner:2")
  expect_error(
    validate_pairwise_results(res_bad, normalize_winner = TRUE),
    "must match `ID1` or `ID2`"
  )
})

test_that("validate_pairwise_results validates judge_col and ids, and can return a report", {
  res <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A", judge = "j1")
  expect_silent(validate_pairwise_results(res, judge_col = "judge", ids = c("A", "B")))

  # Missing judge column
  expect_error(
    validate_pairwise_results(res, judge_col = "missing"),
    "must include"
  )

  # Missing judge values error
  res2 <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A", judge = NA_character_)
  expect_error(
    validate_pairwise_results(res2, judge_col = "judge"),
    "missing values"
  )

  # IDs not in ids error
  res3 <- tibble::tibble(ID1 = "A", ID2 = "X", better_id = "A")
  expect_error(
    validate_pairwise_results(res3, ids = c("A", "B")),
    "not present in `ids`"
  )

  # Report mode
  res4 <- tibble::tibble(ID1 = c("A", "A"), ID2 = c("B", "C"), better_id = c("A", NA_character_))
  out2 <- validate_pairwise_results(res4, return_report = TRUE)
  expect_true(is.list(out2))
  expect_true(all(c("data", "report") %in% names(out2)))
  expect_equal(out2$report$n_rows, 2L)
  expect_equal(out2$report$n_missing_winner, 1L)
})
