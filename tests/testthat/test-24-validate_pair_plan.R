test_that("validate_pair_plan validates columns and basic constraints", {
  pairs <- tibble::tibble(ID1 = c("A", "B"), ID2 = c("B", "C"))
  expect_silent(validate_pair_plan(pairs))

  # Missing required columns
  expect_error(
    validate_pair_plan(tibble::tibble(ID1 = "A")),
    "must contain columns"
  )

  # Self-comparisons: warn by default, error in strict mode
  pairs3 <- tibble::tibble(ID1 = c("A", "B"), ID2 = c("A", "B"))
  expect_warning(validate_pair_plan(pairs3), "self-comparisons")
  expect_error(validate_pair_plan(pairs3, strict = TRUE), "self-comparisons")
})

test_that("validate_pair_plan detects duplicate unordered pairs and can report", {
  pairs_dup <- tibble::tibble(ID1 = c("A", "B"), ID2 = c("B", "A"))

  # warn by default; error in strict mode
  expect_warning(validate_pair_plan(pairs_dup), "duplicate unordered pairs")
  expect_error(validate_pair_plan(pairs_dup, strict = TRUE), "duplicate unordered pairs")

  out <- validate_pair_plan(pairs_dup, return_report = TRUE, allow_duplicates = TRUE)
  expect_true(is.list(out))
  expect_true(all(c("data", "report") %in% names(out)))

  expect_equal(out$report$n_unique_pairs_unordered, 1L)
  expect_true(is.data.frame(out$report$position_balance))
  expect_true(all(c("ID", "n_pos1", "n_pos2", "pos1_rate") %in% names(out$report$position_balance)))
})

test_that("validate_pair_plan enforces ids membership when provided", {
  pairs <- tibble::tibble(ID1 = c("A", "X"), ID2 = c("B", "C"))

  # warn by default; error in strict mode
  expect_warning(validate_pair_plan(pairs, ids = c("A", "B", "C")), "not present in `ids`")
  expect_error(validate_pair_plan(pairs, ids = c("A", "B", "C"), strict = TRUE), "not present in `ids`")
})
