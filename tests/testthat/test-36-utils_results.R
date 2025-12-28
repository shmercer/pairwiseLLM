test_that(".escape_regex escapes regex metacharacters", {
  esc <- pairwiseLLM:::.escape_regex

  expect_equal(esc("a.b"), "a\\\\.b")
  expect_equal(esc("a|b"), "a\\\\|b")
  # backslash is doubled
  expect_equal(esc("a\\b"), "a\\\\b")
})

test_that(".normalize_better_id maps common labels and free text", {
  norm <- pairwiseLLM:::.normalize_better_id

  ID1 <- c("A", "A", "A", "A", "A")
  ID2 <- c("B", "B", "B", "B", "B")
  better <- c("SAMPLE_1", "left", "B", "Winner: B", "N/A")

  out <- norm(better, ID1 = ID1, ID2 = ID2)

  expect_equal(out[1], "A") # SAMPLE_1 -> ID1
  expect_equal(out[2], "A") # left -> ID1
  expect_equal(out[3], "B") # exact ID2
  expect_equal(out[4], "B") # free-text mention
  expect_true(is.na(out[5])) # NA-like token
})

test_that(".validate_judge_results errors helpfully on common issues", {
  validate <- pairwiseLLM:::.validate_judge_results
  ids <- c("A", "B", "C")

  good <- tibble::tibble(
    custom_id = "c1",
    ID1 = "A",
    ID2 = "B",
    better_id = "A",
    judge = "j1"
  )

  # Happy path returns a tibble
  out <- validate(good, ids = ids, judge_col = "judge")
  expect_s3_class(out, "tbl_df")

  # Missing required columns
  expect_error(validate(dplyr::select(good, -better_id), ids = ids), "must return columns")

  # judge_col must exist
  expect_error(
    validate(good, ids = ids, judge_col = "nope"),
    "include a `nope` column|when `judge` is provided"
  )

  # Missing judge values
  bad_judge <- good
  bad_judge$judge <- NA_character_
  expect_error(validate(bad_judge, ids = ids, judge_col = "judge"), "missing values")

  # ID1 == ID2 should error
  bad_same <- good
  bad_same$ID2 <- "A"
  expect_error(validate(bad_same, ids = ids), "ID1 == ID2|returned pairs with ID1 == ID2")

  # IDs not present in samples should error
  bad_ids <- good
  bad_ids$ID2 <- "Z"
  expect_error(validate(bad_ids, ids = ids), "not present")

  # better_id invalid -> should include examples (covers utils::head path)
  many_bad <- tibble::tibble(
    custom_id = paste0("c", 1:5),
    ID1 = rep("A", 5),
    ID2 = rep("B", 5),
    better_id = c("X", "Y", "Z", "W", "Q")
  )
  expect_error(validate(many_bad, ids = c("A", "B")), "Examples:")
})

