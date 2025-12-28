test_that("results_summary reports missing/invalid winners in non-strict mode", {
  res <- tibble::tibble(
    ID1 = c("A", "A", "B", "C"),
    ID2 = c("B", "C", "C", "A"),
    better_id = c("SAMPLE_1", "2", "winner:2", NA_character_)
  )

  s <- results_summary(res, strict = FALSE, normalize_winner = TRUE, compute_reverse = FALSE)

  expect_true(is.list(s))
  expect_true(is.data.frame(s$overall))
  expect_equal(s$overall$n_rows, 4L)
  # SAMPLE_1 and "2" normalize to valid; "winner:2" invalid; NA missing
  expect_equal(s$overall$n_invalid_winner, 1L)
  expect_equal(s$overall$n_missing_winner, 1L)

  # Strict mode should error on invalid winners (even if some are missing)
  expect_error(
    results_summary(res, strict = TRUE, normalize_winner = TRUE, compute_reverse = FALSE),
    "must match `ID1` or `ID2`"
  )
})

test_that("results_summary computes reverse consistency when both directions exist", {
  # Duplicate votes create per-direction majorities; these majorities disagree
  res <- tibble::tibble(
    ID1 = c("A", "A", "B", "B", "A", "B"),
    ID2 = c("B", "B", "A", "A", "B", "A"),
    better_id = c("A", "A", "B", "B", "B", "A")
  )

  s <- results_summary(res, compute_reverse = TRUE, strict = FALSE)

  expect_true(is.list(s$reverse))
  expect_true(is.data.frame(s$reverse$summary))
  expect_equal(s$reverse$summary$n_pairs, 1L)
  expect_equal(s$reverse$summary$prop_consistent, 0)

  # Now make consistent: A wins in both directions (majority)
  res2 <- tibble::tibble(
    ID1 = c("A", "B"),
    ID2 = c("B", "A"),
    better_id = c("A", "A")
  )
  s2 <- results_summary(res2, compute_reverse = TRUE, strict = FALSE)
  expect_equal(s2$reverse$summary$n_pairs, 1L)
  expect_equal(s2$reverse$summary$prop_consistent, 1)
})

test_that("results_summary can include per-judge summaries when judge_col is present", {
  res <- tibble::tibble(
    ID1 = c("A", "A", "B"),
    ID2 = c("B", "C", "C"),
    better_id = c("A", "C", "C"),
    judge = c("j1", "j1", "j2")
  )

  s <- results_summary(res, judge_col = "judge", compute_reverse = TRUE, strict = FALSE)
  expect_true(is.list(s$judge))
  expect_identical(s$judge$judge_col, "judge")
  expect_false(s$judge$has_missing_judge)
  expect_true(is.list(s$judge$per_judge))
  expect_true(is.data.frame(s$judge$per_judge$by_judge))
})
