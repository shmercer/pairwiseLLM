test_that("results_summary errors when required columns are missing", {
  expect_error(
    results_summary(1),
    "must contain columns"
  )
})

test_that("results_summary strict mode validates and can normalize winners", {
  res <- tibble::tibble(
    ID1 = c("A", "A"),
    ID2 = c("B", "C"),
    better_id = c("SAMPLE_1", "2")
  )

  # strict=TRUE should validate; normalize_winner should allow SAMPLE_1 and 2
  s <- results_summary(res, strict = TRUE, normalize_winner = TRUE, compute_reverse = FALSE)
  expect_true(is.list(s))
  expect_equal(s$overall$n_invalid_winner, 0L)
})

test_that("results_summary reverse branch returns 0 pairs when no valid winners or only one direction exists", {
  # No valid winners (all missing) -> reverse summary n_pairs = 0
  res_missing <- tibble::tibble(ID1 = c("A", "B"), ID2 = c("B", "A"), better_id = c(NA, NA))
  s1 <- results_summary(res_missing, compute_reverse = TRUE, strict = FALSE)
  expect_true(is.list(s1$reverse))
  expect_equal(s1$reverse$summary$n_pairs, 0L)

  # Valid winners but only one direction present -> reverse summary n_pairs = 0
  res_one_dir <- tibble::tibble(ID1 = c("A", "A"), ID2 = c("B", "B"), better_id = c("A", "A"))
  s2 <- results_summary(res_one_dir, compute_reverse = TRUE, strict = FALSE)
  expect_true(is.list(s2$reverse))
  expect_equal(s2$reverse$summary$n_pairs, 0L)
})

test_that("results_summary handles non-standard column names and judge missingness", {
  res <- tibble::tibble(
    x = c("A", "B"),
    y = c("B", "A"),
    win = c("A", "A"),
    j = c("j1", NA_character_)
  )

  # judge missingness -> per_judge stays NULL
  s <- results_summary(
    res,
    id1_col = "x",
    id2_col = "y",
    winner_col = "win",
    judge_col = "j",
    compute_reverse = TRUE,
    strict = FALSE
  )

  expect_true(is.list(s$judge))
  expect_true(isTRUE(s$judge$has_missing_judge))
  expect_true(is.null(s$judge$per_judge))
})
