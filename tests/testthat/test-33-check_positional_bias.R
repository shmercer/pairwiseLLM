test_that("check_positional_bias works with a consistency list", {
  # Synthetic details with 3 pairs, 2 consistent, 1 inconsistent
  # Pair A||B: position-1 bias type inconsistency
  details <- tibble::tibble(
    key            = c("A||B", "A||C", "B||C"),
    ID1_main       = c("A", "A", "B"),
    ID2_main       = c("B", "C", "C"),
    better_id_main = c("A", "C", "B"),
    ID1_rev        = c("B", "C", "C"),
    ID2_rev        = c("A", "A", "B"),
    better_id_rev  = c("B", "C", "B"),
    is_consistent  = c(FALSE, TRUE, TRUE)
  )

  consistency <- list(
    overall = NULL,
    details = details
  )

  diag <- check_positional_bias(
    consistency,
    n_boot = 200, # keep small for tests
    seed   = 123
  )

  expect_type(diag, "list")
  expect_true(all(c("summary", "details") %in% names(diag)))

  s <- diag$summary

  # 3 pairs, 2/3 consistent
  expect_equal(s$n_pairs, 3L)
  expect_equal(s$prop_consistent, 2 / 3, tolerance = 1e-8)
  expect_equal(s$n_inconsistent, 1L)

  # Positional bias counts among inconsistent pairs
  expect_equal(s$n_inconsistent_pos1_bias, 1L)
  expect_equal(s$n_inconsistent_pos2_bias, 0L)

  # Bootstrap CI should contain the observed proportion
  expect_true(s$boot_lwr <= s$prop_consistent + 1e-8)
  expect_true(s$boot_upr >= s$prop_consistent - 1e-8)

  # p-values should be numeric and in [0, 1]
  expect_true(is.numeric(s$p_sample1_main))
  expect_true(is.numeric(s$p_sample1_rev))
  expect_true(is.numeric(s$p_sample1_overall))
  expect_gte(s$p_sample1_overall, 0)
  expect_lte(s$p_sample1_overall, 1)

  # Overall counts should be consistent
  expect_true(is.numeric(s$total_pos1_wins))
  expect_true(is.numeric(s$total_comparisons))
  expect_gte(s$total_pos1_wins, 0)
  expect_gte(s$total_comparisons, s$total_pos1_wins)
})

test_that("check_positional_bias accepts a raw details tibble", {
  details <- tibble::tibble(
    key            = c("X||Y", "X||Z"),
    ID1_main       = c("X", "X"),
    ID2_main       = c("Y", "Z"),
    better_id_main = c("X", "Z"),
    ID1_rev        = c("Y", "Z"),
    ID2_rev        = c("X", "X"),
    better_id_rev  = c("X", "Z"),
    is_consistent  = c(TRUE, TRUE)
  )

  diag <- check_positional_bias(
    details,
    n_boot = 100,
    seed   = 42
  )

  s <- diag$summary

  expect_equal(s$n_pairs, 2L)
  expect_equal(s$prop_consistent, 1.0)
  expect_equal(s$n_inconsistent, 0L)
  expect_equal(s$n_inconsistent_pos1_bias, 0L)
  expect_equal(s$n_inconsistent_pos2_bias, 0L)

  # New overall fields exist and are numeric
  expect_true("p_sample1_overall" %in% names(s))
  expect_true("total_pos1_wins" %in% names(s))
  expect_true("total_comparisons" %in% names(s))
  expect_true(is.numeric(s$p_sample1_overall))

  # details tibble should have added columns
  d <- diag$details
  expect_true(all(c(
    "winner_pos_main", "winner_pos_rev",
    "is_pos1_bias", "is_pos2_bias"
  ) %in% names(d)))
})

test_that("check_positional_bias errors cleanly on missing columns", {
  bad_details <- tibble::tibble(
    key      = c("A||B", "A||C"),
    ID1_main = c("A", "A")
    # missing many required columns
  )

  expect_error(
    check_positional_bias(bad_details),
    "must contain columns"
  )
})

test_that("check_positional_bias errors on empty details", {
  empty_details <- tibble::tibble(
    key            = character(),
    ID1_main       = character(),
    ID2_main       = character(),
    better_id_main = character(),
    ID1_rev        = character(),
    ID2_rev        = character(),
    better_id_rev  = character(),
    is_consistent  = logical()
  )

  expect_error(
    check_positional_bias(empty_details),
    "has zero rows"
  )
})
