test_that("compute_reverse_consistency works for fully consistent pairs", {
  main <- tibble::tibble(
    ID1       = c("S1", "S1", "S2"),
    ID2       = c("S2", "S3", "S3"),
    better_id = c("S1", "S3", "S2")
  )

  rev <- tibble::tibble(
    ID1       = c("S2", "S3", "S3"),
    ID2       = c("S1", "S1", "S2"),
    better_id = c("S1", "S3", "S2")
  )

  rc <- compute_reverse_consistency(main, rev)

  expect_s3_class(rc$summary, "tbl_df")
  expect_equal(rc$summary$n_pairs, 3)
  expect_equal(rc$summary$n_consistent, 3)
  expect_equal(rc$summary$prop_consistent, 1)
  expect_true(all(rc$details$is_consistent))
})

test_that("compute_reverse_consistency detects inconsistencies and ignores NAs", {
  main <- tibble::tibble(
    ID1       = c("S1", "S1", "S2", "S3"),
    ID2       = c("S2", "S3", "S3", "S4"),
    better_id = c("S1", "S3", "S2", "S3")
  )

  rev <- tibble::tibble(
    ID1       = c("S2", "S3", "S3", "S4"),
    ID2       = c("S1", "S1", "S2", "S3"),
    better_id = c("S1", "S1", NA,   "S3")  # one inconsistent, one NA
  )

  rc <- compute_reverse_consistency(main, rev)

  # Overlapping keys: S1-S2, S1-S3, S3-S4 -> 3
  expect_equal(rc$summary$n_pairs, 3)

  # S1-S2: consistent, S1-S3: inconsistent, S3-S4: consistent
  expect_equal(rc$summary$n_consistent, 2)
  expect_equal(rc$summary$prop_consistent, 2 / 3)
  expect_equal(sum(!rc$details$is_consistent), 1)
})

test_that("compute_reverse_consistency returns zero pairs when no overlap", {
  main <- tibble::tibble(
    ID1       = c("S1", "S2"),
    ID2       = c("S3", "S4"),
    better_id = c("S1", "S4")
  )

  rev <- tibble::tibble(
    ID1       = c("S5", "S6"),
    ID2       = c("S7", "S8"),
    better_id = c("S5", "S8")
  )

  rc <- compute_reverse_consistency(main, rev)

  expect_equal(rc$summary$n_pairs, 0)
  expect_true(is.na(rc$summary$prop_consistent))
  expect_equal(nrow(rc$details), 0)
})

test_that("compute_reverse_consistency works on example_writing_pairs", {
  data("example_writing_pairs", package = "pairwiseLLM")

  main <- example_writing_pairs[1:10, ]
  rev  <- main
  rev$ID1 <- main$ID2
  rev$ID2 <- main$ID1

  rc <- compute_reverse_consistency(main, rev)

  expect_equal(rc$summary$n_pairs, 10)
  expect_equal(rc$summary$n_consistent, 10)
  expect_equal(rc$summary$prop_consistent, 1)
})

test_that("compute_reverse_consistency errors when required columns are missing", {
  main <- tibble::tibble(
    ID1       = c("S1", "S2"),
    ID2       = c("S3", "S4"),
    better_id = c("S1", "S4")
  )

  rev_bad <- tibble::tibble(
    ID1 = c("S3", "S4"),
    ID2 = c("S1", "S2")
    # missing better_id
  )

  expect_error(
    compute_reverse_consistency(main, rev_bad),
    "must contain columns"
  )

  main_bad <- tibble::tibble(
    ID1 = c("S1", "S2"),
    ID2 = c("S3", "S4")
    # missing better_id
  )

  expect_error(
    compute_reverse_consistency(main_bad, rev_bad),
    "must contain columns"
  )
})
