compute_reverse_consistency <- pairwiseLLM:::compute_reverse_consistency
check_positional_bias <- pairwiseLLM:::check_positional_bias

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

test_that("compute_reverse_consistency detects inconsistencies and
          ignores NAs", {
  main <- tibble::tibble(
    ID1       = c("S1", "S1", "S2", "S3"),
    ID2       = c("S2", "S3", "S3", "S4"),
    better_id = c("S1", "S3", "S2", "S3")
  )

  rev <- tibble::tibble(
    ID1       = c("S2", "S3", "S3", "S4"),
    ID2       = c("S1", "S1", "S2", "S3"),
    better_id = c("S1", "S1", NA, "S3") # one inconsistent, one NA
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
  rev <- main
  rev$ID1 <- main$ID2
  rev$ID2 <- main$ID1

  rc <- compute_reverse_consistency(main, rev)

  expect_equal(rc$summary$n_pairs, 10)
  expect_equal(rc$summary$n_consistent, 10)
  expect_equal(rc$summary$prop_consistent, 1)
})

test_that("compute_reverse_consistency errors when required columns are
          missing", {
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

test_that(
  "compute_reverse_consistency uses per-key majority agreement and remains compatible with check_positional_bias",
  {
  # Duplicates in main, reverse is a sample (only includes A-B)
  # Also includes mixed ordering to exercise most_common_order() logic.
  main <- tibble::tibble(
    ID1       = c("A", "B", "A", "A", "X"),
    ID2       = c("B", "A", "B", "B", "Y"),
    better_id = c("A", "A", "A", NA, "X") # one NA ignored; X-Y not in reverse
  )

  # Reverse includes A-B twice with *tied* presented ordering (A-B and B-A once each),
  # but unanimous winner A -> majority exists.
  rev <- tibble::tibble(
    ID1       = c("B", "A"),
    ID2       = c("A", "B"),
    better_id = c("A", "A")
  )

  rc <- compute_reverse_consistency(main, rev)

  # One overlapping unordered pair key: A||B
  expect_s3_class(rc$summary, "tbl_df")
  expect_equal(rc$summary$n_pairs, 1)
  expect_equal(rc$summary$n_consistent, 1)
  expect_equal(rc$summary$prop_consistent, 1)

  # Details must be ONE row per key (critical for check_positional_bias joins)
  expect_s3_class(rc$details, "tbl_df")
  expect_equal(nrow(rc$details), 1)
  expect_equal(anyDuplicated(rc$details$key), 0L)
  expect_true(all(c(
    "key",
    "ID1_main", "ID2_main", "better_id_main",
    "ID1_rev", "ID2_rev", "better_id_rev",
    "is_consistent",
    "n_main_votes", "n_rev_votes",
    "is_main_tie", "is_rev_tie"
  ) %in% names(rc$details)))

  # Majority winners
  expect_equal(rc$details$better_id_main, "A")
  expect_equal(rc$details$better_id_rev, "A")
  expect_true(rc$details$is_consistent)

  # Vote counts (main had 3 non-NA votes for A||B)
  expect_equal(rc$details$n_main_votes, 3)
  expect_equal(rc$details$n_rev_votes, 2)
  expect_false(rc$details$is_main_tie)
  expect_false(rc$details$is_rev_tie)

  # Ensure ordering fields are deterministic even when presented ordering ties in reverse
  # (tie broken deterministically by sorted "ID1||ID2" string)
  expect_equal(rc$details$ID1_rev, "A")
  expect_equal(rc$details$ID2_rev, "B")

  # Must not break check_positional_bias()
  bias <- check_positional_bias(rc, n_boot = 10, seed = 123)
  expect_true(is.list(bias))
  expect_true("summary" %in% names(bias))
  expect_s3_class(bias$summary, "tbl_df")
  expect_equal(bias$summary$n_pairs, 1)
  expect_equal(bias$summary$n_inconsistent, 0)
  }
)

test_that("compute_reverse_consistency excludes keys with a tie in main majority vote", {
  # Main ties: A-B has one vote each -> no unique majority -> excluded
  main <- tibble::tibble(
    ID1       = c("A", "A"),
    ID2       = c("B", "B"),
    better_id = c("A", "B")
  )

  # Reverse has a clear majority, but the key should still be excluded
  rev <- tibble::tibble(
    ID1       = c("B", "B"),
    ID2       = c("A", "A"),
    better_id = c("A", "A")
  )

  rc <- compute_reverse_consistency(main, rev)

  expect_equal(rc$summary$n_pairs, 0)
  expect_true(is.na(rc$summary$prop_consistent))
  expect_equal(nrow(rc$details), 0)
})

test_that("compute_reverse_consistency excludes keys with a tie in reverse majority vote", {
  # Main has a clear majority for E-F (E wins 2 out of 3)
  main <- tibble::tibble(
    ID1       = c("E", "E", "F"),
    ID2       = c("F", "F", "E"),
    better_id = c("E", "E", "E")
  )

  # Reverse ties: one vote E, one vote F -> excluded
  rev <- tibble::tibble(
    ID1       = c("F", "F"),
    ID2       = c("E", "E"),
    better_id = c("E", "F")
  )

  rc <- compute_reverse_consistency(main, rev)

  expect_equal(rc$summary$n_pairs, 0)
  expect_true(is.na(rc$summary$prop_consistent))
  expect_equal(nrow(rc$details), 0)
})
