# tests/testthat/test-pair_order.R

testthat::test_that("alternate_pair_order requires required columns", {
  bad_pairs <- tibble::tibble(
    ID1   = "S01",
    text1 = "Sample 1"
    # missing ID2, text2
  )

  expect_error(
    alternate_pair_order(bad_pairs),
    "`pairs` must contain columns",
    fixed = FALSE
  )
})

testthat::test_that("alternate_pair_order returns tibble and handles 0/1 rows", {
  # 0 rows
  empty_pairs <- tibble::tibble(
    ID1   = character(0),
    text1 = character(0),
    ID2   = character(0),
    text2 = character(0)
  )

  res_empty <- alternate_pair_order(empty_pairs)
  testthat::expect_s3_class(res_empty, "tbl_df")
  testthat::expect_equal(nrow(res_empty), 0L)

  # 1 row
  one_pair <- tibble::tibble(
    ID1   = "S01",
    text1 = "Sample 1",
    ID2   = "S02",
    text2 = "Sample 2"
  )

  res_one <- alternate_pair_order(one_pair)
  testthat::expect_s3_class(res_one, "tbl_df")
  testthat::expect_equal(nrow(res_one), 1L)

  # With only one row, nothing should be flipped
  testthat::expect_equal(res_one$ID1, one_pair$ID1)
  testthat::expect_equal(res_one$ID2, one_pair$ID2)
})

testthat::test_that("alternate_pair_order flips every second row
                    (2, 4, 6, ...)", {
  pairs <- tibble::tibble(
    ID1   = paste0("S", 1:6),
    text1 = paste("Text", 1:6, "A"),
    ID2   = paste0("T", 1:6),
    text2 = paste("Text", 1:6, "B")
  )

  res <- alternate_pair_order(pairs)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(nrow(res), nrow(pairs))

  # Row 1: unchanged
  testthat::expect_equal(res$ID1[1], pairs$ID1[1])
  testthat::expect_equal(res$ID2[1], pairs$ID2[1])

  # Row 2: swapped
  testthat::expect_equal(res$ID1[2], pairs$ID2[2])
  testthat::expect_equal(res$ID2[2], pairs$ID1[2])

  # Row 3: unchanged
  testthat::expect_equal(res$ID1[3], pairs$ID1[3])
  testthat::expect_equal(res$ID2[3], pairs$ID2[3])

  # Row 4: swapped
  testthat::expect_equal(res$ID1[4], pairs$ID2[4])
  testthat::expect_equal(res$ID2[4], pairs$ID1[4])

  # Row 5: unchanged
  testthat::expect_equal(res$ID1[5], pairs$ID1[5])
  testthat::expect_equal(res$ID2[5], pairs$ID2[5])

  # Row 6: swapped
  testthat::expect_equal(res$ID1[6], pairs$ID2[6])
  testthat::expect_equal(res$ID2[6], pairs$ID1[6])
})

testthat::test_that("alternate_pair_order does not
                    modify the input object", {
  pairs <- tibble::tibble(
    ID1   = paste0("S", 1:4),
    text1 = paste("Text", 1:4, "A"),
    ID2   = paste0("T", 1:4),
    text2 = paste("Text", 1:4, "B")
  )

  pairs_copy <- pairs
  res <- alternate_pair_order(pairs)

  # Input unchanged
  testthat::expect_equal(pairs, pairs_copy)

  # Output is different from input for rows 2 and 4
  testthat::expect_false(identical(res, pairs))
})

testthat::test_that("alternate_pair_order preserves extra columns", {
  pairs <- tibble::tibble(
    ID1   = c("S01", "S02", "S03"),
    text1 = c("Sample 1A", "Sample 2A", "Sample 3A"),
    ID2   = c("T01", "T02", "T03"),
    text2 = c("Sample 1B", "Sample 2B", "Sample 3B"),
    extra = c("X1", "X2", "X3")
  )

  res <- alternate_pair_order(pairs)

  testthat::expect_true("extra" %in% names(res))
  testthat::expect_equal(res$extra, pairs$extra)

  # Row 2 should be swapped for ID/text but not for extra
  testthat::expect_equal(res$ID1[2], pairs$ID2[2])
  testthat::expect_equal(res$ID2[2], pairs$ID1[2])
  testthat::expect_equal(res$extra[2], pairs$extra[2])
})
