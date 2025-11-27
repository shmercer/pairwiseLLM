test_that("make_pairs creates all unordered pairs", {
  samples <- tibble::tibble(
    ID   = c("S1", "S2", "S3"),
    text = c("A", "B", "C")
  )

  pairs <- make_pairs(samples)

  expect_s3_class(pairs, "tbl_df")
  expect_equal(nrow(pairs), 3)  # 3C2 = 3 pairs

  # Check that all combinations are present (order-insensitive)
  pair_labels <- apply(pairs[, c("ID1", "ID2")], 1, function(x) paste(sort(x), collapse = "-"))
  expect_setequal(pair_labels, c("S1-S2", "S1-S3", "S2-S3"))
})

test_that("make_pairs errors if not enough samples", {
  samples_one <- tibble::tibble(
    ID   = "S1",
    text = "Only one sample"
  )

  expect_error(
    make_pairs(samples_one),
    "At least two samples are required"
  )
})

test_that("make_pairs errors if ID or text columns are missing", {
  bad_samples <- tibble::tibble(
    ID_only = c("S1", "S2")
  )

  expect_error(
    make_pairs(bad_samples),
    "`samples` must have columns 'ID' and 'text'"
  )
})

test_that("sample_pairs respects pair_pct and n_pairs, and is reproducible", {
  samples <- tibble::tibble(
    ID   = paste0("S", 1:5),
    text = paste("Sample", 1:5)
  )
  pairs <- make_pairs(samples)
  n_all <- nrow(pairs)

  # Clamp pair_pct > 1 to full set
  pairs_clamped_high <- sample_pairs(pairs, pair_pct = 1.5, seed = 1)
  expect_equal(nrow(pairs_clamped_high), n_all)

  # Clamp pair_pct < 0 to 0
  pairs_zero <- sample_pairs(pairs, pair_pct = -0.1, seed = 1)
  expect_equal(nrow(pairs_zero), 0)

  # 50% sampling
  pairs_half_1 <- sample_pairs(pairs, pair_pct = 0.5, seed = 123)
  pairs_half_2 <- sample_pairs(pairs, pair_pct = 0.5, seed = 123)
  expect_equal(pairs_half_1, pairs_half_2)
  expect_true(nrow(pairs_half_1) >= floor(0.5 * n_all))  # due to floor()

  # Fixed n_pairs
  pairs_fixed <- sample_pairs(pairs, n_pairs = 4, seed = 42)
  expect_equal(nrow(pairs_fixed), 4)

  # Both pair_pct and n_pairs: min
  pairs_min <- sample_pairs(pairs, pair_pct = 0.9, n_pairs = 3, seed = 999)
  expect_equal(nrow(pairs_min), 3)
})

test_that("sample_pairs returns original table when k == n", {
  samples <- tibble::tibble(
    ID   = paste0("S", 1:4),
    text = paste("Sample", 1:4)
  )
  pairs <- make_pairs(samples)
  # pair_pct = 1, n_pairs = NULL -> keep all
  pairs_all <- sample_pairs(pairs, pair_pct = 1, seed = 1)
  expect_equal(pairs_all, pairs)
})

test_that("sample_reverse_pairs reverses sampled subset correctly", {
  samples <- tibble::tibble(
    ID   = c("S1", "S2", "S3"),
    text = c("Text1", "Text2", "Text3")
  )
  pairs <- make_pairs(samples)

  # Reverse 50% of the pairs
  rev_pairs <- sample_reverse_pairs(pairs, reverse_pct = 0.5, seed = 123)

  expect_s3_class(rev_pairs, "tbl_df")
  if (nrow(rev_pairs) > 0) {
    # For each reversed row, there should be a matching original row with swapped IDs/texts
    for (i in seq_len(nrow(rev_pairs))) {
      r <- rev_pairs[i, ]

      match_idx <- which(
        pairs$ID1   == r$ID2 &
          pairs$ID2   == r$ID1 &
          pairs$text1 == r$text2 &
          pairs$text2 == r$text1
      )

      expect_true(length(match_idx) == 1)
    }
  }
})

test_that("sample_reverse_pairs handles edge cases for reverse_pct and n_reverse", {
  samples <- tibble::tibble(
    ID   = paste0("S", 1:4),
    text = paste("Sample", 1:4)
  )
  pairs <- make_pairs(samples)
  n_all <- nrow(pairs)

  # reverse_pct > 1 -> clamp to all
  rev_all <- sample_reverse_pairs(pairs, reverse_pct = 1.2, seed = 1)
  expect_equal(nrow(rev_all), n_all)

  # reverse_pct < 0 -> 0 rows
  rev_none <- sample_reverse_pairs(pairs, reverse_pct = -0.3, seed = 1)
  expect_equal(nrow(rev_none), 0)

  # n_reverse smaller than pct-based count
  rev_limited <- sample_reverse_pairs(pairs, reverse_pct = 1, n_reverse = 2, seed = 1)
  expect_equal(nrow(rev_limited), 2)

  # reverse_pct = 0 and n_reverse = NULL -> empty tibble
  rev_empty <- sample_reverse_pairs(pairs, reverse_pct = 0, seed = 1)
  expect_equal(nrow(rev_empty), 0)
})

test_that("make_pairs on example_writing_samples matches example_writing_pairs", {
  data("example_writing_samples", package = "pairwiseLLM")
  data("example_writing_pairs",  package = "pairwiseLLM")

  pairs_gen <- make_pairs(example_writing_samples)

  # Compare unordered ID pairs between generated and reference tables
  gen_labels <- apply(pairs_gen[, c("ID1", "ID2")], 1, function(x) paste(sort(x), collapse = "-"))
  ref_labels <- apply(example_writing_pairs[, c("ID1", "ID2")], 1, function(x) paste(sort(x), collapse = "-"))

  expect_equal(length(gen_labels), length(ref_labels))
  expect_setequal(gen_labels, ref_labels)
})

test_that("sample_pairs returns a subset of example_writing_pairs", {
  data("example_writing_samples", package = "pairwiseLLM")
  data("example_writing_pairs",  package = "pairwiseLLM")

  pairs <- make_pairs(example_writing_samples)

  set.seed(123)
  sampled <- sample_pairs(pairs, pair_pct = 0.2)

  # Every sampled pair should correspond to some pair in example_writing_pairs
  sampled_labels <- apply(sampled[, c("ID1", "ID2")], 1, function(x) paste(sort(x), collapse = "-"))
  ref_labels     <- apply(example_writing_pairs[, c("ID1", "ID2")], 1, function(x) paste(sort(x), collapse = "-"))

  expect_true(all(sampled_labels %in% ref_labels))
})

test_that("sample_reverse_pairs produces valid reversed pairs on example data", {
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)

  set.seed(999)
  rev_pairs <- sample_reverse_pairs(pairs, reverse_pct = 0.1)

  # Each reversed pair must correspond to an original pair with swapped roles
  if (nrow(rev_pairs) > 0) {
    for (i in seq_len(nrow(rev_pairs))) {
      r <- rev_pairs[i, ]
      match_idx <- which(
        pairs$ID1   == r$ID2 &
          pairs$ID2   == r$ID1 &
          pairs$text1 == r$text2 &
          pairs$text2 == r$text1
      )
      expect_true(length(match_idx) == 1)
    }
  }
})
