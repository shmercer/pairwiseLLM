test_that("randomize_pair_order errors if required columns are missing", {
  data("example_writing_samples", package = "pairwiseLLM")

  # This has ID/text, but not ID1/text1/ID2/text2
  bad_pairs <- example_writing_samples

  expect_error(
    randomize_pair_order(bad_pairs),
    "`pairs` must contain columns",
    fixed = TRUE
  )
})

test_that("randomize_pair_order preserves unordered ID pairs", {
  data("example_writing_samples", package = "pairwiseLLM")

  pairs_all <- make_pairs(example_writing_samples)
  pairs_small <- pairs_all[1:10, ]

  set.seed(123)
  pairs_rand <- randomize_pair_order(pairs_small, seed = 123)

  # Same number of rows
  expect_equal(nrow(pairs_small), nrow(pairs_rand))

  # For each row, the *set* of IDs is unchanged (only order may differ)
  for (i in seq_len(nrow(pairs_small))) {
    orig_ids <- sort(c(pairs_small$ID1[i], pairs_small$ID2[i]))
    rand_ids <- sort(c(pairs_rand$ID1[i], pairs_rand$ID2[i]))
    expect_equal(orig_ids, rand_ids)
  }
})

test_that("randomize_pair_order uses seed for reproducibility", {
  data("example_writing_samples", package = "pairwiseLLM")

  pairs_all <- make_pairs(example_writing_samples)
  pairs_small <- pairs_all[1:20, ]

  out1 <- randomize_pair_order(pairs_small, seed = 999)
  out2 <- randomize_pair_order(pairs_small, seed = 999)

  expect_identical(out1, out2)
})

test_that("randomize_pair_order flips at least one pair when n > 1", {
  data("example_writing_samples", package = "pairwiseLLM")

  pairs_all <- make_pairs(example_writing_samples)
  pairs_small <- pairs_all[1:20, ]

  set.seed(42)
  pairs_rand <- randomize_pair_order(pairs_small, seed = 42)

  any_flipped <- any(
    pairs_small$ID1 != pairs_rand$ID1 |
      pairs_small$ID2 != pairs_rand$ID2
  )

  expect_true(any_flipped)
})

test_that("randomize_pair_order keeps text consistent with IDs", {
  data("example_writing_samples", package = "pairwiseLLM")

  pairs_all <- make_pairs(example_writing_samples)
  pairs_small <- pairs_all[1:15, ]

  # Map from ID -> text to verify alignment
  id_text <- example_writing_samples
  # Make sure the mapping is unique
  expect_true(!any(duplicated(id_text$ID)))

  get_text <- function(id) {
    id_text$text[match(id, id_text$ID)]
  }

  set.seed(101)
  pairs_rand <- randomize_pair_order(pairs_small, seed = 101)

  # For each row, text1/text2 in the randomized output must match
  # the canonical text for ID1/ID2
  for (i in seq_len(nrow(pairs_rand))) {
    expect_identical(pairs_rand$text1[i], get_text(pairs_rand$ID1[i]))
    expect_identical(pairs_rand$text2[i], get_text(pairs_rand$ID2[i]))
  }
})
