test_that("make_pairs creates all unordered pairs", {
  samples <- tibble::tibble(
    ID   = c("S1", "S2", "S3"),
    text = c("A", "B", "C")
  )

  pairs <- make_pairs(samples)

  expect_s3_class(pairs, "tbl_df")
  expect_equal(nrow(pairs), 3) # 3C2 = 3 pairs

  # Check that all combinations are present (order-insensitive)
  pair_labels <- apply(pairs[, c("ID1", "ID2")], 1, function(x) {
    paste(sort(x),
      collapse = "-"
    )
  })
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
  expect_true(nrow(pairs_half_1) >= floor(0.5 * n_all)) # due to floor()

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
    # For each reversed row, there should be a matching original row with
    # swapped IDs/texts
    for (i in seq_len(nrow(rev_pairs))) {
      r <- rev_pairs[i, ]

      match_idx <- which(
        pairs$ID1 == r$ID2 &
          pairs$ID2 == r$ID1 &
          pairs$text1 == r$text2 &
          pairs$text2 == r$text1
      )

      expect_true(length(match_idx) == 1)
    }
  }
})

test_that("sample_reverse_pairs handles edge cases for reverse_pct and
          n_reverse", {
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
  rev_limited <- sample_reverse_pairs(pairs,
    reverse_pct = 1,
    n_reverse = 2, seed = 1
  )
  expect_equal(nrow(rev_limited), 2)

  # reverse_pct = 0 and n_reverse = NULL -> empty tibble
  rev_empty <- sample_reverse_pairs(pairs, reverse_pct = 0, seed = 1)
  expect_equal(nrow(rev_empty), 0)
})

test_that("make_pairs on example_writing_samples matches
          example_writing_pairs", {
  data("example_writing_samples", package = "pairwiseLLM")
  data("example_writing_pairs", package = "pairwiseLLM")

  pairs_gen <- make_pairs(example_writing_samples)

  # Compare unordered ID pairs between generated and reference tables
  gen_labels <- apply(pairs_gen[, c("ID1", "ID2")], 1, function(x) {
    paste(sort(x), collapse = "-")
  })
  ref_labels <- apply(example_writing_pairs[, c("ID1", "ID2")], 1, function(x) {
    paste(sort(x), collapse = "-")
  })

  expect_equal(length(gen_labels), length(ref_labels))
  expect_setequal(gen_labels, ref_labels)
})

test_that("sample_pairs returns a subset of example_writing_pairs", {
  data("example_writing_samples", package = "pairwiseLLM")
  data("example_writing_pairs", package = "pairwiseLLM")

  pairs <- make_pairs(example_writing_samples)

  set.seed(123)
  sampled <- sample_pairs(pairs, pair_pct = 0.2)

  # Every sampled pair should correspond to some pair in example_writing_pairs
  sampled_labels <- apply(sampled[, c("ID1", "ID2")], 1, function(x) {
    paste(sort(x), collapse = "-")
  })
  ref_labels <- apply(example_writing_pairs[, c("ID1", "ID2")], 1, function(x) {
    paste(sort(x), collapse = "-")
  })

  expect_true(all(sampled_labels %in% ref_labels))
})

test_that("sample_reverse_pairs produces valid reversed pairs on
          example data", {
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples)

  set.seed(999)
  rev_pairs <- sample_reverse_pairs(pairs, reverse_pct = 0.1)

  # Each reversed pair must correspond to an original pair with swapped roles
  if (nrow(rev_pairs) > 0) {
    for (i in seq_len(nrow(rev_pairs))) {
      r <- rev_pairs[i, ]
      match_idx <- which(
        pairs$ID1 == r$ID2 &
          pairs$ID2 == r$ID1 &
          pairs$text1 == r$text2 &
          pairs$text2 == r$text1
      )
      expect_true(length(match_idx) == 1)
    }
  }
})

testthat::test_that("compute_reverse_consistency matches pairs order-invariantly", {
  # Main: A vs B -> A wins
  main <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")

  # Reverse: B vs A -> A wins (Consistent)
  # Note order of IDs in columns is swapped relative to main
  rev <- tibble::tibble(ID1 = "B", ID2 = "A", better_id = "A")

  res <- compute_reverse_consistency(main, rev)

  # Should find 1 overlapping pair
  testthat::expect_equal(res$summary$n_pairs, 1)
  testthat::expect_equal(res$summary$n_consistent, 1)
  testthat::expect_equal(res$summary$prop_consistent, 1.0)
})

testthat::test_that("check_positional_bias handles zero inconsistencies gracefully", {
  # Create a details object with 100% consistency
  details <- tibble::tibble(
    key = c("A||B", "C||D"),
    ID1_main = c("A", "C"), ID2_main = c("B", "D"), better_id_main = c("A", "C"),
    ID1_rev = c("B", "D"), ID2_rev = c("A", "C"), better_id_rev = c("A", "C"),
    is_consistent = c(TRUE, TRUE)
  )

  consistency_obj <- list(details = details)

  res <- check_positional_bias(consistency_obj, n_boot = 10)

  # Bias counts should be zero, not NA or error
  testthat::expect_equal(res$summary$n_inconsistent, 0)
  testthat::expect_equal(res$summary$n_inconsistent_pos1_bias, 0)
  testthat::expect_equal(res$summary$n_inconsistent_pos2_bias, 0)

  # Check flags in details
  testthat::expect_false(any(res$details$is_pos1_bias))
})

testthat::test_that("check_positional_bias identifies positional bias correctly", {
  # Scenario: Position 1 always wins, causing inconsistency
  # Main: A vs B (A is pos1) -> A wins
  # Rev:  B vs A (B is pos1) -> B wins
  details <- tibble::tibble(
    key = "A||B",
    ID1_main = "A", ID2_main = "B", better_id_main = "A", # Pos 1 wins
    ID1_rev = "B", ID2_rev = "A", better_id_rev = "B", # Pos 1 wins
    is_consistent = FALSE
  )

  res <- check_positional_bias(details, n_boot = 10)

  testthat::expect_equal(res$summary$n_inconsistent, 1)
  testthat::expect_equal(res$summary$n_inconsistent_pos1_bias, 1)
  testthat::expect_equal(res$summary$n_inconsistent_pos2_bias, 0)
  testthat::expect_true(res$details$is_pos1_bias[1])
})

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

test_that("sample_pairs handles empty input gracefully", {
  empty_pairs <- tibble::tibble(
    ID1 = character(), text1 = character(),
    ID2 = character(), text2 = character()
  )

  # Should return empty tibble immediately
  out <- sample_pairs(empty_pairs, pair_pct = 0.5)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
})

test_that("sample_reverse_pairs validation errors are triggered", {
  pairs <- tibble::tibble(ID1 = "A", text1 = "T1", ID2 = "B", text2 = "T2")

  # 1. Missing columns
  bad_pairs <- tibble::tibble(ID1 = "A")
  expect_error(
    sample_reverse_pairs(bad_pairs),
    "must contain columns"
  )

  # 2. Empty input returns empty immediately (covering n == 0 check)
  empty_pairs <- pairs[0, ]
  out <- sample_reverse_pairs(empty_pairs, reverse_pct = 0.5)
  expect_equal(nrow(out), 0)
  expect_equal(names(out), names(pairs))

  # 3. Missing both pct and n arguments
  expect_error(
    sample_reverse_pairs(pairs),
    "Provide at least one of `reverse_pct` or `n_reverse`"
  )

  # 4. Invalid n_reverse (negative or NA)
  expect_error(
    sample_reverse_pairs(pairs, n_reverse = -1),
    "must be a non-negative integer"
  )
  expect_error(
    sample_reverse_pairs(pairs, n_reverse = NA_integer_),
    "must be a non-negative integer"
  )

  # 5. Invalid reverse_pct (non-numeric, NA, or wrong length)
  expect_error(
    sample_reverse_pairs(pairs, reverse_pct = "bad"),
    "must be a single numeric value"
  )
  expect_error(
    sample_reverse_pairs(pairs, reverse_pct = NA_real_),
    "must be a single numeric value"
  )
  expect_error(
    sample_reverse_pairs(pairs, reverse_pct = c(0.1, 0.2)),
    "must be a single numeric value"
  )
})

test_that("randomize_pair_order handles empty input", {
  empty_pairs <- tibble::tibble(
    ID1 = character(), text1 = character(),
    ID2 = character(), text2 = character()
  )
  out <- randomize_pair_order(empty_pairs)
  expect_equal(nrow(out), 0)
})

test_that("randomize_pair_order correctly restores random seed", {
  # This tests the on.exit() block
  set.seed(12345)
  runif(1) # Advance RNG
  state_before <- .Random.seed

  pairs <- tibble::tibble(ID1 = "A", text1 = "T1", ID2 = "B", text2 = "T2")

  # Call with a specific seed
  randomize_pair_order(pairs, seed = 999)

  state_after <- .Random.seed

  # The global RNG state should be exactly as it was before the call
  expect_identical(state_before, state_after)
})

test_that("alternate_pair_order works correctly", {
  # 1. Validation: Missing columns
  expect_error(
    alternate_pair_order(tibble::tibble(ID1 = "A")),
    "must contain columns"
  )

  # 2. Edge case: < 2 rows (returns unchanged)
  pairs_1 <- tibble::tibble(
    ID1 = "A", text1 = "TA", ID2 = "B", text2 = "TB"
  )
  expect_equal(alternate_pair_order(pairs_1), pairs_1)

  # 3. Logic: Flips even rows (2, 4, etc.)
  pairs_4 <- tibble::tibble(
    ID1   = c("A1", "B1", "C1", "D1"),
    text1 = c("TA1", "TB1", "TC1", "TD1"),
    ID2   = c("A2", "B2", "C2", "D2"),
    text2 = c("TA2", "TB2", "TC2", "TD2")
  )

  out <- alternate_pair_order(pairs_4)

  # Row 1 (odd): Unchanged
  expect_equal(out$ID1[1], "A1")
  expect_equal(out$ID2[1], "A2")

  # Row 2 (even): Swapped
  expect_equal(out$ID1[2], "B2") # Was ID2
  expect_equal(out$ID2[2], "B1") # Was ID1
  expect_equal(out$text1[2], "TB2")
  expect_equal(out$text2[2], "TB1")

  # Row 3 (odd): Unchanged
  expect_equal(out$ID1[3], "C1")

  # Row 4 (even): Swapped
  expect_equal(out$ID1[4], "D2")
  expect_equal(out$ID2[4], "D1")
})


test_that("add_pair_texts joins text1/text2 onto ID pairs and preserves extra columns", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "gamma")
  )

  pairs <- tibble::tibble(
    ID1 = c("A", "B"),
    ID2 = c("C", "A"),
    pair_type = c("core_new", "new_new")
  )

  out <- add_pair_texts(pairs, samples)

  expect_true(all(c("ID1", "text1", "ID2", "text2", "pair_type") %in% names(out)))
  expect_equal(out$ID1, pairs$ID1)
  expect_equal(out$ID2, pairs$ID2)
  expect_equal(out$text1, c("alpha", "beta"))
  expect_equal(out$text2, c("gamma", "alpha"))
  expect_equal(out$pair_type, pairs$pair_type)
})

test_that("add_pair_texts supports object1/object2 schema and can fill/overwrite text columns", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )

  bt_pairs <- tibble::tibble(object1 = "A", object2 = "B")
  out_bt <- add_pair_texts(bt_pairs, samples)
  expect_true(all(c("ID1", "text1", "ID2", "text2") %in% names(out_bt)))
  expect_false(any(c("object1", "object2") %in% names(out_bt)))
  expect_equal(out_bt$text1, "alpha")
  expect_equal(out_bt$text2, "beta")

  pairs_partial <- tibble::tibble(
    ID1 = "A",
    text1 = NA_character_,
    ID2 = "B",
    text2 = "CUSTOM"
  )

  out_fill <- add_pair_texts(pairs_partial, samples, overwrite = FALSE)
  expect_equal(out_fill$text1, "alpha")
  expect_equal(out_fill$text2, "CUSTOM")

  out_over <- add_pair_texts(pairs_partial, samples, overwrite = TRUE)
  expect_equal(out_over$text1, "alpha")
  expect_equal(out_over$text2, "beta")
})

test_that("add_pair_texts errors when pair IDs are not present in samples", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  bad_pairs <- tibble::tibble(ID1 = "A", ID2 = "Z")

  expect_error(
    add_pair_texts(bad_pairs, samples),
    "samples\\$ID|must be present"
  )
})
