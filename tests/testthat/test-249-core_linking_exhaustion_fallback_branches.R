testthat::test_that(".bt_apply_exhaustion_fallback respects forbid_repeats and forbidden_keys branches", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("a", "b", "c", "d")
  )

  # Start with no pairs; force fallback to need pairs
  pairs <- tibble::tibble(ID1 = character(), ID2 = character(), pair_type = character())

  core_ids <- c("A")
  new_ids <- c("B", "C")
  seen_ids <- c(core_ids, new_ids, "D")

  # 1) targeted_repeats does nothing when forbid_repeats = TRUE
  out1 <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = pairs,
    samples = samples,
    core_ids = core_ids,
    new_ids = new_ids,
    seen_ids = seen_ids,
    round_size = 4,
    forbidden_keys = character(),
    exhaustion_fallback = "targeted_repeats",
    exhaustion_min_pairs_frac = 1,
    within_batch_frac = 1,
    core_audit_frac = 0,
    k_neighbors = 10,
    balance_positions = FALSE,
    include_text = FALSE,
    forbid_repeats = TRUE
  )
  testthat::expect_equal(nrow(out1), 0)

  # 2) cross_batch_new_new can be fully eliminated by forbidden_keys
  # Construct keys for all possible (new_ids x extra_ids) pairs.
  extra_ids <- setdiff(unique(seen_ids), c(core_ids, new_ids))
  cand <- tidyr::expand_grid(ID1 = new_ids, ID2 = extra_ids)
  forbidden <- pairwiseLLM:::.unordered_pair_key(cand$ID1, cand$ID2)

  out2 <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = pairs,
    samples = samples,
    core_ids = core_ids,
    new_ids = new_ids,
    seen_ids = seen_ids,
    round_size = 4,
    forbidden_keys = forbidden,
    exhaustion_fallback = "cross_batch_new_new",
    exhaustion_min_pairs_frac = 1,
    within_batch_frac = 1,
    core_audit_frac = 0,
    k_neighbors = 10,
    balance_positions = FALSE,
    include_text = FALSE,
    forbid_repeats = FALSE
  )
  testthat::expect_equal(nrow(out2), 0)

  # 3) if enough pairs already exist (>= min_pairs_frac threshold), no fallback is added
  base_pairs <- tibble::tibble(ID1 = c("A", "B"), ID2 = c("C", "D"), pair_type = "base")
  out3 <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = base_pairs,
    samples = samples,
    core_ids = core_ids,
    new_ids = new_ids,
    seen_ids = seen_ids,
    round_size = 4,
    forbidden_keys = character(),
    exhaustion_fallback = "both",
    exhaustion_min_pairs_frac = 0.5,
    within_batch_frac = 1,
    core_audit_frac = 0,
    k_neighbors = 10,
    balance_positions = FALSE,
    include_text = FALSE,
    forbid_repeats = FALSE
  )
  testthat::expect_equal(nrow(out3), nrow(base_pairs))
  testthat::expect_identical(out3$pair_type, base_pairs$pair_type)
})
