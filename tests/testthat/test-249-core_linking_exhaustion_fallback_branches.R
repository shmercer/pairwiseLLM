testthat::test_that(".bt_apply_exhaustion_fallback respects forbid_repeats and early-return branches", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("a", "b", "c", "d")
  )

  # Start with no pairs; force fallback to need pairs.
  pairs <- tibble::tibble(ID1 = character(), ID2 = character(), pair_type = character())

  core_ids <- c("A")
  new_ids <- c("B", "C")
  seen_ids <- c(core_ids, new_ids, "D")

  # 1) targeted_repeats does nothing when forbid_repeats = TRUE.
  out1 <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = pairs,
    samples = samples,
    core_ids = core_ids,
    new_ids = new_ids,
    seen_ids = seen_ids,
    round_size = 4,
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

  # 2) cross_batch_new_new adds pairs when there are seen-but-not-core/new IDs.
  set.seed(1)
  out2 <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = pairs,
    samples = samples,
    core_ids = core_ids,
    new_ids = new_ids,
    seen_ids = seen_ids,
    round_size = 4,
    exhaustion_fallback = "cross_batch_new_new",
    exhaustion_min_pairs_frac = 1,
    within_batch_frac = 1,
    core_audit_frac = 0,
    k_neighbors = 10,
    balance_positions = FALSE,
    include_text = FALSE,
    forbid_repeats = FALSE
  )
  testthat::expect_true(nrow(out2) >= 1)
  testthat::expect_true(all(out2$ID1 %in% new_ids))
  testthat::expect_true(all(out2$ID2 %in% "D"))
  testthat::expect_true(all(out2$pair_type %in% "fallback_cross_batch_new_new"))

  # 3) If enough pairs already exist (>= min_pairs_frac threshold), no fallback is added.
  base_pairs <- tibble::tibble(ID1 = c("A", "B"), ID2 = c("C", "D"), pair_type = "base")
  out3 <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = base_pairs,
    samples = samples,
    core_ids = core_ids,
    new_ids = new_ids,
    seen_ids = seen_ids,
    round_size = 4,
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
