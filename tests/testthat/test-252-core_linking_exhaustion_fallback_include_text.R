testthat::test_that(".bt_apply_exhaustion_fallback attaches texts when include_text=TRUE and respects exhaustion_fallback='none'", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("a", "b", "c", "d")
  )

  base_pairs <- tibble::tibble(ID1 = "B", ID2 = "C", pair_type = "base")

  # 1) exhaustion_fallback = 'none' should be a no-op.
  out0 <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = base_pairs,
    samples = samples,
    core_ids = c("A"),
    new_ids = c("B", "C"),
    seen_ids = c("A", "B", "C", "D"),
    round_size = 4,
    forbidden_keys = character(),
    exhaustion_fallback = "none",
    exhaustion_min_pairs_frac = 1,
    within_batch_frac = 1,
    core_audit_frac = 0,
    k_neighbors = 10,
    balance_positions = FALSE,
    include_text = TRUE,
    forbid_repeats = FALSE
  )
  testthat::expect_identical(out0, base_pairs)

  # 2) 'both' should fall back to targeted repeats when cross-batch has no candidates
  # (extra_ids empty), and when repeats are allowed.
  pairs_empty <- tibble::tibble(ID1 = character(), ID2 = character(), pair_type = character())

  out1 <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = pairs_empty,
    samples = samples,
    core_ids = c("A"),
    new_ids = c("B", "C"),
    seen_ids = c("A", "B", "C"),
    round_size = 4,
    forbidden_keys = character(),
    exhaustion_fallback = "both",
    exhaustion_min_pairs_frac = 1,
    within_batch_frac = 1,
    core_audit_frac = 0,
    k_neighbors = 10,
    balance_positions = FALSE,
    include_text = TRUE,
    forbid_repeats = FALSE
  )

  testthat::expect_true(all(c("text1", "text2") %in% names(out1)))
  testthat::expect_true(all(out1$pair_type %in% c("fallback_targeted_repeats")))
  testthat::expect_true(all(out1$text1 %in% samples$text))
  testthat::expect_true(all(out1$text2 %in% samples$text))
})
