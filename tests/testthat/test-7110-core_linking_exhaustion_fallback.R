test_that(".bt_apply_exhaustion_fallback returns early when disabled or sufficient pairs", {
  pairs <- tibble::tibble(ID1 = "A", ID2 = "B")
  samples <- tibble::tibble(ID = c("A", "B"), text = c("t1", "t2"))

  out_none <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = pairs,
    samples = samples,
    core_ids = character(),
    new_ids = character(),
    seen_ids = character(),
    round_size = 2L,
    exhaustion_fallback = "none",
    within_batch_frac = 1,
    core_audit_frac = 0,
    k_neighbors = 0L,
    balance_positions = FALSE
  )
  expect_identical(out_none, pairs)

  out_sufficient <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = pairs,
    samples = samples,
    core_ids = character(),
    new_ids = character(),
    seen_ids = character(),
    round_size = 2L,
    exhaustion_fallback = "cross_batch_new_new",
    exhaustion_min_pairs_frac = 0.5,
    within_batch_frac = 1,
    core_audit_frac = 0,
    k_neighbors = 0L,
    balance_positions = FALSE
  )
  expect_identical(out_sufficient, pairs)
})

test_that(".bt_apply_exhaustion_fallback can add targeted repeats when allowed", {
  pairs <- tibble::tibble(ID1 = character(), ID2 = character())
  samples <- tibble::tibble(ID = c("A", "B"), text = c("t1", "t2"))

  out <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = pairs,
    samples = samples,
    core_ids = character(),
    new_ids = c("A", "B"),
    seen_ids = c("A", "B"),
    round_size = 2L,
    exhaustion_fallback = "targeted_repeats",
    exhaustion_min_pairs_frac = 1,
    within_batch_frac = 1,
    core_audit_frac = 0,
    k_neighbors = 0L,
    balance_positions = FALSE,
    include_text = FALSE,
    forbid_repeats = FALSE
  )

  expect_true(nrow(out) >= 1L)
  expect_true(all(out$pair_type %in% "fallback_targeted_repeats"))
})
