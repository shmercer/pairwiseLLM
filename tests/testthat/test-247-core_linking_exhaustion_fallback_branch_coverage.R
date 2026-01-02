test_that(".bt_apply_exhaustion_fallback early-returns when fallback is none", {
  pairs <- tibble::tibble(ID1 = character(), ID2 = character(), pair_type = character())
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))

  out <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = pairs,
    samples = samples,
    core_ids = "A",
    new_ids = "B",
    seen_ids = c("A", "B"),
    round_size = 10,
    forbidden_keys = character(),
    exhaustion_fallback = "none",
    exhaustion_min_pairs_frac = 0.9,
    exhaustion_spectral_gap_threshold = 0,
    within_batch_frac = 0.5,
    core_audit_frac = 0,
    k_neighbors = 1,
    balance_positions = FALSE,
    include_text = FALSE,
    forbid_repeats = TRUE
  )

  expect_identical(out, pairs)
})

test_that(".bt_apply_exhaustion_fallback returns pairs when cross-batch has no extra ids", {
  pairs <- tibble::tibble(ID1 = character(), ID2 = character(), pair_type = character())
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))

  out <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = pairs,
    samples = samples,
    core_ids = "A",
    new_ids = "B",
    seen_ids = c("A", "B"), # => extra_ids becomes length 0
    round_size = 10,
    forbidden_keys = character(),
    exhaustion_fallback = "cross_batch_new_new",
    exhaustion_min_pairs_frac = 0.9,
    exhaustion_spectral_gap_threshold = 0,
    within_batch_frac = 0.5,
    core_audit_frac = 0,
    k_neighbors = 1,
    balance_positions = FALSE,
    include_text = FALSE,
    forbid_repeats = TRUE
  )

  expect_identical(out, pairs)
})

test_that(".bt_apply_exhaustion_fallback returns pairs when targeted repeats are forbidden", {
  pairs <- tibble::tibble(ID1 = character(), ID2 = character(), pair_type = character())
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))

  out <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = pairs,
    samples = samples,
    core_ids = "A",
    new_ids = c("B", "C"),
    seen_ids = c("A", "B", "C"),
    round_size = 10,
    forbidden_keys = character(),
    exhaustion_fallback = "targeted_repeats",
    exhaustion_min_pairs_frac = 0.9,
    exhaustion_spectral_gap_threshold = 0,
    within_batch_frac = 0.5,
    core_audit_frac = 0,
    k_neighbors = 1,
    balance_positions = FALSE,
    include_text = FALSE,
    forbid_repeats = TRUE # => forces NULL from targeted repeats
  )

  expect_identical(out, pairs)
})

test_that(".bt_apply_exhaustion_fallback returns pairs when new_ids has length < 2", {
  pairs <- tibble::tibble(ID1 = character(), ID2 = character(), pair_type = character())
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))

  out <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = pairs,
    samples = samples,
    core_ids = "A",
    new_ids = "B", # length < 2 => NULL from targeted repeats helper
    seen_ids = c("A", "B"),
    round_size = 10,
    forbidden_keys = character(),
    exhaustion_fallback = "targeted_repeats",
    exhaustion_min_pairs_frac = 0.9,
    exhaustion_spectral_gap_threshold = 0,
    within_batch_frac = 0.5,
    core_audit_frac = 0,
    k_neighbors = 1,
    balance_positions = FALSE,
    include_text = FALSE,
    forbid_repeats = FALSE
  )

  expect_identical(out, pairs)
})
