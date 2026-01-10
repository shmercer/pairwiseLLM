testthat::test_that("core linking exhaustion fallback covers all modes", {
  # .bt_apply_exhaustion_fallback is used by the core-linking runners when the
  # normal pair generator can't supply enough pairs.

  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D", "E"),
    text = c("a", "b", "c", "d", "e")
  )

  core_ids <- c("A", "B")
  new_ids <- c("C", "D")
  seen_ids <- c(core_ids, new_ids, "E")

  base_pairs <- tibble::tibble(ID1 = "A", ID2 = "B", pair_type = "base")

  # none => unchanged
  out_none <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = base_pairs,
    samples = samples,
    core_ids = core_ids,
    new_ids = new_ids,
    seen_ids = seen_ids,
    round_size = 10,
    exhaustion_fallback = "none",
    exhaustion_min_pairs_frac = 0.5,
    within_batch_frac = 0.5,
    core_audit_frac = 0,
    k_neighbors = 5,
    balance_positions = TRUE,
    include_text = TRUE,
    forbid_repeats = TRUE
  )
  testthat::expect_equal(nrow(out_none), nrow(base_pairs))

  # cross_batch_new_new uses seen-but-not-core/new IDs as opponents for new_ids
  set.seed(1)
  out_cross <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = tibble::tibble(ID1 = character(), ID2 = character()),
    samples = samples,
    core_ids = core_ids,
    new_ids = new_ids,
    seen_ids = seen_ids,
    round_size = 10,
    exhaustion_fallback = "cross_batch_new_new",
    exhaustion_min_pairs_frac = 0.5,
    within_batch_frac = 0.5,
    core_audit_frac = 0,
    k_neighbors = 5,
    balance_positions = TRUE,
    include_text = TRUE,
    forbid_repeats = TRUE
  )
  # Extra IDs are {E}; candidates are (C,E) and (D,E)
  testthat::expect_true(nrow(out_cross) >= 1)
  testthat::expect_true(all(out_cross$ID1 %in% c("C", "D")))
  testthat::expect_true(all(out_cross$ID2 %in% c("E")))
  testthat::expect_true(all(c("text1", "text2") %in% names(out_cross)))

  # targeted_repeats does nothing when repeats are forbidden
  out_rep_forbidden <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = tibble::tibble(ID1 = character(), ID2 = character()),
    samples = samples,
    core_ids = core_ids,
    new_ids = new_ids,
    seen_ids = seen_ids,
    round_size = 10,
    exhaustion_fallback = "targeted_repeats",
    exhaustion_min_pairs_frac = 0.5,
    within_batch_frac = 0.5,
    core_audit_frac = 0,
    k_neighbors = 5,
    balance_positions = TRUE,
    include_text = TRUE,
    forbid_repeats = TRUE
  )
  testthat::expect_equal(nrow(out_rep_forbidden), 0)

  # targeted_repeats allowed => adds at least one new-new pair
  set.seed(1)
  out_rep <- pairwiseLLM:::.bt_apply_exhaustion_fallback(
    pairs = tibble::tibble(ID1 = character(), ID2 = character()),
    samples = samples,
    core_ids = core_ids,
    new_ids = new_ids,
    seen_ids = seen_ids,
    round_size = 10,
    exhaustion_fallback = "targeted_repeats",
    exhaustion_min_pairs_frac = 0.5,
    within_batch_frac = 0.5,
    core_audit_frac = 0,
    k_neighbors = 5,
    balance_positions = TRUE,
    include_text = TRUE,
    forbid_repeats = FALSE
  )
  testthat::expect_true(nrow(out_rep) >= 1)
  testthat::expect_true(any(out_rep$pair_type == "fallback_targeted_repeats"))
  # At least one new-new pair should be (C,D), order may flip.
  keys <- paste(pmin(out_rep$ID1, out_rep$ID2), pmax(out_rep$ID1, out_rep$ID2), sep = "|")
  testthat::expect_true(any(keys == "C|D"))
})
