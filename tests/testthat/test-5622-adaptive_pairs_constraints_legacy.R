# Workstream E: constraint plumbing and legacy compatibility

# These tests exercise rarely-hit branches in `.ap_apply_constraints()` that are
# responsible for backwards compatibility and edge-case repeat handling.

testthat::test_that(".ap_apply_constraints validates legacy forbid_repeats", {
  id_vec <- c("A", "B")

  cand_tbl <- tibble::tibble(
    i_idx = 1L,
    j_idx = 2L,
    source = "info"
  )

  testthat::expect_error(
    pairwiseLLM:::.ap_apply_constraints(
      cand_tbl = cand_tbl,
      id_vec = id_vec,
      forbid_repeats = "yes"
    ),
    "forbid_repeats"
  )
})

testthat::test_that("reverse_only keeps only the valid repeat_reverse direction", {
  id_vec <- c("A", "B")
  pk <- pairwiseLLM:::.unordered_pair_key("A", "B")

  # Simulate one completed judgment for the unordered pair.
  existing_counts <- stats::setNames(1L, pk)
  existing_dir <- stats::setNames("forward", pk)

  cand_tbl <- tibble::tibble(
    i_idx = c(1L, 2L, 1L),
    j_idx = c(2L, 1L, 2L),
    source = c("repeat_reverse", "repeat_reverse", "info")
  )

  out <- pairwiseLLM:::.ap_apply_constraints(
    cand_tbl = cand_tbl,
    id_vec = id_vec,
    existing_counts = existing_counts,
    existing_dir = existing_dir,
    existing_counts_all = integer(),
    forbid_unordered = TRUE,
    repeat_policy = "reverse_only",
    repeat_cap = 1L
  )

  # Only the reverse repeat should remain: B -> A.
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(out$i_idx, 2L)
  testthat::expect_equal(out$j_idx, 1L)
})

testthat::test_that("repeat_policy = 'forbid_unordered' removes all seen unordered pairs", {
  id_vec <- c("A", "B")
  pk <- pairwiseLLM:::.unordered_pair_key("A", "B")

  cand_tbl <- tibble::tibble(
    i_idx = 2L,
    j_idx = 1L,
    source = "repeat_reverse"
  )

  out <- pairwiseLLM:::.ap_apply_constraints(
    cand_tbl = cand_tbl,
    id_vec = id_vec,
    existing_counts_all = stats::setNames(1L, pk),
    forbid_unordered = FALSE,
    repeat_policy = "forbid_unordered",
    repeat_cap = 1L
  )

  testthat::expect_equal(nrow(out), 0L)
})
