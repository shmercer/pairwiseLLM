test_that("5503 fallback diagnostics have stable columns and no list-cols", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "beta", "gamma", "delta")
  )

  theta <- tibble::tibble(
    ID = samples$ID,
    theta = seq_along(samples$ID),
    se = rep(0.1, length(samples$ID))
  )

  out_ok <- pairwiseLLM::select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = NULL,
    n_pairs = 2,
    repeat_policy = "none",
    seed = 1
  )

  diag_ok <- attr(out_ok, "pairing_diagnostics")
  expect_s3_class(diag_ok, "tbl_df")
  expect_equal(nrow(diag_ok), 1L)

  # Force normal selection to be empty by marking all unordered pairs as seen.
  all_pairs_mat <- t(combn(samples$ID, 2))
  existing_pairs <- tibble::tibble(
    ID1 = all_pairs_mat[, 1],
    ID2 = all_pairs_mat[, 2]
  )

  out_empty <- pairwiseLLM::select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing_pairs,
    n_pairs = 2,
    repeat_policy = "none",
    seed = 1
  )

  diag_empty <- attr(out_empty, "pairing_diagnostics")
  expect_s3_class(diag_empty, "tbl_df")
  expect_equal(nrow(diag_empty), 1L)

  required_cols <- c(
    "fallback_path",
    "fallback_trigger",
    "n_pairs_source_normal",
    "n_pairs_source_bridge",
    "n_pairs_source_repeat_reverse",
    "n_pairs_source_random"
  )

  expect_true(all(required_cols %in% names(diag_ok)))
  expect_true(all(required_cols %in% names(diag_empty)))

  # Schema stability: same columns and classes across fallback states.
  expect_identical(names(diag_ok), names(diag_empty))

  class_sig <- function(x) paste(class(x), collapse = "/")
  sig_ok <- vapply(diag_ok, class_sig, character(1))
  sig_empty <- vapply(diag_empty, class_sig, character(1))
  expect_identical(sig_ok, sig_empty)

  # No list-columns.
  expect_false(any(vapply(diag_ok, is.list, logical(1))))
  expect_false(any(vapply(diag_empty, is.list, logical(1))))
})
