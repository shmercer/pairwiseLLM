test_that("5500 normal-empty leads to exhausted when no pairs remain", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "gamma")
  )

  theta <- tibble::tibble(
    ID = c("A", "B", "C"),
    theta = c(0, 1, 2),
    se = c(0.1, 0.1, 0.1)
  )

  all_pairs_mat <- t(combn(samples$ID, 2))
  existing_pairs <- tibble::tibble(
    ID1 = all_pairs_mat[, 1],
    ID2 = all_pairs_mat[, 2]
  )

  out <- pairwiseLLM::select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing_pairs,
    n_pairs = 2,
    repeat_policy = "none",
    seed = 1
  )

  diag <- attr(out, "pairing_diagnostics")
  expect_s3_class(diag, "tbl_df")
  expect_equal(nrow(diag), 1L)

  expect_identical(diag$fallback_path[[1]], "exhausted_no_pairs")
  expect_identical(diag$fallback_trigger[[1]], "normal_empty")

  expect_true(is.integer(diag$n_pairs_source_normal))
  expect_true(is.integer(diag$n_pairs_source_repeat_reverse))
  expect_true(is.integer(diag$n_pairs_source_bridge))
  expect_true(is.integer(diag$n_pairs_source_random))

  expect_equal(diag$n_pairs_source_normal[[1]], 0L)
  expect_equal(diag$n_pairs_source_repeat_reverse[[1]], 0L)
  expect_equal(diag$n_pairs_source_bridge[[1]], 0L)
  expect_equal(diag$n_pairs_source_random[[1]], 0L)
})
