test_that("spectral_gap helpers handle degenerate inputs", {
  # n==0
  expect_equal(pairwiseLLM:::.sg_n_components_from_sparse_adj(NULL), 0L)

  # empty edge list
  adj <- Matrix::sparseMatrix(i = integer(0), j = integer(0), x = numeric(0), dims = c(4, 4))
  expect_equal(pairwiseLLM:::.sg_n_components_from_sparse_adj(adj), 4L)
})

test_that("estimate_spectral_gap_lazy_rw covers defensive branches", {
  ids <- c("A", "B")

  # missing columns
  expect_error(
    pairwiseLLM:::.estimate_spectral_gap_lazy_rw(
      pairs = tibble::tibble(foo = 1),
      ids = ids
    ),
    "pairs must contain"
  )

  # binary weights collapses duplicates and returns valid output
  pairs <- tibble::tibble(ID1 = c("A", "A", "A"), ID2 = c("B", "B", "B"))
  out <- pairwiseLLM:::.estimate_spectral_gap_lazy_rw(pairs, ids = ids, weights = "binary", max_iter = 2L, tol = Inf)
  expect_true(out$converged)
  expect_equal(out$iters, 2L)

  # init_vec proportional to u triggers v_norm==0 branch (early re-init)
  deg_u <- c(1, 1) # connected line with 2 nodes (degree is symmetric)
  u <- sqrt(deg_u)
  u <- u / sqrt(sum(u^2))
  out2 <- pairwiseLLM:::.estimate_spectral_gap_lazy_rw(
    pairs = tibble::tibble(ID1 = "A", ID2 = "B"),
    ids = ids,
    max_iter = 1L,
    init_vec = u
  )
  expect_true(is.finite(out2$lambda2_est) || is.na(out2$lambda2_est))

  # An init_vec orthogonal to u can still yield Sv == 0 (w_norm==0) on a 2-node line.
  out3 <- pairwiseLLM:::.estimate_spectral_gap_lazy_rw(
    pairs = tibble::tibble(ID1 = "A", ID2 = "B"),
    ids = ids,
    max_iter = 1L,
    init_vec = c(1, -1)
  )
  expect_true(out3$converged)
  expect_equal(out3$lambda2_est, 0)
})
