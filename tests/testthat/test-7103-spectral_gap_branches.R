test_that(".estimate_spectral_gap_lazy_rw handles degenerate / invalid graphs", {
  # self-loops and missing edges are dropped -> no valid edges -> gap 0
  pairs <- tibble::tibble(ID1 = c("A", "B"), ID2 = c("A", "B"))
  out <- pairwiseLLM:::.estimate_spectral_gap_lazy_rw(
    pairs = pairs,
    ids = c("A", "B")
  )
  expect_equal(out$spectral_gap_est[[1]], 0)
  expect_equal(out$lambda2_est[[1]], 1)
  expect_true(out$converged[[1]])
})

test_that(".estimate_spectral_gap_lazy_rw handles zero init and max_iter=0", {
  # 2-node connected graph: the orthogonal subspace eigenvalue is 0, so the
  # method can hit the w_norm == 0 early-exit branch.
  pairs <- tibble::tibble(ID1 = "A", ID2 = "B")

  out_zero_init <- pairwiseLLM:::.estimate_spectral_gap_lazy_rw(
    pairs = pairs,
    ids = c("A", "B"),
    init_vec = c(0, 0),
    max_iter = 10L
  )
  expect_equal(out_zero_init$lambda2_est[[1]], 0)
  expect_equal(out_zero_init$spectral_gap_est[[1]], 1)
  expect_true(out_zero_init$converged[[1]])

  out0 <- pairwiseLLM:::.estimate_spectral_gap_lazy_rw(
    pairs = pairs,
    ids = c("A", "B"),
    max_iter = 0L
  )
  expect_true(is.na(out0$lambda2_est[[1]]))
  expect_true(is.na(out0$spectral_gap_est[[1]]))
  expect_equal(out0$iters[[1]], 0L)
  expect_false(out0$converged[[1]])
})
