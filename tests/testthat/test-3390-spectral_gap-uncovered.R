testthat::test_that("spectral gap helper handles degenerate and disconnected graphs", {
  # n < 2: converged TRUE, NA gap
  out1 <- pairwiseLLM:::.estimate_spectral_gap_lazy_rw(
    pairs = tibble::tibble(ID1 = character(), ID2 = character()),
    ids = "a"
  )
  testthat::expect_true(out1$converged)
  testthat::expect_true(is.na(out1$spectral_gap_est))

  # empty pairs but 2+ ids: gap=0, lambda2=1
  out2 <- pairwiseLLM:::.estimate_spectral_gap_lazy_rw(
    pairs = tibble::tibble(ID1 = character(), ID2 = character()),
    ids = c("a", "b")
  )
  testthat::expect_true(out2$converged)
  testthat::expect_equal(out2$spectral_gap_est, 0)
  testthat::expect_equal(out2$lambda2_est, 1)

  # missing columns => abort
  testthat::expect_error(
    pairwiseLLM:::.estimate_spectral_gap_lazy_rw(
      pairs = tibble::tibble(x = 1),
      ids = c("a", "b")
    ),
    "pairs must contain columns"
  )

  # disconnected graph / isolated node => gap=0
  pairs_disconnected <- tibble::tibble(ID1 = "a", ID2 = "b")
  out3 <- pairwiseLLM:::.estimate_spectral_gap_lazy_rw(
    pairs = pairs_disconnected,
    ids = c("a", "b", "c")
  )
  testthat::expect_true(out3$converged)
  testthat::expect_equal(out3$spectral_gap_est, 0)
  testthat::expect_equal(out3$lambda2_est, 1)
})

testthat::test_that("spectral gap helper supports binary edge de-duplication", {
  pairs_rep <- tibble::tibble(
    ID1 = c("a", "a", "a", "b"),
    ID2 = c("b", "b", "b", "c")
  )

  out_count <- pairwiseLLM:::.estimate_spectral_gap_lazy_rw(
    pairs = pairs_rep,
    ids = c("a", "b", "c"),
    weights = "count",
    max_iter = 50L,
    tol = 1e-8
  )
  out_bin <- pairwiseLLM:::.estimate_spectral_gap_lazy_rw(
    pairs = pairs_rep,
    ids = c("a", "b", "c"),
    weights = "binary",
    max_iter = 50L,
    tol = 1e-8
  )

  testthat::expect_true(out_count$converged)
  testthat::expect_true(out_bin$converged)
  testthat::expect_true(is.finite(out_count$spectral_gap_est))
  testthat::expect_true(is.finite(out_bin$spectral_gap_est))
})