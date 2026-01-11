# SPDX-License-Identifier: MIT

test_that("Spectral-gap estimator is deterministic and schema-stable", {
  ids <- as.character(1:6)

  # A simple cycle graph: no bridges, decent mixing.
  pairs <- tibble::tibble(
    ID1 = c("1", "2", "3", "4", "5", "6"),
    ID2 = c("2", "3", "4", "5", "6", "1")
  )

  out1 <- pairwiseLLM:::.estimate_spectral_gap_lazy_rw(
    pairs = pairs,
    ids = ids,
    weights = "binary",
    max_iter = 200L,
    tol = 1e-8
  )

  out2 <- pairwiseLLM:::.estimate_spectral_gap_lazy_rw(
    pairs = pairs,
    ids = ids,
    weights = "binary",
    max_iter = 200L,
    tol = 1e-8
  )

  expect_true(is.data.frame(out1))
  expect_true(all(c("spectral_gap_est", "lambda2_est", "iters", "converged") %in% names(out1)))
  expect_equal(names(out1), names(out2))
  expect_equal(out1, out2, tolerance = 1e-12)
  expect_true(isTRUE(out1$converged[[1]]))
  expect_true(is.finite(out1$spectral_gap_est[[1]]))
})
