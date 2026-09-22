test_that("Helmert bases are deterministic, orthonormal, and centered", {
  for (n in c(1L, 2L, 3L, 12L)) {
    ids <- paste0("item", seq_len(n))
    b <- pairwiseLLM:::.link_basis(rev(ids))
    expect_identical(b, pairwiseLLM:::.link_basis(ids))
    expect_equal(unname(crossprod(b$H)), diag(n - 1L), tolerance = 1e-12)
    expect_equal(unname(colSums(b$H)), rep(0, n - 1L), tolerance = 1e-12)
    z <- stats::setNames(seq_len(n) - mean(seq_len(n)), b$item_ids)
    u <- pairwiseLLM:::.link_to_reduced(z, b)
    expect_equal(pairwiseLLM:::.link_from_reduced(u, b), z, tolerance = 1e-12)
    draws <- rbind(z, z * 2)
    expect_equal(pairwiseLLM:::.link_from_reduced(pairwiseLLM:::.link_to_reduced(draws, b), b), draws, tolerance = 1e-12)
  }
  b <- pairwiseLLM:::.link_basis(c("c", "b", "a"))
  expect_equal(b$H[, 1], c(a = -1/sqrt(2), b = 1/sqrt(2), c = 0))
  z <- c(c = 3, a = -2, b = -1)
  expect_identical(pairwiseLLM:::.link_to_reduced(z, b), pairwiseLLM:::.link_to_reduced(z[3:1], b))
  expect_error(pairwiseLLM:::.link_to_reduced(z + 1, b), "already be centered")
  expect_error(pairwiseLLM:::.link_to_reduced(unname(z), b), "names must match")
  bad <- b
  bad$H[1, 1] <- 4
  expect_error(pairwiseLLM:::.link_validate_basis(bad), "orthonormal")
  bad <- b
  rownames(bad$H) <- rev(rownames(bad$H))
  expect_error(pairwiseLLM:::.link_validate_basis(bad), "ordering")
})

test_that("covariance transformations retain off-diagonals and identity", {
  b <- pairwiseLLM:::.link_basis(c("a", "b", "c"))
  cov <- matrix(c(2, .7, .7, 1), 2, dimnames = list(c("u1", "u2"), c("u1", "u2")))
  items <- pairwiseLLM:::.link_cov_to_items(cov, b)
  expect_equal(rowSums(items), c(a = 0, b = 0, c = 0), tolerance = 1e-12)
  expect_equal(pairwiseLLM:::.link_cov_to_reduced(items[3:1, 3:1], b), cov, tolerance = 1e-12)
  expect_equal(pairwiseLLM:::.link_cov_to_items(cov[2:1, 2:1], b), items)
  bad <- cov
  bad[1, 2] <- 5
  expect_error(pairwiseLLM:::.link_cov_to_items(bad, b), "symmetric")
  bad <- cov
  bad[1, 1] <- -1
  expect_error(pairwiseLLM:::.link_cov_to_items(bad, b), "positive semidefinite")
  expect_error(pairwiseLLM:::.link_cov_to_reduced(items + diag(3), b), "centered shapes")
  b <- pairwiseLLM:::.link_basis("a")
  empty <- matrix(numeric(), 0, 0)
  expect_equal(pairwiseLLM:::.link_cov_to_items(empty, b), matrix(0, 1, 1, dimnames = list("a", "a")))
  expect_equal(pairwiseLLM:::.link_cov_to_reduced(matrix(0, 1, 1, dimnames = list("a", "a")), b), empty)
})

test_that("joint transforms explicitly identify only the spoke offset", {
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    x <- link_contract_input(id)
    T <- x$item_transform
    expect_equal(T[, 1], c(0, 0, 1, 1))
    expect_identical(colnames(T), if (id == "fixed_shape_offset") "delta" else c("delta", "hub_u1", "spoke_u1"))
  }
  basis <- list(hub = pairwiseLLM:::.link_basis("h"), spoke = pairwiseLLM:::.link_basis("s"))
  expect_equal(pairwiseLLM:::.link_item_transform(basis, "joint_offset"), matrix(c(0, 1), 2, dimnames = list(NULL, "delta")))
})
