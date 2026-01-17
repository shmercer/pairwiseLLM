
testthat::test_that("compute_pair_utility_v3 is stable and finite", {
  withr::local_seed(123)
  theta_draws <- matrix(stats::rnorm(40), nrow = 10, ncol = 4)
  colnames(theta_draws) <- c("A", "B", "C", "D")
  candidates <- tibble::tibble(
    i_id = c("A", "A", "B"),
    j_id = c("B", "C", "D")
  )

  out1 <- pairwiseLLM:::compute_pair_utility_v3(theta_draws, candidates, epsilon_mean = 0.1)
  out2 <- pairwiseLLM:::compute_pair_utility_v3(theta_draws, candidates, epsilon_mean = 0.1)

  expect_equal(out1, out2)
  expect_true(all(is.finite(out1$utility)))
  expect_true(all(out1$utility >= 0))
})
