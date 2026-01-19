test_that("MCMC summaries tolerate non-finite draws", {
  theta_draws <- matrix(stats::rnorm(40), nrow = 10, ncol = 4)
  colnames(theta_draws) <- c("A", "B", "C", "D")
  theta_draws[1, 1] <- NA_real_
  theta_draws[2, 2] <- Inf
  theta_draws[3, 3] <- -Inf

  expect_warning(
    theta_sum <- pairwiseLLM:::summarize_theta(theta_draws),
    "Non-finite values detected"
  )

  expect_equal(theta_sum$ID, colnames(theta_draws))
  expect_true(all(is.finite(theta_sum$mean)))
  expect_true(all(is.finite(theta_sum$sd)))
  expect_true(all(is.finite(theta_sum$median)))
})
