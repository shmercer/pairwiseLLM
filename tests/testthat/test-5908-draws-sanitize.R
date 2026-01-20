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

test_that(".pairwiseLLM_sanitize_draws_matrix validates and fills missing colnames", {
  testthat::expect_error(
    pairwiseLLM:::.pairwiseLLM_sanitize_draws_matrix(draws = 1:3, name = "x"),
    "`x` must be a numeric matrix"
  )

  testthat::expect_error(
    pairwiseLLM:::.pairwiseLLM_sanitize_draws_matrix(draws = matrix(numeric(), nrow = 0, ncol = 2), name = "x"),
    "at least one row and column"
  )

  draws_ok <- matrix(c(1, 2, 3, 4), nrow = 2)
  testthat::expect_silent(
    draws_ok_out <- pairwiseLLM:::.pairwiseLLM_sanitize_draws_matrix(draws_ok, name = "x")
  )
  testthat::expect_identical(draws_ok_out, draws_ok)

  draws <- matrix(c(NA_real_, NA_real_, 1, 2), nrow = 2)
  testthat::expect_warning(
    draws_out <- pairwiseLLM:::.pairwiseLLM_sanitize_draws_matrix(draws, name = "x"),
    "Non-finite values detected"
  )
  testthat::expect_equal(colnames(draws_out), c("1", "2"))
  testthat::expect_equal(draws_out[, 1], c(0, 0))
  testthat::expect_equal(draws_out[, 2], c(1, 2))
})
