testthat::test_that("summarize_draws returns required columns and percentiles", {
  draws <- list(
    theta = matrix(
      c(0, 1, 2,
        1, 2, 3,
        2, 3, 4),
      nrow = 3,
      byrow = TRUE
    ),
    epsilon = c(0.05, 0.10, 0.15)
  )
  colnames(draws$theta) <- c("A", "B", "C")

  out <- pairwiseLLM:::summarize_draws(draws)
  expect_true(all(c("theta_summary", "epsilon_summary") %in% names(out)))

  theta_cols <- c(
    "item_id", "theta_mean", "theta_sd",
    "theta_ci90_low", "theta_ci90_high",
    "theta_ci95_low", "theta_ci95_high"
  )
  expect_true(all(theta_cols %in% names(out$theta_summary)))
  expect_true(all(is.finite(out$theta_summary$theta_mean)))
  expect_true(all(out$theta_summary$theta_ci90_low <= out$theta_summary$theta_ci90_high))
  expect_true(all(out$theta_summary$theta_ci95_low <= out$theta_summary$theta_ci95_high))

  epsilon_cols <- c(
    "epsilon_mean",
    "epsilon_p2.5", "epsilon_p5", "epsilon_p50",
    "epsilon_p95", "epsilon_p97.5"
  )
  expect_true(all(epsilon_cols %in% names(out$epsilon_summary)))
  expect_true(is.finite(out$epsilon_summary$epsilon_mean))
  eps_probs <- stats::quantile(draws$epsilon, probs = c(0.025, 0.05, 0.5, 0.95, 0.975), names = FALSE)
  expect_equal(out$epsilon_summary$epsilon_p2.5, eps_probs[[1L]])
  expect_equal(out$epsilon_summary$epsilon_p5, eps_probs[[2L]])
  expect_equal(out$epsilon_summary$epsilon_p50, eps_probs[[3L]])
  expect_equal(out$epsilon_summary$epsilon_p95, eps_probs[[4L]])
  expect_equal(out$epsilon_summary$epsilon_p97.5, eps_probs[[5L]])
})

testthat::test_that("summarize_draws assigns item_id when theta colnames missing", {
  draws <- list(
    theta = matrix(c(0, 1, 2, 3), nrow = 2, byrow = TRUE),
    epsilon = c(0.05, 0.10)
  )

  out <- pairwiseLLM:::summarize_draws(draws)
  expect_equal(out$theta_summary$item_id, c("1", "2"))
})

testthat::test_that("summarize_draws fills epsilon percentiles with NA when absent", {
  draws <- list(
    theta = matrix(c(0, 1, 2, 3), nrow = 2, byrow = TRUE)
  )
  colnames(draws$theta) <- c("A", "B")

  out <- pairwiseLLM:::summarize_draws(draws, model_variant = "btl")
  testthat::expect_true(all(is.na(out$epsilon_summary)))
})
