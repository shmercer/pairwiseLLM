testthat::test_that("summarize_draws_v3 returns required columns and ordered CIs", {
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

  out <- pairwiseLLM:::summarize_draws_v3(draws)
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
    "epsilon_ci90_low", "epsilon_ci90_high",
    "epsilon_ci95_low", "epsilon_ci95_high"
  )
  expect_true(all(epsilon_cols %in% names(out$epsilon_summary)))
  expect_true(is.finite(out$epsilon_summary$epsilon_mean))
  expect_true(out$epsilon_summary$epsilon_ci90_low <= out$epsilon_summary$epsilon_ci90_high)
  expect_true(out$epsilon_summary$epsilon_ci95_low <= out$epsilon_summary$epsilon_ci95_high)
})
