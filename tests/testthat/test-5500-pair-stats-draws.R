
testthat::test_that("compute_pair_stats_from_draws matches draw-based stats", {
  withr::local_seed(1)
  theta_draws <- matrix(
    c(
      0, 0, 0,
      1, 0, -1,
      2, 0, -2,
      3, 0, -3
    ),
    nrow = 4,
    byrow = TRUE
  )
  colnames(theta_draws) <- c("A", "B", "C")
  candidates <- tibble::tibble(i_id = c("A", "A"), j_id = c("B", "C"))

  out <- pairwiseLLM:::compute_pair_stats_from_draws(theta_draws, candidates)

  d_ab <- theta_draws[, "A"] - theta_draws[, "B"]
  d_ac <- theta_draws[, "A"] - theta_draws[, "C"]

  expect_equal(out$mean_d, c(mean(d_ab), mean(d_ac)))
  expect_equal(out$var_d, c(stats::var(d_ab), stats::var(d_ac)))
  expect_equal(out$p_mean, c(mean(stats::plogis(d_ab)), mean(stats::plogis(d_ac))))
})
