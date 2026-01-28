testthat::test_that("item log percentiles match posterior draws", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))

  theta_draws <- matrix(
    c(
      0.3, 0.1, -0.2, 0.0,
      0.2, 0.0, -0.1, -0.1,
      0.4, 0.2, -0.3, 0.1,
      0.1, 0.1, -0.2, -0.2,
      0.25, 0.05, -0.15, -0.05
    ),
    nrow = 5,
    byrow = TRUE,
    dimnames = list(NULL, state$ids)
  )
  fit <- make_v3_fit_contract(state$ids, theta_draws = theta_draws)

  summary <- pairwiseLLM:::build_item_log(state, fit)
  probs <- c(0.025, 0.05, 0.5, 0.95, 0.975)
  theta_quantiles <- apply(theta_draws, 2, stats::quantile, probs = probs, names = FALSE)
  rank_mat <- t(apply(theta_draws, 1, function(row) rank(-row, ties.method = "average")))
  rank_quantiles <- apply(rank_mat, 2, stats::quantile, probs = probs, names = FALSE)

  testthat::expect_equal(summary[["theta_p2.5"]], as.double(theta_quantiles[1L, ]))
  testthat::expect_equal(summary$theta_p5, as.double(theta_quantiles[2L, ]))
  testthat::expect_equal(summary$theta_p50, as.double(theta_quantiles[3L, ]))
  testthat::expect_equal(summary$theta_p95, as.double(theta_quantiles[4L, ]))
  testthat::expect_equal(summary[["theta_p97.5"]], as.double(theta_quantiles[5L, ]))
  testthat::expect_equal(summary[["rank_p2.5"]], as.double(rank_quantiles[1L, ]))
  testthat::expect_equal(summary$rank_p5, as.double(rank_quantiles[2L, ]))
  testthat::expect_equal(summary$rank_p50, as.double(rank_quantiles[3L, ]))
  testthat::expect_equal(summary$rank_p95, as.double(rank_quantiles[4L, ]))
  testthat::expect_equal(summary[["rank_p97.5"]], as.double(rank_quantiles[5L, ]))
  testthat::expect_false(any(c("theta_ci90_lo", "theta_ci95_lo") %in% names(summary)))
})
