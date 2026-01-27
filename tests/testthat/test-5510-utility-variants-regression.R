testthat::test_that("compute_pair_utility_from_draws matches draw variance by variant", {
  withr::local_seed(1)
  theta_draws <- matrix(
    c(
      0.0, 1.0, -0.5,
      0.2, 1.2, -0.1,
      -0.3, 0.9, 0.0,
      0.5, 1.1, -0.2
    ),
    nrow = 4L,
    byrow = TRUE
  )
  epsilon_draws <- c(0.1, 0.2, 0.15, 0.05)
  beta_draws <- c(0.2, -0.1, 0.0, 0.1)
  deg <- c(0L, 2L, 1L)
  candidate_i <- c(1L, 2L)
  candidate_j <- c(2L, 3L)

  compute_expected <- function(d, variant) {
    p <- stats::plogis(d)
    if (variant == "btl_e") {
      p <- (1 - epsilon_draws) * p + 0.5 * epsilon_draws
    }
    if (variant == "btl_b") {
      p <- stats::plogis(d + beta_draws)
    }
    if (variant == "btl_e_b") {
      p <- stats::plogis(d + beta_draws)
      p <- (1 - epsilon_draws) * p + 0.5 * epsilon_draws
    }
    mean(p^2) - mean(p)^2
  }

  d1 <- theta_draws[, 1] - theta_draws[, 2]
  d2 <- theta_draws[, 2] - theta_draws[, 3]
  expected_btl <- c(compute_expected(d1, "btl"), compute_expected(d2, "btl"))
  expected_btl_e <- c(compute_expected(d1, "btl_e"), compute_expected(d2, "btl_e"))
  expected_btl_b <- c(compute_expected(d1, "btl_b"), compute_expected(d2, "btl_b"))
  expected_btl_e_b <- c(compute_expected(d1, "btl_e_b"), compute_expected(d2, "btl_e_b"))

  out_btl <- pairwiseLLM:::compute_pair_utility_from_draws(
    theta_draws = theta_draws,
    candidate_i = candidate_i,
    candidate_j = candidate_j,
    deg = deg,
    model_variant = "btl"
  )
  out_btl_e <- pairwiseLLM:::compute_pair_utility_from_draws(
    theta_draws = theta_draws,
    candidate_i = candidate_i,
    candidate_j = candidate_j,
    deg = deg,
    model_variant = "btl_e",
    epsilon_draws = epsilon_draws
  )
  out_btl_b <- pairwiseLLM:::compute_pair_utility_from_draws(
    theta_draws = theta_draws,
    candidate_i = candidate_i,
    candidate_j = candidate_j,
    deg = deg,
    model_variant = "btl_b",
    beta_draws = beta_draws
  )
  out_btl_e_b <- pairwiseLLM:::compute_pair_utility_from_draws(
    theta_draws = theta_draws,
    candidate_i = candidate_i,
    candidate_j = candidate_j,
    deg = deg,
    model_variant = "btl_e_b",
    epsilon_draws = epsilon_draws,
    beta_draws = beta_draws
  )

  testthat::expect_equal(out_btl$u0, expected_btl, tolerance = 1e-8)
  testthat::expect_equal(out_btl_e$u0, expected_btl_e, tolerance = 1e-8)
  testthat::expect_equal(out_btl_b$u0, expected_btl_b, tolerance = 1e-8)
  testthat::expect_equal(out_btl_e_b$u0, expected_btl_e_b, tolerance = 1e-8)

  expected_u_btl <- expected_btl / sqrt((deg[candidate_i] + 1) * (deg[candidate_j] + 1))
  testthat::expect_equal(out_btl$u, expected_u_btl, tolerance = 1e-8)
})

testthat::test_that("compute_pair_utility_from_draws validates missing draws", {
  withr::local_seed(1)
  theta_draws <- matrix(c(0, 1, 0, 1), nrow = 2L, byrow = TRUE)
  deg <- c(0L, 0L)

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1L,
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl_e"
    ),
    "epsilon_draws"
  )
  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1L,
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl_e_b"
    ),
    "epsilon_draws"
  )
  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1L,
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl_b"
    ),
    "beta_draws"
  )
  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1L,
      candidate_j = 2L,
      deg = deg,
      model_variant = "btl_e_b",
      epsilon_draws = c(0.1, 0.2)
    ),
    "beta_draws"
  )
})
