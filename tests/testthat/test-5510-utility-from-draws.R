testthat::test_that("compute_pair_utility_from_draws matches draw moments by variant", {
  withr::local_seed(123)
  n_draws <- 200L
  n_items <- 6L
  theta_draws <- matrix(
    stats::rnorm(n_draws * n_items, sd = 0.7),
    nrow = n_draws,
    ncol = n_items
  )
  epsilon_draws <- stats::rbeta(n_draws, 2, 20)
  beta_draws <- stats::rnorm(n_draws, 0, 0.3)
  deg <- as.integer(0:5)
  candidate_i <- c(1L, 1L, 4L, 2L)
  candidate_j <- c(2L, 3L, 5L, 6L)

  out_btl <- pairwiseLLM:::compute_pair_utility_from_draws(
    theta_draws = theta_draws,
    candidate_i = candidate_i,
    candidate_j = candidate_j,
    deg = deg,
    model_variant = "btl"
  )
  d <- theta_draws[, 1] - theta_draws[, 2]
  p_btl <- stats::plogis(d)
  expected_btl <- mean(p_btl^2) - mean(p_btl)^2
  expected_btl_u <- expected_btl / sqrt((deg[[1L]] + 1) * (deg[[2L]] + 1))
  testthat::expect_equal(out_btl$u0[[1L]], expected_btl, tolerance = 1e-8)
  testthat::expect_equal(out_btl$u[[1L]], expected_btl_u, tolerance = 1e-8)

  out_btl_e <- pairwiseLLM:::compute_pair_utility_from_draws(
    theta_draws = theta_draws,
    candidate_i = candidate_i,
    candidate_j = candidate_j,
    deg = deg,
    model_variant = "btl_e",
    epsilon_draws = epsilon_draws
  )
  p_btl_e <- (1 - epsilon_draws) * stats::plogis(d) + epsilon_draws * 0.5
  expected_btl_e <- mean(p_btl_e^2) - mean(p_btl_e)^2
  testthat::expect_equal(out_btl_e$u0[[1L]], expected_btl_e, tolerance = 1e-8)

  out_btl_b <- pairwiseLLM:::compute_pair_utility_from_draws(
    theta_draws = theta_draws,
    candidate_i = candidate_i,
    candidate_j = candidate_j,
    deg = deg,
    model_variant = "btl_b",
    beta_draws = beta_draws
  )
  p_btl_b <- stats::plogis(d + beta_draws)
  expected_btl_b <- mean(p_btl_b^2) - mean(p_btl_b)^2
  testthat::expect_equal(out_btl_b$u0[[1L]], expected_btl_b, tolerance = 1e-8)

  out_btl_e_b <- pairwiseLLM:::compute_pair_utility_from_draws(
    theta_draws = theta_draws,
    candidate_i = candidate_i,
    candidate_j = candidate_j,
    deg = deg,
    model_variant = "btl_e_b",
    epsilon_draws = epsilon_draws,
    beta_draws = beta_draws
  )
  p_btl_e_b <- (1 - epsilon_draws) * stats::plogis(d + beta_draws) + epsilon_draws * 0.5
  expected_btl_e_b <- mean(p_btl_e_b^2) - mean(p_btl_e_b)^2
  testthat::expect_equal(out_btl_e_b$u0[[1L]], expected_btl_e_b, tolerance = 1e-8)
})

testthat::test_that("compute_pair_utility_from_draws handles variant effects", {
  withr::local_seed(123)
  n_draws <- 200L
  n_items <- 6L
  theta_draws <- matrix(
    stats::rnorm(n_draws * n_items, sd = 0.1),
    nrow = n_draws,
    ncol = n_items
  )
  theta_draws[, 1] <- stats::rnorm(n_draws, 3, 0.1)
  theta_draws[, 2] <- stats::rnorm(n_draws, -3, 0.1)
  epsilon_draws <- stats::rbeta(n_draws, 2, 20)
  beta_draws <- stats::rnorm(n_draws, 0, 0.3)
  deg <- rep(0, n_items)

  out_btl <- pairwiseLLM:::compute_pair_utility_from_draws(
    theta_draws = theta_draws,
    candidate_i = 1L,
    candidate_j = 2L,
    deg = deg,
    model_variant = "btl"
  )
  out_btl_e <- pairwiseLLM:::compute_pair_utility_from_draws(
    theta_draws = theta_draws,
    candidate_i = 1L,
    candidate_j = 2L,
    deg = deg,
    model_variant = "btl_e",
    epsilon_draws = epsilon_draws
  )
  testthat::expect_lt(out_btl_e$u0[[1L]], out_btl$u0[[1L]])

  out_btl_b <- pairwiseLLM:::compute_pair_utility_from_draws(
    theta_draws = theta_draws,
    candidate_i = 1L,
    candidate_j = 2L,
    deg = deg,
    model_variant = "btl_b",
    beta_draws = beta_draws
  )
  out_btl_b0 <- pairwiseLLM:::compute_pair_utility_from_draws(
    theta_draws = theta_draws,
    candidate_i = 1L,
    candidate_j = 2L,
    deg = deg,
    model_variant = "btl_b",
    beta_draws = rep(0, n_draws)
  )
  testthat::expect_true(abs(out_btl_b$u0[[1L]] - out_btl_b0$u0[[1L]]) > 1e-6)
})

testthat::test_that("compute_pair_utility_from_draws validates required inputs", {
  withr::local_seed(123)
  n_draws <- 200L
  n_items <- 6L
  theta_draws <- matrix(stats::rnorm(n_draws * n_items), nrow = n_draws, ncol = n_items)
  deg <- rep(0, n_items)

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
      model_variant = "btl_b"
    ),
    "beta_draws"
  )
  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility_from_draws(
      theta_draws = theta_draws,
      candidate_i = 1L,
      candidate_j = 1L,
      deg = deg,
      model_variant = "btl"
    ),
    "different"
  )
})

testthat::test_that("compute_pair_utility_dispatch prefers draw-based utility", {
  withr::local_seed(123)
  n_draws <- 200L
  n_items <- 6L
  ids <- paste0("item-", seq_len(n_items))
  theta_draws <- matrix(
    stats::rnorm(n_draws * n_items, sd = 0.7),
    nrow = n_draws,
    ncol = n_items,
    dimnames = list(NULL, ids)
  )
  fit <- pairwiseLLM:::build_v3_fit_contract(
    theta_draws = theta_draws,
    diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000),
    model_variant = "btl"
  )

  samples <- tibble::tibble(ID = ids, text = ids)
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, model_variant = "btl")
  state$deg <- stats::setNames(as.integer(0:5), ids)

  candidates <- tibble::tibble(i_id = ids[[1L]], j_id = ids[[2L]])
  out <- pairwiseLLM:::compute_pair_utility_dispatch(
    fit = fit,
    candidates = candidates,
    state = state,
    config = state$config$v3,
    diagnostics_pass = TRUE
  )

  d <- theta_draws[, 1] - theta_draws[, 2]
  p <- stats::plogis(d)
  expected_u0 <- mean(p^2) - mean(p)^2
  expected_u <- expected_u0 / sqrt((state$deg[[1L]] + 1) * (state$deg[[2L]] + 1))

  testthat::expect_equal(out$utility_raw[[1L]], expected_u0, tolerance = 1e-8)
  testthat::expect_equal(out$utility[[1L]], expected_u, tolerance = 1e-8)
})
