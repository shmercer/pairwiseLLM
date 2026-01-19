
testthat::test_that("epsilon_mean validation covers error branches", {
  withr::local_seed(1)

  bad_alpha <- list(config = list(v3 = list(epsilon_prior_alpha = NA_real_, epsilon_prior_beta = 2)))
  testthat::expect_error(
    pairwiseLLM:::.adaptive_epsilon_mean_from_state(bad_alpha, fit = NULL),
    "epsilon_prior_alpha"
  )

  bad_beta <- list(config = list(v3 = list(epsilon_prior_alpha = 2, epsilon_prior_beta = Inf)))
  testthat::expect_error(
    pairwiseLLM:::.adaptive_epsilon_mean_from_state(bad_beta, fit = NULL),
    "epsilon_prior_beta"
  )

  bad_pos <- list(config = list(v3 = list(epsilon_prior_alpha = 0, epsilon_prior_beta = 2)))
  testthat::expect_error(
    pairwiseLLM:::.adaptive_epsilon_mean_from_state(bad_pos, fit = NULL),
    "must be positive"
  )

  bad_summary_type <- list(epsilon_summary = 1)
  testthat::expect_error(
    pairwiseLLM:::.adaptive_epsilon_mean_from_state(NULL, bad_summary_type),
    "epsilon_summary"
  )

  bad_summary_cols <- list(epsilon_summary = tibble::tibble(foo = 1))
  testthat::expect_error(
    pairwiseLLM:::.adaptive_epsilon_mean_from_state(NULL, bad_summary_cols),
    "epsilon_mean"
  )

  state_cfg <- list(config = list(v3 = list(epsilon_mean = 0.22)))
  testthat::expect_equal(pairwiseLLM:::.adaptive_epsilon_mean_from_state(state_cfg, NULL), 0.22)

  state_bad_eps <- list(config = list(v3 = list(epsilon_mean = NA_real_)))
  testthat::expect_error(
    pairwiseLLM:::.adaptive_epsilon_mean_from_state(state_bad_eps, NULL),
    "epsilon_mean"
  )

  state_bad_eps2 <- list(config = list(v3 = list(epsilon_mean = 1.2)))
  testthat::expect_error(
    pairwiseLLM:::.adaptive_epsilon_mean_from_state(state_bad_eps2, NULL),
    "in \\[0, 1\\]"
  )

  fit_eps <- list(epsilon_mean = 0.12)
  testthat::expect_equal(pairwiseLLM:::.adaptive_epsilon_mean_from_state(NULL, fit_eps), 0.12)
})

testthat::test_that("compute_pair_stats_from_draws validates inputs", {
  withr::local_seed(1)

  testthat::expect_error(
    pairwiseLLM:::compute_pair_stats_from_draws(list(), tibble::tibble(i_id = "A", j_id = "B")),
    "numeric matrix"
  )

  draws_one <- matrix(0, nrow = 1, ncol = 2)
  colnames(draws_one) <- c("A", "B")
  testthat::expect_error(
    pairwiseLLM:::compute_pair_stats_from_draws(draws_one, tibble::tibble(i_id = "A", j_id = "B")),
    "at least two"
  )

  draws_inf <- matrix(c(0, Inf, 0, 0), nrow = 2)
  colnames(draws_inf) <- c("A", "B")
  testthat::expect_error(
    pairwiseLLM:::compute_pair_stats_from_draws(draws_inf, tibble::tibble(i_id = "A", j_id = "B")),
    "finite"
  )

  draws <- matrix(0, nrow = 2, ncol = 2)
  testthat::expect_error(
    pairwiseLLM:::compute_pair_stats_from_draws(draws, tibble::tibble(i_id = "A", j_id = "B")),
    "column names"
  )

  draws <- matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE)
  colnames(draws) <- c("A", "B")
  testthat::expect_error(
    pairwiseLLM:::compute_pair_stats_from_draws(draws, list()),
    "data frame"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_stats_from_draws(draws, tibble::tibble(x = 1)),
    "i_id"
  )

  empty_out <- pairwiseLLM:::compute_pair_stats_from_draws(
    draws,
    tibble::tibble(i_id = character(), j_id = character())
  )
  testthat::expect_equal(nrow(empty_out), 0L)

  testthat::expect_error(
    pairwiseLLM:::compute_pair_stats_from_draws(draws, tibble::tibble(i_id = NA, j_id = "B")),
    "non-missing"
  )

  testthat::expect_error(
    pairwiseLLM:::compute_pair_stats_from_draws(draws, tibble::tibble(i_id = "A", j_id = "C")),
    "present in `draws`"
  )
})

testthat::test_that("compute_pair_stats_from_draws accepts i/j column names", {
  withr::local_seed(1)

  draws <- matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE)
  colnames(draws) <- c("A", "B")
  candidates <- tibble::tibble(i = "A", j = "B")
  out <- pairwiseLLM:::compute_pair_stats_from_draws(draws, candidates)
  testthat::expect_equal(out$i, "A")
  testthat::expect_equal(out$j, "B")
})

testthat::test_that("utility_delta_var_p validates inputs", {
  withr::local_seed(1)

  testthat::expect_error(
    pairwiseLLM:::utility_delta_var_p("x", 1, 0.1),
    "numeric"
  )
  testthat::expect_error(
    pairwiseLLM:::utility_delta_var_p(c(1, 2), 1, 0.1),
    "same length"
  )
  testthat::expect_equal(
    pairwiseLLM:::utility_delta_var_p(numeric(), numeric(), 0.1),
    numeric()
  )
  testthat::expect_error(
    pairwiseLLM:::utility_delta_var_p(c(Inf), c(1), 0.1),
    "finite"
  )
  testthat::expect_error(
    pairwiseLLM:::utility_delta_var_p(c(0), c(-1), 0.1),
    "non-negative"
  )
  testthat::expect_error(
    pairwiseLLM:::utility_delta_var_p(c(0), c(1), NA_real_),
    "finite"
  )
  testthat::expect_error(
    pairwiseLLM:::utility_delta_var_p(c(0), c(1), 1.5),
    "in \\[0, 1\\]"
  )
})

testthat::test_that("compute_pair_utility handles empty candidates and unordered_key", {
  withr::local_seed(1)

  draws <- matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE)
  colnames(draws) <- c("A", "B")
  empty_candidates <- tibble::tibble(i_id = character(), j_id = character())
  out_empty <- pairwiseLLM:::compute_pair_utility(draws, empty_candidates, epsilon_mean = 0.1)
  testthat::expect_equal(nrow(out_empty), 0L)

  candidates <- tibble::tibble(i_id = "A", j_id = "B", unordered_key = "A:B")
  out <- pairwiseLLM:::compute_pair_utility(draws, candidates, epsilon_mean = 0.1)
  testthat::expect_equal(out$unordered_key, "A:B")
})
