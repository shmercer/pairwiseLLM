
testthat::test_that("epsilon_mean source order and validation behave as specified", {
  withr::local_seed(1)

  state <- list(
    config = list(v3 = list(epsilon_mean = 0.1)),
    posterior = list(epsilon_mean = 0.2)
  )
  fit <- make_v3_fit_contract(c("a", "b"), epsilon_draws = c(0.3, 0.3))
  expect_equal(pairwiseLLM:::.adaptive_epsilon_mean_from_state(state, fit), 0.3)

  expect_equal(pairwiseLLM:::.adaptive_epsilon_mean_from_state(state, fit = NULL), 0.2)

  bad_zero <- fit
  bad_zero$epsilon_mean <- numeric()
  expect_error(
    pairwiseLLM:::.adaptive_epsilon_mean_from_state(state, bad_zero),
    "finite numeric scalar"
  )

  bad_multi <- fit
  bad_multi$epsilon_mean <- c(0.1, 0.2)
  expect_error(
    pairwiseLLM:::.adaptive_epsilon_mean_from_state(state, bad_multi),
    "finite numeric scalar"
  )
})

testthat::test_that("epsilon_mean fallback warns and respects prior settings", {
  withr::local_seed(1)

  state <- list(config = list(v3 = list(epsilon_prior_alpha = 3, epsilon_prior_beta = 9)))
  expect_warning(
    val <- pairwiseLLM:::.adaptive_epsilon_mean_from_state(state, fit = NULL),
    "prior mean fallback"
  )
  expect_equal(val, 3 / 12)

  fit <- list(draws = list(theta = matrix(0, nrow = 2, ncol = 1)), diagnostics = list())
  expect_warning(
    val <- pairwiseLLM:::.adaptive_epsilon_mean_from_state(state, fit),
    "prior mean fallback"
  )
  expect_equal(val, 3 / 12)
})
