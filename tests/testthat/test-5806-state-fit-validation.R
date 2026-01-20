testthat::test_that("validate_state enforces contract-only `state$fit`", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)

  expect_true("fit" %in% names(state))
  expect_null(state$fit)
  expect_silent(pairwiseLLM:::validate_state(state))

  theta_draws <- matrix(0, nrow = 2, ncol = state$N, dimnames = list(NULL, state$ids))
  fit <- list(
    theta_draws = theta_draws,
    theta_mean = stats::setNames(rep(0, state$N), state$ids),
    epsilon_mean = 0.1,
    diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000)
  )
  state$fit <- fit
  expect_silent(pairwiseLLM:::validate_state(state))

  state$fit <- list(theta_draws = theta_draws, theta_mean = fit$theta_mean)
  expect_error(pairwiseLLM:::validate_state(state), "epsilon_mean")

  state$fit <- NULL
  expect_false("fit" %in% names(state))
  expect_error(pairwiseLLM:::validate_state(state), "state\\$fit")

  state$fit <- NULL
  state$fast_fit <- list()
  expect_error(pairwiseLLM:::validate_state(state), "fast_fit")
})

