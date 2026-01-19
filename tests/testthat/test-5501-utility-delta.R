
testthat::test_that("utility_delta_var_p honors shape and variance scaling", {
  withr::local_seed(1)
  mean_d <- c(-2, 0, 2)
  var_d <- rep(1, 3)

  util <- pairwiseLLM:::utility_delta_var_p(mean_d, var_d, epsilon_mean = 0.1)
  expect_true(util[[2L]] > util[[1L]])
  expect_true(util[[2L]] > util[[3L]])

  var_d2 <- c(0.5, 1, 2)
  util2 <- pairwiseLLM:::utility_delta_var_p(rep(0, 3), var_d2, epsilon_mean = 0.1)
  expect_true(all(diff(util2) > 0))

  util_zero <- pairwiseLLM:::utility_delta_var_p(c(-1, 0, 1), c(0, 0, 0), epsilon_mean = 0.1)
  expect_true(all(util_zero == 0))
})
