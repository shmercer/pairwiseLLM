test_that("btl_mcmc theta summary stubs abort with roadmap message", {
  expect_error(
    pairwiseLLM:::btl_mcmc_theta_summary_from_fit(list(), list()),
    "not implemented yet"
  )

  expect_error(
    pairwiseLLM:::btl_mcmc_theta_summary_normalize(list(), list()),
    "not implemented yet"
  )
})
