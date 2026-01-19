testthat::test_that("btl mcmc stan prior uses beta(2, 20) for epsilon", {
  stan_path <- system.file("stan", "btl_mcmc_v3.stan", package = "pairwiseLLM")
  testthat::expect_true(nzchar(stan_path))

  stan_lines <- readLines(stan_path, warn = FALSE)
  prior_pattern <- "^\\s*epsilon\\s*~\\s*beta\\(2,\\s*20\\);"
  testthat::expect_true(any(grepl(prior_pattern, stan_lines)))
})
