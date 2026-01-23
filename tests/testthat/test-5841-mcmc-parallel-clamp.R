testthat::test_that("mcmc parallel chains clamp to configured chains", {
  resolved <- testthat::with_mocked_bindings(
    pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(chains = 4L, parallel_chains = 7L)),
    detectCores = function(logical = FALSE, ...) {
      if (isTRUE(logical)) 16L else 12L
    },
    .package = "parallel"
  )

  testthat::expect_equal(resolved$parallel_chains, 4L)
})

testthat::test_that("mcmc parallel chains honor core fraction budget", {
  resolved <- testthat::with_mocked_bindings(
    pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(core_fraction = 0.8)),
    detectCores = function(logical = FALSE, ...) {
      if (isTRUE(logical)) 16L else 12L
    },
    .package = "parallel"
  )

  testthat::expect_equal(resolved$chains, 8L)
  testthat::expect_equal(resolved$parallel_chains, 8L)
})

testthat::test_that("mcmc core fraction and parallel inputs are validated", {
  testthat::with_mocked_bindings(
    testthat::expect_error(
      pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(core_fraction = 0)),
      "core_fraction"
    ),
    detectCores = function(logical = FALSE, ...) 4L,
    .package = "parallel"
  )

  testthat::with_mocked_bindings(
    testthat::expect_error(
      pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(chains = 2L, parallel_chains = 0L)),
      "parallel_chains"
    ),
    detectCores = function(logical = FALSE, ...) 4L,
    .package = "parallel"
  )
})

testthat::test_that("mcmc config returns early for invalid chains", {
  resolved <- testthat::with_mocked_bindings(
    pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list(chains = 0L)),
    detectCores = function(logical = FALSE, ...) 4L,
    .package = "parallel"
  )

  testthat::expect_true(is.na(resolved$parallel_chains))
})
