testthat::test_that("mcmc defaults scale chains with available cores", {
  resolved <- testthat::with_mocked_bindings(
    pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(list()),
    detectCores = function(logical = FALSE, ...) {
      if (isTRUE(logical)) 16L else 12L
    },
    .package = "parallel"
  )

  testthat::expect_equal(resolved$chains, 8L)
})

testthat::test_that("core detection falls back to logical cores and defaults", {
  fallback <- testthat::with_mocked_bindings(
    pairwiseLLM:::.btl_mcmc_detect_cores(),
    detectCores = function(logical = FALSE, ...) {
      if (isTRUE(logical)) 4L else NA_integer_
    },
    .package = "parallel"
  )
  testthat::expect_equal(fallback$effective, 4L)

  defaulted <- testthat::with_mocked_bindings(
    pairwiseLLM:::.btl_mcmc_detect_cores(),
    detectCores = function(logical = FALSE, ...) NA_integer_,
    .package = "parallel"
  )
  testthat::expect_equal(defaulted$effective, 1L)
})

testthat::test_that("cmdstanr version helper respects namespace availability", {
  version_na <- testthat::with_mocked_bindings(
    pairwiseLLM:::.btl_mcmc_cmdstanr_version(),
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
  testthat::expect_true(is.na(version_na))

  version_ok <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      pairwiseLLM:::.btl_mcmc_cmdstanr_version(),
      packageVersion = function(...) "0.0.0",
      .package = "utils"
    ),
    requireNamespace = function(...) TRUE,
    .package = "base"
  )
  testthat::expect_equal(version_ok, "0.0.0")
})
