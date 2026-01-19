make_v3_bt_data <- function() {
  list(
    A = c(1L, 2L),
    B = c(2L, 1L),
    Y = c(1L, 0L),
    N = 2L,
    item_id = c("A", "B")
  )
}

testthat::test_that("fit_bayes_btl_mcmc_adaptive validates cmdstan config", {
  bt_data <- make_v3_bt_data()
  config <- pairwiseLLM:::adaptive_v3_config(2L)

  config$cmdstan <- 1
  testthat::with_mocked_bindings(
    expect_error(
      pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config),
      "config\\$cmdstan"
    ),
    .btl_mcmc_require_cmdstanr = function() NULL,
    .package = "pairwiseLLM"
  )

  config$cmdstan <- list(output_dir = 1)
  testthat::with_mocked_bindings(
    expect_error(
      pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config),
      "output_dir"
    ),
    .btl_mcmc_require_cmdstanr = function() NULL,
    .package = "pairwiseLLM"
  )
})

testthat::test_that("fit_bayes_btl_mcmc_adaptive returns draws with mocked cmdstan", {
  bt_data <- make_v3_bt_data()
  config <- pairwiseLLM:::adaptive_v3_config(2L)
  config$cmdstan <- list(
    chains = 2L,
    iter_warmup = 2L,
    iter_sampling = 2L,
    core_fraction = 0.5
  )

  fake_fit <- list(
    draws = function(variables = NULL, format = NULL) {
      mat <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.05, 0.06), nrow = 2, byrow = TRUE)
      colnames(mat) <- c("theta[1]", "theta[2]", "epsilon")
      mat
    },
    diagnostic_summary = function() tibble::tibble(num_divergent = 0L),
    summary = function(variables = NULL) {
      tibble::tibble(
        rhat = 1,
        ess_bulk = 100,
        ess_tail = 100
      )
    }
  )
  fake_model <- list(sample = function(...) fake_fit)

  out <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config, seed = 1),
      .btl_mcmc_require_cmdstanr = function() NULL,
      .package = "pairwiseLLM"
    ),
    cmdstan_model = function(file) fake_model,
    write_stan_file = function(code) "fake.stan",
    .package = "cmdstanr"
  )

  expect_true(is.list(out$draws))
  expect_equal(ncol(out$draws$theta), 2L)
  expect_true(all(c("theta_summary", "epsilon_summary", "diagnostics") %in% names(out)))
})

testthat::test_that("fit_bayes_btl_mcmc_adaptive catches bad draws output", {
  bt_data <- make_v3_bt_data()
  config <- pairwiseLLM:::adaptive_v3_config(2L)
  config$cmdstan <- list(chains = 1L, iter_warmup = 1L, iter_sampling = 1L)

  fake_fit <- list(
    draws = function(variables = NULL, format = NULL) {
      mat <- matrix(c(0.1, 0.2), nrow = 1, byrow = TRUE)
      colnames(mat) <- c("theta[1]", "theta[2]")
      mat
    },
    diagnostic_summary = function() tibble::tibble(num_divergent = 0L),
    summary = function(variables = NULL) tibble::tibble(rhat = 1, ess_bulk = 100, ess_tail = 100)
  )
  fake_model <- list(sample = function(...) fake_fit)

  testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      expect_error(
        pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config),
        "epsilon"
      ),
      .btl_mcmc_require_cmdstanr = function() NULL,
      .package = "pairwiseLLM"
    ),
    cmdstan_model = function(file) fake_model,
    write_stan_file = function(code) "fake.stan",
    .package = "cmdstanr"
  )
})

testthat::test_that("fit_bayes_btl_mcmc_adaptive rejects invalid thinning", {
  bt_data <- make_v3_bt_data()
  config <- pairwiseLLM:::adaptive_v3_config(2L)
  config$thin_draws <- 0L

  fake_fit <- list(
    draws = function(variables = NULL, format = NULL) {
      mat <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.05, 0.06), nrow = 2, byrow = TRUE)
      colnames(mat) <- c("theta[1]", "theta[2]", "epsilon")
      mat
    },
    diagnostic_summary = function() tibble::tibble(num_divergent = 0L),
    summary = function(variables = NULL) tibble::tibble(rhat = 1, ess_bulk = 100, ess_tail = 100)
  )
  fake_model <- list(sample = function(...) fake_fit)

  testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      expect_error(
        pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config),
        "thin_draws"
      ),
      .btl_mcmc_require_cmdstanr = function() NULL,
      .package = "pairwiseLLM"
    ),
    cmdstan_model = function(file) fake_model,
    write_stan_file = function(code) "fake.stan",
    .package = "cmdstanr"
  )
})
