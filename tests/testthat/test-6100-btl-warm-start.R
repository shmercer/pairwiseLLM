warm_btl_results <- function() {
  build_btl_results_data(data.frame(ID1 = c("a", "a"), ID2 = c("b", "b"), better_id = c("a", "b")))
}

warm_btl_sampler <- function(capture) {
  function(stan_file, cpp_options) {
    list(sample = function(data, ...) {
      capture$data <- data
      list(draws = function(variables, format) {
        theta <- matrix(rep(data$prior_mean, each = 4L), nrow = 4L)
        colnames(theta) <- paste0("theta[", seq_len(data$N), "]")
        cbind(theta, epsilon = rep(0.1, 4), beta = rep(0, 4))
      }, diagnostic_summary = function() data.frame(num_divergent = 0L),
      summary = function(variables) data.frame(rhat = 1, ess_bulk = 1000, ess_tail = 1000))
    })
  }
}

test_that("all active samplers receive explicit cold and informative prior vectors", {
  testthat::local_mocked_bindings(.btl_mcmc_require_cmdstanr = function() NULL, .package = "pairwiseLLM")
  ids <- c("b", "a", "c")
  prior <- make_warm_start_prior(c(a = 2, b = 4, c = 9))
  for (variant in c("btl", "btl_e", "btl_b", "btl_e_b")) {
    capture <- new.env(parent = emptyenv())
    cfg <- pairwiseLLM:::btl_mcmc_config(3L, list(model_variant = variant))
    for (p in list(NULL, prior)) {
      data <- pairwiseLLM:::.btl_mcmc_prepare_bt_data(warm_btl_results(), ids, p)
      out <- pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(data, cfg, model_fn = warm_btl_sampler(capture))
      expect_identical(capture$data$prior_mean, if (is.null(p)) c(0, 0, 0) else c(-1, -3, 4))
      expect_identical(capture$data$prior_sd, rep(if (is.null(p)) 1 else 0.5, 3))
      expect_identical(colnames(out$draws$theta), ids)
    }
    stan <- readLines(pairwiseLLM:::stan_file_for_variant(variant))
    expect_true(any(grepl("theta_raw ~ normal(prior_mean, prior_sd)", stan, fixed = TRUE)))
    expect_true(any(grepl("theta_raw - mean(theta_raw)", stan, fixed = TRUE)))
  }
})

test_that("BTL data validation rejects partial or malformed prior fields", {
  x <- pairwiseLLM:::.btl_mcmc_prepare_bt_data(warm_btl_results(), c("a", "b"))
  expect_identical(pairwiseLLM:::.btl_mcmc_validate_bt_data(x)$prior_sd, c(1, 1))
  x$prior_mean <- NULL
  expect_error(pairwiseLLM:::.btl_mcmc_validate_bt_data(x), "both")
  x$prior_sd <- NULL
  expect_identical(pairwiseLLM:::.btl_mcmc_validate_bt_data(x)$prior_mean, c(0, 0))
  x$item_id <- c("a", "a")
  expect_error(pairwiseLLM:::.btl_mcmc_validate_bt_data(x), "item_id")
  x$item_id <- c("a", "b")
  x$prior_mean <- c(0, Inf)
  x$prior_sd <- c(1, 1)
  expect_error(pairwiseLLM:::.btl_mcmc_validate_bt_data(x), "finite")
  x$prior_mean <- c(0, 0)
  x$prior_sd <- c(1, 0)
  expect_error(pairwiseLLM:::.btl_mcmc_validate_bt_data(x), "greater than zero")
})

test_that("standalone fits propagate prior metadata and preserve positional APIs", {
  capture <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    .fit_bayes_btl_mcmc_adaptive = function(bt_data, config, seed = NULL) {
      capture$data <- bt_data
      list(draws = list(theta = matrix(c(-1, 1, -0.5, 0.5), nrow = 2,
        dimnames = list(NULL, bt_data$item_id)), epsilon = c(0.1, 0.1), beta = c(0, 0)),
        model_variant = config$model_variant, diagnostics = list(), mcmc_config_used = list())
    }, .package = "pairwiseLLM")
  prior <- make_warm_start_prior(c(a = 3, b = 1))
  fit <- fit_bayes_btl_mcmc(warm_btl_results(), c("a", "b"), list(chains = 1L),
    warm_start_prior = prior)
  expect_identical(capture$data$prior_mean, c(1, -1))
  expect_identical(fit$fit$predictive_prior$digest, prior$digest)
  expect_identical(fit$round_log$predictive_prior_digest, prior$digest)
  cold <- fit_bayes_btl_mcmc(warm_btl_results(), c("a", "b"), "btl", list(chains = 1L))
  expect_identical(capture$data$prior_sd, c(1, 1))
  expect_null(cold$fit$predictive_prior)
  expect_error(fit_bayes_btl_mcmc(warm_btl_results(), c("a", "c"), warm_start_prior = prior), "exactly")
})

test_that("fit contracts preserve and validate actual raw theta priors", {
  capture <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(.btl_mcmc_require_cmdstanr = function() NULL, .package = "pairwiseLLM")
  prior <- make_warm_start_prior(c(a = 1, b = 3))
  data <- pairwiseLLM:::.btl_mcmc_prepare_bt_data(warm_btl_results(), c("a", "b"), prior)
  cfg <- pairwiseLLM:::btl_mcmc_config(2L, list(model_variant = "btl"))
  raw <- pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(data, cfg, model_fn = warm_btl_sampler(capture))
  fit <- pairwiseLLM:::as_btl_fit_contract_from_mcmc(raw, c("b", "a"))
  expect_identical(fit$theta_prior$prior_mean, c(1, -1))
  expect_identical(fit$theta_prior$item_id, c("b", "a"))
  expect_silent(pairwiseLLM:::validate_btl_fit_contract(fit, c("b", "a")))
  bad <- fit
  bad$theta_prior$item_id <- c("a", "b")
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad, c("b", "a")), "prior IDs")
  raw$theta_prior$item_id <- c("x", "y")
  expect_error(pairwiseLLM:::as_btl_fit_contract_from_mcmc(raw, c("b", "a")), "prior IDs")
})
