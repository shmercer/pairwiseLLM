test_that("E3 MCMC controls forward explicitly and obey the CPU budget", {
  source <- system.file("stan", "link_joint_offset.stan", package = "pairwiseLLM")
  received <- NULL
  model_fn <- function(stan_file, cpp_options) {
    expect_identical(stan_file, source)
    expect_true(cpp_options$stan_threads)
    list(stan_file = function() source, sample = function(...) {
      received <<- list(...)
      "synthetic-fit"
    })
  }
  local_mocked_bindings(.link_e3_model_file = function() source,
    .btl_mcmc_available_cores = function() 4L,
    .btl_mcmc_detect_cores = function() list(physical = 4L, logical = 8L, effective = 4L),
    .package = "pairwiseLLM")
  withr::local_envvar(`_R_CHECK_LIMIT_CORES_` = "false")
  input <- link_e3_input()
  config <- list(chains = 4L, parallel_chains = 2L, iter_warmup = 2000L, iter_sampling = 3000L,
    adapt_delta = .99, max_treedepth = 18L, seed = 278L, output_dir = withr::local_tempdir(),
    threads_per_chain = 1L, core_fraction = .5)
  result <- pairwiseLLM:::.link_e3_sample(pairwiseLLM:::.link_e3_stan_data(input),
    list(cmdstan = config), model_fn)
  for (k in setdiff(names(config), "core_fraction")) expect_identical(received[[k]], config[[k]])
  expect_identical(result$fit, "synthetic-fit")
  expect_identical(received$data$M, 42L)
  config$threads_per_chain <- 3L
  expect_error(pairwiseLLM:::.link_e3_sample(list(), list(cmdstan = config), model_fn), "CPU budget")
})

test_that("MCMC retains common results, raw draws, and complete audit diagnostics", {
  skip_if_not_installed("posterior")
  args <- link_e3_args(0)
  args$control <- list(estimator = list(engine = "mcmc", cmdstan = list(seed = 278L)))
  input <- do.call(prepare_link_input, args)
  sampled <- link_e3_mock_sampler(input)
  local_mocked_bindings(.btl_mcmc_require_cmdstanr = function() invisible(NULL),
    .link_e3_sample = function(...) sampled, .package = "pairwiseLLM")
  fit <- fit_link(input)
  expect_true(fit$diagnostics$fit_valid)
  expect_true(fit$diagnostics$sampler$audit_gate$passed)
  expect_identical(fit$offset$identification, "prior_only")
  expect_equal(unlist(fit$offset[1:4]), c(0, 5, qnorm(c(.025, .975), 0, 5)), ignore_attr = TRUE)
  expect_true(all(fit$uncertainty$covariance[1, -1] == 0))
  expect_true(all(fit$uncertainty$covariance[2:3, 4:5] == 0))
  expect_identical(fit$diagnostics$sampler$parameters$coordinate, colnames(input$item_transform))
  expect_length(fit$diagnostics$sampler$parameters$mcse_sd_ratio, 5L)
  expect_length(fit$diagnostics$sampler$per_chain$ebfmi, 4L)
  expect_null(fit$continuation$mode)
  expect_true(pairwiseLLM:::.link_data_only(fit$prediction$state))
  expect_equal(unname(fit$prediction$state$free_draws), matrix(sampled$fit$draws(), ncol = 5))
  pairs <- link_e3_args(2)$cross[, -6]
  theta <- fit$prediction$state$free_draws %*% t(input$item_transform)
  p <- .88 * mean(plogis(theta[, 1] - theta[, 4] + .17)) + .06
  expect_equal(predict_link(fit, pairs)[1], p)
  expect_identical(predict_link(fit, pairs[FALSE, ]), numeric())
  path <- tempfile(tmpdir = withr::local_tempdir(), fileext = ".rds")
  saveRDS(fit, path)
  expect_identical(predict_link(readRDS(path), pairs), predict_link(fit, pairs))
  bad <- fit
  bad$prediction$state$free_draws[1, 1] <- 99
  expect_error(predict_link(bad, pairs), "Invalid E3 MCMC prediction state")
  # Config hashes differ by engine; evidence hashes and domains do not.
  map_args <- args
  map_args$control <- list()
  map <- fit_link(do.call(prepare_link_input, map_args))
  expect_identical(fit$provenance$hashes[1:4], map$provenance$hashes[1:4])
  expect_identical(fit$provenance$counts, map$provenance$counts)
})

test_that("positive-budget MCMC summaries are empirical full-covariance transforms", {
  skip_if_not_installed("posterior")
  args <- link_e3_args()
  args$control <- list(estimator = list(engine = "mcmc"))
  input <- do.call(prepare_link_input, args)
  sampled <- link_e3_mock_sampler(input)
  local_mocked_bindings(.btl_mcmc_require_cmdstanr = function() invisible(NULL),
    .link_e3_sample = function(...) sampled, .package = "pairwiseLLM")
  fit <- fit_link(input)
  expect_true(fit$diagnostics$fit_valid)
  draws <- fit$prediction$state$free_draws
  theta <- draws %*% t(input$item_transform)
  expect_equal(fit$items$theta_link_mean, colMeans(theta))
  expect_equal(fit$items$theta_link_sd, apply(theta, 2, sd))
  expect_equal(fit$items$theta_link_lower, apply(theta, 2, quantile, .025), ignore_attr = TRUE)
  expect_equal(fit$uncertainty$covariance, cov(draws))
  expect_equal(fit$offset$delta_mean, mean(draws[, 1]))
  args$judge$epsilon <- 1
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_equal(fit$items$theta_link_mean, rep(0, 6))
  expect_equal(unname(fit$uncertainty$covariance), diag(c(25, 1, 1, 1, 1)))
  expect_identical(fit$offset$identification, "unidentified")
  expect_equal(predict_link(fit, args$cross[, -6]), rep(.5, nrow(args$cross)))
})

test_that("failed and missing sampler diagnostics remain visible and block validity", {
  skip_if_not_installed("posterior")
  args <- link_e3_args()
  args$control <- list(estimator = list(engine = "mcmc"))
  input <- do.call(prepare_link_input, args)
  sampled <- link_e3_mock_sampler(input)
  sampled$fit$diagnostic_summary <- function(...) list(num_divergent = c(0, 0, NA, 0),
    num_max_treedepth = c(20, 20, 20, 20), ebfmi = c(1, 1, 1, .1))
  local_mocked_bindings(.btl_mcmc_require_cmdstanr = function() invisible(NULL),
    .link_e3_sample = function(...) sampled, .package = "pairwiseLLM")
  fit <- fit_link(input)
  expect_false(fit$diagnostics$fit_valid)
  expect_identical(fit$diagnostics$failure_code, "mcmc_audit_gate_failed")
  expect_true(is.na(fit$diagnostics$sampler$divergences))
  expect_true(all(c("divergences", "ebfmi", "treedepth") %in% fit$diagnostics$sampler$audit_gate$failed))
  expect_true(is.matrix(fit$prediction$state$free_draws))
  expect_true(all(is.finite(fit$items$theta_link_mean)))
  expect_error(predict_link(fit, args$cross[, -6]), "invalid linking fit")
  sampled$fit$diagnostic_summary <- function(...) stop("missing")
  fit <- fit_link(input)
  expect_false(fit$diagnostics$fit_valid)
  expect_true(all(is.na(fit$diagnostics$sampler$per_chain$ebfmi)))
  sampled <- link_e3_mock_sampler(input, iterations = 10L, chains = 1L)
  fit <- fit_link(input)
  expect_false(fit$diagnostics$fit_valid)
  expect_true(any(c("rhat", "bulk_ess", "tail_ess", "mcse") %in% fit$diagnostics$sampler$audit_gate$failed))
})

test_that("sampling errors and malformed draws return explicit E3 failures", {
  skip_if_not_installed("posterior")
  args <- link_e3_args()
  args$control <- list(estimator = list(engine = "mcmc"))
  input <- do.call(prepare_link_input, args)
  sampled <- link_e3_mock_sampler(input)
  sampled$fit$draws <- function(...) array(NA_real_, c(2, 4, 5))
  local_mocked_bindings(.btl_mcmc_require_cmdstanr = function() invisible(NULL),
    .link_e3_sample = function(...) sampled, .package = "pairwiseLLM")
  fit <- fit_link(input)
  expect_false(fit$diagnostics$fit_valid)
  expect_identical(fit$diagnostics$failure_code, "e3_mcmc_failure")
  expect_true(all(is.na(fit$items$theta_link_sd)))
  sampled <- link_e3_mock_sampler(input)
  sampled$config$chains <- 5L
  fit <- fit_link(input)
  expect_false(fit$diagnostics$fit_valid)
  expect_match(fit$diagnostics$sampler$message, "unexpected chain")
})
