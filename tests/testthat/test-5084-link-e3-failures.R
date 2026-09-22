test_that("E3 validates engine and numerical controls before fitting", {
  bad <- list(list(engine = "legacy"), list(engine = c("mcmc", "map_laplace")),
    list(cmdstan = list(seed = 1)), list(maxit = 0), list(maxit = 1.1), list(rel_tol = Inf),
    list(unknown = 1), list(engine = "mcmc", cmdstan = list(adapt_delta = 1)),
    list(engine = "mcmc", cmdstan = list(chains = 1.5)),
    list(engine = "mcmc", cmdstan = list(seed = -1)),
    list(engine = "mcmc", cmdstan = list(seed = 1.5)),
    list(engine = "mcmc", cmdstan = list(output_dir = "")),
    list(engine = "mcmc", cmdstan = list(core_fraction = 0)))
  for (control in bad) {
    args <- link_e3_args()
    args$control <- list(estimator = control)
    expect_error(do.call(prepare_link_input, args))
  }
  args <- link_e3_args()
  args$control <- list(estimator = list(maxit = 1L, gradient_tol = 1e-12))
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_false(fit$diagnostics$fit_valid)
  expect_identical(fit$diagnostics$failure_code, "optimizer_failure")
  expect_length(fit$diagnostics$optimization$attempts, 5L)
  expect_true(all(is.na(fit$items$theta_link_sd)))
  expect_null(fit$uncertainty$covariance)
  expect_error(predict_link(fit, args$cross[, -6]), "invalid linking fit")
})

test_that("E3 singular Hessians and nonfinite calculations cannot silently pass", {
  args <- link_e3_args(0)
  for (k in c("hub", "spoke")) args$phase_a[[k]]$observations <- args$phase_a[[k]]$observations[FALSE, ]
  input <- do.call(prepare_link_input, args)
  objective <- pairwiseLLM:::.link_gaussian_objective
  for (problem in c("hessian", "value", "gradient")) {
    local_mocked_bindings(.link_gaussian_objective = function(w, kernel, hessian = FALSE) {
      x <- objective(w, kernel, hessian)
      if (problem == "hessian" && hessian) x$hessian[1, 1] <- 0
      if (problem == "value") x$value <- Inf
      if (problem == "gradient") x$gradient[1] <- NA_real_
      x
    }, .package = "pairwiseLLM")
    fit <- fit_link(input)
    expect_false(fit$diagnostics$fit_valid)
    expect_identical(fit$diagnostics$failure_code, if (problem == "hessian") "hessian_not_pd" else "nonfinite_mode")
    expect_null(fit$uncertainty$covariance)
  }
})

test_that("E3 rejects corrupt prediction state and reports integration failures", {
  fit <- fit_link(link_e3_input())
  pairs <- fit$continuation$input$cross[, -6]
  bad <- fit
  bad$prediction$state$mean[1] <- 10
  expect_error(predict_link(bad, pairs), "Invalid E3 prediction state")
  local_mocked_bindings(integrate = function(...) stop("integration failure"), .package = "stats")
  expect_error(predict_link(fit, pairs), class = "pairwiseLLM_e3_numerical_error")
})

test_that("production E3 does not require or run optional MCMC", {
  local_mocked_bindings(.btl_mcmc_require_cmdstanr = function(...) stop("must not load CmdStan"),
    .link_e3_sample = function(...) stop("must not sample"), .package = "pairwiseLLM")
  expect_true(fit_link(link_e3_input())$diagnostics$fit_valid)
})
