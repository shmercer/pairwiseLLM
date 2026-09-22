test_that("E2 controls are explicit and validated", {
  for (control in list(list(bad = 1), list(maxit = 0), list(maxit = 1.5),
    list(rel_tol = Inf), list(gradient_tol = -1), list(subdivisions = 0))) {
    args <- link_e2_args()
    args$control <- list(estimator = control)
    expect_error(do.call(prepare_link_input, args), class = "pairwiseLLM_link_contract_error")
  }
  args <- link_e2_args()
  args$control <- list(estimator = list(maxit = 1, gradient_tol = 1e-12))
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_false(fit$diagnostics$fit_valid)
  expect_identical(fit$diagnostics$failure_code, "optimizer_failure")
  expect_length(fit$diagnostics$optimization$attempts, 5)
  expect_true(all(is.na(fit$items$theta_link_sd)))
  expect_null(fit$uncertainty$covariance)
  expect_error(predict_link(fit, args$cross[, -6]), "invalid linking fit")
})

test_that("failed bridges remain E2 failures with partial diagnostics", {
  input <- link_e2_input()
  bridge <- pairwiseLLM:::.link_e2_bridge
  local_mocked_bindings(.link_e2_bridge = function(draws, basis) {
    if (mean(draws[, 1]) > 0) pairwiseLLM:::.link_e2_fail("bridge_jitter_exhausted", "exhausted")
    bridge(draws, basis)
  }, .package = "pairwiseLLM")
  # Force the second bridge to fail, leaving the first bridge diagnostic intact.
  args <- link_e2_args()
  args$phase_a$hub$draws <- sweep(args$phase_a$hub$draws, 2, c(-10, 5, 5), "+")
  args$phase_a$spoke$draws <- sweep(args$phase_a$spoke$draws, 2, c(10, -5, -5), "+")
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_false(fit$diagnostics$fit_valid)
  expect_identical(fit$diagnostics$failure_code, "bridge_jitter_exhausted")
  expect_named(fit$diagnostics$bridge, "hub")
})

test_that("non-positive observed Hessians are not regularized into valid fits", {
  objective <- pairwiseLLM:::.link_e2_objective
  local_mocked_bindings(.link_e2_objective = function(w, kernel, hessian = FALSE) {
    out <- objective(w, kernel, hessian)
    if (hessian) out$hessian[1, 1] <- -100
    out
  }, .package = "pairwiseLLM")
  fit <- fit_link(link_e2_input(0))
  expect_false(fit$diagnostics$fit_valid)
  expect_false(fit$diagnostics$hessian_pd)
  expect_identical(fit$diagnostics$failure_code, "hessian_not_pd")
  expect_null(fit$uncertainty$covariance)
})

test_that("prediction rejects modified state and failed numerical integration", {
  fit <- fit_link(link_e2_input())
  pairs <- fit$continuation$input$cross[, -6]
  bad <- fit
  bad$prediction$state$mean[1] <- 100
  expect_error(predict_link(bad, pairs), "Invalid E2 prediction state")
  bad <- fit
  bad$prediction$state$controls$prediction_rel_tol <- 1
  expect_error(predict_link(bad, pairs), "Invalid E2 prediction state")
  local_mocked_bindings(integrate = function(...) stop("forced integration failure"), .package = "stats")
  expect_error(predict_link(fit, pairs), class = "pairwiseLLM_e2_numerical_error")
})

test_that("optional numerical diagnostics contain only serializable data", {
  fit <- fit_link(link_e2_input())
  fit$diagnostics$bridge$bad <- function() 1
  expect_error(pairwiseLLM:::.link_validate_result(fit), "serializable")
})
