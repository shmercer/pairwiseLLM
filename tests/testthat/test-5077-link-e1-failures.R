test_that("E1 validates controls and fails explicitly when quadrature is exhausted", {
  args <- link_contract_args(edges = 10L)
  for (control in list(list(rel_tol = 0), list(abs_tol = NA_real_), list(subdivisions = 1.5),
    list(quantile_tol = -1), list(unknown = 1))) {
    args$control <- list(estimator = control)
    expect_error(do.call(prepare_link_input, args), class = "pairwiseLLM_link_contract_error")
  }
  args$control <- list(estimator = list(subdivisions = 1L))
  input <- do.call(prepare_link_input, args)
  fit <- fit_link(input)
  expect_false(fit$diagnostics$fit_valid)
  expect_identical(fit$diagnostics$failure_code, "quadrature_subdivisions")
  expect_identical(fit$diagnostics$quadrature$controls$subdivisions, 1L)
  expect_identical(fit$offset$identification, "failed")
  expect_identical(fit$offset$delta_sd, NA_real_)
  expect_true(all(is.na(fit$items$theta_link_mean)))
  expect_true(all(is.na(fit$items$theta_link_sd)))
  expect_null(fit$uncertainty$covariance)
  expect_identical(fit$continuation$input, input)
  expect_error(predict_link(fit, input$cross[, -6L]), "invalid linking fit")
  args$control <- list()
  repaired <- fit_link(do.call(prepare_link_input, args), fit)
  expect_true(repaired$diagnostics$fit_valid)
})

test_that("CDF integration failures invalidate an otherwise integrated E1 fit", {
  local_mocked_bindings(integrate = function(...) {
    list(message = "forced CDF integration failure", value = NA_real_, abs.error = Inf)
  }, .package = "stats")
  fit <- fit_link(link_contract_input(edges = 1L))
  expect_false(fit$diagnostics$fit_valid)
  expect_identical(fit$diagnostics$failure_code, "quantile_integration_failure")
  expect_identical(fit$diagnostics$quadrature$status, "E1 CDF integration failed its tolerance.")
  expect_true(is.finite(fit$diagnostics$quadrature$log_normalizer))
  expect_true(all(is.na(fit$items$theta_link_mean)))
})

test_that("normalization, roundoff and nonfinite arithmetic remain explicit failures", {
  kernel <- .link_e1_kernel(link_contract_input(edges = 1L))
  expect_error(.link_e1_split(list(lower = 1, upper = 1)), class = "pairwiseLLM_e1_numerical_error")
  kernel$base[] <- NaN
  expect_error(.link_e1_panel(-1, 1, kernel), class = "pairwiseLLM_e1_numerical_error")
  kernel <- .link_e1_kernel(link_contract_input(edges = 1L))
  panel <- .link_e1_panel(-1, 1, kernel)
  panel$log_k[] <- -Inf
  expect_error(.link_e1_rule(list(panel), .link_e1_controls(list())), "no finite posterior mass")
  panel <- .link_e1_panel(-1, 1, kernel)
  panel$log_bound <- NaN
  expect_error(.link_e1_rule(list(panel), .link_e1_controls(list())), "could not bound")
  args <- link_contract_args()
  args$control <- list(delta_prior = list(mean = 0, sd = 1e200))
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_false(fit$diagnostics$fit_valid)
  expect_identical(fit$diagnostics$failure_code, "nonfinite_summary")
})
