test_that("tuning rejects nonfinite inner predictions at the engine boundary", {
  x <- cbind(a = 1:8, b = (1:8)^2)
  z <- as.numeric(scale(1:8))
  testthat::local_mocked_bindings(.warm_start_glmnet_path = function(x, z, alpha, lambda = NULL, ...) {
    list(lambda = c(1, 0.1), a0 = c(Inf, Inf),
      beta = matrix(0, ncol(x), 2, dimnames = list(colnames(x), NULL)))
  }, .package = "pairwiseLLM")
  expect_error(pairwiseLLM:::.warm_start_tune(x, z, rep(1:2, 4), 0.5, "lambda.min"),
    "Nonfinite inner predictions")
})

test_that("ensemble validation rejects unnamed, scalar, and incomplete components", {
  model <- warm_bundle_model()
  ensemble <- pairwiseLLM::ensemble_warm_start_models(first = model, second = model)
  for (components in list(1, list(model), list(model, model))) {
    bad <- ensemble
    bad$components <- components
    expect_error(pairwiseLLM:::.validate_warm_start_ensemble(bad), "ensemble contract")
  }
  coefficients <- pairwiseLLM::warm_start_coefficients(ensemble)
  expect_identical(coefficients$first_std_coefficient, coefficients$second_std_coefficient)
  expect_identical(coefficients$feature, model$features)
})

test_that("registry ancestry checks distinguish sibling names from descendants", {
  root <- withr::local_tempdir()
  nested <- file.path(root, "new", "models")
  canonical <- pairwiseLLM:::.warm_start_canonical_path(nested)
  expect_identical(canonical, file.path(normalizePath(root), "new", "models"))
  expect_false(dir.exists(nested))
  expect_true(pairwiseLLM:::.warm_start_within(canonical, normalizePath(root)))
  expect_false(pairwiseLLM:::.warm_start_within(paste0(root, "-sibling"), root))
})
