test_that("weighted CV loss, fold SE and penalty ties follow explicit conventions", {
  loss <- rbind(c(1, 2, 3), c(4, 3, 2))
  sizes <- c(2L, 3L)
  actual <- .warm_start_loss_summary(loss, sizes)
  expected <- colSums(loss * sizes) / sum(sizes)
  expect_equal(actual$cvm, expected)
  expect_equal(actual$cvsd, sqrt(colSums(sweep(loss, 2, expected)^2 * sizes) / sum(sizes)))
  for (bad in list(NULL, matrix(NA_real_, 2, 3), matrix(-1, 2, 3), matrix(1, 1, 3))) {
    expect_error(.warm_start_loss_summary(bad, sizes), "CV loss")
  }
  expect_error(.warm_start_loss_summary(loss, c(0, 3)), "fold sizes")
  expect_error(.warm_start_loss_summary(loss, 2), "fold sizes")
  expect_true(.warm_start_near(1, 1 + 5e-11))
  expect_false(.warm_start_near(1, 1 + 5e-9))
  expect_identical(.warm_start_alpha_choice(c(1, 1 - 0.8e-10, 1 - 1.6e-10)), 2L)
  lambda <- c(10, 5, 1)
  choice <- .warm_start_lambda_choice(lambda, c(1.1, 1 + 5e-11, 1), c(0.2, 0.2, 0.1), "lambda.1se")
  expect_identical(choice$index_min, 2L)
  expect_identical(choice$index_1se, 1L)
  expect_identical(choice$index, 1L)
  expect_equal(choice$lambda_min, 5)
  expect_identical(.warm_start_lambda_choice(lambda, c(1.1, 1, 1), rep(0.2, 3), "lambda.min")$index, 2L)
})

test_that("exact paths match independent scalar fits and support one retained predictor", {
  skip_if_not_installed("glmnet")
  x <- as.matrix(warm_core_features(15)[, -1])
  p <- .warm_start_preprocess_fit(x)
  scaled <- .warm_start_preprocess_apply(x, p)
  z <- as.numeric(scale(seq_len(15)))
  for (alpha in c(0, 0.5, 1)) {
    lambda <- c(0.5, 0.1, 0.01)
    fit <- .warm_start_glmnet_path(scaled, z, alpha, lambda)
    expect_equal(fit$lambda, lambda)
    for (j in seq_along(lambda)) {
      scalar <- .warm_start_glmnet_fit(scaled, z, alpha, lambda[j])
      expect_equal(as.numeric(scaled %*% as.matrix(fit$beta)[, j] + fit$a0[j]),
        as.numeric(scaled %*% as.matrix(scalar$beta) + scalar$a0), tolerance = 1e-5)
    }
  }
  one <- .warm_start_glmnet_path(scaled[, 1, drop = FALSE], z, 0.5, c(1, 0.1))
  expect_equal(as.numeric(one$beta[2, ]), c(0, 0))
  expect_length(one$lambda, 2)
  for (bad in list(c(1, 2), c(1, 1), c(1, NA), c(1, -1), numeric())) {
    expect_error(.warm_start_glmnet_path(scaled, z, 0.5, bad), "lambda path")
  }
  expect_error(.warm_start_glmnet_path(scaled, rep(1, 15), 0.5), "nonconstant")
  expect_error(.warm_start_glmnet_path(scaled, z, -1), "valid alpha")
})

test_that("tuning shares folds, records exact selection and rejects incomplete engine paths", {
  skip_if_not_installed("glmnet")
  withr::local_seed(4)
  features <- warm_core_features(17)
  x <- as.matrix(features[, -1])
  z <- as.numeric(scale(warm_core_theta(features)))
  folds <- .warm_start_folds(z, 5L)
  tune <- .warm_start_tune(x, z, folds, c(0, 0.5, 1), "lambda.1se")
  expect_identical(tune$foldid, folds)
  expect_identical(tune$oof$fold, folds)
  expect_identical(tune$selected$oof, tune$oof$raw_prediction)
  expect_invisible(.validate_warm_start_tuning(tune, 17L))
  errors <- vapply(tune$traces, function(t) t$cvm[t$index_min], numeric(1))
  expect_equal(tune$selected$error, min(errors))
  for (t in tune$traces) {
    expect_identical(t$fold_sizes, tabulate(folds))
    expect_equal(t$cvm, colSums(t$fold_mse * t$fold_sizes) / 17)
    expect_equal(t$lambda[t$index], t$lambda_1se)
  }
  local_mocked_bindings(glmnet = function(...) list(jerr = -1), .package = "glmnet")
  expect_error(.warm_start_glmnet_path(x, z, 0.5, c(1, 0.1)), "did not converge")
  local_mocked_bindings(glmnet = function(...) list(jerr = 0L, lambda = 1, a0 = 0, beta = matrix(0, 20, 1)),
    .package = "glmnet")
  expect_error(.warm_start_glmnet_path(x, z, 0.5, c(1, 0.1)), "every requested lambda")
  local_mocked_bindings(glmnet = function(...) {
    list(jerr = 0L, lambda = c(1, 0.1), a0 = c(0, Inf), beta = matrix(0, 20, 2))
  }, .package = "glmnet")
  expect_error(.warm_start_glmnet_path(x, z, 0.5, c(1, 0.1)), "finite coefficients")
})

test_that("numerically tied alpha errors favor smaller alpha", {
  x <- cbind(a = 1:10, b = (1:10)^2)
  z <- as.numeric(scale(1:10))
  local_mocked_bindings(.warm_start_glmnet_path = function(x, z, alpha, lambda = NULL, ...) {
    if (is.null(lambda)) lambda <- c(1, 0.1)
    list(lambda = lambda, a0 = rep(alpha * 1e-12, length(lambda)),
      beta = matrix(0, ncol(x), length(lambda), dimnames = list(colnames(x), NULL)))
  })
  t <- .warm_start_tune(x, z, rep(1:5, 2), c(0, 0.5, 1), "lambda.min")
  expect_equal(t$selected$alpha, 0)
  expect_equal(t$selected$lambda, 1)
})
