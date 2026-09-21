test_that("path and fixed fits forward the solver ceiling through both glmnet interfaces", {
  skip_if_not_installed("glmnet")
  x <- cbind(a = 1:5, b = (1:5)^2)
  expected <- list(thresh = 1e-12, maxit = 10000000L)
  seen <- list()
  fit_stub <- function(x, lambda = NULL, nlambda = NULL, ...) {
    if (is.null(lambda)) lambda <- seq(1, 0.01, length.out = nlambda)
    list(jerr = 0L, lambda = lambda, a0 = rep(0, length(lambda)),
      beta = matrix(0, ncol(x), length(lambda)))
  }
  engines <- list(
    function(..., control) {
      seen[[length(seen) + 1L]] <<- control
      fit_stub(...)
    },
    function(..., thresh, maxit) {
      seen[[length(seen) + 1L]] <<- list(thresh = thresh, maxit = maxit)
      fit_stub(...)
    }
  )
  for (engine in engines) {
    with_mocked_bindings({
      reference <- .warm_start_glmnet_path(x, 1:5, 0.05)
      expect_length(reference$lambda, 100L)
      reused <- .warm_start_glmnet_path(x, 1:5, 0.05, reference$lambda)
      expect_identical(reused$lambda, reference$lambda)
      fixed <- .warm_start_glmnet_fit(x, 1:5, 0.05, reference$lambda[100L])
      expect_identical(fixed$lambda, reference$lambda[100L])
    }, glmnet = engine, .package = "glmnet")
  }
  expect_identical(seen, rep(list(expected), 6L))
})

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
  expect_error(.warm_start_glmnet_path(x, z, 0.5, c(1, 0.1)), "converged prefix")
  local_mocked_bindings(glmnet = function(...) list(jerr = -100L), .package = "glmnet")
  expect_error(.warm_start_glmnet_path(x, z, 0.05, c(1, 0.1)), "did not converge")
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
    list(jerr = 0L, lambda = lambda, a0 = rep(alpha * 1e-12, length(lambda)),
      beta = matrix(0, ncol(x), length(lambda), dimnames = list(colnames(x), NULL)))
  })
  t <- .warm_start_tune(x, z, rep(1:5, 2), c(0, 0.5, 1), "lambda.min")
  expect_equal(t$selected$alpha, 0)
  expect_equal(t$selected$lambda, 1)
})

test_that("different converged prefixes use only complete fold evidence for both lambda rules", {
  skip_if_not_installed("glmnet")
  x <- cbind(a = 1:13, b = (1:13)^2)
  z <- as.numeric(scale(1:13))
  folds <- rep(1:3, length.out = 13)
  local_mocked_bindings(glmnet = warm_tail_engine(), .package = "glmnet")
  loss <- vapply(c(0.2, 0), function(intercept) {
    vapply(1:3, function(fold) mean((intercept - z[folds == fold])^2), numeric(1))
  }, numeric(3))
  weights <- tabulate(folds) / length(folds)
  cvm <- colSums(loss * weights)
  cvsd <- sqrt(colSums(sweep(loss, 2, cvm)^2 * weights) / 2)
  minimum <- which.min(cvm)
  one_se <- which(cvm <= cvm[minimum] + cvsd[minimum])[1]
  for (rule in c("lambda.min", "lambda.1se")) {
    tune <- .warm_start_tune(x, z, folds, c(0, 1), rule)
    trace <- tune$traces[[1]]
    audit <- tune$candidate_validity$alphas[[1]]
    expect_invisible(.validate_warm_start_tuning(tune, 13L))
    expect_identical(trace$lambda, c(4, 2, 1, 0.5))
    expect_identical(audit$requested_lambda, trace$lambda)
    expect_identical(audit$fold_converged_count, c(3L, 2L, 4L))
    expect_identical(audit$fold_jerr, c(-4L, -3L, 0L))
    expect_identical(audit$eligible, c(TRUE, TRUE, FALSE, FALSE))
    expect_identical(audit$invalid_tail_count, 2L)
    expect_equal(trace$fold_mse[, 1:2], loss)
    expect_true(all(is.finite(trace$fold_mse[c(1, 3), 3])))
    expect_true(is.finite(trace$fold_mse[3, 4]))
    expect_identical(trace$fold_mse[2, 3:4], c(NA_real_, NA_real_))
    expect_equal(trace$cvm, c(cvm, NA_real_, NA_real_))
    expect_equal(trace$cvsd, c(cvsd, NA_real_, NA_real_))
    expect_identical(trace$index_min, minimum)
    expect_identical(trace$index_1se, one_se)
    expected <- if (rule == "lambda.min") minimum else one_se
    expect_identical(trace$index, expected)
    expect_identical(audit$selected_smallest_eligible, expected == 2L)
    expect_identical(tune$selected$alpha_index, 1L)
    expect_equal(tune$selected$oof, rep(c(0.2, 0)[expected], 13))
  }
})

test_that("zero eligible lambdas exclude an alpha and all-alpha failure remains fatal", {
  skip_if_not_installed("glmnet")
  x <- cbind(a = 1:12, b = (1:12)^2)
  z <- as.numeric(scale(1:12))
  local_mocked_bindings(glmnet = warm_tail_engine(c(1L, 2L, 4L), empty_alpha = 0),
    .package = "glmnet")
  tune <- .warm_start_tune(x, z, rep(1:3, 4), c(0, 1), "lambda.min")
  expect_invisible(.validate_warm_start_tuning(tune, 12L))
  expect_identical(tune$selected$alpha, 1)
  expect_identical(tune$selected$lambda, 4)
  invalid <- tune$candidate_validity$alphas[[1]]
  expect_false(invalid$alpha_eligible)
  expect_identical(invalid$fold_lambda[[1]], Inf)
  expect_identical(invalid$fold_converged_count, c(0L, 2L, 4L))
  expect_identical(invalid$invalid_tail_count, 4L)
  expect_identical(invalid$selected_smallest_eligible, NA)
  expect_true(all(is.na(tune$traces[[1]]$cvm)))
  expect_identical(tune$traces[[1]]$index, NA_integer_)
  expect_identical(tune$traces[[1]]$lambda_min, NA_real_)
  expect_true(tune$candidate_validity$alphas[[2]]$selected_smallest_eligible)
  expect_error(.warm_start_tune(x, z, rep(1:3, 4), 0, "lambda.min"),
    "No alpha has a lambda converged in every inner fold")
  expect_identical(.warm_start_alpha_choice(c(NA_real_, 2, NA_real_, 2)), 2L)
})

test_that("only iteration-limit failures with an exact finite prefix are accepted", {
  skip_if_not_installed("glmnet")
  x <- cbind(a = 1:6, b = (1:6)^2)
  valid <- list(jerr = -3L, lambda = c(4, 2), a0 = c(0, 0),
    beta = matrix(0, 2, 2, dimnames = list(colnames(x), NULL)))
  local_mocked_bindings(glmnet = function(...) valid, .package = "glmnet")
  expect_identical(.warm_start_glmnet_path(x, 1:6, 0.5, c(4, 2, 1)), valid)
  expect_error(.warm_start_glmnet_path(x, 1:6, 0.5), "did not converge")
  for (code in list(1L, -10000L, -10003L, NA_integer_, -2.5, c(-3, -3), NULL)) {
    bad <- valid
    bad$jerr <- code
    with_mocked_bindings(
      expect_error(.warm_start_glmnet_path(x, 1:6, 0.5, c(4, 2, 1)), "did not converge"),
      glmnet = function(...) bad, .package = "glmnet")
  }
  for (change in list(list(lambda = c(4, 1)), list(lambda = c(2, 1)),
    list(lambda = c(4, 2 + 1e-5)), list(lambda = c(4, 4)), list(lambda = c(4, NA)),
    list(lambda = c(4, Inf)), list(jerr = -2L), list(jerr = -4L), list(jerr = 0L),
    list(a0 = c(0, Inf)), list(a0 = 0), list(beta = matrix(Inf, 2, 2)),
    list(beta = matrix(0, 1, 2)))) {
    bad <- utils::modifyList(valid, change)
    with_mocked_bindings(
      expect_error(.warm_start_glmnet_path(x, 1:6, 0.5, c(4, 2, 1))),
      glmnet = function(...) bad, .package = "glmnet")
  }
  empty <- list(jerr = -1L, lambda = numeric(), a0 = numeric(), beta = matrix(0, 2, 0))
  with_mocked_bindings(
    expect_identical(.warm_start_glmnet_path(x, 1:6, 0.5, c(4, 2, 1)), empty),
    glmnet = function(...) empty, .package = "glmnet")
  for (change in list(list(a0 = Inf), list(a0 = numeric()), list(beta = matrix(1, 2, 1)),
    list(beta = matrix(0, 1, 1)), list(jerr = -2L))) {
    bad <- utils::modifyList(list(jerr = -1L, lambda = Inf, a0 = c(0, 0, 0),
      beta = matrix(0, 2, 1)), change)
    with_mocked_bindings(
      expect_error(.warm_start_glmnet_path(x, 1:6, 0.5, c(4, 2, 1))),
      glmnet = function(...) bad, .package = "glmnet")
  }
})
