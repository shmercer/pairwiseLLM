test_that("portable models predict and summarize without optional development dependencies", {
  local_mocked_bindings(.warm_start_require_glmnet = function() stop("glmnet called"),
    .warm_start_python_request = function(...) stop("Python called"),
    warm_start_python_status = function(...) stop("Python status called"))
  model <- warm_core_model()
  x <- warm_core_features()
  x$first_order_coherence[1] <- NA_real_
  x <- x[rev(seq_len(nrow(x))), ]
  pred <- predict(model, x)
  scaled <- .warm_start_preprocess_apply(as.matrix(x[, -1]), model$preprocessing)
  expect_equal(pred$raw_prediction, as.numeric(0.2 + scaled %*% model$coefficients))
  expect_identical(pred$item_id, x$item_id)
  expect_identical(names(pred), c("item_id", "raw_prediction", "calibrated_prediction"))
  expect_identical(pred$calibrated_prediction, rep(NA_real_, nrow(x)))
  expect_identical(attr(pred, "warm_start_schema"), "writing_features_v1")
  expect_identical(attr(pred, "warm_start_model"), list(format_version = 1L,
    task_id = "synthetic-assessment", outcome_definition = "within_task_z",
    calibration_status = "uncalibrated"))
  info <- summary(model)
  expect_identical(info$calibration, "uncalibrated")
  expect_identical(info$validation, "not performed")
  expect_identical(info$retained_predictors, 20L)
  expect_output(expect_identical(print(model), model), "Task-specific.*synthetic-assessment")
  expect_output(print(model), "uncalibrated.*not performed")
  path <- tempfile(fileext = ".rds")
  withr::defer(unlink(path))
  saveRDS(model, path)
  restored <- readRDS(path)
  expect_identical(model, restored)
  expect_identical(predict(restored, x), pred)
  expect_true(.warm_start_portable(model))
  expect_null(model$tuning)
  expect_null(model$validation)
})

test_that("prediction validates schema and required fields before preprocessing", {
  model <- warm_core_model()
  x <- warm_core_features()
  for (bad in list(NULL, list(), x[, -1])) {
    expect_error(predict(model, bad), "data frame containing item_id")
  }
  bad <- x
  bad$n_tokens <- NULL
  expect_error(predict(model, bad), "Missing required.*n_tokens")
  for (schema in list(NULL, "v2")) {
    bad <- x
    attr(bad, "warm_start_schema") <- schema
    expect_error(predict(model, bad), "schema mismatch")
  }
  bad <- x
  bad$item_id[1] <- bad$item_id[2]
  expect_error(predict(model, bad), "unique")
  bad <- x
  bad$item_id <- seq_len(nrow(x))
  expect_identical(predict(model, bad)$item_id, x$item_id)
  bad$extra <- "ignore"
  expect_identical(predict(model, bad), predict(model, x))
  for (fn in list(function() predict(model, x, unknown = 1), function() summary(model, unknown = 1),
    function() print(model, unknown = 1))) expect_error(fn(), "must be empty")
  model$coefficients[] <- 1e308
  expect_error(predict(model, x), "nonfinite")
})

test_that("model validator rejects unsupported and damaged deployment contracts", {
  model <- warm_core_model()
  validate <- .validate_warm_start_model
  expect_invisible(validate(model))
  expect_error(validate(unclass(model)), "contract")
  for (field in names(model)) {
    if (field %in% c("tuning", "validation")) next
    bad <- model
    bad[field] <- list(NULL)
    expect_error(validate(bad), info = field)
  }
  bad <- model
  bad$format_version <- 3L
  expect_error(validate(bad), "format version")
  bad <- model
  bad$features <- rev(bad$features)
  expect_error(validate(bad), "contract")
  for (coefficients in list(unname(model$coefficients), rev(model$coefficients),
    model$coefficients[-1], stats::setNames(rep(Inf, 20), names(model$coefficients)))) {
    bad <- model
    bad$coefficients <- coefficients
    expect_error(validate(bad), "contract")
  }
  bad <- model
  bad$calibration <- list(status = "calibrated", intercept = 0, slope = 1)
  expect_error(validate(bad), "calibration contract")
  for (field in c("tuning", "validation")) {
    bad <- model
    bad[[field]] <- list(fake = TRUE)
    expect_error(validate(bad), "cannot contain")
  }
  for (field in names(model$training)) {
    bad <- model
    bad$training[field] <- list(NULL)
    expect_error(validate(bad), info = field)
  }
  for (field in c("n", "n_nonzero")) {
    bad <- model
    bad$training[[field]] <- 4.5
    expect_error(validate(bad), "contract")
  }
  for (value in list(new.env(parent = emptyenv()), function() 1, as.Date("2020-01-01"))) {
    bad <- model
    bad$extra <- value
    expect_error(validate(bad), "contract")
  }
  bad <- model
  attr(bad$training, "hidden_environment") <- new.env(parent = emptyenv())
  expect_error(validate(bad), "contract")
})

test_that("fixed fits support p below, equal to and substantially above n without screening", {
  skip_if_not_installed("glmnet")
  withr::local_seed(123)
  rng <- .Random.seed
  for (n in c(40L, 20L, 5L)) {
    x <- warm_core_features(n)
    theta <- warm_core_theta(x)
    model <- .fit_warm_start_fixed(x, x$item_id, theta, "one-task", 0.5, 0.1)
    expect_s3_class(model, "pairwiseLLM_warm_model")
    expect_length(model$coefficients, 20)
    expect_identical(model$features, warm_start_feature_schema()$feature)
    expect_equal(model$outcome$mean, mean(theta))
    expect_equal(model$outcome$sd, sd(theta))
    expect_true(all(is.finite(predict(model, x)$raw_prediction)))
    expect_identical(model, .fit_warm_start_fixed(x, x$item_id, theta, "one-task", 0.5, 0.1))
    shuffled <- x[rev(seq_len(nrow(x))), ]
    expect_identical(model, .fit_warm_start_fixed(shuffled, x$item_id, theta, "one-task", 0.5, 0.1))
    expect_identical(.Random.seed, rng)
  }
  # Arbitrary matrix is an internal stress fixture, not a new feature schema.
  x <- matrix(stats::rnorm(8 * 200), 8, 200, dimnames = list(NULL, paste0("x", 1:200)))
  p <- .warm_start_preprocess_fit(x)
  fit <- .warm_start_glmnet_fit(.warm_start_preprocess_apply(x, p), seq_len(8), 0.5, 0.1)
  expect_equal(nrow(fit$beta), 200)
  expect_true(all(is.finite(fit$beta)))
})

test_that("manual coefficients match glmnet with imputation, nonzero means and unequal scales", {
  skip_if_not_installed("glmnet")
  x <- warm_core_features(40)
  x$first_order_coherence[1:5] <- NA_real_
  theta <- warm_core_theta(x)
  new <- warm_core_features(12)
  new$first_order_coherence[c(1, 3)] <- NA_real_
  for (alpha in c(0, 0.5, 1)) {
    model <- .fit_warm_start_fixed(x, x$item_id, theta, "one-task", alpha, 0.1)
    expect_equal(unname(model$preprocessing$missing_fraction["first_order_coherence"]), 5 / 40)
    scaled <- .warm_start_preprocess_apply(as.matrix(x[, -1]), model$preprocessing)
    z <- .warm_start_outcome_apply(theta, model$outcome)
    reference <- do.call(glmnet::glmnet, c(list(x = scaled, y = z, family = "gaussian", alpha = alpha,
      lambda = 0.1, standardize = FALSE, intercept = TRUE), .warm_start_glmnet_controls()))
    new_scaled <- .warm_start_preprocess_apply(as.matrix(new[, -1]), model$preprocessing)
    expected <- as.numeric(predict(reference, newx = new_scaled))
    expect_equal(predict(model, new)$raw_prediction, expected, tolerance = 1e-8)
    expect_equal(model$coefficients, stats::setNames(as.numeric(reference$beta), colnames(scaled)),
      tolerance = 1e-8)
    expect_equal(model$intercept, unname(reference$a0[1]), tolerance = 1e-8)
  }
  zero <- .fit_warm_start_fixed(x, x$item_id, theta, "one-task", 1, 0)
  expect_identical(zero$training$lambda, 0)
  expect_true(all(is.finite(predict(zero, new)$raw_prediction)))
})

test_that("one retained predictor uses an excluded column without changing its penalty", {
  skip_if_not_installed("glmnet")
  x <- warm_core_features(30)
  for (name in names(x)[-1]) {
    if (name != "n_tokens") x[[name]] <- 1
  }
  theta <- 2 + 3 * x$n_tokens + sin(x$n_tokens)
  for (alpha in c(0, 0.5, 1)) {
    model <- .fit_warm_start_fixed(x, x$item_id, theta, "one-task", alpha, 0.2)
    expect_identical(names(model$coefficients), "n_tokens")
    scaled <- .warm_start_preprocess_apply(as.matrix(x[, -1]), model$preprocessing)
    z <- .warm_start_outcome_apply(theta, model$outcome)
    reference <- do.call(glmnet::glmnet, c(list(x = cbind(scaled, excluded = 0), y = z,
      alpha = alpha, lambda = 0.2, exclude = 2L, standardize = FALSE, intercept = TRUE),
      .warm_start_glmnet_controls()))
    expect_equal(predict(model, x)$raw_prediction,
      as.numeric(predict(reference, newx = cbind(scaled, excluded = 0))), tolerance = 1e-8)
    # Gaussian glmnet normalizes response to population SD internally for the elastic-net penalty.
    ys <- sqrt(mean((z - mean(z))^2))
    covariance <- mean(scaled[, 1] * (z - mean(z)))
    expected <- sign(covariance) * max(abs(covariance) - 0.2 * alpha, 0) /
      (mean(scaled[, 1]^2) + 0.2 * (1 - alpha) / ys)
    expect_equal(unname(model$coefficients), expected, tolerance = 1e-8)
    bad <- x
    bad$token_length_mean <- NULL
    expect_error(predict(model, bad), "Missing required")
  }
})

test_that("training boundaries reject invalid inputs before fitting", {
  local_mocked_bindings(.warm_start_require_glmnet = function() stop("unexpected glmnet"))
  x <- warm_core_features()
  fit <- function(ids = x$item_id, theta = warm_core_theta(x), task_id = "task", alpha = 0.5,
                  lambda = 0.1, features = x) {
    .fit_warm_start_fixed(features, ids, theta, task_id, alpha, lambda)
  }
  expect_error(fit(ids = c("absent", x$item_id[-1])), "match requested IDs")
  expect_error(fit(ids = rep("a", nrow(x))), "unique")
  expect_error(fit(theta = 1), "one theta")
  expect_error(fit(theta = rep(1, nrow(x))), "positive sample SD")
  expect_error(fit(theta = rep(Inf, nrow(x))), "theta")
  expect_error(fit(ids = x$item_id[1:2], theta = 1:2, features = x[1:2, ]), "at least three")
  for (value in list(NULL, NA_character_, "", " ", 1, c("a", "b"))) {
    expect_error(fit(task_id = value), "task_id")
  }
  for (value in list(NULL, NA_real_, Inf, -1, 2, c(0, 1), "0.5")) {
    expect_error(fit(alpha = value), "alpha")
  }
  for (value in list(NULL, NA_real_, Inf, -1, c(0, 1), "0.1")) {
    expect_error(fit(lambda = value), "lambda")
  }
  attr(x, "warm_start_schema") <- NULL
  expect_error(fit(), "schema mismatch")
})

test_that("raw schema validation precedes filtering, with explicit no-survivor failure", {
  skip_if_not_installed("glmnet")
  x <- warm_core_features()
  x$first_order_coherence <- NA_real_
  x$pos_prop_adj <- 0
  model <- .fit_warm_start_fixed(x, x$item_id, warm_core_theta(x), "task", 0.5, 0.1)
  expect_identical(model$preprocessing$removed[c("first_order_coherence", "pos_prop_adj")],
    c(first_order_coherence = "all_missing", pos_prop_adj = "constant"))
  expect_true(all(is.finite(predict(model, x)$raw_prediction)))
  for (name in names(x)[-1]) x[[name]] <- 1
  expect_error(.fit_warm_start_fixed(x, x$item_id, seq_len(nrow(x)), "task", 0.5, 0.1),
    "No predictors survive")
})

test_that("glmnet boundary handles missing dependency and solver failures explicitly", {
  local_mocked_bindings(.warm_start_glmnet_available = function() FALSE)
  expect_error(.warm_start_require_glmnet(), "Install it explicitly")
})

test_that("solver control routing supports both glmnet interfaces", {
  controls <- list(thresh = 1e-12, maxit = 100000L)
  expect_identical(.warm_start_glmnet_controls(function(control = list()) NULL), list(control = controls))
  expect_identical(.warm_start_glmnet_controls(function(thresh, maxit) NULL), controls)
})

test_that("glmnet boundary rejects malformed fits and convergence failures", {
  skip_if_not_installed("glmnet")
  x <- cbind(a = 1:5, b = 6:10)
  expect_error(.warm_start_glmnet_fit(x, 1:4, 0.5, 0.1), "aligned rows")
  expect_error(.warm_start_glmnet_fit(x, rep(1, 5), 0.5, 0.1), "nonconstant")
  x_na <- x
  x_na[1] <- NA
  expect_error(.warm_start_glmnet_fit(x_na, 1:5, 0.5, 0.1), "matrix")
  local_mocked_bindings(glmnet = function(...) stop("solver error"), .package = "glmnet")
  expect_error(.warm_start_glmnet_fit(x, 1:5, 0.5, 0.1), "Elastic-net fitting failed")
  local_mocked_bindings(glmnet = function(...) list(jerr = -1L), .package = "glmnet")
  expect_error(.warm_start_glmnet_fit(x, 1:5, 0.5, 0.1), "did not converge")
  for (bad in list(list(jerr = 0L, lambda = 0.2, a0 = 0, beta = x),
    list(jerr = 0L, lambda = 0.1, a0 = Inf, beta = x),
    list(jerr = 0L, lambda = 0.1, a0 = 0, beta = matrix(Inf, 2, 1)))) {
    local_mocked_bindings(glmnet = function(...) bad, .package = "glmnet")
    expect_error(.warm_start_glmnet_fit(x, 1:5, 0.5, 0.1), "requested lambda")
  }
})
