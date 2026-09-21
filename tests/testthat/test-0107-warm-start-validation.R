test_that("portable tuning validation rejects missing and inconsistent audit records", {
  skip_if_not_installed("glmnet")
  features <- warm_core_features(15)
  model <- fit_warm_start_model(features$item_id, warm_core_theta(features), "audit",
    features = features, alpha_grid = c(0, 1))
  tuning <- model$tuning
  expect_invisible(.validate_warm_start_tuning(tuning, 15L))
  for (field in setdiff(names(tuning), c("ids", "seed"))) {
    bad <- tuning
    bad[field] <- list(NULL)
    expect_error(.validate_warm_start_tuning(bad, 15L), info = field)
  }
  for (field in names(tuning$traces[[1]])) {
    bad <- tuning
    bad$traces[[1]][field] <- list(NULL)
    expect_error(.validate_warm_start_tuning(bad, 15L), info = field)
  }
  for (field in names(tuning$selected)) {
    bad <- tuning
    bad$selected[field] <- list(NULL)
    expect_error(.validate_warm_start_tuning(bad, 15L), info = field)
  }
  for (field in names(tuning$oof)) {
    bad <- tuning
    bad$oof[[field]][1] <- 99
    # Observed values have no independent source inside tuning alone;
    # complete model validation checks their outcome/calibration consistency.
    if (field == "observed") {
      broken <- model
      broken$tuning <- bad
      expect_error(.validate_warm_start_model(broken))
    } else {
      expect_error(.validate_warm_start_tuning(bad, 15L))
    }
  }
  bad <- tuning
  bad$inner_preprocessing[[1]]$n_training <- 99L
  expect_error(.validate_warm_start_tuning(bad, 15L), "tuning contract")
  bad <- tuning
  bad$foldid[1] <- 99L
  expect_error(.validate_warm_start_tuning(bad, 15L), "tuning contract")
  bad <- tuning
  bad$traces[[1]]$fold_mse[1, 1] <- Inf
  expect_error(.validate_warm_start_tuning(bad, 15L), "candidate-validity")
  bad <- tuning
  bad$traces[[1]]$lambda[1] <- -1
  expect_error(.validate_warm_start_tuning(bad, 15L), "tuning contract")
})

test_that("candidate validity is reconstructed from raw fold convergence evidence", {
  skip_if_not_installed("glmnet")
  x <- cbind(a = 1:12, b = (1:12)^2)
  local_mocked_bindings(glmnet = warm_tail_engine(empty_alpha = 0), .package = "glmnet")
  tuning <- .warm_start_tune(x, as.numeric(scale(1:12)), rep(1:3, 4), c(0, 1), "lambda.min")
  expect_invisible(.validate_warm_start_tuning(tuning, 12L))
  numeric_index <- tuning
  numeric_index$traces[[2]]$index <- as.numeric(numeric_index$traces[[2]]$index)
  expect_invisible(.validate_warm_start_tuning(numeric_index, 12L))
  numeric_index$candidate_validity$alphas[[2]]$selected_smallest_eligible <- FALSE
  expect_error(.validate_warm_start_tuning(numeric_index, 12L), "candidate-validity")
  for (i in 1:2) {
    for (field in names(tuning$candidate_validity$alphas[[i]])) {
      bad <- tuning
      bad$candidate_validity$alphas[[i]][field] <- list(NULL)
      expect_error(.validate_warm_start_tuning(bad, 12L), info = paste(i, field))
    }
  }
  for (change in list(
    list(fold_converged_count = c(4L, 4L, 4L)), list(eligible = rep(TRUE, 4)),
    list(fold_eligible = matrix(TRUE, 3, 4)), list(invalid_tail_count = 0L),
    list(alpha_eligible = FALSE), list(selected_smallest_eligible = FALSE),
    list(fold_jerr = c(0L, -3L, 0L)), list(fold_lambda = list(c(4, 1, 0.5), c(4, 2), c(4, 2, 1, 0.5))),
    list(requested_lambda = c(4, 2, 1, 0.4)))) {
    bad <- tuning
    bad$candidate_validity$alphas[[2]][names(change)] <- change
    expect_error(.validate_warm_start_tuning(bad, 12L), info = names(change))
  }
  for (change in list(
    function(t) {
      t$traces[[2]]$fold_mse[2, 3] <- 0
      t
    }, function(t) {
      t$traces[[2]]$fold_mse[1, 3] <- NA_real_
      t
    }, function(t) {
      t$traces[[2]]$fold_mse[1, 1] <- -1
      t
    }, function(t) {
      t$traces[[2]]$fold_mse[1, 1] <- Inf
      t
    }, function(t) {
      t$traces[[2]]$cvm[3] <- 0
      t
    }, function(t) {
      t$traces[[2]]$cvsd[3] <- 0
      t
    }, function(t) {
      t$traces[[2]]$index <- 3L
      t
    }, function(t) {
      t$traces[[2]]$index_1se <- 3L
      t
    }, function(t) {
      t$traces[[1]]$index_min <- 1L
      t
    }, function(t) {
      t$selected$alpha_index <- 1L
      t
    }, function(t) {
      t$candidate_validity$version <- 2L
      t
    }, function(t) {
      t$candidate_validity <- NULL
      t
    })) {
    expect_error(.validate_warm_start_tuning(change(tuning), 12L))
  }
})

test_that("portable model validation rejects inconsistent nested predictions and metadata", {
  skip_if_not_installed("glmnet")
  features <- warm_core_features(15)
  model <- fit_warm_start_model(features$item_id, warm_core_theta(features), "audit",
    features = features, alpha_grid = c(0, 1))
  expect_invisible(.validate_warm_start_model(model))
  bad <- model
  bad$tuning$oof$observed <- bad$tuning$oof$observed + 1
  bad$calibration <- pairwiseLLM:::.warm_start_calibration_fit(
    bad$tuning$oof$raw_prediction, bad$tuning$oof$observed)
  expect_error(.validate_warm_start_model(bad), "validation contract")
  bad <- model
  bad$validation$folds[1] <- list(1)
  expect_error(.validate_warm_start_model(bad), "validation contract")
  bad <- model
  bad$validation$folds[[1]]$predictions$item_id[1] <- "wrong-id"
  expect_error(.validate_warm_start_model(bad), "validation contract")
  for (field in names(model$validation)) {
    bad <- model
    bad$validation[field] <- list(NULL)
    expect_error(.validate_warm_start_model(bad), info = field)
  }
  for (field in names(model$validation$folds[[1]])) {
    bad <- model
    bad$validation$folds[[1]][field] <- list(NULL)
    expect_error(.validate_warm_start_model(bad), info = field)
  }
  for (field in names(model$validation$predictions)) {
    bad <- model
    bad$validation$predictions[[field]][1] <- NA
    expect_error(.validate_warm_start_model(bad), info = field)
  }
  bad <- model
  bad$validation$folds[[1]]$outcome$mean <- 100
  expect_error(.validate_warm_start_model(bad), "validation contract")
  bad <- model
  bad$validation$folds[[1]]$predictions$raw_prediction[1] <- 100
  expect_error(.validate_warm_start_model(bad), "validation contract")
  bad <- model
  bad$validation$folds[[1]]$test_ids <- rev(bad$validation$folds[[1]]$test_ids)
  expect_error(.validate_warm_start_model(bad), "validation contract")
  bad <- model
  bad$validation$predictions$calibrated_prediction[1] <- 100
  expect_error(.validate_warm_start_model(bad), "validation contract")
  bad <- model
  bad$validation$metrics$rmse <- 100
  expect_error(.validate_warm_start_model(bad), "validation contract")
  bad <- model
  bad$tuning$seed <- -1
  expect_error(.validate_warm_start_model(bad), "validation contract")
  bad <- model
  bad$calibration$slope <- bad$calibration$slope + 1
  expect_error(.validate_warm_start_model(bad), "validation contract")
})

test_that("nested validation tolerates near-zero outcome mean reconstruction drift", {
  expect_true(.warm_start_audit_equal(1.18e-08, 1.18e-08 + 1.7e-18))
  expect_false(.warm_start_audit_equal(1.18e-08, 1e-6))
  skip_if_not_installed("glmnet")
  features <- warm_core_features(15)
  theta <- warm_core_theta(features)
  theta <- theta - mean(theta) + 1.18e-08
  model <- fit_warm_start_model(features$item_id, theta, "near-zero-mean",
    features = features, alpha_grid = c(0, 0.5, 1))
  expect_lt(abs(mean(theta) - 1.18e-08), 1e-15)
  expect_invisible(.validate_warm_start_model(model))
})

test_that("warning provenance retains context and fatal errors retain their parent", {
  expect_warning(value <- .warm_start_cv_context("Outer fold 2", function() {
    .warm_start_cv_context("Alpha 0.5", function() {
      rlang::warn("diagnostic fixture")
      42
    })
  }), "Outer fold 2: Alpha 0.5: diagnostic fixture")
  expect_equal(value, 42)
  error <- tryCatch(.warm_start_cv_context("Outer fold 2", function() rlang::abort("broken")), error = identity)
  expect_match(conditionMessage(error), "Outer fold 2: broken")
  expect_s3_class(error$parent, "rlang_error")
  skip_if_not_installed("glmnet")
  features <- warm_core_features(15)
  original <- .warm_start_train_cv
  local_mocked_bindings(.warm_start_train_cv = function(...) {
    rlang::warn("stored warning")
    original(...)
  }, .package = "pairwiseLLM")
  model <- suppressWarnings(fit_warm_start_model(features$item_id, warm_core_theta(features), "warnings",
    features = features, alpha_grid = 0))
  expect_length(model$validation$warnings, 6)
  expect_match(model$validation$warnings[1], "Outer fold 1: stored warning")
  expect_match(model$validation$warnings[6], "Final full-data fit: stored warning")
})
