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
  expect_error(.validate_warm_start_tuning(bad, 15L), "CV loss")
  bad <- tuning
  bad$traces[[1]]$lambda[1] <- -1
  expect_error(.validate_warm_start_tuning(bad, 15L), "tuning contract")
})

test_that("portable model validation rejects inconsistent nested predictions and metadata", {
  skip_if_not_installed("glmnet")
  features <- warm_core_features(15)
  model <- fit_warm_start_model(features$item_id, warm_core_theta(features), "audit",
    features = features, alpha_grid = c(0, 1))
  expect_invisible(.validate_warm_start_model(model))
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
  })
  model <- suppressWarnings(fit_warm_start_model(features$item_id, warm_core_theta(features), "warnings",
    features = features, alpha_grid = 0))
  expect_length(model$validation$warnings, 6)
  expect_match(model$validation$warnings[1], "Outer fold 1: stored warning")
  expect_match(model$validation$warnings[6], "Final full-data fit: stored warning")
})
