test_that("rank-block folds are balanced, reproducible and preserve public RNG state", {
  withr::local_seed(123)
  theta <- rep(1:8, each = 3)
  first <- withr::with_seed(1, .warm_start_folds(theta, 5L))
  expect_identical(first, withr::with_seed(1, .warm_start_folds(theta, 5L)))
  expect_false(identical(first, withr::with_seed(2, .warm_start_folds(theta, 5L))))
  expect_lte(diff(range(tabulate(first))), 1)
  for (k in c(1, 2.5, 6, NA, Inf)) expect_error(.warm_start_fold_count(k, 5), "Requested folds")
  skip_if_not_installed("glmnet")
  features <- warm_core_features(15)
  seed_before <- .Random.seed
  model <- fit_warm_start_model(features$item_id, warm_core_theta(features), "rng",
    features = features, alpha_grid = c(0, 0.5, 1))
  expect_identical(.Random.seed, seed_before)
  again <- fit_warm_start_model(features$item_id, warm_core_theta(features), "rng",
    features = features, alpha_grid = c(1, 0, 0.5))
  expect_identical(model, again)
  changed <- fit_warm_start_model(features$item_id, warm_core_theta(features), "rng",
    features = features, alpha_grid = c(0, 0.5, 1), seed = 2L)
  expect_false(identical(model$validation$predictions$fold, changed$validation$predictions$fold))
})

test_that("nested CV retains outer outcomes and final selected OOF calibration", {
  skip_if_not_installed("glmnet")
  features <- warm_core_features(15)
  theta <- warm_core_theta(features)
  model <- fit_warm_start_model(features$item_id, theta, "assessment", features = features,
    alpha_grid = c(0, 0.5, 1))
  expect_length(model$coefficients, 20)
  expect_identical(model$training$n, 15L)
  expect_identical(model$tuning$alpha_grid, c(0, 0.5, 1))
  p <- model$validation$predictions
  expect_identical(p$item_id, features$item_id)
  expect_identical(sort(unlist(lapply(model$validation$folds, `[[`, "test_ids"))), sort(features$item_id))
  for (i in seq_len(5)) {
    record <- model$validation$folds[[i]]
    train <- p$fold != i
    holdout <- !train
    expect_equal(record$outcome$mean, mean(theta[train]))
    expect_equal(record$outcome$sd, sd(theta[train]))
    expect_equal(p$observed[holdout], (theta[holdout] - mean(theta[train])) / sd(theta[train]))
    expect_equal(record$calibration,
      .warm_start_calibration_fit(record$tuning$oof$raw_prediction, record$tuning$oof$observed))
    expect_equal(p$calibrated_prediction[holdout],
      record$calibration$intercept + record$calibration$slope * p$raw_prediction[holdout])
    expect_identical(record$n_nonzero, sum(record$coefficients != 0))
  }
  expect_equal(model$validation$metrics, .warm_start_validation_metrics(p$calibrated_prediction, p$observed))
  expect_equal(model$calibration,
    .warm_start_calibration_fit(model$tuning$oof$raw_prediction, model$tuning$oof$observed))
  predictions <- predict(model, features)
  expect_false(isTRUE(all.equal(predictions$raw_prediction, model$tuning$oof$raw_prediction)))
  expect_equal(predictions$calibrated_prediction,
    model$calibration$intercept + model$calibration$slope * predictions$raw_prediction)
  expect_output(print(model), "Nested validation")
  expect_identical(summary(model)$validation, model$validation$metrics)
  expect_identical(attr(predictions, "warm_start_model")$task_id, "assessment")
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(model, path)
  expect_identical(readRDS(path), model)
  local_mocked_bindings(.warm_start_require_glmnet = function() stop("glmnet called"),
    .warm_start_python_request = function(...) stop("Python called"),
    warm_start_python_status = function(...) stop("Python status called"))
  expect_identical(predict(readRDS(path), features), predictions)
})

test_that("outer holdout perturbations cannot change its fitted training pipeline", {
  skip_if_not_installed("glmnet")
  features <- warm_core_features(20)
  theta <- warm_core_theta(features)
  original_folds <- .warm_start_folds
  fixed <- rep(1:5, 4)
  local_mocked_bindings(.warm_start_folds = function(theta, k) {
    if (length(theta) == 20L) fixed else original_folds(theta, k)
  })
  first <- fit_warm_start_model(features$item_id, theta, "leakage", features = features,
    alpha_grid = 0, lambda_rule = "lambda.min")
  changed <- features
  changed$first_order_coherence[fixed == 1] <- 100
  changed$token_length_mean[fixed == 1] <- 500
  changed_theta <- theta
  changed_theta[fixed == 1] <- changed_theta[fixed == 1] + 25
  second <- fit_warm_start_model(changed$item_id, changed_theta, "leakage", features = changed,
    alpha_grid = 0, lambda_rule = "lambda.min")
  fields <- setdiff(names(first$validation$folds[[1]]), "predictions")
  expect_identical(first$validation$folds[[1]][fields], second$validation$folds[[1]][fields])
  expect_false(identical(first$validation$folds[[1]]$predictions, second$validation$folds[[1]]$predictions))
})

test_that("each inner preprocessing object excludes its holdout", {
  skip_if_not_installed("glmnet")
  withr::local_seed(10)
  n <- 55L
  x <- cbind(missing = stats::rnorm(n), nzv = rep(0, n), scaled = seq_len(n), impute = stats::rnorm(n))
  foldid <- rep(1:5, 11)
  x[foldid == 1, "missing"] <- NA
  x[1, "nzv"] <- 1
  x[c(2, 7, 12), "impute"] <- NA
  z <- as.numeric(scale(seq_len(n)))
  first <- .warm_start_tune(x, z, foldid, 0, "lambda.min")
  for (fold in 1:5) {
    expected <- .warm_start_preprocess_fit(x[foldid != fold, , drop = FALSE])
    expect_identical(first$inner_preprocessing[[fold]], expected)
  }
  expect_equal(first$inner_preprocessing[[1]]$missing_fraction["missing"], c(missing = 0))
  expect_identical(unname(first$inner_preprocessing[[1]]$removed["nzv"]), "constant")
  expect_identical(unname(first$inner_preprocessing[[2]]$removed["nzv"]), "near_zero_variance")
  expect_identical(unname(first$inner_preprocessing[[2]]$removed["missing"]), "missingness")
  expect_equal(first$inner_preprocessing[[1]]$medians["impute"],
    c(impute = median(x[foldid != 1, "impute"], na.rm = TRUE)))
  x[foldid == 1, ] <- 10000
  second <- .warm_start_tune(x, z, foldid, 0, "lambda.min")
  expect_identical(first$inner_preprocessing[[1]], second$inner_preprocessing[[1]])
  expect_false(identical(first$reference_preprocessing, second$reference_preprocessing))
  x[, ] <- 1
  expect_error(.warm_start_tune(x, z, foldid, 0, "lambda.min"), "Inner fold 1.*No predictors")
  expect_error(.warm_start_tune(cbind(a = 1:5, b = 1:5), 1:5, c(1, 1, 1, 2, 2), 0, "lambda.min"),
    "Inner fold 1.*three nonconstant")
})

test_that("public wrapper extracts once or stays Python-free and honors expert controls", {
  skip_if_not_installed("glmnet")
  features <- warm_core_features(15)
  theta <- warm_core_theta(features)
  calls <- 0L
  local_mocked_bindings(extract_warm_start_features = function(ids, texts, schema, python) {
    calls <<- calls + 1L
    expect_identical(ids, features$item_id)
    expect_identical(texts, rep("offline fixture", 15))
    expect_identical(schema, "writing_features_v1")
    expect_identical(python, "/explicit/python")
    features
  })
  from_text <- fit_warm_start_model(features$item_id, theta, "texts", texts = rep("offline fixture", 15),
    python = "/explicit/python", alpha_grid = 0, outer_folds = 3L, inner_folds = 3L,
    lambda_rule = "lambda.min")
  expect_identical(calls, 1L)
  shuffled <- features[15:1, ]
  attr(shuffled, "warm_start_schema") <- "writing_features_v1"
  from_features <- fit_warm_start_model(features$item_id, theta, "texts", features = shuffled,
    alpha_grid = 0, outer_folds = 3L, inner_folds = 3L, lambda_rule = "lambda.min")
  expect_identical(calls, 1L)
  expect_identical(from_features, from_text)
  expect_identical(from_text$validation$outer_folds, 3L)
  expect_identical(from_text$validation$inner_folds, 3L)
  t <- from_text$tuning
  expect_identical(t$selected$lambda, t$traces[[t$selected$alpha_index]]$lambda_min)
  expect_identical(formals(fit_warm_start_model)$alpha_grid, quote(seq(0, 1, by = 0.025)))
})

test_that("invalid public inputs fail before extraction and insufficient splits never shrink", {
  features <- warm_core_features(15)
  theta <- warm_core_theta(features)
  local_mocked_bindings(.warm_start_require_glmnet = function() stop("engine boundary"),
    extract_warm_start_features = function(...) stop("extraction called"))
  fit <- function(...) fit_warm_start_model(features$item_id, theta, "task", features = features, ...)
  for (bad in list(NULL, "bad", c(0, 0), -1, 2, NA, matrix(0), structure(0, class = "bad"))) {
    expect_error(fit(alpha_grid = bad), "alpha_grid")
  }
  for (bad in list(NULL, NA, -1, Inf, 0.5, .Machine$integer.max + 1)) {
    expect_error(fit(seed = bad), "seed")
  }
  expect_error(fit(texts = rep("a", 15)), "exactly one")
  expect_error(fit(python = "/python"), "only used with")
  expect_error(fit(outer_folds = 16), "Requested folds")
  expect_error(fit(inner_folds = 13), "Requested folds")
  expect_error(fit(lambda_rule = "bad"), "arg")
  expect_error(fit(schema = "bad"), "schema")
  expect_error(fit(), "engine boundary")
  expect_error(fit_warm_start_model(features$item_id, theta, "task"), "exactly one")
  expect_error(fit_warm_start_model(features$item_id, theta[-1], "task", features = features), "one theta")
  expect_error(fit_warm_start_model(features$item_id, theta, "", features = features), "task_id")
  expect_error(fit_warm_start_model(as.character(1:5), 1:5, "task", texts = rep("a", 5)), "Requested folds")
})

test_that("schema failures and degenerate calibration have clear public errors", {
  skip_if_not_installed("glmnet")
  features <- warm_core_features(15)
  theta <- warm_core_theta(features)
  bad <- features
  attr(bad, "warm_start_schema") <- "wrong"
  expect_error(fit_warm_start_model(features$item_id, theta, "task", features = bad), "schema mismatch")
  bad <- features[, -2]
  attr(bad, "warm_start_schema") <- "writing_features_v1"
  expect_error(fit_warm_start_model(features$item_id, theta, "task", features = bad), "Missing required")
  local_mocked_bindings(.warm_start_calibration_fit = function(...) rlang::abort("degenerate fixture"))
  expect_error(fit_warm_start_model(features$item_id, theta, "task", features = features, alpha_grid = 0),
    "Outer fold 1.*degenerate fixture")
})

test_that("the optional audited Python stack supports the text-to-model wrapper", {
  python <- Sys.getenv("PAIRWISELLM_TEST_PYTHON", "")
  skip_if(!nzchar(python), "Set PAIRWISELLM_TEST_PYTHON to run the audited text-to-model integration.")
  skip_if(!file.exists(python), "The explicitly configured Python interpreter is unavailable.")
  skip_if_not_installed("reticulate")
  skip_if_not_installed("glmnet")
  texts <- vapply(seq_len(15), function(n) {
    paste(rep("Students explain their reasoning and support the argument with evidence.", n), collapse = " ")
  }, character(1))
  known_warning <- "Importing 'parser.split_arg_string' is deprecated"
  model <- withCallingHandlers(
    fit_warm_start_model(as.character(seq_len(15)), seq_len(15), "synthetic-length-fixture",
      texts = texts, python = python, alpha_grid = 0, lambda_rule = "lambda.min"),
    warning = function(w) {
      if (startsWith(conditionMessage(w), known_warning)) invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(model, "pairwiseLLM_warm_model")
  expect_identical(model$calibration$status, "oof_linear")
  expect_equal(nrow(model$validation$predictions), 15)
  expect_true(all(is.finite(model$validation$predictions$calibrated_prediction)))
  expect_false("texts" %in% names(model))
  expect_true(all(startsWith(model$validation$warnings, known_warning)))
})
