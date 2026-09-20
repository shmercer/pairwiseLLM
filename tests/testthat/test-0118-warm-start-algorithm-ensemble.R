test_that("same-task ensemble deployment and outer validation use separate calibrated means", {
  f <- warm_algorithm_fixture()
  e <- f$ensemble
  expect_identical(class(e), "pairwiseLLM_warm_algorithm_ensemble")
  expect_identical(e$format_version, 1L)
  expect_identical(e$artifact_type, "algorithm_ensemble")
  expect_identical(e$components, f$components)
  expect_identical(e$cv_identity, f$components[[1]]$cv_identity)
  expected_outer <- Reduce(`+`, lapply(f$components, function(m) m$validation$predictions$calibrated_prediction)) / 3
  observed <- numeric(nrow(f$x))
  for (fold in seq_len(f$plan$outer_folds)) {
    train <- f$plan$outer_foldid != fold
    observed[!train] <- (f$theta[!train] - mean(f$theta[train])) / sd(f$theta[train])
  }
  expect_equal(e$validation$predictions$observed, observed)
  expect_equal(e$validation$predictions$calibrated_prediction, expected_outer)
  expect_identical(e$validation$predictions$item_id, f$x$item_id)
  expect_identical(e$validation$predictions$fold, unname(f$plan$outer_foldid))
  expect_equal(e$validation$metrics$rmse, sqrt(mean((expected_outer - observed)^2)))
  expect_equal(e$validation$metrics$mae, mean(abs(expected_outer - observed)))
  expect_equal(e$validation$metrics$pearson_r, cor(expected_outer, observed))
  expect_equal(e$validation$metrics$spearman_rho, cor(expected_outer, observed, method = "spearman"))
  expect_false(isTRUE(all.equal(observed, as.numeric(scale(f$theta)))))
  p <- predict(e, f$x)
  values <- vapply(f$components, function(m) predict(m, f$x)$calibrated_prediction, numeric(nrow(f$x)))
  expect_equal(p$ensemble_mean, rowMeans(values))
  expect_equal(p$ensemble_sd, apply(values, 1, sd))
  expect_false(isTRUE(all.equal(p$ensemble_mean, expected_outer)))
  expect_identical(attr(p, "warm_start_model")$artifact_type, "algorithm_ensemble")
  expect_identical(attr(p, "warm_start_model")$cv_identity, e$cv_identity)
  expect_identical(names(attr(p, "component_predictions")), names(f$components))
  expect_identical(summary(e)$validation$metrics, e$validation$metrics)
  expect_identical(summary(e)$audit_status, "full")
  expect_output(print(e), "Outer-held-out ensemble RMSE")
  expect_output(print(e), "not Bayesian prior SD")
  expect_output(print(p), "diagnostic")
  expect_identical(summary(p)$components, names(f$components))
  error <- tryCatch(warm_start_coefficients(e), error = identity)
  expect_s3_class(error, "pairwiseLLM_warm_nonlinear_coefficients")
  expect_identical(error$component, "svr_rbf")
  linear <- ensemble_warm_start_algorithms(latent = f$components$pls, elastic = f$components$glmnet)
  coefs <- warm_start_coefficients(linear)
  expect_identical(names(coefs), c("feature", "latent_std_coefficient", "elastic_std_coefficient"))
  expect_identical(coefs$latent_std_coefficient,
    warm_start_coefficients(f$components$pls)$calibrated_std_coefficient)
  q <- predict(linear, f$x[25:1, ])
  expect_identical(q$item_id, rev(f$x$item_id))
  expect_equal(q$ensemble_mean, rev(rowMeans(values[, c("pls", "glmnet")])))
  expect_equal(q$ensemble_sd, rev(abs(values[, "pls"] - values[, "glmnet"]) / sqrt(2)))
  expect_equal(predict(linear, f$x[1, , drop = FALSE])$ensemble_mean, q$ensemble_mean[25])
  expect_error(summary(e, extra = TRUE), "empty")
  expect_error(warm_start_coefficients(linear, extra = TRUE), "empty")
  expect_error(predict(e, f$x, extra = TRUE), "empty")
  expect_error(predict(e), "exactly one")
  calls <- 0L
  local_mocked_bindings(extract_warm_start_features = function(texts, ids, schema, python) {
    calls <<- calls + 1L
    expect_identical(ids, f$x$item_id)
    expect_identical(schema, e$schema)
    f$x
  }, .package = "pairwiseLLM")
  expect_identical(predict(e, texts = rep("synthetic", nrow(f$x)), ids = f$x$item_id), p)
  expect_identical(calls, 1L)
})

test_that("algorithm construction rejects missing names, nested, legacy and reduced evidence", {
  f <- warm_algorithm_fixture(engines = c("glmnet", "pls"))
  a <- f$components$glmnet
  b <- f$components$pls
  expect_error(ensemble_warm_start_algorithms(), "two")
  expect_error(ensemble_warm_start_algorithms(a = a), "two")
  expect_error(ensemble_warm_start_algorithms(a, b), "named")
  expect_error(ensemble_warm_start_algorithms(a = a, b), "nonblank")
  expect_error(ensemble_warm_start_algorithms(a = a, a = b), "unique")
  expect_error(ensemble_warm_start_algorithms(" " = a, b = b), "nonblank")
  for (bad in list(NULL, "model.rds", list(path = "model.rds"), warm_core_model(),
    prepare_warm_start_model(a, omit_audit = TRUE), f$ensemble,
    ensemble_warm_start_models(a = a, b = b))) {
    expect_error(ensemble_warm_start_algorithms(a = a, rejected = bad), "rejected.*failed")
  }
  expect_identical(names(ensemble_warm_start_algorithms(second = b, first = a)$components), c("second", "first"))
  expect_identical(predict(ensemble_warm_start_algorithms(a = a, duplicate = a), f$x)$ensemble_sd, rep(0, nrow(f$x)))
  expect_identical(ensemble_warm_start_models(a = a, duplicate = a)$components, list(a = a, duplicate = a))
})

test_that("valid but incompatible task, ordered outcome, schema and plan identities fail", {
  f <- warm_algorithm_fixture(engines = c("glmnet", "pls"))
  first <- f$components$glmnet
  variants <- list(task = list(task_id = "different"), outcome = list(theta = f$theta + 1),
    order = list(ids = rev(f$x$item_id), theta = rev(f$theta)), plan = list(seed = 260L),
    schema = list(features = warm_svr_fixture("writing_features_v2")$x, schema = "writing_features_v2"))
  for (change in variants) {
    args <- list(ids = f$x$item_id, theta = f$theta, task_id = "phase6", features = f$x,
      seed = 259L, engine = "pls", engine_control = list(ncomp = 1L))
    args[names(change)] <- change
    other <- do.call(fit_warm_start_model, args)
    expect_error(ensemble_warm_start_algorithms(first = first, incompatible = other), "incompatible.*failed")
  }
  other <- f$components$pls
  other$validation$predictions$observed[1] <- other$validation$predictions$observed[1] + 1e-12
  expect_silent(.validate_warm_start_model(other))
  expect_error(ensemble_warm_start_algorithms(first = first, perturbed = other), "aligned outer")
  for (field in c("cv_plan", "calibration", "validation", "tuning")) {
    bad <- f$components$pls
    bad[field] <- list(NULL)
    expect_error(ensemble_warm_start_algorithms(first = first, broken = bad), "broken.*failed")
  }
})

test_that("full ensemble validation reconstructs evidence and rejects corruption", {
  f <- warm_algorithm_fixture(engines = c("glmnet", "pls"))
  e <- f$ensemble
  for (field in names(e)) {
    bad <- e
    bad[field] <- list(NULL)
    expect_error(.validate_warm_start_algorithm_ensemble(bad), info = field)
  }
  for (field in c("schema", "features", "outcome", "cv_identity")) {
    bad <- e
    bad[[field]] <- "incompatible"
    expect_error(.validate_warm_start_algorithm_ensemble(bad), "contract")
  }
  bad <- e
  class(bad) <- c(class(e), "pairwiseLLM_warm_ensemble")
  expect_error(.validate_warm_start_algorithm_ensemble(bad), "contract")
  bad <- e
  bad$weighting <- c(0.2, 0.8)
  expect_error(.validate_warm_start_algorithm_ensemble(bad), "contract")
  for (field in names(e$validation)) {
    bad <- e
    bad$validation[field] <- list(NULL)
    expect_error(.validate_warm_start_algorithm_ensemble(bad), info = field)
  }
  for (field in names(e$validation$predictions)) {
    bad <- e
    bad$validation$predictions[[field]] <- rev(bad$validation$predictions[[field]])
    expect_error(.validate_warm_start_algorithm_ensemble(bad), info = field)
  }
  rounded <- e
  rounded$validation$predictions$calibrated_prediction[1] <-
    rounded$validation$predictions$calibrated_prediction[1] + 1e-12
  expect_silent(.validate_warm_start_algorithm_ensemble(rounded))
  bad <- e
  bad$validation$predictions$calibrated_prediction[1] <-
    bad$validation$predictions$calibrated_prediction[1] + 1e-5
  expect_error(.validate_warm_start_algorithm_ensemble(bad), "contract")
  bad <- e
  bad$validation$predictions$observed[1] <- bad$validation$predictions$observed[1] + 1e-12
  expect_error(.validate_warm_start_algorithm_ensemble(bad), "contract")
  for (values in list(rep(NA_real_, nrow(f$x)), matrix(1, nrow(f$x)), "invalid")) {
    bad <- e
    bad$validation$predictions$calibrated_prediction <- values
    expect_error(.validate_warm_start_algorithm_ensemble(bad), "contract")
  }
  bad <- e
  bad$validation$metrics$rmse <- bad$validation$metrics$rmse + 1
  expect_error(.validate_warm_start_algorithm_ensemble(bad), "contract")
  bad <- e
  bad$validation$predictions$calibrated_prediction <- predict(e, f$x)$ensemble_mean
  bad$validation$metrics <- .warm_start_validation_metrics(bad$validation$predictions$calibrated_prediction,
    bad$validation$predictions$observed)
  expect_error(.validate_warm_start_algorithm_ensemble(bad), "contract")
  bad <- e
  bad$metadata <- list(unknown = "invalid")
  expect_error(.validate_warm_start_algorithm_ensemble(bad), "Metadata")
  bad <- e
  bad$extra <- "unknown"
  expect_error(.validate_warm_start_algorithm_ensemble(bad), "contract")
  bad <- e
  attr(bad, "nonportable") <- new.env()
  expect_error(.validate_warm_start_algorithm_ensemble(bad), "contract")
})
