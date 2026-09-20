test_that("format-3 full and reduced artifacts deploy without a fitting backend", {
  skip_if_not_installed("glmnet")
  f <- warm_phase2_fixture()
  model <- warm_phase2_fit(f)
  model$private_text <- "must disappear"
  model$training$private_ids <- f$x$item_id
  reduced <- prepare_warm_start_model(model, warm_bundle_metadata("phase2"), omit_audit = TRUE)
  expect_identical(reduced$format_version, 3L)
  expect_identical(reduced$audit_status, "summary_only")
  expect_null(reduced$cv_plan)
  expect_identical(reduced$cv_identity, model$cv_identity)
  expect_identical(reduced$engine_payload, model$engine_payload)
  expect_identical(reduced$validation$metrics, model$validation$metrics)
  expect_null(reduced$validation$predictions)
  expect_null(reduced$tuning$ids)
  expect_null(reduced$private_text)
  expect_null(reduced$training$private_ids)
  expect_identical(prepare_warm_start_model(reduced, omit_audit = TRUE), reduced)
  expect_identical(summary(reduced)$audit_status, "summary_only")
  expect_identical(summary(model)$engine, "glmnet")
  local_mocked_bindings(.warm_start_require_glmnet = function() stop("unexpected backend"),
    .warm_start_python_request = function(...) stop("unexpected Python"), .package = "pairwiseLLM")
  root <- withr::local_tempdir()
  for (artifact in list(model, reduced)) {
    path <- file.path(root, "artifact.rds")
    save_warm_start_model(artifact, path, overwrite = TRUE)
    expect_identical(load_warm_start_model(path), artifact)
    expect_identical(predict(load_warm_start_model(path), f$x), predict(model, f$x))
    expect_identical(warm_start_coefficients(artifact), warm_start_coefficients(model))
    predictions <- predict(artifact, f$x)
    prior <- make_warm_start_prior(predictions)
    expect_identical(prior$scores, predictions$calibrated_prediction)
    expect_equal(prior$prior_mean, prior$scores - mean(prior$scores))
    expect_identical(prior$prior_sd, rep(0.5, nrow(f$x)))
    expect_identical(prior$provenance$model$format_version, 3L)
    expect_identical(prior$provenance$model$engine, "glmnet")
    expect_identical(prior$provenance$model$cv_digest, model$cv_identity$digest)
  }
  for (field in names(reduced)) {
    if (field == "cv_plan") next
    bad <- reduced
    bad[field] <- list(NULL)
    expect_error(.validate_warm_start_model(bad), info = field)
  }
  bad <- reduced
  bad$cv_plan <- model$cv_plan
  expect_error(.validate_warm_start_model(bad), "format-3")
  bad <- reduced
  bad$extra <- "row evidence"
  expect_error(.validate_warm_start_model(bad))
})

test_that("format-3 registry and verified bundles retain engine and CV metadata", {
  skip_if_not_installed("glmnet")
  model <- warm_phase2_fit()
  root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = root)
  empty <- list_warm_start_models("user")
  expect_true(all(c("engine", "engine_version", "component_engines") %in% names(empty)))
  register_warm_start_model(model, "phase2")
  rows <- list_warm_start_models("user")
  expect_identical(rows$engine, "glmnet")
  expect_identical(rows$engine_version, model$training$engine_version)
  expect_identical(rows$n, 20L)
  expect_identical(rows$artifact_type, "model")
  expect_identical(load_warm_start_model(name = "phase2", source = "user"), model)
  reduced <- prepare_warm_start_model(model, warm_bundle_metadata("phase2"), omit_audit = TRUE)
  bundle <- file.path(root, "bundle")
  dir.create(bundle)
  record <- warm_bundle_write(bundle, reduced)$artifacts[[1]]
  expect_identical(record$components[[1]]$cv_identity, model$cv_identity)
  expect_identical(.warm_start_bundle_model(bundle, "phase2"), reduced)
  expect_error(.warm_start_bundle_record(prepare_warm_start_model(model, warm_bundle_metadata("phase2")),
    file.path(bundle, "phase2.rds"), "2026-09-19T00:00:00Z"), "summary-only")
  legacy <- warm_bundle_model("legacy")
  path <- file.path(root, "legacy.rds")
  save_warm_start_model(legacy, path)
  legacy_record <- .warm_start_bundle_record(legacy, path, "2026-09-19T00:00:00Z")
  expect_identical(names(legacy_record$components[[1]]),
    c("component_name", "metadata", "format_version", "training", "tuning", "validation"))
})

test_that("mixed legacy and format-3 cross-task ensembles retain existing semantics", {
  skip_if_not_installed("glmnet")
  f <- warm_phase2_fixture()
  model <- warm_phase2_fit(f)
  legacy <- readRDS(test_path("fixtures", "warm-start-legacy", "baseline-1.5.1.rds"))$cases$default$model
  ensemble <- ensemble_warm_start_models(legacy = legacy, current = model, repeated = model)
  expected <- (predict(legacy, f$x)$calibrated_prediction + 2 * predict(model, f$x)$calibrated_prediction) / 3
  expect_equal(predict(ensemble, f$x)$ensemble_mean, expected)
  reduced <- prepare_warm_start_model(ensemble, omit_audit = TRUE)
  expect_identical(vapply(reduced$components, `[[`, integer(1), "format_version"),
    c(legacy = 2L, current = 3L, repeated = 3L))
  expect_identical(summary(reduced)$audit_status, "summary_only")
  expect_equal(predict(reduced, f$x)$ensemble_mean, expected)
  root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = root)
  register_warm_start_model(reduced, "mixed")
  rows <- list_warm_start_models("user")
  expect_identical(rows$n, NA_integer_)
  expect_identical(rows$component_engines[[1]], c(legacy = "glmnet", current = "glmnet", repeated = "glmnet"))
  expect_identical(rows$validation[[1]], lapply(reduced$components, function(m) m$validation$metrics))
  prior <- make_warm_start_prior(predict(reduced, f$x))
  expect_equal(prior$prior_mean, expected - mean(expected))
  expect_identical(prior$prior_sd, rep(0.5, nrow(f$x)))
})

test_that("PLS full and reduced artifacts preserve both schemas and deploy without backend calls", {
  skip_if_not_installed("pls")
  root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = root)
  for (schema in c("writing_features_v1", "writing_features_v2")) {
    f <- warm_pls_fixture(schema)
    model <- warm_pls_fit(f, engine_control = list(ncomp = c(1, 2)))
    model$private_text <- "must disappear"
    model$training$private_ids <- f$x$item_id
    reduced <- prepare_warm_start_model(model, warm_bundle_metadata("pls"), omit_audit = TRUE)
    expect_identical(reduced$format_version, 3L)
    expect_identical(reduced$audit_status, "summary_only")
    expect_null(reduced$cv_plan)
    expect_null(reduced$tuning$ids)
    expect_null(reduced$tuning$traces)
    expect_null(reduced$private_text)
    expect_null(reduced$training$private_ids)
    expect_null(reduced$validation$predictions)
    expect_identical(reduced$cv_identity, model$cv_identity)
    expect_identical(reduced$engine_payload, model$engine_payload)
    expect_identical(reduced$training$hyperparameters, model$training$hyperparameters)
    expect_identical(reduced$validation$metrics, model$validation$metrics)
    expect_identical(prepare_warm_start_model(reduced, omit_audit = TRUE), reduced)
    expect_identical(summary(reduced)$ncomp, model$tuning$selected$ncomp)
    expect_output(print(reduced), "Components:")
    with_mocked_bindings({
      for (artifact in list(model, reduced)) {
        path <- file.path(root, "pls.rds")
        save_warm_start_model(artifact, path, overwrite = TRUE)
        expect_identical(load_warm_start_model(path), artifact)
        actual <- predict(load_warm_start_model(path), f$x)
        expect_identical(actual$calibrated_prediction, predict(model, f$x)$calibrated_prediction)
        expect_identical(warm_start_coefficients(artifact), warm_start_coefficients(model))
        prior <- make_warm_start_prior(actual)
        expect_identical(prior$provenance$model$engine, "pls")
        expect_identical(prior$provenance$model$cv_digest, model$cv_identity$digest)
        expect_equal(prior$prior_mean, actual$calibrated_prediction - mean(actual$calibrated_prediction))
        expect_identical(prior$prior_sd, rep(0.5, nrow(f$x)))
      }
      register_warm_start_model(reduced, "pls", overwrite = TRUE)
      expect_identical(load_warm_start_model(name = "pls", source = "user"), reduced)
      rows <- list_warm_start_models("user")
      expect_identical(rows$engine, "pls")
      expect_identical(rows$schema, schema)
      bundle <- file.path(root, schema)
      dir.create(bundle)
      record <- warm_bundle_write(bundle, reduced)$artifacts[[1]]
      expect_identical(record$components[[1]]$cv_identity, model$cv_identity)
      expect_identical(.warm_start_bundle_model(bundle, "pls"), reduced)
    }, .warm_start_require_pls = function() stop("unexpected pls"),
      .warm_start_require_glmnet = function() stop("unexpected glmnet"),
      .warm_start_python_request = function(...) stop("unexpected Python"), .package = "pairwiseLLM")
  }
})

test_that("PLS reduced contracts reject corruption and mix with legacy cross-task components", {
  skip_if_not_installed("pls")
  f <- warm_pls_fixture()
  model <- warm_pls_fit(f)
  reduced <- prepare_warm_start_model(model, omit_audit = TRUE)
  for (field in setdiff(names(reduced), "cv_plan")) {
    bad <- reduced
    bad[field] <- list(NULL)
    expect_error(.validate_warm_start_model(bad), info = field)
  }
  changes <- list(
    list(c("tuning", "ncomp_grid"), c(1L, 10L)),
    list(c("tuning", "ncomp_requested"), c(1L, 2L)),
    list(c("training", "hyperparameters", "ncomp"), 11L),
    list(c("validation", "warning_count"), -1L),
    list(c("validation", "metrics", "rmse"), -1),
    list(c("extra"), "unlisted field")
  )
  for (change in changes) {
    bad <- reduced
    bad[[change[[1]]]] <- change[[2]]
    expect_error(.validate_warm_start_model(bad), info = paste(change[[1]], collapse = "$"))
  }
  legacy <- readRDS(test_path("fixtures", "warm-start-legacy", "baseline-1.5.1.rds"))$cases$default$model
  mixed <- ensemble_warm_start_models(legacy = legacy, pls = model, repeated = model)
  expected <- (predict(legacy, f$x)$calibrated_prediction + 2 * predict(model, f$x)$calibrated_prediction) / 3
  expect_equal(predict(mixed, f$x)$ensemble_mean, expected)
  expect_equal(predict(prepare_warm_start_model(mixed, omit_audit = TRUE), f$x)$ensemble_mean, expected)
  expect_identical(names(warm_start_coefficients(mixed)),
    c("feature", "legacy_std_coefficient", "pls_std_coefficient", "repeated_std_coefficient"))
})
