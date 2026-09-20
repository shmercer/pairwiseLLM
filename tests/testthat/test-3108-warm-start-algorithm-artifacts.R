test_that("full and reduced algorithm artifacts retain predictions and portable matrix payloads", {
  root <- withr::local_tempdir()
  for (schema in c("writing_features_v1", "writing_features_v2")) {
    f <- warm_algorithm_fixture(schema)
    full <- f$ensemble
    full$components$svr_rbf$private_text <- "remove"
    attr(full$components$svr_rbf$engine_payload$support_vectors, "private") <- "remove"
    prepared <- prepare_warm_start_model(full, warm_bundle_metadata("same-task"))
    reduced <- prepare_warm_start_model(prepared, omit_audit = TRUE)
    expect_identical(reduced$format_version, 1L)
    expect_identical(reduced$audit_status, "summary_only")
    expect_identical(reduced$cv_identity, full$cv_identity)
    expect_identical(reduced$validation$metrics, full$validation$metrics)
    expect_null(reduced$validation$predictions)
    expect_identical(summary(reduced)$audit_status, "summary_only")
    expect_identical(prepare_warm_start_model(reduced, omit_audit = TRUE), reduced)
    for (name in names(reduced$components)) {
      component <- reduced$components[[name]]
      expect_identical(component$audit_status, "summary_only")
      expect_null(component$cv_plan)
      expect_null(component$tuning$ids)
      expect_null(component$tuning$traces)
      expect_null(component$validation$predictions)
      expect_null(component$validation$folds)
    }
    payload <- reduced$components$svr_rbf$engine_payload
    expect_identical(payload, f$components$svr_rbf$engine_payload)
    expect_identical(colnames(payload$support_vectors), reduced$components$svr_rbf$preprocessing$retained)
    expect_null(reduced$components$svr_rbf$private_text)
    expect_null(attr(payload$support_vectors, "private"))
    with_mocked_bindings({
      for (model in list(prepared, reduced)) {
        path <- file.path(root, "artifact.rds")
        save_warm_start_model(model, path, overwrite = TRUE)
        restored <- load_warm_start_model(path)
        expect_identical(restored, model)
        expect_identical(predict(restored, f$x), predict(prepared, f$x))
        expect_error(warm_start_coefficients(restored), class = "pairwiseLLM_warm_nonlinear_coefficients")
      }
    }, .warm_start_require_glmnet = function() stop("backend"),
      .warm_start_require_pls = function() stop("backend"), .warm_start_require_svr = function() stop("backend"),
      .warm_start_python_request = function(...) stop("Python"), .package = "pairwiseLLM")
  }
})

test_that("reduced algorithm evidence is explicit, strict and cannot be promoted", {
  f <- warm_algorithm_fixture(engines = c("glmnet", "pls"))
  reduced <- prepare_warm_start_model(f$ensemble, omit_audit = TRUE)
  for (field in names(reduced)) {
    if (field == "metadata") next
    bad <- reduced
    bad[field] <- list(NULL)
    expect_error(.validate_warm_start_algorithm_ensemble(bad), info = field)
  }
  for (field in c("rmse", "mae", "pearson_r", "undefined_reasons")) {
    bad <- reduced
    bad$validation$metrics[field] <- list(NULL)
    expect_error(.validate_warm_start_algorithm_ensemble(bad), info = field)
  }
  bad <- reduced
  bad$audit_status <- "full"
  expect_error(.validate_warm_start_algorithm_ensemble(bad))
  bad <- reduced
  bad$components$glmnet <- f$components$glmnet
  expect_error(.validate_warm_start_algorithm_ensemble(bad))
  bad <- reduced
  bad$validation$predictions <- f$ensemble$validation$predictions
  expect_error(.validate_warm_start_algorithm_ensemble(bad), "contract")
  bad <- reduced
  attr(bad$validation, "private") <- "remove"
  expect_error(.validate_warm_start_algorithm_ensemble(bad), "contract")
  expect_error(do.call(ensemble_warm_start_algorithms, reduced$components), "failed")
  expect_identical(warm_start_coefficients(reduced), warm_start_coefficients(f$ensemble))
})

test_that("registry and verified bundles distinguish honest same-task ensemble validation", {
  f <- warm_algorithm_fixture()
  root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = root)
  empty <- list_warm_start_models("user")
  expect_identical(empty$component_engine_versions, list())
  register_warm_start_model(f$ensemble, "algorithms")
  cross <- do.call(ensemble_warm_start_models, f$components)
  register_warm_start_model(cross, "cross-task")
  rows <- list_warm_start_models("user")
  expect_identical(rows$artifact_type, c("algorithm_ensemble", "ensemble"))
  expect_identical(rows$n, c(nrow(f$x), NA_integer_))
  expect_identical(rows$validation[[1]], f$ensemble$validation$metrics)
  expect_identical(rows$validation[[2]], lapply(f$components, function(m) m$validation$metrics))
  expect_identical(rows$component_engines[[1]], c(glmnet = "glmnet", pls = "pls", svr_rbf = "svr_rbf"))
  expect_identical(rows$component_engine_versions[[1]],
    vapply(f$components, function(m) m$training$engine_version, character(1)))
  expect_identical(load_warm_start_model(name = "algorithms", source = "user"), f$ensemble)
  prepared <- f$ensemble
  for (name in names(prepared$components)) {
    prepared$components[[name]] <- prepare_warm_start_model(prepared$components[[name]], warm_bundle_metadata(name))
  }
  prepared <- prepare_warm_start_model(prepared, warm_bundle_metadata("algorithms"), omit_audit = TRUE)
  bundle <- file.path(root, "bundle")
  dir.create(bundle)
  record <- warm_bundle_write(bundle, prepared)$artifacts[[1]]
  expect_identical(record$artifact_type, "algorithm_ensemble")
  expect_identical(record$cv_identity, prepared$cv_identity)
  expect_identical(record$audit_status, "summary_only")
  expect_identical(record$validation, prepared$validation)
  expect_identical(vapply(record$components, `[[`, character(1), "component_name"), names(f$components))
  expect_identical(.warm_start_bundle_model(bundle, "algorithms"), prepared)
  record$validation$metrics$rmse <- record$validation$metrics$rmse + 1
  manifest <- list(manifest_version = 1L, artifacts = list(record))
  writeLines(.warm_start_bundle_json(manifest), file.path(bundle, "manifest.json"))
  expect_error(.warm_start_bundle_model(bundle, "algorithms"), "metadata does not match")
})


test_that("algorithm reduction preserves a single retained SVR feature and one-row predictions", {
  f <- warm_algorithm_fixture(singleton = TRUE)
  reduced <- prepare_warm_start_model(f$ensemble, omit_audit = TRUE)
  payload <- reduced$components$svr_rbf$engine_payload
  expect_identical(ncol(payload$support_vectors), 1L)
  expect_identical(colnames(payload$support_vectors), "n_tokens")
  expect_identical(payload, f$components$svr_rbf$engine_payload)
  expect_equal(predict(reduced, f$x[1, , drop = FALSE])$ensemble_mean,
    predict(f$ensemble, f$x)$ensemble_mean[1])
  expect_equal(predict(reduced, f$x)$ensemble_mean, predict(f$ensemble, f$x)$ensemble_mean)
})
