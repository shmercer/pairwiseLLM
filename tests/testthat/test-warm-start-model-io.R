test_that("lossless public storage preserves portable models and missing-feature preprocessing", {
  model <- warm_core_model()
  x <- warm_core_features()
  x$first_order_coherence[1] <- NA_real_
  path <- tempfile(fileext = ".rds")
  withr::defer(unlink(path))
  local_mocked_bindings(.warm_start_require_glmnet = function() stop("must not train"),
    warm_start_python_status = function(...) stop("must not initialize Python"))
  expect_identical(save_warm_start_model(model, path), path)
  expect_identical(load_warm_start_model(path), model)
  expect_identical(predict(load_warm_start_model(path), x), predict(model, x))
  expect_identical(model$features[model$features == "upstream_entropy_per_token"], "upstream_entropy_per_token")
  expect_identical(load_warm_start_model(path)$preprocessing, model$preprocessing)
  expect_error(save_warm_start_model(model, path), "already exists")
  expect_identical(save_warm_start_model(model, path, overwrite = TRUE), path)
  reduced <- prepare_warm_start_model(model, omit_audit = TRUE)
  expect_identical(reduced$calibration, model$calibration)
  expect_null(reduced$validation)
  expect_identical(predict(reduced, x)$raw_prediction, predict(model, x)$raw_prediction)
  expect_true(all(is.na(predict(reduced, x)$calibrated_prediction)))
  expect_identical(summary(reduced)$audit_status, "summary_only")
  expect_output(print(reduced), "summary_only")
  bad <- x[, -2]
  attr(bad, "warm_start_schema") <- attr(x, "warm_start_schema")
  expect_error(predict(reduced, bad), "feature")
  attr(x, "warm_start_schema") <- "future"
  expect_error(predict(reduced, x), "schema")
})

test_that("preparation validates metadata and preserves explicit provenance", {
  model <- warm_core_model()
  fields <- list(name = "example", version = "2026.1", domain = "synthetic test", license = "MIT",
    notes = "Not a validated writing predictor", prepared_at = "2026-09-08T00:00:00Z",
    preparation_package_version = "1.3.1", extraction_provenance = c(status = "user-supplied"))
  prepared <- prepare_warm_start_model(model, fields)
  expect_identical(prepared$metadata, fields)
  expect_identical(prepared$training, model$training)
  expect_identical(prepared$format_version, 1L)
  expect_null(model$metadata)
  expect_identical(prepare_warm_start_model(prepared), prepared)
  revised <- prepare_warm_start_model(prepared, list(notes = "Revised"))
  expect_identical(revised$metadata$notes, "Revised")
  expect_identical(revised$metadata$prepared_at, fields$prepared_at)
  defaults <- prepare_warm_start_model(model)$metadata
  expect_identical(defaults$extraction_provenance, c(status = "unavailable"))
  expect_match(defaults$prepared_at, "Z$")
  for (metadata in list(NULL, 1, list(unknown = "x"), list("x"), list(notes = NA_character_),
                        list(notes = 2), list(notes = ""), list(notes = Sys.Date()),
                        list(extraction_provenance = "x"), list(extraction_provenance = c(status = NA_character_)),
                        list(prepared_at = "yesterday"), structure(list("a", "b"), names = c("notes", "notes")))) {
    expect_error(prepare_warm_start_model(model, metadata))
  }
  for (flag in list(NA, 1, logical(), c(TRUE, FALSE), matrix(TRUE))) {
    expect_error(prepare_warm_start_model(model, omit_audit = flag), "TRUE or FALSE")
  }
  bad <- prepared
  bad$metadata$notes <- new.env()
  expect_error(.validate_warm_start_model(bad))
})

test_that("calibrated storage preserves audits and explicit reduction retains predictions", {
  skip_if_not_installed("glmnet")
  x <- warm_core_features(15)
  model <- fit_warm_start_model(x$item_id, warm_core_theta(x), "synthetic audit", features = x, alpha_grid = c(0, 1))
  path <- tempfile(fileext = ".rds")
  withr::defer(unlink(path))
  save_warm_start_model(model, path)
  expect_identical(load_warm_start_model(path), model)
  expect_identical(predict(load_warm_start_model(path), x), predict(model, x))
  expect_identical(load_warm_start_model(path)$tuning$ids, x$item_id)
  expect_identical(load_warm_start_model(path)$validation, model$validation)
  # Unknown fields/attributes must not carry row-level material across reduction.
  model$training$private_ids <- x$item_id
  attr(model$coefficients, "private_ids") <- x$item_id
  model$private_texts <- "private test-only text"
  reduced <- prepare_warm_start_model(model, omit_audit = TRUE)
  expect_identical(reduced$format_version, 2L)
  expect_identical(reduced$validation$metrics, model$validation$metrics)
  expect_identical(reduced$validation$warning_count, length(model$validation$warnings))
  expect_identical(reduced$tuning$seed, model$tuning$seed)
  expect_null(reduced$training$private_ids)
  expect_null(attr(reduced$coefficients, "private_ids"))
  expect_null(reduced$private_texts)
  expect_null(reduced$tuning$ids)
  expect_null(reduced$tuning$oof)
  expect_null(reduced$validation$folds)
  expect_null(reduced$validation$predictions)
  expect_null(reduced$validation$warnings)
  expect_identical(predict(reduced, x)$raw_prediction, predict(model, x)$raw_prediction)
  expect_identical(predict(reduced, x)$calibrated_prediction, predict(model, x)$calibrated_prediction)
  expect_identical(prepare_warm_start_model(reduced, omit_audit = TRUE), reduced)
  expect_identical(prepare_warm_start_model(reduced), reduced)
  save_warm_start_model(reduced, path, overwrite = TRUE)
  expect_identical(load_warm_start_model(path), reduced)
  expect_identical(predict(load_warm_start_model(path), x), predict(reduced, x))
  expect_output(print(reduced), "Nested validation")
  for (field in c("audit_status", "tuning", "validation")) {
    bad <- reduced
    bad[field] <- list(NULL)
    expect_error(.validate_warm_start_model(bad), info = field)
  }
  for (field in names(reduced$tuning)) {
    bad <- reduced
    bad$tuning[field] <- list(NULL)
    expect_error(.validate_warm_start_model(bad), info = field)
  }
  for (field in names(reduced$validation)) {
    bad <- reduced
    bad$validation[field] <- list(NULL)
    expect_error(.validate_warm_start_model(bad), info = field)
  }
  for (field in names(reduced$validation$metrics)) {
    bad <- reduced
    bad$validation$metrics[field] <- list(NULL)
    expect_error(.validate_warm_start_model(bad), info = field)
  }
  changes <- list(function(m) {
      m$tuning$seed <- -1
      m
    },
    function(m) {
      m$tuning$alpha_grid <- c(1, 0)
      m
    },
    function(m) {
      m$tuning$lambda_rule <- "invalid"
      m
    },
    function(m) {
      m$validation$outer_folds <- 1
      m
    },
    function(m) {
      m$validation$warning_count <- -1
      m
    },
    function(m) {
      m$validation$metrics$rmse <- -1
      m
    },
    function(m) {
      m$validation$metrics$mae <- 100
      m
    },
    function(m) {
      m$validation$metrics$pearson_r <- 2
      m
    },
    function(m) {
      m$validation$metrics$squared_pearson_r <- 0.123
      m
    },
    function(m) {
      m$validation$metrics$undefined_reasons <- NA_character_
      m
    },
    function(m) {
      m$validation$metrics$pearson_r <- NA_real_
      m
    },
    function(m) {
      m$tuning$oof <- model$tuning$oof
      m
    })
  for (change in changes) expect_error(.validate_warm_start_model(change(reduced)))
  undefined <- reduced
  undefined$validation$metrics$pearson_r <- NA_real_
  undefined$validation$metrics$squared_pearson_r <- NA_real_
  undefined$validation$metrics$undefined_reasons <- "Correlations undefined."
  expect_invisible(.validate_warm_start_model(undefined))
  partial <- model
  partial$tuning <- NULL
  expect_error(.validate_warm_start_model(partial))
  partial <- model
  partial$audit_status <- "summary_only"
  expect_error(.validate_warm_start_model(partial))
})

test_that("I/O rejects bad paths and artifacts and preserves destinations on failure", {
  root <- withr::local_tempdir()
  path <- file.path(root, "model.rds")
  model <- warm_core_model()
  for (bad in list(NULL, NA_character_, "", c("a", "b"), 1)) {
    expect_error(save_warm_start_model(model, bad), "path")
  }
  expect_error(save_warm_start_model(model, file.path(root, "absent", "model.rds")), "parent")
  expect_error(save_warm_start_model(model, root), "directory")
  expect_error(save_warm_start_model(model, path, overwrite = NA), "TRUE or FALSE")
  expect_error(load_warm_start_model(), "exactly one")
  expect_error(load_warm_start_model(path, name = "a"), "exactly one")
  expect_error(load_warm_start_model(path, source = "user"), "only to model names")
  expect_error(load_warm_start_model(path), "does not exist")
  expect_error(load_warm_start_model(root), "does not exist")
  writeLines("not an RDS", path)
  expect_error(load_warm_start_model(path), "Cannot load")
  for (version in list(0L, 99L, "1")) {
    bad <- model
    bad$format_version <- version
    saveRDS(bad, path)
    expect_error(load_warm_start_model(path), "Unsupported.*format version")
  }
  bad <- model
  bad$schema <- "future"
  saveRDS(bad, path)
  expect_error(load_warm_start_model(path), "schema")
  bad <- model
  bad$coefficients <- rev(bad$coefficients)
  expect_error(save_warm_start_model(bad, path, TRUE), "contract")
  save_warm_start_model(model, path, TRUE)
  local_mocked_bindings(.warm_start_rename = function(...) FALSE)
  expect_error(save_warm_start_model(model, path, TRUE), "Cannot publish")
  expect_identical(load_warm_start_model(path), model)
  expect_identical(list.files(root, all.files = TRUE, no.. = TRUE), "model.rds")
  local_mocked_bindings(.warm_start_link = function(...) FALSE)
  expect_error(save_warm_start_model(model, file.path(root, "new.rds")), "Cannot publish")
  expect_identical(list.files(root, all.files = TRUE, no.. = TRUE), "model.rds")
  local_mocked_bindings(.warm_start_read_model = function(...) list())
  expect_error(save_warm_start_model(model, file.path(root, "new.rds")), "round-trip")
  expect_identical(list.files(root, all.files = TRUE, no.. = TRUE), "model.rds")
})

test_that("the constructor accepts the explicit reduced contract without changing core defaults", {
  model <- warm_core_model()
  reduced <- prepare_warm_start_model(model, omit_audit = TRUE)
  rebuilt <- .new_warm_start_model(reduced$schema, reduced$preprocessing, reduced$coefficients,
    reduced$intercept, reduced$outcome, reduced$training, reduced$calibration,
    format_version = 2L, audit_status = "summary_only", metadata = reduced$metadata)
  expect_identical(rebuilt, reduced)
  invalid <- reduced
  invalid$tuning <- list(seed = 1L)
  expect_error(.validate_warm_start_model(invalid), "Uncalibrated")
})
