# Historical artifacts are committed evidence, not rebuilt from the implementation under test.
warm_legacy_fixture <- function() {
  readRDS(testthat::test_path("fixtures", "warm-start-legacy", "baseline-1.5.1.rds"))
}

test_that("frozen v1 schema and historical full/reduced deployment remain compatible", {
  f <- warm_legacy_fixture()
  expect_identical(as.data.frame(warm_start_feature_schema()), f$schema)
  schema_path <- system.file("warm-start", "feature-schema-writing-v1.csv", package = "pairwiseLLM")
  expect_identical(unname(tools::md5sum(schema_path)), "4a72459e78090168c6755d433397c3b6")
  local_mocked_bindings(.warm_start_require_glmnet = function() stop("unexpected engine"),
    .warm_start_python_request = function(...) stop("unexpected Python"),
    .package = "pairwiseLLM")
  root <- withr::local_tempdir()
  for (name in names(f$cases)) {
    case <- f$cases[[name]]
    expect_identical(case$model$format_version, 1L)
    expect_identical(case$reduced$format_version, 2L)
    for (artifact in list(case$model, case$reduced)) {
      path <- file.path(root, paste0(name, "-", artifact$format_version, ".rds"))
      save_warm_start_model(artifact, path)
      restored <- load_warm_start_model(path)
      expect_identical(restored, artifact)
      actual <- predict(restored, f$newdata)
      expect_identical(actual$item_id, case$prediction$item_id)
      expect_equal(actual$raw_prediction, case$prediction$raw_prediction, tolerance = 1e-12)
      expect_equal(actual$calibrated_prediction, case$prediction$calibrated_prediction, tolerance = 1e-12)
      expect_equal(summary(restored)$validation, case$model$validation$metrics, tolerance = 1e-12)
    }
    reduced_again <- prepare_warm_start_model(case$reduced, omit_audit = TRUE)
    expect_identical(reduced_again, case$reduced)
  }
})

test_that("historical cross-task arithmetic and repeated components remain compatible", {
  f <- warm_legacy_fixture()
  local_mocked_bindings(.warm_start_require_glmnet = function() stop("unexpected engine"),
    .warm_start_python_request = function(...) stop("unexpected Python"),
    .package = "pairwiseLLM")
  root <- withr::local_tempdir()
  for (i in 1:2) {
    artifact <- list(f$ensemble, f$reduced_ensemble)[[i]]
    path <- file.path(root, paste0("ensemble-", i, ".rds"))
    save_warm_start_model(artifact, path)
    restored <- load_warm_start_model(path)
    expect_identical(restored, artifact)
    actual <- predict(restored, f$newdata)
    columns <- names(f$ensemble_prediction)
    for (column in columns) {
      expect_equal(actual[[column]], f$ensemble_prediction[[column]], tolerance = 1e-12)
    }
    expect_identical(names(restored$components), c("default", "other_task", "repeated"))
  }
  rebuilt <- ensemble_warm_start_models(default = f$cases$default$model,
    other_task = f$cases$tied_missing$model, repeated = f$cases$default$model)
  expect_identical(rebuilt, f$ensemble)
})

test_that("glmnet refits preserve frozen folds, tuning, calibration and validation", {
  skip_if_not_installed("glmnet")
  f <- warm_legacy_fixture()
  skip_if(as.character(utils::packageVersion("glmnet")) != f$engine_version,
    "Historical numerical refit requires glmnet 5.0; legacy deployment checks still run.")
  withr::local_seed(259L, .rng_kind = f$rng_kind[1],
    .rng_normal_kind = f$rng_kind[2], .rng_sample_kind = f$rng_kind[3])
  before <- .Random.seed
  for (case in f$cases) {
    current <- do.call(fit_warm_start_model, case$input)
    previous_ceiling <- with_mocked_bindings(
      do.call(fit_warm_start_model, case$input),
      .warm_start_glmnet_controls = function(engine = glmnet::glmnet) {
        controls <- list(thresh = 1e-12, maxit = 100000L)
        if ("control" %in% names(formals(engine))) return(list(control = controls))
        controls
      }, .package = "pairwiseLLM"
    )
    # Compare under the same engine/BLAS, with no tolerance or fixture recapture.
    expect_identical(current, previous_ceiling)
    expect_identical(predict(current, f$newdata), predict(previous_ceiling, f$newdata))
    old <- case$model
    expect_identical(.Random.seed, before)
    expect_identical(current$features, old$features)
    expect_identical(current$validation$predictions$fold, old$validation$predictions$fold)
    expect_identical(current$tuning$foldid, old$tuning$foldid)
    expect_identical(current$tuning$selected$alpha, old$tuning$selected$alpha)
    for (field in c("preprocessing", "coefficients", "intercept", "outcome", "calibration")) {
      expect_equal(current[[field]], old[[field]], tolerance = 1e-8, info = field)
    }
    # New audit fields may be added; every historical tuning field must retain its meaning.
    expect_equal(current$tuning[names(old$tuning)], old$tuning, tolerance = 1e-8)
    expect_equal(current$validation$predictions, old$validation$predictions, tolerance = 1e-8)
    expect_equal(current$validation$metrics, old$validation$metrics, tolerance = 1e-8)
    for (i in seq_along(old$validation$folds)) {
      actual <- current$validation$folds[[i]]
      expected <- old$validation$folds[[i]]
      expect_identical(actual$tuning$foldid, expected$tuning$foldid)
      expect_identical(actual$tuning$selected$alpha, expected$tuning$selected$alpha)
      expect_equal(actual[names(expected)], expected, tolerance = 1e-8)
    }
    predicted <- predict(current, f$newdata)
    expect_equal(predicted$raw_prediction, case$prediction$raw_prediction, tolerance = 1e-8)
    expect_equal(predicted$calibrated_prediction, case$prediction$calibrated_prediction, tolerance = 1e-8)
  }
})
