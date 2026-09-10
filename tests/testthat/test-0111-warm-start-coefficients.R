coefficient_model <- function(slope = 1, removed = character(), zero = character(),
                              name = "coefficient-fixture") {
  model <- warm_bundle_model(name)
  model$calibration$slope <- slope
  retained <- model$features[!model$features %in% removed]
  removed <- setdiff(model$features, retained)
  model$preprocessing$retained <- retained
  model$preprocessing$removed <- stats::setNames(rep("constant", length(removed)), removed)
  for (field in c("medians", "centers", "scales")) {
    model$preprocessing[[field]] <- model$preprocessing[[field]][retained]
  }
  model$coefficients <- model$coefficients[retained]
  model$coefficients[intersect(zero, retained)] <- 0
  model$training$n_nonzero <- sum(model$coefficients != 0)
  pairwiseLLM:::.validate_warm_start_model(model)
  model
}

test_that("individual coefficients preserve schema order and distinguish removal from zero", {
  fields <- warm_start_feature_schema()$feature
  removed <- fields[1]
  zero <- fields[2]
  model <- coefficient_model(slope = 2.5, removed = removed, zero = zero)
  original <- model
  withr::local_seed(111)
  rng <- .Random.seed
  coefficients <- warm_start_coefficients(model)

  expect_s3_class(coefficients, "tbl_df")
  expect_identical(names(coefficients),
    c("feature", "retained", "calibrated_std_coefficient"))
  expect_identical(coefficients$feature, fields)
  expect_type(coefficients$feature, "character")
  expect_type(coefficients$retained, "logical")
  expect_type(coefficients$calibrated_std_coefficient, "double")
  expect_false(coefficients$retained[coefficients$feature == removed])
  expect_identical(coefficients$calibrated_std_coefficient[coefficients$feature == removed], NA_real_)
  expect_true(coefficients$retained[coefficients$feature == zero])
  expect_identical(coefficients$calibrated_std_coefficient[coefficients$feature == zero], 0)

  retained <- coefficients$retained
  expect_equal(coefficients$calibrated_std_coefficient[retained],
    unname(model$coefficients[fields[retained]] * model$calibration$slope))
  expect_identical(model, original)
  expect_identical(.Random.seed, rng)
})

test_that("calibration slope controls coefficient sign and zero exactly", {
  positive <- coefficient_model(slope = 1.75)
  negative <- coefficient_model(slope = -1.75)
  flat <- coefficient_model(slope = 0)

  positive_values <- warm_start_coefficients(positive)$calibrated_std_coefficient
  expect_identical(warm_start_coefficients(negative)$calibrated_std_coefficient, -positive_values)
  expect_identical(warm_start_coefficients(flat)$calibrated_std_coefficient,
    rep(0, length(flat$features)))
})

test_that("reported coefficient matches a one-training-SD prediction change", {
  model <- coefficient_model(slope = -1.25)
  feature <- "first_order_coherence"
  x <- warm_core_features()[rep(1L, 2L), ]
  x$item_id <- c("baseline", "one-sd-higher")
  x[[feature]][2] <- x[[feature]][1] + model$preprocessing$scales[feature]
  prediction <- predict(model, x)
  reported <- warm_start_coefficients(model)
  expected <- reported$calibrated_std_coefficient[reported$feature == feature]

  expect_equal(diff(prediction$calibrated_prediction), expected, tolerance = 1e-12)
})

test_that("uncalibrated and malformed models fail through public boundaries", {
  expect_error(warm_start_coefficients(warm_core_model()),
    "fitted with learned OOF calibration.*fit_warm_start_model")
  model <- coefficient_model()
  expect_error(warm_start_coefficients(model, unused = TRUE), "must be empty")
  bad <- model
  bad$coefficients <- rev(bad$coefficients)
  expect_error(warm_start_coefficients(bad), "model contract")
  expect_error(warm_start_coefficients(1), "no applicable method")
})

test_that("ensemble coefficients preserve component order and component-specific semantics", {
  fields <- warm_start_feature_schema()$feature
  first <- coefficient_model(removed = fields[1], zero = fields[2], name = "first")
  second <- coefficient_model(slope = -2, removed = fields[2], zero = fields[1], name = "second")
  third <- coefficient_model(slope = 0.5, name = "third")
  two <- ensemble_warm_start_models("assessment a" = first, assessment_b = second)
  original <- two
  coefficients <- warm_start_coefficients(two)

  expect_identical(names(coefficients), c(
    "feature", "assessment a_std_coefficient", "assessment_b_std_coefficient"
  ))
  expect_identical(coefficients$feature, fields)
  expect_identical(coefficients[[2]],
    warm_start_coefficients(first)$calibrated_std_coefficient)
  expect_identical(coefficients[[3]],
    warm_start_coefficients(second)$calibrated_std_coefficient)
  expect_true(is.na(coefficients[[2]][1]))
  expect_identical(coefficients[[3]][1], 0)
  expect_identical(coefficients[[2]][2], 0)
  expect_true(is.na(coefficients[[3]][2]))
  expect_identical(two, original)

  three <- ensemble_warm_start_models(first = first, second = second, third = third)
  expect_identical(names(warm_start_coefficients(three)),
    c("feature", "first_std_coefficient", "second_std_coefficient", "third_std_coefficient"))
  expect_identical(warm_start_coefficients(three)[[4]],
    warm_start_coefficients(third)$calibrated_std_coefficient)
  expect_error(warm_start_coefficients(two, unused = TRUE), "must be empty")
  bad <- two
  bad$features <- rev(bad$features)
  expect_error(warm_start_coefficients(bad), "ensemble contract")
})

test_that("full, reduced, and mixed artifacts return deployment-equivalent coefficients", {
  skip_if_not_installed("glmnet")
  x <- warm_core_features(15)
  full <- fit_warm_start_model(x$item_id, warm_core_theta(x), "coefficient-format",
    features = x, alpha_grid = c(0, 1))
  reduced <- prepare_warm_start_model(full, omit_audit = TRUE)

  expect_identical(warm_start_coefficients(full), warm_start_coefficients(reduced))
  mixed <- ensemble_warm_start_models(full = full, reduced = reduced)
  mixed_coefficients <- warm_start_coefficients(mixed)
  expect_identical(mixed_coefficients$full_std_coefficient,
    mixed_coefficients$reduced_std_coefficient)
  all_reduced <- prepare_warm_start_model(mixed, omit_audit = TRUE)
  expect_identical(warm_start_coefficients(mixed), warm_start_coefficients(all_reduced))
})

test_that("saved, registered, and bundled artifacts retain coefficient output", {
  root <- withr::local_tempdir()
  registry <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = registry)
  model <- coefficient_model(slope = -0.75)
  expected <- warm_start_coefficients(model)
  path <- file.path(root, "coefficient-model.rds")

  save_warm_start_model(model, path)
  expect_identical(warm_start_coefficients(load_warm_start_model(path)), expected)
  register_warm_start_model(model, "coefficient-model")
  expect_identical(warm_start_coefficients(
    load_warm_start_model(name = "coefficient-model", source = "user")), expected)

  bundle <- withr::local_tempdir()
  bundled <- coefficient_model(slope = 1.25, name = "bundled-coefficients")
  warm_bundle_write(bundle, bundled)
  user_root <- pairwiseLLM:::.warm_start_registry_root("user")
  testthat::local_mocked_bindings(.warm_start_registry_root = function(source) {
    if (source == "user") user_root else bundle
  }, .package = "pairwiseLLM")
  expect_identical(warm_start_coefficients(
    load_warm_start_model(name = "bundled-coefficients", source = "bundled")),
    warm_start_coefficients(bundled))
})

test_that("coefficient inspection does not initialize optional development dependencies", {
  testthat::local_mocked_bindings(
    .warm_start_require_glmnet = function() stop("glmnet called"),
    .warm_start_python_request = function(...) stop("Python called"),
    warm_start_python_status = function(...) stop("Python status called"),
    .package = "pairwiseLLM"
  )
  model <- coefficient_model()
  ensemble <- ensemble_warm_start_models(first = model, second = model)
  expect_s3_class(warm_start_coefficients(model), "tbl_df")
  expect_s3_class(warm_start_coefficients(ensemble), "tbl_df")
})
