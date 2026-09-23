test_that("both rubric methods consume one- and multi-spoke common results across BTL variants", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  withr::local_seed(9200)
  before_rng <- .Random.seed
  for (variant in c("btl", "btl_e", "btl_b", "btl_e_b")) {
    for (n_sets in 2:3) {
      data <- rubric_linked_fixture(n_sets, variant)
      original <- serialize(data$state, NULL)
      for (method in c("ordinal_linear", "ordinal_monotone")) {
        fit <- rubric_linked_fit(data, method)
        pred <- stats::predict(fit, data$state)
        expect_identical(fit$cj$model_variant, variant)
        expect_identical(fit$reference, fit$cj$reference)
        expect_identical(fit$transformation,
          list(center = mean(data$reference$items$theta_raw_mean),
            scale = stats::sd(data$reference$items$theta_raw_mean)))
        target <- summarize_items(data$state)
        target <- target[target$set_id != "1", ]
        expect_equal(pred$theta, target$theta_link_mean + mean(data$reference$items$theta_raw_mean))
        expect_identical(pred$theta_sd, target$theta_link_sd)
        expect_identical(pred$item_id, target$global_item_id)
        expect_identical(pred$set_id, target$set_id)
        expect_identical(pred$source_item_id, target$item_id)
        expect_false(any(pred$item_id %in% data$rubric$item_id))
        z <- (pred$theta - fit$transformation$center) / fit$transformation$scale
        direct <- if (method == "ordinal_linear") {
          stats::predict(fit$backend$model, data.frame(z = z), type = "prob")$fit
        } else mgcv::predict.gam(fit$backend$model, data.frame(z = z), type = "response")
        p <- do.call(rbind, pred$probabilities)
        expect_equal(unname(p), unname(direct), tolerance = 1e-12)
        expect_equal(rowSums(p), rep(1, nrow(p)), tolerance = 1e-12)
        expect_true(all(is.finite(p) & p >= 0 & p <= 1))
        expect_identical(colnames(p), data$levels)
        expect_identical(pred$category, pred$median_category)
        expect_identical(pred$rubric_score, data$levels[pred$category])
        expect_equal(pred$expected_level, as.vector(p %*% 1:3))
        mode <- stats::predict(fit, data$state, hard_score = "mode")
        expect_identical(mode$category, mode$modal_category)
        expect_identical(mode$probabilities, pred$probabilities)
        expect_identical(attr(pred, "linking")$provenance$estimator_id, "fixed_shape_offset")
        expect_identical(serialize(data$state, NULL), original)
        expect_identical(stats::predict(fit)$item_id, data$rubric$item_id)
      }
    }
  }
  expect_identical(.Random.seed, before_rng)
})

test_that("linked fitting uses only labeled hub rows and preserves the frozen transformation", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  data <- rubric_linked_fixture()
  keep <- data$reference$items$theta_raw_mean <= 1.5
  data$rubric <- data$rubric[keep, ]
  shifted <- rubric_linked_fixture(target_shift = 2)
  for (method in c("ordinal_linear", "ordinal_monotone")) {
    fit <- rubric_linked_fit(data, method)
    expect_identical(fit$calibration_range, c(-2, 1.5))
    immutable <- c("reference", "transformation", "calibration_range", "calibration_data", "category_counts", "cj")
    original <- serialize(fit[immutable], NULL)
    for (state in list(data$state, shifted$state)) {
      pred <- stats::predict(fit, state)
      z <- (pred$theta - fit$transformation$center) / fit$transformation$scale
      direct <- if (method == "ordinal_linear") {
        stats::predict(fit$backend$model, data.frame(z = z), type = "prob")$fit
      } else mgcv::predict.gam(fit$backend$model, data.frame(z = z), type = "response")
      expect_equal(unname(do.call(rbind, pred$probabilities)), unname(direct), tolerance = 1e-12)
      expect_identical(pred$extrapolated, pred$theta < -2 | pred$theta > 1.5)
    }
    expect_identical(serialize(fit[immutable], NULL), original)
    expect_identical(stats::predict(fit)$extrapolated, !keep)
  }
})

test_that("linked predictions reject independent, changed and invalid common scales", {
  skip_if_not_installed("ordinal")
  data <- rubric_linked_fixture()
  fit <- rubric_linked_fit(data)
  expect_error(stats::predict(fit, data$reference), "inappropriate")
  expect_error(stats::predict(fit, rubric_test_fixed()), "inappropriate")
  expect_error(stats::predict(fit, data.frame(theta = 1)), "completed CJ")
  other <- rubric_linked_fixture(reference_shift = .25)
  expect_error(stats::predict(fit, other$state), "stored rubric reference hub")
  other <- rubric_linked_fixture(variant = "btl_b")
  expect_error(stats::predict(fit, other$state), "stored rubric reference hub")
  bad <- data$state
  bad$linking$estimator$accepted_state_by_spoke[["2"]]$items$theta_link_eap[1] <- 99
  expect_error(stats::predict(fit, bad), "identity hash mismatch")
  input <- data$state$linking$estimator$accepted_state_by_spoke[["2"]]$continuation$input
  frozen <- resume_link_session(data$state, input, "frozen")
  expect_identical(stats::predict(fit, frozen), stats::predict(fit, data$state))
  expect_error(fit_rubric_calibration(data$reference, method = "percentile",
    calibration_design = "linked_anchors", K = 3), "does not support")
})

test_that("reordered labels are compatible, while changed artifact identity requires a new link", {
  skip_if_not_installed("ordinal")
  data <- rubric_linked_fixture()
  fit <- rubric_linked_fit(data)
  expected <- stats::predict(fit, data$state)
  data$rubric <- data$rubric[54:1, ]
  expect_equal(stats::predict(rubric_linked_fit(data), data$state), expected, tolerance = 1e-12)
  data$reference$fit_config_hash <- "legacy-config-hash"
  legacy_fit <- rubric_linked_fit(data)
  expect_error(stats::predict(legacy_fit, data$state), "stored rubric reference hub")
})

test_that("stored linked reference metadata is validated before scoring", {
  withr::local_seed(9205)
  data <- rubric_linked_fixture()
  prepare <- pairwiseLLM:::.rubric_prepare_calibration
  object <- prepare(data$reference, data$rubric, calibration_design = "linked_anchors", levels = data$levels)
  validate <- pairwiseLLM:::.rubric_validate_calibration
  for (value in list(NULL, list(), 1)) {
    bad <- object
    bad["reference"] <- list(value)
    expect_error(validate(bad), "reference identity")
  }
  for (value in list(NA_real_, Inf, 1e100, 1.5, "1", c(1L, 2L), matrix(1L))) {
    bad <- object
    bad$reference$set_id <- value
    expect_error(validate(bad), "reference identity")
  }
  for (field in c("items", "fit_contract", "fit_contract_hash", "evidence", "evidence_hash")) {
    bad <- object
    bad$reference[[field]] <- NULL
    expect_error(validate(bad), "reference identity")
  }
  bad <- object
  bad$reference$items$item_id[[2L]] <- bad$reference$items$item_id[[1L]]
  expect_error(validate(bad), "unique")
  bad <- object
  bad$reference$evidence_hash <- "wrong"
  expect_error(validate(bad), "evidence hash")
  for (field in c("theta", "theta_sd")) {
    bad <- object
    bad$reference$items[[field]][[1L]] <- bad$reference$items[[field]][[1L]] + 1e-10
    bad$cj$reference <- bad$reference
    expect_error(validate(bad), "calibration CJ metric")
  }
  bad <- object
  bad$reference$set_id <- 2L
  bad$cj$reference <- bad$reference
  expect_error(validate(bad), "calibration CJ metric")
  bad <- object
  bad$cj$items$set_id <- NULL
  expect_error(validate(bad), "calibration CJ metric")
  bad <- object
  bad$cj$items$set_id[[1L]] <- NA_integer_
  expect_error(validate(bad), "calibration CJ metric")
  bad <- object
  bad$reference$fit_contract$model_variant <- "btl_b"
  bad$cj$reference <- bad$reference
  expect_error(validate(bad), "calibration CJ metric")
  bad <- object
  bad$cj$fit_contract_hash <- "different"
  expect_error(validate(bad), "calibration CJ metric")
  bad <- object
  bad$cj$scale_status <- "within_set"
  expect_error(validate(bad), "linked reference scale")
  bad <- object
  bad$cj$estimation_mode <- "fixed"
  expect_error(validate(bad), "linked reference estimation mode")
})

test_that("linked calibrations serialize and preserve optional backend dependency behavior", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  skip_if_not_installed("withr")
  withr::local_seed(9207)
  data <- rubric_linked_fixture()
  directory <- withr::local_tempdir()
  for (method in c("ordinal_linear", "ordinal_monotone")) {
    fit <- rubric_linked_fit(data, method)
    path <- file.path(directory, paste0(method, ".rds"))
    saveRDS(fit, path)
    expect_identical(stats::predict(readRDS(path), data$state), stats::predict(fit, data$state))
  }
  linear <- rubric_linked_fit(data)
  expected <- stats::predict(linear, data$state)
  testthat::local_mocked_bindings(.rubric_ordinal_available = function() FALSE, .package = "pairwiseLLM")
  expect_identical(stats::predict(linear, data$state), expected)
  expect_error(rubric_linked_fit(data), class = "pairwiseLLM_rubric_dependency_missing")
  testthat::local_mocked_bindings(.rubric_monotone_dependencies = function(...) {
    rlang::abort("Install mgcv", class = "pairwiseLLM_rubric_dependency_missing")
  }, .package = "pairwiseLLM")
  expect_error(stats::predict(fit, data$state), class = "pairwiseLLM_rubric_dependency_missing")
  expect_error(rubric_linked_fit(data, "ordinal_monotone"), class = "pairwiseLLM_rubric_dependency_missing")
})
