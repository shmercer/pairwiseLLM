test_that("monotone backend K = 3-6 verifies probabilities, direction, and metadata", {
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  skip_if_not_installed("withr")
  withr::local_seed(2040)
  before <- .Random.seed
  for (K in 3:6) {
    data <- rubric_monotone_data(K)
    fit <- rubric_monotone_fit(data)
    pred <- stats::predict(fit)
    p <- do.call(rbind, pred$probabilities)
    z <- (pred$theta - fit$transformation$center) / fit$transformation$scale
    direct <- mgcv::predict.gam(fit$backend$model, data.frame(z = z), type = "response")
    expect_equal(unname(p), unname(direct), tolerance = 1e-12)
    expect_equal(rowSums(p), rep(1, nrow(p)), tolerance = 1e-12)
    expect_true(all(is.finite(p) & p >= 0 & p <= 1))
    expect_identical(colnames(p), as.character(seq_len(K)))
    expect_equal(fit$transformation,
      list(center = mean(pred$theta), scale = stats::sd(pred$theta)))
    expect_identical(fit$backend$thresholds[[1L]], -1)
    expect_true(all(diff(fit$backend$thresholds) > 0))
    expect_identical(fit$backend$thresholds, fit$backend$model$family$getTheta(TRUE))
    expect_identical(fit$backend$intercept, unname(fit$backend$model$coefficients[[1L]]))
    expect_identical(fit$backend$version, as.character(utils::packageVersion("mgcv")))
    expect_identical(fit$backend$basis$constraint, "m+")
    expect_identical(fit$backend$basis$k, 6L)
    expect_equal(fit$backend$smoothing$sp, fit$backend$model$sp)
    expect_equal(fit$backend$edf, sum(fit$backend$model$edf))
    expect_true(fit$backend$smooth_edf > 1 && fit$backend$smooth_edf < 6)
    expect_identical(fit$backend$smoothing$penalties, fit$backend$model$smooth[[1L]]$S)
    expect_true(fit$diagnostics$ordinal$converged)
    expect_true(fit$diagnostics$ordinal$conditional_on_cj)
    expect_true(fit$diagnostics$category_probabilities_available)
    expect_false(fit$diagnostics$ordinal$threshold_uncertainty_available)
    expect_identical(fit$backend$threshold_standard_errors, c(0, rep(NA_real_, K - 2L)))
    expect_length(fit$warnings, 0)
    grid <- seq(min(pred$theta), max(pred$theta), length.out = 1001)
    grid_p <- pairwiseLLM:::.rubric_monotone_probabilities(grid, fit)
    cumulative <- t(apply(grid_p, 1L, cumsum))
    expect_lte(max(apply(cumulative, 2L, diff)), 1e-8)
    expect_true(all(diff(pairwiseLLM:::.rubric_ordinal_decisions(grid_p)$expected_level) >= -1e-8))
    expect_gte(fit$diagnostics$ordinal$monotonicity$min_latent_increment, -1e-8)
    expect_identical(fit$diagnostics$ordinal$monotonicity$grid_size, 1001L)
    expect_false(fit$diagnostics$ordinal$monotonicity$essentially_flat)
    expect_true(all(fit$backend$cutpoint_status == "unique"))
    boundary_p <- pairwiseLLM:::.rubric_monotone_probabilities(fit$backend$cutpoints_theta, fit)
    for (k in seq_len(K - 1L)) expect_equal(sum(boundary_p[k, seq_len(k)]), 0.5, tolerance = 1e-8)
    expect_equal(pred$expected_level, as.vector(p %*% seq_len(K)))
    expect_identical(pred$category, as.integer(apply(p, 1, function(row) which(cumsum(row) >= 0.5)[[1L]])))
    expect_identical(pred$category, pred$median_category)
    expect_identical(pred$modal_category, max.col(p, ties.method = "first"))
    mode <- stats::predict(fit, hard_score = "mode")
    expect_identical(mode$category, mode$modal_category)
    expect_identical(mode$probabilities, pred$probabilities)
  }
  expect_identical(.Random.seed, before)
})

test_that("monotone fits preserve labels and use only labeled scores for calibration", {
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  skip_if_not_installed("withr")
  data <- rubric_monotone_data()
  labels <- c("developing", "proficient", "advanced")
  data$rubric$rubric_score <- labels[data$rubric$rubric_score]
  # The completed CJ set extends beyond the central, labeled calibration range.
  keep <- abs(data$cj$fit$theta_mean) <= 1.6
  rubric <- data$rubric[keep, ]
  partial <- data
  partial$rubric <- rubric[rev(seq_len(nrow(rubric))), ]
  fit <- rubric_monotone_fit(partial, levels = labels)
  pred <- stats::predict(fit)
  expect_identical(fit$levels, labels)
  expect_identical(pred$rubric_score, labels[pred$category])
  expect_identical(names(pred$probabilities[[1L]]), labels)
  expect_equal(fit$transformation, list(center = mean(data$cj$fit$theta_mean[keep]),
    scale = stats::sd(data$cj$fit$theta_mean[keep])))
  expect_equal(unname(fit$category_counts), as.integer(table(factor(rubric$rubric_score, levels = labels))))
  expect_identical(pred$extrapolated, !unname(keep))
  expect_true(all(is.finite(unlist(pred$probabilities))))
  partial$rubric <- data$rubric
  partial$rubric$rubric_score[!keep] <- NA_character_
  explicit_na <- rubric_monotone_fit(partial, levels = labels)
  expect_identical(stats::predict(explicit_na), pred)
  expect_identical(stats::predict(fit, data$cj), pred)
  expect_identical(fit$diagnostics$cj, fit$cj$diagnostics)
  expect_identical(fit$reference, fit$cj$reference)
  for (labels in list(c(10, 20, 50), c("low", "middle", "high"))) {
    data <- rubric_monotone_data()
    data$rubric$rubric_score <- ordered(data$rubric$rubric_score, levels = 1:3, labels = labels)
    fitted <- rubric_monotone_fit(data)
    expect_identical(fitted$levels, as.character(labels))
    expect_true(all(stats::predict(fitted)$expected_level >= 1 & stats::predict(fitted)$expected_level <= 3))
  }
})

test_that("basis and smoothing controls are narrow, explicit, and validated", {
  controls <- pairwiseLLM:::.rubric_monotone_controls
  expect_identical(controls(), list(k = 6L, sp = NULL))
  expect_identical(controls(k = 5, sp = 2), list(k = 5L, sp = 2))
  for (k in list(NULL, NA, Inf, 4, 5.5, "6", c(5, 6), matrix(6), .Machine$integer.max + 1)) {
    expect_error(controls(k = k), "single integer >= 5")
  }
  for (sp in list(NA, Inf, 0, -1, "1", c(1, 2), matrix(1))) {
    expect_error(controls(sp = sp), "finite positive")
  }
  expect_error(controls(6), "uniquely named")
  expect_error(controls(k = 5, k = 6), "uniquely named")
  expect_error(controls(method = "REML"), "uniquely named")
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  skip_if_not_installed("withr")
  fixed <- rubric_monotone_fit(rubric_monotone_data(), k = 7, sp = 1)
  expect_identical(fixed$backend$basis$k, 7L)
  expect_identical(fixed$backend$smoothing$sp_requested, 1)
  expect_equal(unname(fixed$backend$smoothing$sp), 1)
  data <- rubric_monotone_data()
  named_method <- pairwiseLLM::fit_rubric_calibration(data$cj, data$rubric,
    method = c(chosen = "ordinal_monotone"), trait = "organization", k = 7, sp = 1)
  expect_identical(stats::predict(named_method), stats::predict(fixed))
  expect_warning(reduced <- rubric_monotone_fit(rubric_monotone_data(n_unique = 5), k = 8),
    "basis dimension reduced from 8 to 5")
  expect_identical(reduced$backend$basis$k_requested, 8L)
  expect_identical(reduced$backend$basis$k, 5L)
  expect_true(all(is.finite(unlist(stats::predict(reduced)$probabilities))))
  expect_error(stats::predict(fixed, k = 7), "must be empty")
  data <- rubric_monotone_data()
  expect_error(pairwiseLLM::fit_rubric_calibration(data$cj, method = "percentile", K = 3,
    trait = "organization", k = 6), "must be empty")
  expect_error(pairwiseLLM::fit_rubric_calibration(data$cj, data$rubric,
    trait = "organization", sp = 1), "must be empty")
})

test_that("small, missing-category, and reversed calibration failures are clear", {
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  skip_if_not_installed("withr")
  for (n in 2:4) expect_error(rubric_monotone_fit(rubric_monotone_data(n_unique = n)), "five unique")
  data <- rubric_monotone_data()
  missing <- data
  missing$rubric$rubric_score[missing$rubric$rubric_score == 2] <- NA_integer_
  expect_error(rubric_monotone_fit(missing, levels = 1:3), "Missing requested rubric categories")
  constant <- data
  constant$rubric$rubric_score[constant$cj$fit$theta_mean != 0] <- NA_integer_
  expect_error(rubric_monotone_fit(constant), "nonzero standard deviation")
  # This backend may fail when the increasing constraint forces a flat fit.
  # Its failure is translated without reversing labels or substituting a GAM.
  data$rubric$rubric_score <- 4L - data$rubric$rubric_score
  result <- tryCatch(suppressWarnings(rubric_monotone_fit(data)), error = identity)
  if (inherits(result, "error")) {
    expect_s3_class(result, "pairwiseLLM_rubric_monotone_fit_error")
    expect_match(conditionMessage(result), "score orientation")
  } else {
    expect_identical(result$levels, 1:3)
    expect_gte(result$diagnostics$ordinal$monotonicity$min_latent_increment, -1e-8)
    expect_true(result$diagnostics$ordinal$monotonicity$essentially_flat)
  }
})

test_that("numeric guards detect reversals and invalid probabilities", {
  check <- pairwiseLLM:::.rubric_monotone_grid_check
  expect_error(check(seq(1, -1, length.out = 101), c(-1, 1)), "monotonicity verification")
  flat <- check(rep(0, 101), c(-1, 1))
  expect_true(flat$essentially_flat)
  expect_equal(flat$min_latent_increment, 0)
  check_p <- pairwiseLLM:::.rubric_monotone_check_probabilities
  for (p in list(c(0.2, 0.3, 0.5), matrix(c(0.2, 0.3, 0.5), ncol = 1),
    matrix(c(-0.1, 0.5, 0.6), nrow = 1), matrix(c(0.1, 0.2, 0.3), nrow = 1),
    matrix(c(NA, 0.5, 0.5), nrow = 1), matrix(c(Inf, 0, 0), nrow = 1))) {
    expect_error(check_p(p, 1L, 3L), "valid category probabilities")
  }
  # Shared hard-score conventions are exact, including boundary and modal ties.
  p <- rbind(c(0.5, 0.25, 0.25), c(0.25, 0.25, 0.5), c(0.4, 0.4, 0.2))
  decisions <- pairwiseLLM:::.rubric_ordinal_decisions(p)
  expect_identical(decisions$median, c(1L, 2L, 2L))
  expect_identical(decisions$mode, c(1L, 3L, 1L))
})

test_that("saved monotone predictions and all four BTL variants share the same mapping", {
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  skip_if_not_installed("withr")
  withr::local_seed(2041)
  before <- .Random.seed
  data <- rubric_monotone_data()
  fit <- rubric_monotone_fit(data)
  directory <- withr::local_tempdir()
  path <- file.path(directory, "calibration.rds")
  saveRDS(fit, path)
  restored <- readRDS(path)
  expect_identical(stats::predict(restored), stats::predict(fit))
  expect_identical(environment(fit$backend$model$formula), asNamespace("mgcv"))
  for (variant in c("btl_e", "btl_b", "btl_e_b")) {
    variant_fit <- rubric_monotone_fit(rubric_monotone_data(variant = variant))
    expect_identical(variant_fit$cj$model_variant, variant)
    expect_identical(stats::predict(variant_fit), stats::predict(fit))
  }
  expect_identical(.Random.seed, before)
})

test_that("same-set prediction retains exact source guards and allows item reordering", {
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  skip_if_not_installed("withr")
  data <- rubric_monotone_data()
  fit <- rubric_monotone_fit(data)
  reordered <- rubric_monotone_data(reverse_items = TRUE)$cj
  pred <- stats::predict(fit, reordered)
  expect_identical(pred$item_id, rev(data$rubric$item_id))
  expect_identical(pred$probabilities, rev(stats::predict(fit)$probabilities))
  other <- data$cj
  other$round_log$reliability_EAP <- 0.96
  expect_error(stats::predict(fit, other), "Same-set")
  expect_error(stats::predict(fit, data.frame(item_id = "x", theta = 0)), "supported completed")
  expect_error(stats::predict(fit, fit$cj), "Unsupported")
})

test_that("monotone dependency guidance covers fitting and saved prediction", {
  data <- rubric_monotone_data()
  testthat::local_mocked_bindings(.rubric_mgcv_available = function() FALSE, .package = "pairwiseLLM")
  expect_error(rubric_monotone_fit(data), "mgcv.*1.9-4", class = "pairwiseLLM_rubric_dependency_missing")
  expect_error(pairwiseLLM:::.rubric_monotone_dependencies(), 'install.packages\\("mgcv"\\)')
})

test_that("missing RNG dependency is required only for fitting", {
  testthat::local_mocked_bindings(.rubric_mgcv_available = function() TRUE,
    .rubric_monotone_rng_available = function() FALSE, .package = "pairwiseLLM")
  expect_error(rubric_monotone_fit(rubric_monotone_data()), "withr", class = "pairwiseLLM_rubric_dependency_missing")
  expect_invisible(pairwiseLLM:::.rubric_monotone_dependencies())
})

test_that("old mgcv versions give update guidance", {
  skip_if_not_installed("mgcv")
  testthat::local_mocked_bindings(packageVersion = function(...) package_version("1.9.3"), .package = "utils")
  expect_false(pairwiseLLM:::.rubric_mgcv_available())
  expect_error(pairwiseLLM:::.rubric_monotone_dependencies(), "Install or update",
    class = "pairwiseLLM_rubric_dependency_missing")
})

test_that("invalid fitted monotone objects and backend errors are rejected", {
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  skip_if_not_installed("withr")
  data <- rubric_monotone_data()
  fit <- rubric_monotone_fit(data)
  for (thresholds in list(NULL, c(0, 1), c(-1, -2), c(-1, -1), c(-1, Inf), c(-1, NA), matrix(c(-1, 1)))) {
    bad <- fit
    bad$backend$thresholds <- thresholds
    expect_error(stats::predict(bad), "Invalid fitted monotone ordinal")
  }
  for (scale in list(NULL, 0, -1, NA_real_, Inf)) {
    bad <- fit
    bad$transformation$scale <- scale
    expect_error(stats::predict(bad), "Invalid fitted monotone ordinal")
  }
  bad <- fit
  bad$backend$model$smooth[[1L]]$xt <- "m-"
  expect_error(stats::predict(bad), "Invalid fitted monotone ordinal")
  bad <- fit
  bad$backend$model$coefficients[[1L]] <- Inf
  expect_error(stats::predict(bad), "Invalid fitted monotone ordinal")
  expect_error(pairwiseLLM:::.rubric_monotone_probabilities(Inf, fit), "must be finite")
  testthat::local_mocked_bindings(scasm = function(...) stop("synthetic backend failure"), .package = "mgcv")
  expect_error(rubric_monotone_fit(data), "could not be fitted", class = "pairwiseLLM_rubric_monotone_fit_error")
})

test_that("convergence, sparse-category, and backend warnings remain visible", {
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  skip_if_not_installed("withr")
  data <- rubric_monotone_data()
  model <- rubric_monotone_fit(data)$backend$model
  model$outer.info$converged <- FALSE
  testthat::local_mocked_bindings(scasm = function(...) {
    warning("synthetic numerical warning")
    model
  }, .package = "mgcv")
  middle <- which(data$rubric$rubric_score == 2)
  data$rubric$rubric_score[middle[-1L]] <- NA_integer_
  expect_warning(fit <- rubric_monotone_fit(data), "synthetic numerical warning")
  expect_false(fit$diagnostics$ordinal$converged)
  expect_identical(fit$diagnostics$ordinal$singleton_categories, "2")
  expect_true(any(grepl("convergence requires review", fit$warnings)))
  expect_true(any(grepl("Sparse calibration", fit$warnings)))
  expect_true(all(is.finite(unlist(stats::predict(fit)$probabilities))))
})

test_that("flat or unreached thresholds have no invented unique boundaries", {
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  skip_if_not_installed("withr")
  data <- rubric_monotone_data()
  fit <- rubric_monotone_fit(data)
  grid <- seq(-1, 1, length.out = 101)
  flat <- pairwiseLLM:::.rubric_monotone_boundaries(fit, grid, rep(-1, 101))
  expect_identical(flat$status, c("flat_or_unresolved", "outside_range"))
  expect_true(all(is.na(flat$cutpoints)))
  model <- fit$backend$model
  model$coefficients[-1L] <- 0
  testthat::local_mocked_bindings(scasm = function(...) model, .package = "mgcv")
  expect_warning(flat_fit <- rubric_monotone_fit(data), "essentially flat")
  expect_true(flat_fit$diagnostics$ordinal$monotonicity$essentially_flat)
})

test_that("saved prediction checks mgcv but does not require fitting-only dependencies", {
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  skip_if_not_installed("withr")
  fit <- rubric_monotone_fit(rubric_monotone_data())
  pred <- stats::predict(fit)
  testthat::local_mocked_bindings(.rubric_monotone_rng_available = function() {
    stop("prediction must not check fitting-only dependency")
  }, .package = "pairwiseLLM")
  expect_identical(stats::predict(fit), pred)
  testthat::local_mocked_bindings(.rubric_mgcv_available = function() FALSE, .package = "pairwiseLLM")
  expect_error(stats::predict(fit), "mgcv", class = "pairwiseLLM_rubric_dependency_missing")
})
