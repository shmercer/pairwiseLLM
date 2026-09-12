# Synthetic completed CJ locations with deterministic category frequencies.
rubric_linear_fixed <- function(theta, ids = paste0("item", seq_along(theta)), variant = "btl") {
  draws <- outer(c(-0.125, 0.125), theta, `+`)
  colnames(draws) <- ids
  fit <- pairwiseLLM:::build_btl_fit_contract(draws, model_variant = variant,
    epsilon_draws = if (pairwiseLLM:::model_has_e(variant)) c(0.03, 0.05) else NULL,
    beta_draws = if (pairwiseLLM:::model_has_b(variant)) c(-0.1, 0.1) else NULL,
    diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000), diagnostics_pass = TRUE)
  list(fit = fit, fits = list(fit),
    item_log_list = list(tibble::tibble(refit_id = 1L, ID = ids,
      theta_mean = unname(fit$theta_mean), theta_sd = unname(fit$theta_sd))),
    round_log = tibble::tibble(round_id = 1L, model_variant = variant, reliability_EAP = 0.95))
}

rubric_linear_data <- function(K = 3L, slope = 1.2, variant = "btl") {
  theta <- seq(-2, 2, length.out = 9)
  thresholds <- seq(-1.5, 1.5, length.out = K - 1L)
  cumulative <- stats::plogis(outer(theta, thresholds, function(x, tau) tau - slope * x))
  probabilities <- cbind(cumulative, 1) - cbind(0, cumulative)
  counts <- round(50 * probabilities)
  grid <- expand.grid(category = seq_len(K), theta = theta)
  data <- grid[rep(seq_len(nrow(grid)), as.vector(t(counts))), ]
  ids <- paste0("item", seq_len(nrow(data)))
  list(cj = rubric_linear_fixed(data$theta, ids, variant),
    rubric = data.frame(item_id = ids, rubric_score = data$category),
    theta = data$theta, thresholds = thresholds)
}

rubric_linear_fit <- function(data) {
  pairwiseLLM::fit_rubric_calibration(data$cj, data$rubric, trait = "organization")
}

test_that("linear ordinal K = 3-6 fits recover direction and match backend probabilities", {
  skip_if_not_installed("ordinal")
  withr::local_seed(2030)
  before <- .Random.seed
  for (K in 3:6) {
    data <- rubric_linear_data(K)
    fit <- rubric_linear_fit(data)
    pred <- stats::predict(fit)
    p <- do.call(rbind, pred$probabilities)
    z <- (pred$theta - fit$transformation$center) / fit$transformation$scale
    direct <- stats::predict(fit$backend$model, newdata = data.frame(z = z), type = "prob")$fit
    expect_equal(unname(p), unname(direct), tolerance = 1e-12)
    expect_equal(rowSums(p), rep(1, nrow(p)), tolerance = 1e-12)
    expect_true(all(p >= 0 & p <= 1))
    expect_identical(colnames(p), as.character(seq_len(K)))
    expect_equal(fit$backend$slope / fit$transformation$scale, 1.2, tolerance = 0.06)
    expect_equal(unname(fit$backend$thresholds), data$thresholds, tolerance = 0.06)
    expect_true(all(diff(fit$backend$thresholds) > 0))
    expect_equal(fit$transformation, list(center = mean(data$theta), scale = stats::sd(data$theta)))
    expect_equal(fit$backend$vcov, stats::vcov(fit$backend$model))
    expect_equal(fit$backend$standard_errors, sqrt(diag(fit$backend$vcov)))
    expect_equal(fit$backend$cutpoints_z, fit$backend$thresholds / fit$backend$slope)
    expect_equal(fit$backend$cutpoints_theta,
      fit$transformation$center + fit$transformation$scale * fit$backend$cutpoints_z)
    expect_true(fit$diagnostics$ordinal$converged)
    expect_true(fit$diagnostics$ordinal$covariance_available)
    expect_true(fit$diagnostics$ordinal$conditional_on_cj)
    expect_true(fit$diagnostics$category_probabilities_available)
    expect_identical(fit$backend$version, as.character(utils::packageVersion("ordinal")))
    expect_length(fit$warnings, 0)
    expect_equal(pred$expected_level, as.vector(p %*% seq_len(K)))
    expected_median <- apply(p, 1, function(row) which(cumsum(row) >= 0.5)[[1L]])
    expect_identical(pred$median_category, expected_median)
    expect_identical(pred$category, pred$median_category)
    expect_identical(pred$modal_category, max.col(p, ties.method = "first"))
    mode <- stats::predict(fit, hard_score = "mode")
    expect_identical(mode$category, mode$modal_category)
    expect_identical(mode$probabilities, pred$probabilities)
    expect_false(any(pred$extrapolated))
  }
  expect_identical(.Random.seed, before)
})

test_that("median equality chooses the lower category and modal ties choose the first", {
  skip_if_not_installed("ordinal")
  p <- rbind(c(0.5, 0.25, 0.25), c(0.25, 0.25, 0.5), c(0.4, 0.4, 0.2), c(0.2, 0.4, 0.4))
  d <- pairwiseLLM:::.rubric_ordinal_decisions(p)
  expect_identical(d$median, c(1L, 2L, 2L, 2L))
  expect_identical(d$mode, c(1L, 3L, 1L, 2L))
  expect_equal(d$expected_level, as.vector(p %*% 1:3))
  fit <- rubric_linear_fit(rubric_linear_data())
  fit$transformation <- list(center = 0, scale = 1)
  fit$backend$thresholds <- c(-1, 1)
  fit$backend$slope <- 1
  p <- pairwiseLLM:::.rubric_ordinal_probabilities(c(-1, 1), fit)
  expect_identical(pairwiseLLM:::.rubric_ordinal_decisions(p)$median, c(1L, 2L))
  single <- pairwiseLLM:::.rubric_ordinal_probabilities(0, fit)
  expect_identical(dim(single), c(1L, 3L))
  expect_equal(sum(single), 1)
  expect_true(all(diff(pairwiseLLM:::.rubric_ordinal_decisions(
    pairwiseLLM:::.rubric_ordinal_probabilities(seq(-10, 10, length.out = 101), fit))$median) >= 0))
})

test_that("original ordered labels round trip and expected levels use category indices", {
  skip_if_not_installed("ordinal")
  data <- rubric_linear_data()
  for (labels in list(c("Developing", "On track", "Accomplished"), c(10, 20, 50))) {
    rubric <- data$rubric
    rubric$rubric_score <- labels[rubric$rubric_score]
    fit <- pairwiseLLM::fit_rubric_calibration(data$cj, rubric[rev(seq_len(nrow(rubric))), ],
      trait = "organization", levels = labels)
    pred <- stats::predict(fit)
    expect_identical(fit$levels, labels)
    expect_identical(pred$rubric_score, labels[pred$category])
    expect_identical(names(pred$probabilities[[1L]]), as.character(labels))
    expect_true(all(pred$expected_level >= 1 & pred$expected_level <= 3))
  }
  data$rubric$rubric_score <- ordered(data$rubric$rubric_score, levels = 1:3,
    labels = c("low", "medium", "high"))
  expect_identical(rubric_linear_fit(data)$levels, c("low", "medium", "high"))
})

test_that("unlabeled responses do not affect fitting or standardization and can extrapolate", {
  skip_if_not_installed("ordinal")
  data <- rubric_linear_data()
  base <- rubric_linear_fit(data)
  theta <- c(-4, data$theta, 4)
  ids <- c("unlabeled_low", data$rubric$item_id, "unlabeled_high")
  data$cj <- rubric_linear_fixed(theta, ids)
  fit <- rubric_linear_fit(data)
  expect_identical(fit$transformation, base$transformation)
  expect_identical(fit$backend$thresholds, base$backend$thresholds)
  expect_identical(fit$backend$slope, base$backend$slope)
  expect_identical(fit$calibration_range, c(-2, 2))
  expect_identical(fit$category_counts, base$category_counts)
  pred <- stats::predict(fit)
  expect_identical(pred$extrapolated, c(TRUE, rep(FALSE, length(data$theta)), TRUE))
  data$rubric <- rbind(data$rubric,
    data.frame(item_id = c("unlabeled_low", "unlabeled_high"), rubric_score = NA_integer_))
  na_fit <- rubric_linear_fit(data)
  expect_identical(na_fit$backend$thresholds, fit$backend$thresholds)
  expect_identical(na_fit$transformation, fit$transformation)
  expect_identical(stats::predict(na_fit), pred)
  expect_identical(stats::predict(fit, data$cj), pred)
  order <- rev(seq_along(theta))
  reordered <- rubric_linear_fixed(theta[order], ids[order])
  expect_identical(stats::predict(fit, reordered), pred[order, ])
  data$cj$provenance <- list(collection_mode = "batch")
  expect_identical(stats::predict(fit, data$cj), pred)
  expect_error(stats::predict(fit, rubric_linear_fixed(theta + 1e-10, ids)), "unchanged")
  expect_error(stats::predict(fit, rubric_linear_fixed(theta)), "unchanged")
  expect_error(stats::predict(fit, fit$cj$items), "completed CJ")
  expect_error(stats::predict(fit, fit$cj), "Unsupported")
})

test_that("linear calibration validates public labels, source scales, and degeneracy", {
  skip_if_not_installed("ordinal")
  data <- rubric_linear_data()
  fit <- rubric_linear_fit
  bad <- data
  bad$cj <- rubric_linear_fixed(rep(1, length(data$theta)))
  expect_error(fit(bad), "nonzero standard deviation")
  bad <- data
  bad$rubric$item_id[[1L]] <- NA_character_
  expect_error(fit(bad), "nonmissing")
  bad$rubric$item_id[[1L]] <- bad$rubric$item_id[[2L]]
  expect_error(fit(bad), "unique")
  bad$rubric$item_id[[1L]] <- "unknown"
  expect_error(fit(bad), "present")
  bad <- data
  bad$rubric <- bad$rubric[bad$rubric$rubric_score != 2, ]
  expect_error(pairwiseLLM::fit_rubric_calibration(bad$cj, bad$rubric,
    trait = "organization", levels = 1:3), "Missing requested.*2")
  bad <- data
  bad$rubric$rubric_score <- factor(bad$rubric$rubric_score)
  expect_error(fit(bad), "unordered-factor")
  expect_error(pairwiseLLM::fit_rubric_calibration(data$cj, data$rubric,
    trait = "organization", calibration_design = "linked_anchors"), "inappropriate")
  withr::local_seed(2031)
  state <- rubric_test_adaptive()
  state$round_log$diagnostics_pass <- TRUE
  state$step_log <- tibble::tibble(pair_id = 1:3, step_id = 1:3, A = c(1L, 2L, 3L), B = c(2L, 3L, 4L),
    Y = 1L, set_i = 1L, set_j = 1L)
  artifact <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, 1L)
  expect_error(pairwiseLLM::fit_rubric_calibration(artifact,
    data.frame(item_id = letters[1:3], rubric_score = 1:3), trait = "organization",
    calibration_design = "linked_anchors"), class = "pairwiseLLM_rubric_backend_unavailable")
})

test_that("negative, zero, sparse and separated relationships produce reviewable diagnostics", {
  skip_if_not_installed("ordinal")
  expect_warning(negative <- rubric_linear_fit(rubric_linear_data(slope = -1.2)), "zero or negative")
  expect_lt(negative$backend$slope, 0)
  expect_true(negative$diagnostics$ordinal$nonpositive_slope)
  expect_null(negative$backend$cutpoints_z)
  expect_null(negative$backend$cutpoints_theta)
  pred <- stats::predict(negative)
  expect_gt(mean(pred$expected_level[pred$theta < 0]), mean(pred$expected_level[pred$theta > 0]))
  data <- expand.grid(theta = -2:2, category = 1:3)
  zero <- list(cj = rubric_linear_fixed(data$theta),
    rubric = data.frame(item_id = paste0("item", seq_len(nrow(data))), rubric_score = data$category))
  expect_warning(zero_fit <- rubric_linear_fit(zero), "zero or negative")
  expect_identical(zero_fit$backend$slope, 0)
  expect_identical(zero_fit$levels, 1:3)
  sparse <- zero
  sparse$rubric$rubric_score[sparse$rubric$rubric_score == 2 & data$theta != 0] <- NA_integer_
  expect_warning(sparse_fit <- rubric_linear_fit(sparse), "Sparse calibration")
  expect_identical(sparse_fit$diagnostics$ordinal$singleton_categories, "2")
  separated <- list(cj = rubric_linear_fixed(-4:4),
    rubric = data.frame(item_id = paste0("item", 1:9), rubric_score = rep(1:3, each = 3)))
  expect_warning(separated_fit <- rubric_linear_fit(separated), class = "pairwiseLLM_rubric_ordinal_diagnostics")
  expect_false(separated_fit$diagnostics$ordinal$converged)
  expect_true(any(grepl("singular|convergence", separated_fit$warnings)))
  expect_true(all(is.finite(unlist(stats::predict(separated_fit)$probabilities))))
})

test_that("invalid fitted coefficients, transformations, and backend failures fail clearly", {
  skip_if_not_installed("ordinal")
  data <- rubric_linear_data()
  fit <- rubric_linear_fit(data)
  for (thresholds in list(NULL, c(1, -1), c(0, 0), c(NA, 1), c(-1, Inf), matrix(c(-1, 1)))) {
    bad <- fit
    bad$backend$thresholds <- thresholds
    expect_error(stats::predict(bad), "Invalid fitted linear ordinal")
  }
  for (slope in list(NULL, NA_real_, Inf, c(1, 2), "1")) {
    bad <- fit
    bad$backend$slope <- slope
    expect_error(stats::predict(bad), "Invalid fitted linear ordinal")
  }
  for (scale in list(NULL, 0, -1, NA_real_, Inf)) {
    bad <- fit
    bad$transformation$scale <- scale
    expect_error(stats::predict(bad), "Invalid fitted linear ordinal")
  }
  bad <- fit
  bad$calibration_design <- "linked_anchors"
  expect_error(stats::predict(bad), "Invalid fitted linear ordinal")
  bad <- fit
  bad$calibration_range <- c(1, -1)
  expect_error(stats::predict(bad), "Invalid fitted linear ordinal")
  expect_error(pairwiseLLM:::.rubric_ordinal_probabilities(Inf, fit), "must be finite")
  testthat::local_mocked_bindings(clm = function(...) stop("synthetic backend failure"), .package = "ordinal")
  expect_error(rubric_linear_fit(data), "could not be fitted")
})

test_that("unavailable covariance is marked and diagnostic warnings remain in the object", {
  skip_if_not_installed("ordinal")
  data <- rubric_linear_data()
  model <- rubric_linear_fit(data)$backend$model
  class(model) <- "rubric_test_without_covariance_method"
  testthat::local_mocked_bindings(clm = function(...) model, .package = "ordinal")
  expect_warning(fit <- rubric_linear_fit(data), "Coefficient covariance unavailable")
  expect_false(fit$diagnostics$ordinal$covariance_available)
  expect_true(all(is.na(fit$backend$vcov)))
  expect_true(all(is.na(fit$backend$standard_errors)))
  expect_true(any(grepl("standard errors are unavailable", fit$warnings)))
  expect_true(all(is.finite(unlist(stats::predict(fit)$probabilities))))
})

test_that("four Bayesian variants use the same downstream calibration", {
  skip_if_not_installed("ordinal")
  fits <- lapply(c("btl", "btl_e", "btl_b", "btl_e_b"), function(variant) {
    rubric_linear_fit(rubric_linear_data(variant = variant))
  })
  for (fit in fits[-1L]) expect_identical(stats::predict(fit), stats::predict(fits[[1L]]))
  expect_identical(vapply(fits, function(fit) fit$cj$model_variant, character(1)),
    c("btl", "btl_e", "btl_b", "btl_e_b"))
})

test_that("completed adaptive and Phase A sources share same-set ordinal predictions", {
  skip_if_not_installed("ordinal")
  withr::local_seed(2032)
  rubric <- data.frame(item_id = letters[1:4], rubric_score = c(1, 3, 2, 1))
  state <- rubric_test_adaptive()
  state$round_log$diagnostics_pass <- TRUE
  state$step_log <- tibble::tibble(pair_id = 1:3, step_id = 1:3, A = c(1L, 2L, 3L), B = c(2L, 3L, 4L),
    Y = 1L, set_i = 1L, set_j = 1L)
  artifact <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, 1L)
  fits <- lapply(list(rubric_test_fixed(), state, artifact), function(cj) {
    expect_warning(fit <- pairwiseLLM::fit_rubric_calibration(cj, rubric, trait = "organization"),
      "Sparse calibration")
    expect_identical(stats::predict(fit, cj), stats::predict(fit))
    fit
  })
  expect_identical(stats::predict(fits[[2L]]), stats::predict(fits[[1L]]))
  expect_identical(stats::predict(fits[[3L]]), stats::predict(fits[[1L]]))
  expect_identical(fits[[3L]]$reference, fits[[3L]]$cj$reference)
})


test_that("missing ordinal gives installation guidance while percentile remains usable", {
  data <- rubric_linear_data()
  testthat::local_mocked_bindings(.rubric_ordinal_available = function() FALSE, .package = "pairwiseLLM")
  expect_error(rubric_linear_fit(data), 'install.packages\\("ordinal"\\)',
    class = "pairwiseLLM_rubric_dependency_missing")
  fit <- pairwiseLLM::fit_rubric_calibration(data$cj, method = "percentile", trait = "organization", K = 3)
  expect_identical(fit$status, "fitted")
  expect_equal(nrow(stats::predict(fit)), nrow(data$rubric))
})

test_that("stored linear predictions do not require ordinal availability", {
  skip_if_not_installed("ordinal")
  fit <- rubric_linear_fit(rubric_linear_data())
  expected <- stats::predict(fit)
  testthat::local_mocked_bindings(.rubric_ordinal_available = function() {
    stop("Prediction must not check or load the fitting backend.")
  }, .package = "pairwiseLLM")
  expect_identical(stats::predict(fit), expected)
})
