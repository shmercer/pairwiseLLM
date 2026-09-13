test_that("linear recovery handles shifted scales, K = 3-6 and category imbalance", {
  rubric_skip_method("ordinal_linear")
  for (K in 3:6) {
    for (imbalanced in c(FALSE, TRUE)) {
      data <- rubric_recovery_data(K, imbalanced = imbalanced, shift = 4,
        spacing = if (K %% 2L == 0L) 2 else 1)
      training <- data$rubric[data$train, ]
      counts <- tabulate(training$rubric_score, K)
      expect_true(all(counts > 0))
      if (imbalanced) expect_gt(max(counts) / min(counts), 5)
      fit <- rubric_workflow_fit(data$cj, training, "ordinal_linear", K)
      pred <- stats::predict(fit)
      p <- do.call(rbind, pred$probabilities)
      raw_slope <- fit$backend$slope / fit$transformation$scale
      # tau - beta * (theta - center) / scale = raw_tau - raw_beta * theta.
      raw_thresholds <- fit$backend$thresholds + raw_slope * fit$transformation$center
      expect_equal(raw_slope, 1.2, tolerance = 0.12)
      expect_equal(unname(raw_thresholds), data$thresholds + 1.2 * 4, tolerance = 0.25)
      expect_lt(mean(abs(p - data$truth)), 0.03)
      expect_true(fit$diagnostics$ordinal$converged)
      assessment <- pairwiseLLM::evaluate_rubric_predictions(fit, data$rubric[!data$train, ])
      expect_identical(assessment$metadata$n_training_label_overlap, 0L)
      expect_equal(assessment$metrics$rps,
        rubric_oracle_rps(p[!data$train, ], data$rubric$rubric_score[!data$train]), tolerance = 1e-12)
      marginal <- matrix(counts / sum(counts), sum(!data$train), K, byrow = TRUE)
      expect_lt(assessment$metrics$rps,
        rubric_oracle_rps(marginal, data$rubric$rubric_score[!data$train]))
    }
  }
})

test_that("monotone compression and S-shapes recover probability truth for K = 3-6", {
  rubric_skip_method("ordinal_monotone")
  withr::local_seed(92030L)
  rng <- .Random.seed
  for (K in 3:6) {
    for (shape in c("compression", "s_shape")) {
      data <- rubric_recovery_data(K, shape)
      fit <- rubric_workflow_fit(data$cj, data$rubric[data$train, ], "ordinal_monotone", K)
      pred <- stats::predict(fit)
      p <- do.call(rbind, pred$probabilities)
      expect_true(fit$diagnostics$ordinal$converged)
      expect_equal(rowSums(p), rep(1, nrow(p)), tolerance = 1e-12)
      expect_true(all(is.finite(p) & p >= 0 & p <= 1))
      expect_lt(mean(abs(p - data$truth)), 0.04)
      expect_lt(max(abs(p - data$truth)), 0.15)
      cumulative <- t(apply(p, 1L, cumsum))
      expect_lte(max(apply(cumulative, 2L, diff)), 1e-8)
      expect_gte(min(diff(pred$expected_level)), -1e-8)
      expect_identical(pred$category,
        as.integer(apply(cumulative, 1L, function(x) which(x >= 0.5)[[1L]])))
      assessment <- pairwiseLLM::evaluate_rubric_predictions(fit, data$rubric[!data$train, ])
      counts <- tabulate(data$rubric$rubric_score[data$train], K)
      marginal <- matrix(counts / sum(counts), sum(!data$train), K, byrow = TRUE)
      expect_lt(assessment$metrics$rps,
        rubric_oracle_rps(marginal, data$rubric$rubric_score[!data$train]))
      expect_true(assessment$metadata$conditional_on_cj)
      expect_false(fit$diagnostics$ordinal$threshold_uncertainty_available)
    }
  }
  expect_identical(.Random.seed, rng)
})

for (method in c("ordinal_linear", "ordinal_monotone")) {
  test_that(paste(method, "reports poor targeting without using held-out labels or target scaling"), {
    rubric_skip_method(method)
    data <- rubric_recovery_data(K = 4L)
    narrow <- data$train & abs(data$x) <= 0.75
    spanning <- data$train & data$x %in% seq(-3, 3, length.out = 7L)
    expect_identical(sum(narrow), sum(spanning))
    fits <- lapply(list(narrow, spanning), function(keep) {
      rubric_workflow_fit(data$cj, data$rubric[keep, ], method, K = 4L)
    })
    for (i in 1:2) {
      keep <- list(narrow, spanning)[[i]]
      fit <- fits[[i]]
      expect_equal(fit$transformation,
        list(center = mean(data$theta[keep]), scale = stats::sd(data$theta[keep])))
      pred <- stats::predict(fit)
      expect_identical(pred$extrapolated,
        data$theta < min(data$theta[keep]) | data$theta > max(data$theta[keep]))
      assessment <- pairwiseLLM::evaluate_rubric_predictions(fit, data$rubric[!data$train, ])
      expect_identical(assessment$metadata$n_training_label_overlap, 0L)
      expect_equal(assessment$metadata$n_extrapolated, sum(pred$extrapolated[!data$train]))
      before <- serialize(fit, NULL)
      changed <- data$rubric[!data$train, ]
      changed$rubric_score <- 5L - changed$rubric_score
      pairwiseLLM::evaluate_rubric_predictions(fit, changed)
      expect_identical(serialize(fit, NULL), before)
      expect_identical(stats::predict(fit), pred)
    }
    expect_true(any(stats::predict(fits[[1L]])$extrapolated & data$x < 0))
    expect_true(any(stats::predict(fits[[1L]])$extrapolated & data$x > 0))
    expect_false(any(stats::predict(fits[[2L]])$extrapolated))
  })
}

test_that("weak and reversed linear relationships retain uncertainty and direction diagnostics", {
  rubric_skip_method("ordinal_linear")
  data <- rubric_recovery_data(shape = "weak")
  fit <- rubric_workflow_fit(data$cj, data$rubric[data$train, ], "ordinal_linear")
  pred <- stats::predict(fit)
  expect_lt(abs(fit$backend$slope / fit$transformation$scale), 0.1)
  expect_lt(diff(range(pred$expected_level)), 0.4)
  expect_lt(max(unlist(pred$probabilities)), 0.8)
  expect_true(fit$diagnostics$ordinal$conditional_on_cj)
  data <- rubric_recovery_data(shape = "reversed")
  expect_warning(fit <- rubric_workflow_fit(data$cj, data$rubric, "ordinal_linear"),
    "zero or negative")
  expect_true(fit$diagnostics$ordinal$nonpositive_slope)
  expect_null(fit$backend$cutpoints_theta)
  expect_identical(fit$orientation, "higher_is_better")
  expect_lt(fit$backend$slope, 0)
  expect_true(all(diff(stats::predict(fit)$expected_level) <= 1e-12))
})

test_that("reversed monotone labels cannot silently produce a decreasing production fit", {
  rubric_skip_method("ordinal_monotone")
  data <- rubric_recovery_data(shape = "reversed")
  captured <- pairwiseLLM:::.rubric_capture(function() {
    rubric_workflow_fit(data$cj, data$rubric, "ordinal_monotone")
  })
  if (is.null(captured$value)) {
    expect_match(captured$reason, "Monotone ordinal|monotonicity|probabilities")
  } else {
    fit <- captured$value
    expect_identical(fit$orientation, "higher_is_better")
    expect_gte(min(diff(stats::predict(fit)$expected_level)), -1e-8)
    expect_true(fit$diagnostics$ordinal$monotonicity$essentially_flat ||
      !fit$diagnostics$ordinal$converged)
    expect_true(length(fit$warnings) > 0L)
  }
})

for (method in c("ordinal_linear", "ordinal_monotone")) {
  test_that(paste(method, "never collapses an intended missing category under imbalance"), {
    rubric_skip_method(method)
    data <- rubric_recovery_data(K = 6L, imbalanced = TRUE)
    labels <- data$rubric[data$rubric$rubric_score != 6L, ]
    expect_error(rubric_workflow_fit(data$cj, labels, method, K = 6L), "Missing|missing")
  })
}
