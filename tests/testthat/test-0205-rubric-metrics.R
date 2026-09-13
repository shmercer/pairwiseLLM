test_that("normalized RPS and log loss match independent hand calculations", {
  metric <- pairwiseLLM:::.rubric_metric_values
  p <- rbind(c(0.2, 0.5, 0.3), c(0.1, 0.2, 0.7))
  result <- metric(c(2L, 3L), c(2L, 3L), 3L, p)
  expect_equal(result$rps, c((0.2^2 + (0.7 - 1)^2) / 2, (0.1^2 + 0.3^2) / 2))
  expect_equal(result$metrics$rps, 0.0575)
  expect_equal(result$log_loss, -log(c(0.5, 0.7)))
  expect_equal(result$metrics$log_loss, mean(-log(c(0.5, 0.7))))
  for (K in 3:6) {
    perfect <- metric(seq_len(K), seq_len(K), K, diag(K))
    expect_equal(perfect$rps, rep(0, K))
    expect_equal(perfect$log_loss, rep(0, K))
    expect_equal(perfect$metrics$quadratic_weighted_kappa, 1)
    adjacent <- metric(1L, 2L, K, diag(K)[2L, , drop = FALSE])
    extreme <- metric(1L, K, K, diag(K)[K, , drop = FALSE])
    expect_equal(adjacent$rps, 1 / (K - 1))
    expect_equal(extreme$rps, 1)
    expect_equal(extreme$log_loss, -log(.Machine$double.xmin))
    expect_true(extreme$log_loss_floored)
  }
  tiny <- matrix(c(.Machine$double.xmin * 2, 0.5, 0.5), 1L)
  expect_false(metric(1L, 2L, 3L, tiny)$log_loss_floored)
})

test_that("hard metrics and quadratic weights match an independent confusion table", {
  metric <- pairwiseLLM:::.rubric_metric_values
  result <- metric(c(1L, 1L, 2L, 3L), c(1L, 2L, 3L, 3L), 3L)
  # Observed table /4 has off-diagonal mass .25 at (1,2) and (2,3).
  # Weighted observed disagreement = .125; independent expected = .40625.
  expect_equal(result$metrics$quadratic_weighted_kappa, 1 - 0.125 / 0.40625)
  expect_equal(result$metrics$exact_accuracy, 0.5)
  expect_equal(result$metrics$within_one_accuracy, 1)
  expect_equal(result$metrics$mae, 0.5)
  expect_true(is.na(result$metrics$rps))
  expect_null(result$kappa_reason)
  constant <- metric(c(2L, 2L), c(2L, 2L), 3L)
  expect_true(is.na(constant$metrics$quadratic_weighted_kappa))
  expect_match(constant$kappa_reason, "zero")
  expect_equal(metric(c(2L, 2L), c(1L, 3L), 3L)$metrics$quadratic_weighted_kappa, 0)
})

test_that("one probability validator enforces finite ordered K-category matrices", {
  check <- pairwiseLLM:::.rubric_check_probabilities
  levels <- c("low", "mid", "high")
  p <- matrix(c(0.2, 0.3, 0.5), 1L, dimnames = list(NULL, levels))
  expect_identical(check(p, levels), p)
  invalid <- list(as.vector(p), unname(p), p[, 3:1, drop = FALSE],
    matrix(0.5, 1L, 2L), matrix(as.character(p), 1L, dimnames = dimnames(p)))
  for (value in c(NA, NaN, Inf, -1e-5, 1.1, 0.1)) {
    bad <- p
    bad[1L, 1L] <- value
    invalid[[length(invalid) + 1L]] <- bad
  }
  for (bad in invalid) expect_error(check(bad, levels), "probabilities")
  expect_error(check(p, levels, 2L), "dimensions")
  near <- p
  near[1L, ] <- c(-1e-14, 0, 1 + 1e-14)
  before <- near
  copy <- pairwiseLLM:::.rubric_probability_copy(near, levels)
  expect_equal(unname(copy), matrix(c(0, 0, 1), 1L))
  expect_identical(near, before)
  near[1L, ] <- c(0.8, 0.2 + 5e-13, 0)
  expect_identical(check(near, levels), near)
  expect_true(all(pairwiseLLM:::.rubric_cumulative(near) <= 1))
})

test_that("cumulative probability and theta summaries preserve ties and boundary order", {
  p <- rbind(c(0.2, 0.3, 0.5), c(0.2, 0.4, 0.4), c(0.8, 0.1, 0.1), c(1, 0, 0))
  result <- pairwiseLLM:::.rubric_calibration_summary(p, c(1L, 2L, 3L, 1L),
    c(-1, -1, 1, 1), c("a", "b", "c"), 2L)
  for (group in c("probability", "theta")) {
    for (k in 1:2) expect_equal(sum(result$n[result$group_by == group & result$boundary == k]), 4)
  }
  row <- result[result$group_by == "probability" & result$boundary == 1L & result$bin == 1L, ]
  expect_equal(row$n, 2L)
  expect_equal(row$predicted, 0.2)
  expect_equal(row$observed, 0.5)
  expect_equal(row$residual, 0.3)
  row <- result[result$group_by == "theta" & result$boundary == 2L & result$bin == 1L, ]
  expect_equal(row$predicted, 0.55)
  expect_equal(row$observed, 1)
  flat <- pairwiseLLM:::.rubric_calibration_summary(p, c(1L, 2L, 3L, 1L), rep(0, 4), 1:3, 10L)
  expect_identical(flat$bin[flat$group_by == "theta"], c(1L, 1L))
  expect_true(all(is.finite(result$predicted)))
})

test_that("public evaluation aligns labels, reports overlap, and preserves predictions", {
  skip_if_not_installed("ordinal")
  data <- rubric_linear_data()
  labels <- c(10, 20, 90)
  data$rubric$rubric_score <- labels[data$rubric$rubric_score]
  training <- data$rubric[seq(1L, nrow(data$rubric), by = 2L), ]
  fit <- pairwiseLLM::fit_rubric_calibration(data$cj, training, trait = "organization", levels = labels)
  before <- serialize(fit, NULL)
  actual <- data$rubric[rev(seq_len(nrow(data$rubric))), ]
  actual$rubric_score[1L] <- NA_real_
  result <- pairwiseLLM::evaluate_rubric_predictions(fit, actual)
  prediction <- stats::predict(fit)
  observed <- match(data$rubric$rubric_score, labels)[seq_len(nrow(data$rubric) - 1L)]
  expect_equal(result$metrics$mae, mean(abs(observed - prediction$category[seq_along(observed)])))
  expect_identical(result$per_item$probabilities, prediction$probabilities[seq_along(observed)])
  expect_equal(result$per_item$theta_sd, fit$cj$items$theta_sd[seq_along(observed)])
  expect_equal(result$metadata$n_missing_labels, 1L)
  expect_equal(result$metadata$n_unscored_predictions, 1L)
  expect_equal(result$metadata$n_training_label_overlap, sum(result$per_item$item_id %in% training$item_id))
  expect_identical(result$metadata$primary_metric, "rps")
  expect_identical(result$metadata$evaluation_design, "same_set")
  expect_identical(result$diagnostics$status, "not_requested")
  expect_identical(serialize(fit, NULL), before)
  mode <- pairwiseLLM::evaluate_rubric_predictions(fit, actual, hard_score = "mode")
  expect_identical(mode$per_item$probabilities, result$per_item$probabilities)
  expect_identical(mode$per_item$category, mode$per_item$modal_category)
  expect_identical(mode$metrics$rps, result$metrics$rps)
  one <- pairwiseLLM::evaluate_rubric_predictions(fit, data$rubric[1L, ])
  expect_equal(one$metrics$n, 1L)
  expect_equal(nrow(one$calibration), 4L)
})

test_that("evaluation rejects ambiguous labels and malformed controls without collapsing levels", {
  fit <- pairwiseLLM::fit_rubric_calibration(rubric_test_fixed(), method = "percentile",
    trait = "organization", levels = c("a", "b", "c"))
  rubric <- data.frame(item_id = "a", rubric_score = "a")
  evaluate <- function(x = rubric, ...) pairwiseLLM::evaluate_rubric_predictions(fit, x, ...)
  for (bad in list(NULL, data.frame(item_id = "a"), rbind(rubric, rubric),
    data.frame(item_id = "unknown", rubric_score = "a"),
    data.frame(item_id = "a", rubric_score = "other"), data.frame(item_id = "a", rubric_score = NA_character_),
    transform(rubric, trait = "another"),
    data.frame(item_id = "a", rubric_score = ordered("a", levels = c("c", "b", "a"))))) {
    expect_error(evaluate(bad))
  }
  for (bad in list(0, -1, 0.5, Inf, NA, "ten", numeric(), c(1, 2))) {
    expect_error(evaluate(bins = bad), "bins")
  }
  for (bad in list(NA, 1, c(TRUE, FALSE), logical())) expect_error(evaluate(diagnostics = bad), "diagnostics")
  expect_error(evaluate(hard_score = "mean"))
  unfitted <- fit
  unfitted$status <- "unfitted"
  unfitted[c("backend", "transformation")] <- list(NULL, NULL)
  expect_error(pairwiseLLM::evaluate_rubric_predictions(unfitted, rubric), "fitted")
})

test_that("percentile evaluation reports hard metrics without probability fabrication", {
  fit <- pairwiseLLM::fit_rubric_calibration(rubric_test_fixed(), method = "percentile",
    trait = "organization", K = 3L)
  prediction <- stats::predict(fit)
  rubric <- prediction[, c("item_id", "rubric_score")]
  result <- pairwiseLLM::evaluate_rubric_predictions(fit, rubric, diagnostics = TRUE)
  expect_equal(result$metrics$exact_accuracy, 1)
  expect_equal(result$metrics$mae, 0)
  expect_true(is.na(result$metrics$rps) && is.na(result$metrics$log_loss))
  expect_false(result$metadata$probabilities_available)
  expect_match(result$metadata$probability_reason, "no probabilities")
  expect_null(result$calibration)
  expect_equal(result$metadata$n_training_label_overlap, 0L)
  expect_identical(result$diagnostics$status, "not_applicable")
  expect_false("probabilities" %in% names(result$per_item))
})

test_that("evaluation verifies the probability source of every ordinal decision", {
  skip_if_not_installed("ordinal")
  data <- rubric_linear_data()
  fit <- rubric_linear_fit(data)
  prediction <- stats::predict(fit)
  evaluate <- function(p) pairwiseLLM:::.rubric_evaluate_table(fit, data$rubric, p, "median", 10L)
  for (column in c("category", "median_category", "modal_category", "expected_level")) {
    bad <- prediction
    bad[[column]][1L] <- bad[[column]][1L] + 1L
    expect_error(evaluate(bad), "derived")
  }
  bad <- prediction
  bad$rubric_score[1L] <- 100
  expect_error(evaluate(bad), "labels must agree")
  for (vector in list(c(0.2, 0.3, 0.5), stats::setNames(c(0.2, 0.3, 0.5), c("3", "2", "1")),
    matrix(c(0.2, 0.3, 0.5), 1L))) {
    bad <- prediction
    bad$probabilities[[1L]] <- vector
    expect_error(evaluate(bad), "level order")
  }
})

test_that("linked external evaluation retains common-scale target uncertainty and provenance", {
  skip_if_not_installed("ordinal")
  data <- rubric_linked_fixture(n_sets = 3L, target_shift = 1)
  fit <- rubric_linked_fit(data)
  prediction <- stats::predict(fit, data$state)
  external <- data.frame(item_id = rev(prediction$item_id), rubric_score = rep(data$levels, 2L))
  before <- serialize(data$state, NULL)
  result <- pairwiseLLM::evaluate_rubric_predictions(fit, external, newdata = data$state)
  expect_identical(result$metadata$evaluation_design, "linked_target_transport")
  expect_equal(result$metadata$n_training_label_overlap, 0L)
  expect_identical(result$per_item$theta, prediction$theta)
  expect_identical(result$per_item$theta_sd, prediction$theta_sd)
  expect_identical(result$metadata$linking, attr(prediction, "linking"))
  expect_identical(result$per_item$extrapolated, prediction$extrapolated)
  expect_equal(result$metadata$n_extrapolated, sum(prediction$extrapolated))
  expect_identical(serialize(data$state, NULL), before)
  reference <- pairwiseLLM::evaluate_rubric_predictions(fit, data$rubric)
  expect_identical(reference$metadata$evaluation_design, "reference_calibration")
  expect_equal(reference$metadata$n_training_label_overlap, nrow(data$rubric))
  expect_error(pairwiseLLM::evaluate_rubric_predictions(fit, external, data$reference), "scale")
})
