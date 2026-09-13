test_that("label folds are deterministic, balanced and validated by stable ID", {
  skip_if_not_installed("withr")
  assign <- pairwiseLLM:::.rubric_cv_assignments
  data <- data.frame(item_id = letters[1:12], category = rep(1:3, each = 4))
  withr::local_seed(2060)
  before <- .Random.seed
  folds <- assign(data, 3L, NULL, 7L)
  expect_identical(assign(data, 3L, NULL, 7L), folds)
  expect_equal(tabulate(folds), rep(4L, 3L))
  for (k in 1:3) expect_true(all(tabulate(folds[data$category == k], 3L) >= 1L))
  expect_identical(.Random.seed, before)
  explicit <- data.frame(item_id = rev(data$item_id), fold = rev(folds))
  expect_identical(assign(data, 3L, explicit, NULL), folds)
  for (bad in list(1, 0, NA, 2.5, 13, "3")) expect_error(assign(data, bad, NULL, 1L), "folds")
  for (bad in list(-1, NA, "1", 1.5, c(1, 2))) expect_error(assign(data, 3L, NULL, bad), "seed")
  for (bad in list(1:12, explicit[-1L, ], rbind(explicit, explicit[1L, ]),
    transform(explicit, item_id = paste0("x", item_id)), transform(explicit, fold = 0),
    transform(explicit, fold = 1), transform(explicit, fold = 1.5), transform(explicit, fold = NA_real_),
    transform(explicit, fold = as.character(fold)))) expect_error(assign(data, 3L, bad, 1L))
})

test_that("CV hides labels and reconstructs each training-only mapping with completed CJ intact", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("withr")
  data <- rubric_linear_data()
  fit <- rubric_linear_fit(data)
  original <- serialize(fit, NULL)
  calls <- list()
  refit <- pairwiseLLM:::.rubric_refit_labels
  testthat::local_mocked_bindings(.rubric_refit_labels = function(cj, rubric, method,
    calibration_design, levels, controls) {
    fitted <- refit(cj, rubric, method, calibration_design, levels, controls)
    calls[[length(calls) + 1L]] <<- list(cj = cj, rubric = rubric, fitted = fitted)
    fitted
  }, .package = "pairwiseLLM")
  withr::local_seed(2061)
  before <- .Random.seed
  cv <- pairwiseLLM:::.rubric_cross_validate(fit, folds = 3L)
  expect_identical(cv$status, "complete")
  expect_length(calls, 3L)
  expect_identical(.Random.seed, before)
  for (k in 1:3) {
    held <- cv$assignments$item_id[cv$assignments$fold == k]
    expect_false(any(calls[[k]]$rubric$item_id %in% held))
    expect_identical(calls[[k]]$cj, fit$cj)
    training_data <- calls[[k]]$fitted$calibration_data
    expect_true(all(is.na(training_data$category[training_data$item_id %in% held])))
    expect_true(all(is.na(training_data$rubric_score[training_data$item_id %in% held])))
    theta <- training_data$theta[!is.na(training_data$category)]
    expect_identical(cv$folds$transformation[[k]], list(center = mean(theta), scale = stats::sd(theta)))
    expect_identical(cv$folds$calibration_range[[k]], range(theta))
    expected <- stats::predict(calls[[k]]$fitted)
    actual <- cv$per_item[cv$per_item$fold == k, ]
    expect_identical(actual$probabilities, expected$probabilities[match(actual$item_id, expected$item_id)])
    expect_false(any(actual$training_label_overlap))
  }
  expect_equal(cv$metrics$rps, mean(cv$per_item$rps))
  expect_equal(cv$metrics$log_loss, mean(cv$per_item$log_loss))
  expected_kappa <- pairwiseLLM:::.rubric_metric_values(cv$per_item$observed_category,
    cv$per_item$category, 3L)$metrics$quadratic_weighted_kappa
  expect_equal(cv$metrics$quadratic_weighted_kappa, expected_kappa)
  expect_equal(cv$metrics$n, nrow(data$rubric))
  expect_identical(cv$metadata$validation_estimand, "same_set")
  expect_false(cv$metadata$selection_performed)
  expect_identical(serialize(fit, NULL), original)
})

test_that("fixed fold predictions cannot depend on that fold's held-out labels", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("withr")
  data <- rubric_linear_data()
  fit <- rubric_linear_fit(data)
  cv <- pairwiseLLM:::.rubric_cross_validate(fit, folds = 3L)
  held <- cv$assignments$item_id[cv$assignments$fold == 1L]
  changed <- data
  mask <- changed$rubric$item_id %in% held
  changed$rubric$rubric_score[mask] <- 4L - changed$rubric$rubric_score[mask]
  changed_fit <- rubric_linear_fit(changed)
  changed_cv <- suppressWarnings(pairwiseLLM:::.rubric_cross_validate(changed_fit, folds = 3L,
    fold_id = cv$assignments))
  expect_identical(changed_cv$folds$status[1L], "valid")
  expect_identical(changed_cv$per_item$probabilities[changed_cv$per_item$fold == 1L],
    cv$per_item$probabilities[cv$per_item$fold == 1L])
  expect_identical(changed_cv$folds$transformation[[1L]], cv$folds$transformation[[1L]])
  reordered <- fit
  reordered$calibration_data <- reordered$calibration_data[rev(seq_len(nrow(reordered$calibration_data))), ]
  again <- pairwiseLLM:::.rubric_cross_validate(reordered, folds = 3L)
  expect_identical(again, cv)
})

test_that("missing-category and failed training folds prevent an overall CV estimate", {
  skip_if_not_installed("ordinal")
  data <- rubric_linear_data()
  fit <- rubric_linear_fit(data)
  assignments <- data.frame(item_id = data$rubric$item_id, fold = data$rubric$rubric_score)
  cv <- pairwiseLLM:::.rubric_cross_validate(fit, folds = 3L, fold_id = assignments)
  expect_identical(cv$status, "invalid")
  expect_true(all(cv$folds$status == "invalid"))
  expect_true(all(grepl("Missing requested rubric categories", cv$folds$reason)))
  expect_null(cv$metrics)
  expect_null(cv$calibration)
  expect_equal(nrow(cv$per_item), 0L)
  expect_match(cv$metadata$aggregate_reason, "no overall")
  refit <- pairwiseLLM:::.rubric_refit_labels
  count <- 0L
  testthat::local_mocked_bindings(.rubric_refit_labels = function(...) {
    count <<- count + 1L
    if (count == 1L) rlang::abort("Synthetic fit failure")
    out <- refit(...)
    if (count == 2L) out$diagnostics$ordinal$converged <- FALSE
    if (count == 3L) rlang::warn("Synthetic retained warning")
    out
  }, .package = "pairwiseLLM")
  skip_if_not_installed("withr")
  mixed <- pairwiseLLM:::.rubric_cross_validate(fit, folds = 3L)
  expect_identical(mixed$folds$status, c("invalid", "invalid", "valid"))
  expect_match(mixed$folds$reason[1L], "Synthetic fit failure")
  expect_match(mixed$folds$reason[2L], "converge")
  expect_match(mixed$folds$warnings[[3L]], "retained warning")
  expect_null(mixed$metrics)
  expect_equal(nrow(mixed$per_item), mixed$folds$n[3L])
  percentile <- pairwiseLLM::fit_rubric_calibration(data$cj, method = "percentile", trait = "organization", K = 3L)
  expect_error(pairwiseLLM:::.rubric_cross_validate(percentile), "prespecified ordinal")
})

test_that("monotone CV refits requested controls and retains uncertainty", {
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  skip_if_not_installed("withr")
  data <- rubric_monotone_data(n_unique = 9L)
  fit <- rubric_monotone_fit(data, k = 6L, sp = 0.5)
  withr::local_seed(2063)
  before <- .Random.seed
  cv <- pairwiseLLM:::.rubric_cross_validate(fit, folds = 3L, seed = 6L)
  expect_identical(cv$status, "complete")
  expect_identical(cv$metadata$controls, list(k = 6L, sp = 0.5))
  expect_true(all(is.finite(cv$per_item$rps)))
  expect_true(all(cv$per_item$theta_sd > 0))
  expect_false(any(cv$per_item$training_label_overlap))
  expect_identical(.Random.seed, before)
  expect_identical(pairwiseLLM:::.rubric_cross_validate(fit, folds = 3L, seed = 6L), cv)
  fit$backend$smoothing$sp_requested <- NULL
  seen <- list()
  refit <- pairwiseLLM:::.rubric_refit_labels
  testthat::local_mocked_bindings(.rubric_refit_labels = function(cj, rubric, method,
    calibration_design, levels, controls) {
    seen[[length(seen) + 1L]] <<- controls
    refit(cj, rubric, method, calibration_design, levels, controls)
  }, .package = "pairwiseLLM")
  estimated <- pairwiseLLM:::.rubric_cross_validate(fit, folds = 2L)
  expect_identical(estimated$status, "complete")
  expect_true(all(vapply(seen, function(x) is.null(x$sp) && x$k == 6L, logical(1L))))
})

test_that("linked-reference CV keeps the reference identity and only validates calibration", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("withr")
  data <- rubric_linked_fixture()
  fit <- rubric_linked_fit(data)
  before <- serialize(fit$reference, NULL)
  cv <- pairwiseLLM:::.rubric_cross_validate(fit, folds = 3L)
  expect_identical(cv$status, "complete")
  expect_identical(cv$metadata$validation_estimand, "reference_calibration")
  expect_setequal(cv$per_item$item_id, data$rubric$item_id)
  expect_identical(serialize(fit$reference, NULL), before)
  expect_false(any(cv$per_item$training_label_overlap))
  expect_identical(cv$metadata$calibration_design, "linked_anchors")
})

test_that("linear diagnostics match a direct backend likelihood comparison after serialization", {
  skip_if_not_installed("ordinal")
  data <- rubric_linear_data()
  fit <- unserialize(serialize(rubric_linear_fit(data), NULL))
  before <- serialize(fit, NULL)
  result <- pairwiseLLM::evaluate_rubric_predictions(fit, data$rubric, diagnostics = TRUE)
  diagnostic <- result$diagnostics$common_effect
  direct_data <- data.frame(category = ordered(data$rubric$rubric_score, levels = 1:3),
    z = (data$theta - fit$transformation$center) / fit$transformation$scale)
  direct <- ordinal::clm(category ~ z, nominal = ~z, data = direct_data, link = "logit")
  lr <- as.numeric(2 * (stats::logLik(direct) - stats::logLik(fit$backend$model)))
  expect_identical(diagnostic$status, "formal")
  expect_equal(diagnostic$statistic, max(0, lr))
  expect_equal(diagnostic$df, 1L)
  expect_equal(diagnostic$p_value, stats::pchisq(lr, 1L, lower.tail = FALSE))
  expect_false(diagnostic$review)
  expect_identical(result$diagnostics$data_source, "training_labels")
  expect_identical(result$diagnostics$stored$slope, fit$backend$slope)
  expect_identical(result$diagnostics$stored$standard_errors, fit$backend$standard_errors)
  expect_identical(result$diagnostics$stored$cj, fit$cj$diagnostics)
  expect_true(all(c("theta", "probability") %in% result$diagnostics$functional_form$group_by))
  alternative_labels <- data$rubric
  alternative_labels$rubric_score <- 4L - alternative_labels$rubric_score
  altered <- pairwiseLLM::evaluate_rubric_predictions(fit, alternative_labels, diagnostics = TRUE)
  expect_identical(altered$diagnostics, result$diagnostics)
  expect_identical(serialize(fit, NULL), before)
})

test_that("linear diagnostics flag explicit violations and retain failed-comparison reasons", {
  skip_if_not_installed("ordinal")
  theta <- seq(-2, 2, length.out = 13L)
  cumulative <- cbind(stats::plogis(-1 - 0.15 * theta), stats::plogis(1 - 1.05 * theta))
  counts <- round(80 * (cbind(cumulative, 1) - cbind(0, cumulative)))
  grid <- expand.grid(category = 1:3, theta = theta)
  rows <- grid[rep(seq_len(nrow(grid)), as.vector(t(counts))), ]
  data <- list(cj = rubric_linear_fixed(rows$theta),
    rubric = data.frame(item_id = paste0("item", seq_len(nrow(rows))), rubric_score = rows$category))
  fit <- rubric_linear_fit(data)
  before <- stats::predict(fit)
  expect_warning(result <- pairwiseLLM::evaluate_rubric_predictions(fit, data$rubric, diagnostics = TRUE),
    "Exploratory proportional-odds")
  expect_true(result$diagnostics$common_effect$review)
  expect_lt(result$diagnostics$common_effect$p_value, 0.05)
  expect_identical(stats::predict(fit), before)
  fit$diagnostics$ordinal$converged <- FALSE
  unavailable <- pairwiseLLM::evaluate_rubric_predictions(fit, data$rubric, diagnostics = TRUE)
  expect_identical(unavailable$diagnostics$common_effect$status, "unavailable")
  expect_match(unavailable$diagnostics$common_effect$reason, "baseline")
  testthat::local_mocked_bindings(.rubric_ordinal_available = function() FALSE, .package = "pairwiseLLM")
  missing <- pairwiseLLM::evaluate_rubric_predictions(fit, data$rubric, diagnostics = TRUE)
  expect_match(missing$diagnostics$common_effect$reason, "install.packages")
  expect_equal(missing$metrics, result$metrics)
})

test_that("nonlinear common-effect diagnostics are descriptive and distinct from functional form", {
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  skip_if_not_installed("withr")
  data <- rubric_monotone_data(n_unique = 9L)
  fit <- rubric_monotone_fit(data)
  before <- serialize(fit, NULL)
  result <- pairwiseLLM::evaluate_rubric_predictions(fit, data$rubric, diagnostics = TRUE)
  diagnostic <- result$diagnostics$common_effect
  expect_identical(diagnostic$status, "descriptive")
  expect_identical(diagnostic$boundaries$status, rep("descriptive", 2L))
  expect_equal(diagnostic$boundaries$expected_slope, c(-1, -1))
  expect_false("p_value" %in% names(diagnostic))
  expect_false("p_value" %in% names(diagnostic$boundaries))
  expect_identical(result$diagnostics$stored$smooth_edf, fit$backend$smooth_edf)
  expect_identical(result$diagnostics$stored$basis, fit$backend$basis)
  eta <- pairwiseLLM:::.rubric_monotone_eta(
    (fit$calibration_data$theta - fit$transformation$center) / fit$transformation$scale, fit)
  direct <- stats::glm(I(data$rubric$rubric_score <= 1L) ~ eta, family = stats::binomial())
  expect_equal(diagnostic$boundaries$slope[1L], unname(stats::coef(direct)[2L]))
  expect_equal(diagnostic$boundaries$slope_se[1L], sqrt(diag(stats::vcov(direct)))[[2L]])
  expect_identical(serialize(fit, NULL), before)
  testthat::local_mocked_bindings(.rubric_monotone_eta = function(z, object) rep(0, length(z)),
    .package = "pairwiseLLM")
  flat <- pairwiseLLM::evaluate_rubric_predictions(fit, data$rubric, diagnostics = TRUE)
  expect_true(all(flat$diagnostics$common_effect$boundaries$status == "unavailable"))
  expect_true(all(grepl("flat", flat$diagnostics$common_effect$boundaries$reason)))
})

test_that("failed or numerically invalid PO alternatives remain unavailable", {
  skip_if_not_installed("ordinal")
  data <- rubric_linear_data()
  fit <- rubric_linear_fit(data)
  evaluate <- function() pairwiseLLM::evaluate_rubric_predictions(fit, data$rubric, diagnostics = TRUE)
  broken <- testthat::with_mocked_bindings(evaluate(), clm = function(...) stop("diagnostic error"),
    .package = "ordinal")
  expect_match(broken$diagnostics$common_effect$reason, "diagnostic error")
  for (failure in c("convergence", "likelihood", "degrees")) {
    alternative <- fit$backend$model
    alternative$edf <- alternative$edf + 1L
    if (failure == "convergence") alternative$convergence$code <- 1L
    if (failure == "likelihood") alternative$logLik <- NA_real_
    if (failure == "degrees") alternative$edf <- alternative$edf - 1L
    result <- testthat::with_mocked_bindings(evaluate(), clm = function(...) alternative, .package = "ordinal")
    expect_identical(result$diagnostics$common_effect$status, "unavailable")
    expect_true(is.na(result$diagnostics$common_effect$p_value))
    expect_equal(result$metrics, broken$metrics)
  }
})

test_that("descriptive boundary diagnostic failures retain their reasons", {
  object <- list(K = 3L, levels = c("a", "b", "c"))
  data <- data.frame(z = 1:6, category = ordered(rep(1:3, 2L)))
  diagnostic <- function() pairwiseLLM:::.rubric_common_effect_descriptive(object, data)
  failed <- testthat::with_mocked_bindings(diagnostic(),
    .rubric_monotone_eta = function(...) stop("latent prediction failure"), .package = "pairwiseLLM")
  expect_identical(failed$status, "unavailable")
  expect_match(failed$reason, "latent prediction failure")
  testthat::local_mocked_bindings(.rubric_monotone_eta = function(...) 1:6, .package = "pairwiseLLM")
  broken <- testthat::with_mocked_bindings(diagnostic(), glm = function(...) stop("boundary failure"),
    .package = "stats")
  expect_true(all(broken$boundaries$status == "unavailable"))
  expect_true(all(grepl("boundary failure", broken$boundaries$reason)))
  original_glm <- stats::glm
  separated <- testthat::with_mocked_bindings(diagnostic(), glm = function(...) {
    model <- original_glm(...)
    model$boundary <- TRUE
    model
  }, .package = "stats")
  expect_true(all(grepl("separated", separated$boundaries$reason)))
  warned <- testthat::with_mocked_bindings(diagnostic(), glm = function(...) {
    warning("numerical boundary warning")
    original_glm(...)
  }, .package = "stats")
  expect_true(all(grepl("numerical boundary warning", warned$boundaries$reason)))
})
