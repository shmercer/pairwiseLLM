# Proportional-odds calibration conditional on accepted CJ point locations.

.rubric_ordinal_available <- function() requireNamespace("ordinal", quietly = TRUE)

.rubric_fit_ordinal_linear <- function(object) {
  if (!.rubric_ordinal_available()) {
    rlang::abort(paste0("Linear ordinal calibration requires the optional package 'ordinal'. ",
      "Install it with install.packages(\"ordinal\")."),
      class = "pairwiseLLM_rubric_dependency_missing")
  }
  data <- object$calibration_data[!is.na(object$calibration_data$category), ]
  center <- mean(data$theta)
  scale <- stats::sd(data$theta)
  if (!is.finite(center) || !is.finite(scale) || scale <= 0) {
    rlang::abort("Labeled calibration CJ scores must have a finite, nonzero standard deviation.")
  }
  data <- data.frame(category = ordered(data$category, levels = seq_len(object$K)),
    z = (data$theta - center) / scale)
  if (any(!is.finite(data$z))) rlang::abort("Standardized calibration scores must be finite.")
  conditions <- new.env(parent = emptyenv())
  conditions$warnings <- character()
  capture_warning <- function(w) {
    conditions$warnings <- c(conditions$warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
  # Keep the formula environment independent of this fit's local data/object.
  formula <- stats::reformulate("z", response = "category", env = environment(.rubric_fit_ordinal_linear))
  model <- tryCatch(withCallingHandlers(
    ordinal::clm(formula, data = data, link = "logit", threshold = "flexible",
      control = ordinal::clm.control(sign.location = "negative")),
    warning = capture_warning),
    error = function(e) rlang::abort("Linear ordinal calibration could not be fitted.", parent = e))
  object$transformation <- list(center = center, scale = scale)
  object$backend <- list(name = "ordinal::clm", version = as.character(utils::packageVersion("ordinal")),
    link = "logit", threshold = "flexible", model = model,
    thresholds = model$alpha, slope = unname(model$beta), convergence = model$convergence)
  object$status <- "fitted"
  .rubric_validate_ordinal_linear(object)

  coefficient_names <- names(model$coefficients)
  covariance <- tryCatch(withCallingHandlers(stats::vcov(model), warning = capture_warning),
    error = function(e) {
      conditions$warnings <- c(conditions$warnings,
        paste0("Coefficient covariance unavailable: ", conditionMessage(e)))
      NULL
    })
  warnings <- conditions$warnings
  covariance_ok <- is.matrix(covariance) &&
    identical(dim(covariance), rep(object$K, 2L)) && all(is.finite(covariance)) &&
    all(diag(covariance) >= 0)
  if (!covariance_ok) {
    covariance <- matrix(NA_real_, object$K, object$K, dimnames = list(coefficient_names, coefficient_names))
    warnings <- c(warnings, "Coefficient standard errors are unavailable; calibration uncertainty is unreliable.")
  }
  object$backend$vcov <- covariance
  object$backend$standard_errors <- sqrt(diag(covariance))
  object$backend$cutpoints_z <- object$backend$cutpoints_theta <- NULL
  slope <- object$backend$slope
  if (slope > 0) {
    object$backend$cutpoints_z <- model$alpha / slope
    object$backend$cutpoints_theta <- center + scale * object$backend$cutpoints_z
    if (any(!is.finite(c(object$backend$cutpoints_z, object$backend$cutpoints_theta)))) {
      warnings <- c(warnings, "Median cutpoints exceed numeric range; use category probabilities for prediction.")
    }
  } else {
    warnings <- c(warnings, paste0("The ordinal slope is zero or negative; check coding, weak calibration, ",
      "or disagreement with the rubric. Orientation has not been reversed."))
  }
  singletons <- names(object$category_counts)[object$category_counts == 1L]
  if (length(singletons)) {
    warnings <- c(warnings, paste0("Sparse calibration: only one labeled response in categories ",
      paste(singletons, collapse = ", "), ". Inspect threshold uncertainty."))
  }
  converged <- identical(model$convergence$code, 0L) || identical(model$convergence$code, 0)
  if (!converged) {
    warnings <- c(warnings, paste0("Ordinal convergence/identification requires review: ",
      paste(model$convergence$messages, collapse = "; ")))
  }
  object$diagnostics$category_probabilities_available <- TRUE
  object$diagnostics$ordinal <- list(converged = converged, convergence = model$convergence,
    max_gradient = model$maxGradient, hessian_condition = model$cond.H,
    covariance_available = covariance_ok, nonpositive_slope = slope <= 0,
    singleton_categories = singletons, conditional_on_cj = TRUE)
  object$warnings <- unique(c(object$warnings, warnings))
  if (length(warnings)) {
    rlang::warn(paste(unique(warnings), collapse = "\n"), class = "pairwiseLLM_rubric_ordinal_diagnostics")
  }
  .rubric_validate_calibration(object)
  object
}

.rubric_validate_ordinal_linear <- function(object) {
  backend <- object$backend
  transformation <- object$transformation
  valid_scalar <- function(x) is.numeric(x) && is.null(dim(x)) && length(x) == 1L && is.finite(x)
  if (!object$calibration_design %in% c("same_set", "linked_anchors") || !is.list(backend) ||
    !identical(backend$name, "ordinal::clm") || !identical(backend$link, "logit") ||
    !identical(backend$threshold, "flexible") || !valid_scalar(backend$slope) ||
    !is.numeric(backend$thresholds) || !is.null(dim(backend$thresholds)) ||
    length(backend$thresholds) != object$K - 1L || any(!is.finite(backend$thresholds)) ||
    any(diff(backend$thresholds) <= 0) || !is.list(transformation) ||
    !valid_scalar(transformation$center) || !valid_scalar(transformation$scale) || transformation$scale <= 0 ||
    !is.numeric(object$calibration_range) || length(object$calibration_range) != 2L ||
    any(!is.finite(object$calibration_range)) || diff(object$calibration_range) <= 0) {
    rlang::abort("Invalid fitted linear ordinal coefficients, transformation, range, or design.")
  }
  invisible(object)
}

.rubric_ordinal_probabilities <- function(theta, object) {
  z <- (theta - object$transformation$center) / object$transformation$scale
  if (any(!is.finite(z))) rlang::abort("Standardized prediction scores must be finite.")
  cumulative <- stats::plogis(outer(z, object$backend$thresholds,
    function(z, threshold) threshold - object$backend$slope * z))
  probabilities <- cbind(cumulative, 1) - cbind(0, cumulative)
  colnames(probabilities) <- as.character(object$levels)
  .rubric_check_probabilities(probabilities, object$levels, length(theta))
  probabilities
}

.rubric_ordinal_decisions <- function(probabilities) {
  cumulative <- probabilities
  for (k in seq.int(2L, ncol(probabilities))) {
    cumulative[, k] <- cumulative[, k - 1L] + probabilities[, k]
  }
  list(median = as.integer(rowSums(cumulative < 0.5) + 1L),
    mode = max.col(probabilities, ties.method = "first"),
    expected_level = as.vector(probabilities %*% seq_len(ncol(probabilities))))
}

.rubric_predict_ordinal_linear <- function(object, newdata, hard_score) {
  items <- .rubric_same_set_prediction_items(object, newdata)
  probabilities <- .rubric_ordinal_probabilities(items$theta, object)
  .rubric_ordinal_prediction_table(object, items, probabilities, hard_score)
}

.rubric_ordinal_prediction_table <- function(object, items, probabilities, hard_score) {
  .rubric_check_probabilities(probabilities, object$levels, nrow(items))
  decisions <- .rubric_ordinal_decisions(.rubric_probability_copy(probabilities, object$levels))
  category <- decisions[[hard_score]]
  tibble::tibble(item_id = items$item_id, theta = items$theta, category = category,
    rubric_score = object$levels[category],
    extrapolated = items$theta < object$calibration_range[[1L]] |
      items$theta > object$calibration_range[[2L]],
    probabilities = lapply(seq_len(nrow(probabilities)), function(i) probabilities[i, ]),
    median_category = decisions$median, modal_category = decisions$mode,
    expected_level = decisions$expected_level)
}
