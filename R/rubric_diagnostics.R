# Diagnostic fits never replace the production calibration.

.rubric_capture <- function(fn) {
  warnings <- character()
  reason <- NULL
  value <- tryCatch(withCallingHandlers(fn(), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) {
    reason <<- conditionMessage(e)
    NULL
  })
  list(value = value, reason = reason, warnings = unique(warnings))
}

.rubric_po_diagnostic <- function(object, data) {
  result <- .rubric_capture(function() {
    if (!.rubric_ordinal_available()) {
      rlang::abort("Proportional-odds diagnostics require 'ordinal'; install.packages(\"ordinal\").")
    }
    baseline <- object$backend$model
    if (!isTRUE(object$diagnostics$ordinal$converged) || !isTRUE(baseline$convergence$code == 0)) {
      rlang::abort("The baseline calibration did not converge or is unidentified.")
    }
    # Explicit data avoids update() resolving the original fit's expired locals.
    formula <- stats::reformulate("z", response = "category", env = environment(.rubric_po_diagnostic))
    nominal <- stats::reformulate("z", env = environment(.rubric_po_diagnostic))
    alternative <- ordinal::clm(formula, nominal = nominal, data = data,
      link = "logit", threshold = "flexible", control = ordinal::clm.control(sign.location = "negative"))
    if (!isTRUE(alternative$convergence$code == 0)) {
      rlang::abort("The threshold-varying diagnostic fit did not converge or is unidentified.")
    }
    null_likelihood <- stats::logLik(baseline)
    alternative_likelihood <- stats::logLik(alternative)
    statistic <- as.numeric(2 * (alternative_likelihood - null_likelihood))
    df <- attr(alternative_likelihood, "df") - attr(null_likelihood, "df")
    if (length(statistic) != 1L || !is.finite(statistic) || statistic < -1e-8 ||
      length(df) != 1L || !is.finite(df) || df <= 0) {
      rlang::abort("The diagnostic likelihood comparison is not numerically valid.")
    }
    statistic <- max(0, statistic)
    list(statistic = statistic, df = df, p_value = stats::pchisq(statistic, df, lower.tail = FALSE),
      convergence = alternative$convergence)
  })
  out <- c(list(status = if (is.null(result$reason)) "formal" else "unavailable",
    method = "threshold_varying_likelihood_ratio", statistic = NA_real_, df = NA_real_, p_value = NA_real_,
    review_threshold = 0.05, review = FALSE, reason = result$reason, warnings = result$warnings),
    list(convergence = NULL))
  if (!is.null(result$value)) {
    out[names(result$value)] <- result$value
    out$review <- out$p_value < out$review_threshold
    if (out$review) {
      message <- paste0("Exploratory proportional-odds diagnostic p < .05; review common-effect adequacy ",
        "separately from linearity. No alternative production model was selected.")
      out$warnings <- unique(c(out$warnings, message))
      rlang::warn(message, class = "pairwiseLLM_rubric_assumption_diagnostics")
    }
  }
  out
}

.rubric_common_effect_descriptive <- function(object, data) {
  eta_result <- .rubric_capture(function() .rubric_monotone_eta(data$z, object))
  if (!is.null(eta_result$reason)) {
    return(list(status = "unavailable", method = "boundary_binomial_descriptive",
      reason = eta_result$reason, warnings = eta_result$warnings, boundaries = NULL))
  }
  eta <- eta_result$value
  rows <- lapply(seq_len(object$K - 1L), function(k) {
    result <- .rubric_capture(function() {
      if (diff(range(eta)) <= 1e-8) rlang::abort("The fitted latent effect is essentially flat.")
      data <- data.frame(observed = as.integer(data$category) <= k, eta = eta)
      model <- stats::glm(observed ~ eta, family = stats::binomial(), data = data)
      coefficients <- stats::coef(model)
      standard_errors <- sqrt(diag(stats::vcov(model)))
      if (!isTRUE(model$converged) || isTRUE(model$boundary) ||
        any(!is.finite(c(coefficients, standard_errors)))) {
        rlang::abort("Boundary regression is nonconverged, separated, or unidentified.")
      }
      c(intercept = unname(coefficients[1L]), slope = unname(coefficients[2L]),
        slope_se = unname(standard_errors[2L]))
    })
    valid <- !is.null(result$value) && !length(result$warnings)
    values <- if (valid) result$value else c(intercept = NA_real_, slope = NA_real_, slope_se = NA_real_)
    tibble::tibble(boundary = k, rubric_level = object$levels[k], status = if (valid) "descriptive" else "unavailable",
      intercept = values[["intercept"]], slope = values[["slope"]], slope_se = values[["slope_se"]],
      expected_slope = -1, converged = valid,
      reason = if (valid) NA_character_ else result$reason %||% paste(result$warnings, collapse = "; "),
      warnings = list(result$warnings))
  })
  list(status = "descriptive", method = "boundary_binomial_descriptive", conditional_on_estimated_effect = TRUE,
    interpretation = "Boundary slopes are descriptive; no omnibus test or automatic adequacy threshold is supplied.",
    reason = NULL, warnings = eta_result$warnings, boundaries = dplyr::bind_rows(rows))
}

.rubric_model_diagnostics <- function(object, bins) {
  stored <- list(category_counts = object$category_counts, calibration_range = object$calibration_range,
    transformation = object$transformation, ordinal = object$diagnostics$ordinal,
    cj = object$cj$diagnostics, reliability = object$cj$reliability, warnings = object$warnings,
    slope = object$backend$slope, standard_errors = object$backend$standard_errors,
    threshold_standard_errors = object$backend$threshold_standard_errors,
    convergence = object$backend$convergence, edf = object$backend$edf, smooth_edf = object$backend$smooth_edf,
    basis = object$backend$basis, smoothing = object$backend$smoothing)
  if (object$method == "percentile") {
    return(list(status = "not_applicable", stored = stored, functional_form = NULL,
      common_effect = list(status = "not_applicable", reason = "Percentile scoring fits no ordinal model.")))
  }
  data <- object$calibration_data[!is.na(object$calibration_data$category), ]
  data$z <- (data$theta - object$transformation$center) / object$transformation$scale
  data$category <- ordered(data$category, levels = seq_len(object$K))
  probabilities <- if (object$method == "ordinal_linear") {
    .rubric_ordinal_probabilities(data$theta, object)
  } else {
    .rubric_monotone_probabilities(data$theta, object)
  }
  functional_form <- .rubric_calibration_summary(probabilities, as.integer(data$category),
    data$theta, object$levels, bins)
  common_effect <- if (object$method == "ordinal_linear") .rubric_po_diagnostic(object, data) else
    .rubric_common_effect_descriptive(object, data)
  list(status = "computed", stored = stored, functional_form = functional_form,
    common_effect = common_effect, conditional_on_cj = TRUE, data_source = "training_labels")
}
