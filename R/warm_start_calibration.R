# OOF calibration is a deployment transformation, not an independent validation estimate.
.warm_start_calibration_fit <- function(raw, z) {
  .warm_start_outcome_values(raw)
  .warm_start_outcome_values(z)
  if (length(raw) != length(z) || length(raw) < 3L) {
    rlang::abort("Calibration requires at least three aligned finite OOF predictions and outcomes.")
  }
  design <- cbind(intercept = 1, slope = raw)
  fit <- stats::lm.fit(design, z, tol = 1e-7)
  if (fit$rank != 2L || any(!is.finite(fit$coefficients))) {
    rlang::abort("Calibration is unidentified: constant/degenerate OOF predictions or nonfinite coefficients.")
  }
  list(status = "oof_linear", intercept = unname(fit$coefficients[1]),
    slope = unname(fit$coefficients[2]), n = length(raw), method = "ordinary_least_squares",
    qr_tolerance = 1e-7, source = "selected_hyperparameter_oof")
}

.validate_warm_start_calibration <- function(x) {
  if (identical(x, list(status = "uncalibrated", intercept = NULL, slope = NULL))) {
    return(invisible(x))
  }
  if (!is.list(x) || !identical(x$status, "oof_linear") ||
      !.warm_start_number(x$intercept) || !.warm_start_number(x$slope) ||
      !.warm_start_number(x$n, 3) || x$n != floor(x$n) ||
      !identical(x$method, "ordinary_least_squares") || !identical(x$qr_tolerance, 1e-7) ||
      !identical(x$source, "selected_hyperparameter_oof")) {
    rlang::abort("Unsupported calibration contract.")
  }
  invisible(x)
}

.warm_start_calibration_apply <- function(raw, calibration) {
  .validate_warm_start_calibration(calibration)
  .warm_start_outcome_values(raw)
  if (calibration$status != "oof_linear") rlang::abort("Learned OOF calibration is required.")
  out <- calibration$intercept + calibration$slope * raw
  if (any(!is.finite(out))) rlang::abort("Calibration produced nonfinite predictions.")
  out
}

.warm_start_validation_metrics <- function(predicted, observed) {
  .warm_start_outcome_values(predicted)
  .warm_start_outcome_values(observed)
  if (length(predicted) != length(observed) || length(predicted) < 3L) {
    rlang::abort("Validation requires at least three aligned predictions and outcomes.")
  }
  reasons <- character()
  r <- rho <- intercept <- slope <- NA_real_
  if (stats::sd(predicted) > 0 && stats::sd(observed) > 0) {
    r <- stats::cor(predicted, observed)
    rho <- stats::cor(predicted, observed, method = "spearman")
  } else {
    reasons <- c(reasons, "Correlations undefined for constant predictions or outcomes.")
  }
  diagnostic <- tryCatch(.warm_start_calibration_fit(predicted, observed),
    error = function(e) {
      reasons <<- c(reasons, conditionMessage(e))
      NULL
    })
  if (!is.null(diagnostic)) {
    intercept <- diagnostic$intercept
    slope <- diagnostic$slope
  }
  residual <- predicted - observed
  rmse <- sqrt(mean(residual^2))
  mae <- mean(abs(residual))
  if (!is.finite(rmse) || !is.finite(mae)) rlang::abort("Validation loss overflowed.")
  list(pearson_r = r, squared_pearson_r = r^2, spearman_rho = rho,
    rmse = rmse, mae = mae, calibration_intercept = intercept, calibration_slope = slope,
    undefined_reasons = reasons)
}
