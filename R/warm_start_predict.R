#' Predict relative quality from a portable warm-start model
#'
#' @param object A valid `pairwiseLLM_warm_model` object.
#' @param newdata Precomputed feature rows with `item_id`, all original frozen
#'   schema columns, and matching `warm_start_schema` metadata. IDs must be unique,
#'   nonmissing and nonblank. Numeric IDs normalize to character. Extra columns
#'   are ignored; required columns cannot be omitted even if training removed them.
#' @param ... Reserved for future extensions; must be empty.
#'
#' @return A tibble in `newdata` row order with character `item_id`, numeric
#'   `raw_prediction`, and numeric `calibrated_prediction`. Raw predictions are on
#'   the within-task standardized outcome scale, not the original BT/BTL scale.
#'   Public fits apply the stored OOF calibration intercept and slope to raw values.
#'   For uncalibrated core fits, calibrated predictions are `NA_real_`, never
#'   identity-calibrated substitutes. Attributes `warm_start_schema` and
#'   `warm_start_model` record schema identity and model metadata (format version,
#'   task ID, outcome definition, and calibration status), respectively.
#'
#' @details
#' Prediction applies training medians, centers, and sample SDs, followed by the
#' stored intercept and coefficients. It does not recompute preprocessing, train
#' a model, load glmnet, initialize Python, or check a Python environment.
#' Predictions are not calibrated Bayesian prior means or prior standard
#' deviations. See [pairwiseLLM_warm_model] for the portable model contract.
#' @export
predict.pairwiseLLM_warm_model <- function(object, newdata, ...) {
  rlang::check_dots_empty()
  .validate_warm_start_model(object)
  if (!is.data.frame(newdata) || !"item_id" %in% names(newdata)) {
    rlang::abort("`newdata` must be a feature data frame containing item_id.")
  }
  features <- .validate_warm_start_features(newdata, newdata$item_id, object$schema)
  scaled <- .warm_start_preprocess_apply(as.matrix(features[, -1, drop = FALSE]), object$preprocessing)
  raw <- as.numeric(object$intercept + scaled %*% object$coefficients)
  if (any(!is.finite(raw))) rlang::abort("Warm-start prediction produced nonfinite values.")
  out <- tibble::tibble(item_id = features$item_id, raw_prediction = raw,
    calibrated_prediction = if (object$calibration$status == "oof_linear") {
      .warm_start_calibration_apply(raw, object$calibration)
    } else {
      rep(NA_real_, length(raw))
    })
  attr(out, "warm_start_schema") <- object$schema
  attr(out, "warm_start_model") <- list(format_version = object$format_version,
    task_id = object$training$task_id, outcome_definition = object$outcome$definition,
    calibration_status = object$calibration$status)
  out
}
