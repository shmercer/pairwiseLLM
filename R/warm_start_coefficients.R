#' Inspect calibrated standardized warm-start coefficients
#'
#' `warm_start_coefficients()` reports the fitted elastic-net coefficients after
#' applying the model's learned OOF linear calibration. It supports individual
#' calibrated warm-start models and equal-weight warm-start ensembles.
#'
#' @param object A calibrated `pairwiseLLM_warm_model` or
#'   `pairwiseLLM_warm_ensemble` object.
#' @param ... Reserved for future extensions; must be empty.
#'
#' @return For an individual model, a tibble with `feature`, `retained`, and
#'   `calibrated_std_coefficient`. For an ensemble, a tibble with `feature` and
#'   one `<component>_std_coefficient` column per component.
#'
#' @details
#' Predictors use each component model's fitted training medians, centers, and
#' sample SDs. The outcome is within-task standardized BT/BTL quality. For each
#' retained feature, the reported value is the stored elastic-net coefficient
#' multiplied by the learned OOF calibration slope. `NA` means preprocessing
#' removed the feature; zero means it was retained but has zero calibrated
#' weight.
#'
#' Signs describe direction conditional on the other included predictors.
#' Correlated predictors can share or trade fitted weight, so coefficient
#' magnitudes are not unique predictive-importance or causal-effect estimates.
#' Ensemble coefficients are shown side by side; they neither define an
#' aggregate coefficient nor change equal prediction weighting. Inspection from
#' an existing artifact needs neither Python nor glmnet.
#'
#' @seealso [fit_warm_start_model()], [ensemble_warm_start_models()],
#'   [predict.pairwiseLLM_warm_model()]
#' @examples
#' if (requireNamespace("glmnet", quietly = TRUE) &&
#'     requireNamespace("withr", quietly = TRUE)) {
#'   local({
#'     withr::local_seed(3103)
#'     fields <- warm_start_feature_schema()$feature
#'     x <- as.data.frame(matrix(runif(15 * length(fields)), nrow = 15))
#'     names(x) <- fields
#'     x$n_tokens <- 11:25
#'     x$token_length_mean <- 2 + 10 * x$token_length_mean
#'     x$token_length_std <- 0.2 + x$token_length_std
#'     x$dale_chall_readability_score <- 5 + 20 * x$dale_chall_readability_score
#'     x <- data.frame(item_id = as.character(1:15), x)
#'     attr(x, "warm_start_schema") <- "writing_features_v1"
#'     theta <- 10 + 0.4 * x$n_tokens - 2 * x$token_length_mean
#'     model <- fit_warm_start_model(x$item_id, theta, "synthetic-example",
#'       features = x, alpha_grid = c(0, 1))
#'     warm_start_coefficients(model)
#'   })
#' }
#' @export
warm_start_coefficients <- function(object, ...) {
  UseMethod("warm_start_coefficients")
}

.warm_start_model_coefficients <- function(model) {
  if (!identical(model$calibration$status, "oof_linear")) {
    rlang::abort(paste0(
      "Calibrated standardized coefficients require a warm-start model fitted ",
      "with learned OOF calibration. Use fit_warm_start_model()."
    ))
  }
  retained <- model$features %in% model$preprocessing$retained
  coefficients <- rep(NA_real_, length(model$features))
  coefficients[retained] <- unname(
    model$coefficients[model$features[retained]] * model$calibration$slope
  )
  tibble::tibble(
    feature = model$features,
    retained = retained,
    calibrated_std_coefficient = coefficients
  )
}

#' @rdname warm_start_coefficients
#' @export
warm_start_coefficients.pairwiseLLM_warm_model <- function(object, ...) {
  rlang::check_dots_empty()
  .validate_warm_start_model(object)
  .warm_start_model_coefficients(object)
}

#' @rdname warm_start_coefficients
#' @export
warm_start_coefficients.pairwiseLLM_warm_ensemble <- function(object, ...) {
  rlang::check_dots_empty()
  .validate_warm_start_ensemble(object)
  out <- tibble::tibble(feature = object$features)
  for (name in names(object$components)) {
    component <- .warm_start_model_coefficients(object$components[[name]])
    if (!identical(component$feature, object$features)) {
      rlang::abort("Warm-start ensemble component features are misaligned.")
    }
    out[[paste0(name, "_std_coefficient")]] <- component$calibrated_std_coefficient
  }
  out
}
