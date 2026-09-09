#' Predict with an equal-weight warm-start ensemble
#' @param object A valid [ensemble_warm_start_models()] ensemble.
#' @param newdata Precomputed frozen feature rows, including `item_id` and
#'   `warm_start_schema` metadata. Mutually exclusive with `texts`.
#' @param ... Reserved; must be empty.
#' @param texts Character texts, paired positionally with explicit `ids`.
#' @param ids Item IDs for text input only.
#' @param python Optional explicitly selected Python interpreter for text extraction
#'   only; see [extract_warm_start_features()].
#' @return A `pairwiseLLM_warm_predictions` tibble in input order with `item_id`,
#'   one `component_<name>` calibrated prediction column per component,
#'   `ensemble_mean`, and `ensemble_sd`. SD is the sample SD (denominator k - 1)
#'   across all k calibrated predictions, including for exactly two components.
#'   Attributes `warm_start_schema` and `warm_start_model` retain schema and
#'   ensemble/component identity and format metadata. `component_predictions`
#'   retains complete named single-model results, including raw predictions;
#'   `component_columns` maps component names to output columns.
#' @details
#' Text features are extracted once. Each model independently applies its stored
#' preprocessing and OOF calibration to the same raw feature rows. Original BTL
#' scales never enter the average; calibration is not applied a second time.
#' Every component must return exactly the input IDs in order and finite numeric
#' predictions. A failure names the component; no model or row is dropped.
#' Precomputed prediction needs neither Python nor glmnet.
#'
#' Ensemble SD measures between-model disagreement. It is diagnostic, not a
#' Bayesian prior SD or a calibrated uncertainty estimate. These predictions do
#' not perform BTL prior conversion. No ensemble validation metric is inferred
#' from component validation metrics.
#' @family adaptive warm start
#' @seealso [ensemble_warm_start_models()], [summary.pairwiseLLM_warm_predictions()],
#'   [make_warm_start_prior()]
#' @examples
#' if (requireNamespace("glmnet", quietly = TRUE) &&
#'     requireNamespace("withr", quietly = TRUE)) {
#'   local({
#'     # Synthetic features illustrate the interface, not predictive validity.
#'     example_features <- function(seed) {
#'       withr::local_seed(seed)
#'       fields <- warm_start_feature_schema()$feature
#'       x <- as.data.frame(matrix(runif(15 * length(fields)), nrow = 15))
#'       names(x) <- fields
#'       x$n_tokens <- 11:25
#'       x$token_length_mean <- 2 + 10 * x$token_length_mean
#'       x$token_length_std <- 0.2 + x$token_length_std
#'       x$dale_chall_readability_score <- 5 + 20 * x$dale_chall_readability_score
#'       x <- data.frame(item_id = as.character(1:15), x)
#'       attr(x, "warm_start_schema") <- "writing_features_v1"
#'       x
#'     }
#'     features <- example_features(3103)
#'     theta <- 10 + 0.4 * features$n_tokens - 2 * features$token_length_mean
#'     # A small alpha grid keeps this example fast; the default has 41 values.
#'     model <- fit_warm_start_model(features$item_id, theta, "synthetic-a",
#'       features = features, alpha_grid = c(0, 1))
#'     features_b <- example_features(3104)
#'     theta_b <- 30 + features_b$n_tokens - 3 * features_b$token_length_mean
#'     model_b <- fit_warm_start_model(features_b$item_id, theta_b, "synthetic-b",
#'       features = features_b, alpha_grid = c(0, 1))
#'     ensemble <- ensemble_warm_start_models(assessment_a = model, assessment_b = model_b)
#'     predictions <- predict(ensemble, features)
#'     head(predictions)
#'   })
#' }
#' @export
predict.pairwiseLLM_warm_ensemble <- function(object, newdata = NULL, ..., texts = NULL,
                                             ids = NULL, python = NULL) {
  rlang::check_dots_empty()
  .validate_warm_start_ensemble(object)
  if (is.null(newdata) == is.null(texts)) rlang::abort("Supply exactly one of newdata or texts.")
  if (!is.null(texts)) {
    if (is.null(ids)) rlang::abort("Text prediction requires explicit ids.")
    newdata <- extract_warm_start_features(texts = texts, ids = ids, schema = object$schema, python = python)
  } else if (!is.null(ids) || !is.null(python)) {
    rlang::abort("ids and python apply only to text input.")
  }
  if (!is.data.frame(newdata) || !"item_id" %in% names(newdata)) {
    rlang::abort("newdata must be a feature data frame containing item_id.")
  }
  features <- .validate_warm_start_features(newdata, newdata$item_id, object$schema)
  results <- lapply(names(object$components), function(name) {
    .warm_start_component_context(name, {
      result <- stats::predict(object$components[[name]], features)
      if (!is.data.frame(result) || !identical(result$item_id, features$item_id) ||
          anyDuplicated(result$item_id)) rlang::abort("Component prediction IDs must match input IDs in order.")
      for (field in c("raw_prediction", "calibrated_prediction")) {
        values <- result[[field]]
        if (!is.numeric(values) || !is.null(dim(values)) || length(values) != nrow(features) ||
            any(!is.finite(values))) rlang::abort("Component predictions must be finite numeric vectors.")
      }
      result
    })
  })
  names(results) <- names(object$components)
  values <- do.call(cbind, lapply(results, function(x) x$calibrated_prediction))
  means <- rowMeans(values)
  sds <- apply(values, 1L, stats::sd)
  if (any(!is.finite(means)) || any(!is.finite(sds))) rlang::abort("Nonfinite ensemble mean or SD.")
  columns <- stats::setNames(paste0("component_", names(results)), names(results))
  out <- tibble::tibble(item_id = features$item_id)
  for (i in seq_along(results)) out[[columns[i]]] <- values[, i]
  out$ensemble_mean <- means
  out$ensemble_sd <- sds
  attr(out, "warm_start_schema") <- object$schema
  attr(out, "warm_start_model") <- list(artifact_type = "ensemble", format_version = object$format_version,
    outcome_definition = object$outcome$definition, weighting = object$weighting, metadata = object$metadata,
    components = lapply(results, attr, which = "warm_start_model"))
  attr(out, "component_columns") <- columns
  attr(out, "component_predictions") <- results
  class(out) <- c("pairwiseLLM_warm_predictions", class(out))
  out
}

#' Inspect ensemble predictions
#' @param object,x A `pairwiseLLM_warm_predictions` tibble.
#' @param ... Passed to tibble printing; summary arguments must be empty.
#' @return `summary()` returns item count, component names, and summaries of the
#'   mean and diagnostic sample SD. `print()` invisibly returns its input.
#' @family adaptive warm start
#' @seealso [predict.pairwiseLLM_warm_ensemble()], [make_warm_start_prior()]
#' @examples
#' if (requireNamespace("glmnet", quietly = TRUE) &&
#'     requireNamespace("withr", quietly = TRUE)) {
#'   local({
#'     # Synthetic features illustrate the interface, not predictive validity.
#'     example_features <- function(seed) {
#'       withr::local_seed(seed)
#'       fields <- warm_start_feature_schema()$feature
#'       x <- as.data.frame(matrix(runif(15 * length(fields)), nrow = 15))
#'       names(x) <- fields
#'       x$n_tokens <- 11:25
#'       x$token_length_mean <- 2 + 10 * x$token_length_mean
#'       x$token_length_std <- 0.2 + x$token_length_std
#'       x$dale_chall_readability_score <- 5 + 20 * x$dale_chall_readability_score
#'       x <- data.frame(item_id = as.character(1:15), x)
#'       attr(x, "warm_start_schema") <- "writing_features_v1"
#'       x
#'     }
#'     features <- example_features(3103)
#'     theta <- 10 + 0.4 * features$n_tokens - 2 * features$token_length_mean
#'     # A small alpha grid keeps this example fast; the default has 41 values.
#'     model <- fit_warm_start_model(features$item_id, theta, "synthetic-a",
#'       features = features, alpha_grid = c(0, 1))
#'     features_b <- example_features(3104)
#'     theta_b <- 30 + features_b$n_tokens - 3 * features_b$token_length_mean
#'     model_b <- fit_warm_start_model(features_b$item_id, theta_b, "synthetic-b",
#'       features = features_b, alpha_grid = c(0, 1))
#'     ensemble <- ensemble_warm_start_models(assessment_a = model, assessment_b = model_b)
#'     predictions <- predict(ensemble, features)
#'     summary(predictions)
#'     print(predictions)
#'   })
#' }
#' @export
summary.pairwiseLLM_warm_predictions <- function(object, ...) {
  rlang::check_dots_empty()
  list(n = nrow(object), components = names(attr(object, "component_columns")),
    ensemble_mean = summary(object$ensemble_mean), ensemble_sd = summary(object$ensemble_sd),
    sd_interpretation = "Between-model sample SD is diagnostic, not Bayesian prior SD.")
}

#' @rdname summary.pairwiseLLM_warm_predictions
#' @export
print.pairwiseLLM_warm_predictions <- function(x, ...) {
  cat("Warm-start ensemble predictions; sample SD is diagnostic, not Bayesian prior SD.\n")
  NextMethod("print")
  invisible(x)
}
