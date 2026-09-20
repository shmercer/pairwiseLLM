#' Average algorithms trained on the same task
#'
#' Unlike [ensemble_warm_start_models()], this constructor requires auditable
#' shared training outcomes and evaluation partitions. Validation averages the
#' untouched outer-held-out calibrated predictions, not full-fit predictions or
#' component validation metrics.
#' @param ... At least two explicitly named, fully audited format-3
#'   [fit_warm_start_model()] objects. Names must be nonblank and unique. Load
#'   saved components before calling this constructor. Nested ensembles fail.
#' @return A standalone `pairwiseLLM_warm_algorithm_ensemble`, format 1, with
#'   `artifact_type = "algorithm_ensemble"`, ordered components, common schema,
#'   outcome and CV identity, and honest nested-CV ensemble validation.
#' @details
#' Components must share the exact task, ordered IDs, original outcomes, schema,
#'   CV plan and outer observed values/folds. Every component requires learned
#'   `oof_linear` calibration. Legacy and reduced models cannot establish the
#'   necessary audit evidence and must not be used to construct a new ensemble.
#' Deployment averages each component's calibrated prediction with equal weight.
#' There are no learned weights, automatic component selection or ensemble-level
#' recalibration. Validation calibration intercept/slope are diagnostics only.
#' Component names identify members; distinct engine names are not required.
#'
#' [prepare_warm_start_model()] can reduce an existing ensemble to summary-only
#' audit status. Its portable component payloads and identity survive, but the
#' stored validation summaries cannot then be recomputed. Between-component SD
#' is diagnostic disagreement, never a Bayesian prior SD.
#' @family adaptive warm start
#' @seealso [make_warm_start_cv_plan()], [make_warm_start_prior()],
#'   [predict.pairwiseLLM_warm_algorithm_ensemble()]
#' @examples
#' if (requireNamespace("glmnet", quietly = TRUE) &&
#'     requireNamespace("pls", quietly = TRUE) && requireNamespace("withr", quietly = TRUE)) {
#'   local({
#'     withr::local_seed(259L)
#'     fields <- warm_start_feature_schema()$feature
#'     x <- as.data.frame(matrix(runif(20 * length(fields)), nrow = 20))
#'     names(x) <- fields
#'     x$n_tokens <- 11:30
#'     x <- data.frame(item_id = as.character(1:20), x)
#'     attr(x, "warm_start_schema") <- "writing_features_v1"
#'     theta <- x$n_tokens - 3 * x$token_length_mean
#'     plan <- make_warm_start_cv_plan(x$item_id, theta, "synthetic", seed = 259L)
#'     en <- fit_warm_start_model(x$item_id, theta, "synthetic", features = x,
#'       cv_plan = plan, alpha_grid = c(0, 1))
#'     pls <- fit_warm_start_model(x$item_id, theta, "synthetic", features = x,
#'       cv_plan = plan, engine = "pls", engine_control = list(ncomp = 1L))
#'     ensemble <- ensemble_warm_start_algorithms(elastic_net = en, pls = pls)
#'     print(ensemble)
#'     summary(ensemble)$validation$metrics
#'     predictions <- predict(ensemble, x)
#'     make_warm_start_prior(predictions, prior_sd = 0.5)
#'   })
#' }
#' @export
ensemble_warm_start_algorithms <- function(...) {
  components <- list(...)
  first <- .warm_start_algorithm_components(components, "full")
  out <- structure(list(format_version = 1L, artifact_type = "algorithm_ensemble",
    audit_status = "full", components = components, schema = first$schema,
    features = first$features, outcome = first$outcome, cv_identity = first$cv_identity,
    weighting = "equal", validation = .warm_start_algorithm_validation(components)),
    class = "pairwiseLLM_warm_algorithm_ensemble")
  .validate_warm_start_algorithm_ensemble(out)
  out
}

.warm_start_algorithm_components <- function(components, audit_status) {
  if (!is.list(components) || length(components) < 2L || is.null(names(components))) {
    rlang::abort("An algorithm ensemble requires at least two explicitly named format-3 models.")
  }
  .warm_start_component_names(names(components))
  first <- components[[1]]
  for (name in names(components)) {
    .warm_start_component_context(name, {
      model <- components[[name]]
      .validate_warm_start_model(model)
      if (!identical(model$format_version, 3L) || !identical(model$audit_status, audit_status) ||
          !identical(model$calibration$status, "oof_linear")) {
        rlang::abort(paste0("Requires a ", audit_status, " format-3 model with learned oof_linear calibration."))
      }
      for (field in c("schema", "features", "outcome", "cv_identity")) {
        if (!identical(model[[field]], first[[field]])) {
          rlang::abort(paste0("Same-task component ", field, " is incompatible."))
        }
      }
      if (audit_status == "full" &&
          (!identical(model$cv_plan, first$cv_plan) ||
            !identical(model$validation$predictions[c("item_id", "fold", "observed")],
              first$validation$predictions[c("item_id", "fold", "observed")]))) {
        rlang::abort("Same-task components require identical plans and aligned outer IDs, folds and observed outcomes.")
      }
    })
  }
  first
}

.validate_warm_start_algorithm_identity <- function(identity) {
  invalid <- function() rlang::abort("Invalid algorithm ensemble shared CV identity.")
  fields <- c("format_version", "digest", "task_id", "n", "outcome_digest", "seed",
    "outer_folds", "inner_folds", "rng_kind")
  if (!is.list(identity) || !identical(names(identity), fields) ||
      !identical(identity$format_version, 1L) || !.warm_start_string(identity$task_id) ||
      !.warm_start_number(identity$n, 3, .Machine$integer.max) ||
      !identical(identity$n, as.integer(identity$n))) invalid()
  for (field in c("digest", "outcome_digest")) {
    if (!.warm_start_string(identity[[field]]) || !grepl("^[a-f0-9]{32}$", identity[[field]])) invalid()
  }
  for (field in c("seed", "outer_folds", "inner_folds")) {
    value <- identity[[field]]
    if (!.warm_start_number(value, 0, .Machine$integer.max) || !identical(value, as.integer(value))) invalid()
  }
  .warm_start_fold_count(identity$outer_folds, identity$n)
  .warm_start_fold_count(identity$inner_folds, identity$n - ceiling(identity$n / identity$outer_folds))
  .warm_start_plan_rng(identity$rng_kind)
  invisible(identity)
}

.warm_start_algorithm_validation <- function(components) {
  first <- components[[1]]
  predictions <- first$validation$predictions[c("item_id", "fold", "observed")]
  predictions$calibrated_prediction <- rowMeans(do.call(cbind, lapply(components,
    function(x) x$validation$predictions$calibrated_prediction)))
  list(method = "nested_cv", source = "aligned_outer_calibrated",
    outer_folds = first$cv_identity$outer_folds, inner_folds = first$cv_identity$inner_folds,
    predictions = predictions, metrics = .warm_start_validation_metrics(
      predictions$calibrated_prediction, predictions$observed))
}

.validate_warm_start_algorithm_ensemble <- function(model) {
  invalid <- function() rlang::abort("Invalid portable same-task algorithm ensemble contract or validation evidence.")
  required <- c("format_version", "artifact_type", "audit_status", "components", "schema", "features",
    "outcome", "cv_identity", "weighting", "validation")
  if (!is.list(model) || !identical(class(model), "pairwiseLLM_warm_algorithm_ensemble") ||
      anyDuplicated(names(model)) || !all(required %in% names(model)) ||
      !all(names(model) %in% c(required, "metadata")) || !identical(model$format_version, 1L) ||
      !identical(model$artifact_type, "algorithm_ensemble") || !identical(model$weighting, "equal") ||
      !.warm_start_string(model$audit_status) || !model$audit_status %in% c("full", "summary_only")) invalid()
  first <- .warm_start_algorithm_components(model$components, model$audit_status)
  for (field in c("schema", "features", "outcome", "cv_identity")) {
    if (!identical(model[[field]], first[[field]])) invalid()
  }
  v <- model$validation
  if (!is.list(v) || !identical(v$method, "nested_cv") ||
      !identical(v$source, "aligned_outer_calibrated") ||
      !identical(v$outer_folds, model$cv_identity$outer_folds) ||
      !identical(v$inner_folds, model$cv_identity$inner_folds)) invalid()
  if (model$audit_status == "full") {
    expected <- .warm_start_algorithm_validation(model$components)
    p <- v$predictions
    identity <- c("item_id", "fold", "observed")
    if (!is.data.frame(p) || !identical(names(p), names(expected$predictions)) ||
        !identical(p[identity], expected$predictions[identity])) invalid()
    values <- p$calibrated_prediction
    if (!is.numeric(values) || !is.null(dim(values)) || any(!is.finite(values)) ||
        !.warm_start_audit_equal(values, expected$predictions$calibrated_prediction) ||
        !identical(names(v), names(expected)) || !.warm_start_audit_equal(v$metrics, expected$metrics)) invalid()
  } else {
    .validate_warm_start_summary_metrics(v$metrics)
    if (!identical(model, .warm_start_algorithm_reduced(model))) invalid()
  }
  if (!.warm_start_portable(model)) invalid()
  if ("metadata" %in% names(model)) .validate_warm_start_metadata(model$metadata)
  invisible(model)
}

.warm_start_algorithm_reduced <- function(model) {
  out <- model[c("format_version", "artifact_type", "audit_status", "schema", "features",
    "outcome", "cv_identity", "weighting")]
  out$audit_status <- "summary_only"
  out$validation <- model$validation[c("method", "source", "outer_folds", "inner_folds", "metrics")]
  out$validation$metrics <- out$validation$metrics[c("pearson_r", "squared_pearson_r", "spearman_rho",
    "rmse", "mae", "calibration_intercept", "calibration_slope", "undefined_reasons")]
  if (!is.null(model$metadata)) out$metadata <- model$metadata
  out <- .warm_start_plain(out)
  # Reduce components separately: the general plain copier removes matrix dimensions.
  out$components <- lapply(model$components, .warm_start_reduced)
  structure(out, class = "pairwiseLLM_warm_algorithm_ensemble")
}

#' Predict with a same-task algorithm ensemble
#' @inheritParams predict.pairwiseLLM_warm_ensemble
#' @param object A full or summary-only [ensemble_warm_start_algorithms()] artifact.
#' @return A `pairwiseLLM_warm_predictions` tibble with ordered component calibrated
#'   predictions, their `ensemble_mean`, and diagnostic sample `ensemble_sd`.
#'   Component results and shared CV identity are retained as metadata. Prediction
#'   uses stored numeric payloads without loading any fitting backend. Text input
#'   extracts features once; precomputed features require no Python.
#' @family adaptive warm start
#' @seealso [ensemble_warm_start_algorithms()], [make_warm_start_prior()]
#' @inherit ensemble_warm_start_algorithms examples
#' @export
predict.pairwiseLLM_warm_algorithm_ensemble <- function(object, newdata = NULL, ..., texts = NULL,
                                                       ids = NULL, python = NULL) {
  rlang::check_dots_empty()
  .validate_warm_start_algorithm_ensemble(object)
  .warm_start_ensemble_predict(object, newdata, texts, ids, python, "algorithm_ensemble")
}

#' Inspect a same-task algorithm ensemble
#' @param object,x A full or summary-only [ensemble_warm_start_algorithms()] artifact.
#' @param ... Reserved; must be empty.
#' @return `summary()` returns shared identity, component summaries, audit status
#'   and honest outer ensemble validation metrics. Summary-only metrics are
#'   explicitly labeled and cannot be recomputed without the original audit.
#'   `print()` returns its input invisibly.
#' @family adaptive warm start
#' @inherit ensemble_warm_start_algorithms examples
#' @export
summary.pairwiseLLM_warm_algorithm_ensemble <- function(object, ...) {
  rlang::check_dots_empty()
  .validate_warm_start_algorithm_ensemble(object)
  list(format_version = object$format_version, artifact_type = object$artifact_type,
    schema = object$schema, target = object$outcome, cv_identity = object$cv_identity,
    weighting = object$weighting, audit_status = object$audit_status,
    validation = object$validation[c("method", "source", "outer_folds", "inner_folds", "metrics")],
    components = lapply(object$components, summary), metadata = object$metadata,
    sd_interpretation = "Between-algorithm sample SD is diagnostic, not Bayesian prior SD.")
}

#' @rdname summary.pairwiseLLM_warm_algorithm_ensemble
#' @export
print.pairwiseLLM_warm_algorithm_ensemble <- function(x, ...) {
  info <- summary(x, ...)
  cat("Same-task warm-start algorithm ensemble:", length(info$components), "equally weighted models\n")
  cat("Task:", info$cv_identity$task_id, "; n =", info$cv_identity$n, "; audit:", info$audit_status, "\n")
  cat("Components:", paste(names(info$components), collapse = ", "), "\n")
  cat("Outer-held-out ensemble RMSE:", info$validation$metrics$rmse, "\n")
  cat(info$sd_interpretation, "\n")
  invisible(x)
}
