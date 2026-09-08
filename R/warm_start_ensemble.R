#' Combine independently trained warm-start models
#'
#' An ensemble averages calibrated standardized predictions from separate task
#' models. Each task standardizes its own outcome, so original BT/BTL scales need
#' not be linked. Callers are responsible for independently sourced training data;
#' task labels cannot establish independence.
#' @param ... Individual calibrated models, path strings, or reference lists with
#'   `path`, or `name` and optional `source`, as in [load_warm_start_model()].
#'   Supply argument names for explicit component identities. Unnamed arguments
#'   receive `component_<position>`; duplicate final names and blank supplied names
#'   are rejected. Order and supplied names are preserved. Nested ensembles fail.
#' @return A portable `pairwiseLLM_warm_ensemble` list with ensemble format version
#'   1, named `components`, common `schema`, original `features`, standardized
#'   `outcome` definition/sample-SD convention, and `weighting = "equal"`.
#' @details
#' Components must have learned OOF calibration and compatible frozen schemas.
#' Full-audit model format 1 and summary-only model format 2 can be mixed. Each
#' component keeps its own preprocessing, calibration, and training metadata.
#' References are loaded once; prediction uses stored components. Character
#' references always mean paths, never implicit registry names. Registry names
#' retain existing normalization and source ambiguity rules; component names do
#' not use registry normalization. Custom or learned weights are not supported.
#'
#' [prepare_warm_start_model()] can add ensemble metadata or explicitly reduce
#' all component audits. Ordinary save/load is lossless. Ensemble format 1 is
#' independent of component formats and remains 1 after reduction.
#' @export
ensemble_warm_start_models <- function(...) {
  components <- list(...)
  if (length(components) < 2L) rlang::abort("An ensemble requires at least two component models.")
  labels <- names(components)
  if (is.null(labels)) labels <- rep("", length(components))
  unnamed <- labels == ""
  labels[unnamed] <- paste0("component_", which(unnamed))
  .warm_start_component_names(labels)
  names(components) <- labels
  components <- lapply(seq_along(components), function(i) {
    .warm_start_component_context(labels[i], {
      value <- components[[i]]
      if (is.character(value)) value <- load_warm_start_model(path = value)
      if (is.list(value) && !is.object(value)) {
        if (is.null(names(value)) || anyDuplicated(names(value)) ||
            !all(names(value) %in% c("path", "name", "source"))) {
          rlang::abort("Invalid model reference; supply path or name and optional source.")
        }
        value <- do.call(load_warm_start_model, value)
      }
      .validate_warm_start_model(value)
      value
    })
  })
  names(components) <- labels
  first <- components[[1]]
  out <- structure(list(format_version = 1L, components = components, schema = first$schema,
    features = first$features, outcome = first$outcome[c("definition", "sd_convention")],
    weighting = "equal"), class = "pairwiseLLM_warm_ensemble")
  .validate_warm_start_ensemble(out)
  out
}

.warm_start_component_names <- function(labels) {
  if (!is.character(labels) || anyNA(labels) || any(!nzchar(trimws(labels))) ||
      anyDuplicated(labels)) rlang::abort("Component names must be nonblank and unique.")
  invisible(labels)
}

.warm_start_component_context <- function(name, expr) {
  tryCatch(expr, error = function(e) {
    rlang::abort(paste0("Warm-start component '", name, "' failed."), parent = e)
  })
}

.validate_warm_start_ensemble <- function(model) {
  invalid <- function() rlang::abort("Invalid portable warm-start ensemble contract.")
  required <- c("format_version", "components", "schema", "features", "outcome", "weighting")
  if (!identical(class(model), "pairwiseLLM_warm_ensemble") || !is.list(model) ||
      anyDuplicated(names(model)) || !all(required %in% names(model)) ||
      !all(names(model) %in% c(required, "metadata"))) invalid()
  if (!identical(model$format_version, 1L)) {
    rlang::abort("Unsupported warm-start ensemble format version; update pairwiseLLM or re-export format 1.")
  }
  if (!is.list(model$components) || length(model$components) < 2L ||
      is.null(names(model$components)) || !identical(model$weighting, "equal")) invalid()
  .warm_start_component_names(names(model$components))
  if (!identical(model$features, warm_start_feature_schema(model$schema)$feature) ||
      !identical(model$outcome, list(definition = "within_task_z", sd_convention = "sample"))) invalid()
  for (name in names(model$components)) {
    .warm_start_component_context(name, {
      component <- model$components[[name]]
      .validate_warm_start_model(component)
      if (!identical(component$schema, model$schema) || !identical(component$features, model$features) ||
          !identical(component$outcome[c("definition", "sd_convention")], model$outcome)) {
        rlang::abort("Component schema or standardized target is incompatible.")
      }
      if (!identical(component$calibration$status, "oof_linear")) {
        rlang::abort("Component requires learned oof_linear calibration; uncalibrated models cannot be averaged.")
      }
    })
  }
  if (!.warm_start_portable(model)) invalid()
  if ("metadata" %in% names(model)) .validate_warm_start_metadata(model$metadata)
  invisible(model)
}

.validate_warm_start_artifact <- function(model) {
  if (inherits(model, "pairwiseLLM_warm_ensemble")) {
    .validate_warm_start_ensemble(model)
  } else {
    .validate_warm_start_model(model)
  }
  invisible(model)
}

.warm_start_audit_status <- function(model) {
  if (!inherits(model, "pairwiseLLM_warm_ensemble")) {
    return(if (model$format_version == 2L) "summary_only" else "full")
  }
  status <- unique(vapply(model$components, .warm_start_audit_status, character(1)))
  if (length(status) == 1L) status else "mixed"
}

#' Inspect a warm-start ensemble
#' @param object,x A `pairwiseLLM_warm_ensemble`.
#' @param ... Reserved; must be empty.
#' @return `summary()` returns component summaries and ensemble contract details.
#'   `print()` returns its input invisibly. Component metrics do not validate the
#'   ensemble. Between-model SD is diagnostic disagreement, not Bayesian prior SD.
#' @export
summary.pairwiseLLM_warm_ensemble <- function(object, ...) {
  rlang::check_dots_empty()
  .validate_warm_start_ensemble(object)
  list(format_version = object$format_version, schema = object$schema, target = object$outcome,
    weighting = object$weighting, audit_status = .warm_start_audit_status(object),
    components = lapply(object$components, summary), metadata = object$metadata,
    sd_interpretation = "Between-model sample SD is diagnostic, not Bayesian prior SD.")
}

#' @rdname summary.pairwiseLLM_warm_ensemble
#' @export
print.pairwiseLLM_warm_ensemble <- function(x, ...) {
  info <- summary(x, ...)
  cat("Warm-start ensemble:", length(info$components), "equally weighted task models\n")
  cat("Components:", paste(names(info$components), collapse = ", "), "\n")
  cat("Target: within_task_z; audit:", info$audit_status, "\n")
  cat(info$sd_interpretation, "\n")
  invisible(x)
}
