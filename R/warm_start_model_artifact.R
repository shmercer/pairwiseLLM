# Portable artifact preparation and the explicit summary-only contract.
.warm_start_string <- function(x) {
  is.character(x) && !is.object(x) && is.null(dim(x)) && length(x) == 1L &&
    !is.na(x) && nzchar(trimws(x))
}

.warm_start_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x) || !is.null(dim(x))) {
    rlang::abort(paste0("`", name, "` must be TRUE or FALSE."))
  }
}

.validate_warm_start_metadata <- function(x) {
  allowed <- c("name", "version", "domain", "notes", "license", "extraction_provenance",
    "prepared_at", "preparation_package_version")
  if (!is.list(x) || !.warm_start_portable(x) ||
      (length(x) && (!.warm_start_names(names(x)) || !all(names(x) %in% allowed)))) {
    rlang::abort("Metadata must be a named portable list of documented artifact fields.")
  }
  for (field in names(x)) {
    value <- x[[field]]
    if (field == "extraction_provenance") {
      if (!is.character(value) || !is.null(dim(value)) || anyNA(value) ||
          !.warm_start_names(names(value)) || any(!nzchar(trimws(value)))) {
        rlang::abort("Extraction provenance must be a nonempty named character vector.")
      }
    } else if (!.warm_start_string(value)) {
      rlang::abort(paste0("Metadata `", field, "` must be one nonblank character string."))
    }
  }
  if (!is.null(x$prepared_at) && !grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", x$prepared_at)) {
    rlang::abort("Metadata prepared_at must be a UTC ISO-8601 timestamp ending in Z.")
  }
  invisible(x)
}

# Drop unrecognized attributes as well as unrecognized fields during explicit reduction.
.warm_start_plain <- function(x) {
  nms <- names(x)
  if (is.list(x)) x <- lapply(x, .warm_start_plain)
  attributes(x) <- NULL
  if (!is.null(nms)) names(x) <- nms
  x
}

.warm_start_deployment_fields <- function(model) {
  out <- model[c("format_version", "schema", "features", "preprocessing", "coefficients",
    "intercept", "outcome", "calibration", "training", "tuning", "validation")]
  out$preprocessing <- out$preprocessing[c("features", "retained", "removed", "missing_fraction",
    "medians", "centers", "scales", "control", "sd_convention", "n_training")]
  out$preprocessing$control <- out$preprocessing$control[
    c("missing_threshold", "unique_threshold", "frequency_ratio")]
  out$outcome <- out$outcome[c("definition", "mean", "sd", "sd_convention")]
  fields <- c("status", "intercept", "slope")
  if (model$calibration$status == "oof_linear") {
    fields <- c(fields, "n", "method", "qr_tolerance", "source")
  }
  out$calibration <- out$calibration[fields]
  out$training <- out$training[c("task_id", "n", "alpha", "lambda", "n_nonzero", "engine",
    "engine_version", "package_version")]
  out
}

.warm_start_reduced <- function(model) {
  out <- .warm_start_deployment_fields(model)
  out$format_version <- 2L
  out$audit_status <- "summary_only"
  if (model$calibration$status == "oof_linear") {
    out$tuning <- model$tuning[c("seed", "alpha_grid", "lambda_rule", "conventions")]
    out$validation <- model$validation[c("method", "outer_folds", "inner_folds", "metrics")]
    out$validation$metrics <- model$validation$metrics[c("pearson_r", "squared_pearson_r",
      "spearman_rho", "rmse", "mae", "calibration_intercept", "calibration_slope", "undefined_reasons")]
    out$validation$warning_count <- if (model$format_version == 1L) {
      length(model$validation$warnings)
    } else {
      model$validation$warning_count
    }
  }
  if (!is.null(model$metadata)) out$metadata <- model$metadata
  structure(.warm_start_plain(out), class = "pairwiseLLM_warm_model")
}

.validate_warm_start_reduced <- function(model) {
  invalid <- function() rlang::abort("Invalid summary-only warm-start artifact contract.")
  if (!identical(model$audit_status, "summary_only") ||
      !identical(model, .warm_start_reduced(model))) invalid()
  if (model$calibration$status == "uncalibrated") return(invisible(model))
  t <- model$tuning
  v <- model$validation
  if (!is.list(t) || !is.list(v) ||
      !.warm_start_number(t$seed, 0, .Machine$integer.max) || t$seed != floor(t$seed) ||
      !is.numeric(t$alpha_grid) || !length(t$alpha_grid) || any(!is.finite(t$alpha_grid)) ||
      any(t$alpha_grid < 0 | t$alpha_grid > 1) || any(diff(t$alpha_grid) <= 0) ||
      !model$training$alpha %in% t$alpha_grid ||
      !.warm_start_string(t$lambda_rule) || !t$lambda_rule %in% c("lambda.1se", "lambda.min") ||
      !identical(t$conventions, .warm_start_tuning_conventions()) ||
      !identical(v$method, "nested_cv") || model$calibration$n != model$training$n) invalid()
  for (field in c("outer_folds", "inner_folds", "warning_count")) {
    value <- v[[field]]
    lower <- if (field == "warning_count") 0 else 2
    upper <- if (field == "warning_count") Inf else model$training$n
    if (!.warm_start_number(value, lower, upper) || value != floor(value)) invalid()
  }
  m <- v$metrics
  if (!is.list(m) || !is.character(m$undefined_reasons) || anyNA(m$undefined_reasons)) invalid()
  for (field in c("rmse", "mae")) {
    if (!.warm_start_number(m[[field]], 0)) invalid()
  }
  if (m$mae > m$rmse + 1e-10 * max(1, m$rmse)) invalid()
  for (field in c("pearson_r", "squared_pearson_r", "spearman_rho",
                  "calibration_intercept", "calibration_slope")) {
    value <- m[[field]]
    missing <- identical(value, NA_real_)
    bound <- if (field %in% c("pearson_r", "squared_pearson_r", "spearman_rho")) 1 else Inf
    if ((!missing && !.warm_start_number(value, -bound, bound)) ||
        (missing && !length(m$undefined_reasons))) invalid()
  }
  if (!.warm_start_audit_equal(m$squared_pearson_r, m$pearson_r^2)) invalid()
  invisible(model)
}

#' Prepare metadata or a summary-only warm-start artifact
#'
#' @param model A valid [pairwiseLLM_warm_model].
#' @param metadata Named list with optional scalar character `name`, `version`,
#'   `domain`, `notes`, `license`, `prepared_at`, `preparation_package_version`,
#'   and `extraction_provenance` (a named character vector). Supplied fields replace
#'   existing fields. Timestamps use UTC `YYYY-MM-DDTHH:MM:SSZ` strings.
#' @param omit_audit Explicitly omit row-level development evidence. Default FALSE.
#' @return A prepared copy using the same model class and prediction method.
#' @details
#' Preparation adds a preparation timestamp/package version when absent, not a
#' training date. Extraction provenance defaults to `c(status = "unavailable")`;
#' user-supplied records are not independently verified. Schema identity or a
#' later Python status check is not extraction provenance.
#'
#' Ordinary preparation preserves audit records. Explicit omission creates format
#' 2 with summary-only audit status, retaining deployment parameters, tuning
#' settings and nested-validation summaries. IDs, OOF rows, fold records, tuning
#' traces and contextual warning messages are omitted; warning counts remain.
#' Summaries cannot be recomputed without the original evidence. An already
#' reduced artifact cannot recover its audit through this function.
#'
#' No raw texts are added. Review task labels, notes, domain, and provenance for
#' restricted information before bundling; this is not a general anonymizer.
#' User models do not need complete publication metadata. Task-specific outcome
#' scales remain standardized independently; storage does not link BTL scales.
#' @export
prepare_warm_start_model <- function(model, metadata = list(), omit_audit = FALSE) {
  .validate_warm_start_model(model)
  .validate_warm_start_metadata(metadata)
  .warm_start_flag(omit_audit, "omit_audit")
  existing <- model$metadata
  if (is.null(existing)) existing <- list()
  existing[names(metadata)] <- metadata
  defaults <- list(prepared_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    preparation_package_version = as.character(utils::packageVersion("pairwiseLLM")),
    extraction_provenance = c(status = "unavailable"))
  absent <- setdiff(names(defaults), names(existing))
  existing[absent] <- defaults[absent]
  model$metadata <- existing
  if (omit_audit) model <- .warm_start_reduced(model)
  .validate_warm_start_model(model)
  model
}
