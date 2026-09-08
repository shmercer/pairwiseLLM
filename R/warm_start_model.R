#' Portable task-specific warm-start models
#'
#' A warm-start model predicts relative quality from frozen writing features
#' before pairwise comparisons are collected. Each model represents one task or
#' assessment; unlinked BT/BTL scores from different tasks must not be pooled.
#'
#' @name pairwiseLLM_warm_model
#' @param x A `pairwiseLLM_warm_model` object.
#' @param object A `pairwiseLLM_warm_model` object.
#' @param ... Reserved for future extensions; must be empty.
#'
#' @details
#' The fixed-hyperparameter core is internal. It does not tune hyperparameters,
#' run cross-validation, or learn calibration. [fit_warm_start_model()] adds nested
#' validation and OOF calibration. Core models remain explicitly uncalibrated.
#'
#' Format version 1 is an S3 list with these deployment fields:
#' * `format_version`: integer `1L`.
#' * `schema` and `features`: frozen schema identity and original feature order,
#'   including predictors removed during training.
#' * `preprocessing`: original/retained feature names, named removal reasons,
#'   training missing fractions, retained medians, centers, sample SDs, thresholds,
#'   training row count, and SD convention.
#' * `coefficients` and `intercept`: finite linear coefficients on the stored
#'   preprocessed predictor scale, in retained feature order, and intercept.
#' * `outcome`: `definition = "within_task_z"`, original training mean and sample
#'   SD, and `sd_convention = "sample"`. The target is `(theta - mean) / sd`.
#' * `calibration`: `status = "uncalibrated"`, with `intercept = NULL` and
#'   `slope = NULL` for core fits. Public fits store `status = "oof_linear"`,
#'   learned intercept/slope, calibration row count, method, QR tolerance and OOF source.
#' * `training`: task ID, training row count, requested alpha/lambda, nonzero
#'   coefficient count, engine/version, and package version.
#' * `tuning` and `validation`: NULL for core fits; public fits retain exact
#'   alpha/lambda traces, fold preprocessing, OOF calibration inputs, outer
#'   held-out predictions and metrics, and warning messages. See [fit_warm_start_model()].
#'
#' Explicit [prepare_warm_start_model()] audit omission creates format 2 with
#' `audit_status = "summary_only"`. It preserves deployment parameters and
#' validation summaries but omits row-level evidence. Those summaries cannot be
#' recomputed from the reduced artifact. Format 1 remains fully audited.
#'
#' Preprocessing is fitted only on the supplied training rows. It first removes
#' all-missing columns and columns with missing fraction strictly above 0.20,
#' then median-imputes remaining columns. It removes constants and columns with
#' unique-value fraction at most 0.10 AND most-common/second-most-common frequency
#' ratio strictly above 19. Frequencies are calculated after imputation. Retained
#' columns are centered and divided by sample SD (denominator `n - 1`). Internal
#' controls permit changing these thresholds; fitted values are stored.
#' No surviving predictors is an error. With one survivor, an excluded zero
#' column satisfies glmnet's two-column input requirement only during fitting;
#' it is never a schema feature or deployment coefficient.
#'
#' The internal fixed fit requires at least three observations, explicit alpha
#' in `[0, 1]`, and finite lambda greater than or equal to zero. It uses Gaussian
#' glmnet with an intercept and `standardize = FALSE`, with solver threshold
#' `1e-12` and maximum `100000` iterations, directly at the requested lambda.
#' Outcomes are standardized before fitting. No PCA or feature screening based
#' on outcomes is used, including when predictors outnumber observations.
#'
#' Prediction uses only the stored preprocessing and linear coefficients, never
#' a serialized glmnet object. Python is optional for extraction, and glmnet is
#' optional for development; neither is needed to inspect or predict from a
#' deployment object with precomputed features. Schema metadata is an input
#' contract, not verified extraction provenance or evidence of predictive validity.
#'
#' @return `print()` invisibly returns the model. `summary()` returns a named
#'   list describing the task, target, preprocessing, hyperparameters, and
#'   calibration status and stored nested-validation metrics when available.
NULL

.warm_start_hyperparameters <- function(alpha, lambda) {
  if (!.warm_start_number(alpha, 0, 1) || !.warm_start_number(lambda, 0)) {
    rlang::abort("Supply explicit finite alpha in [0, 1] and lambda >= 0.")
  }
  invisible(NULL)
}

.warm_start_task_id <- function(task_id) {
  if (!is.character(task_id) || length(task_id) != 1L || is.na(task_id) ||
      !is.null(dim(task_id)) || !nzchar(trimws(task_id))) {
    rlang::abort("`task_id` must be one nonblank character string identifying one assessment.")
  }
  invisible(task_id)
}

.warm_start_portable <- function(x) {
  if (is.null(x)) return(TRUE)
  if (!all(vapply(attributes(x), .warm_start_portable, logical(1)))) return(FALSE)
  if (is.list(x)) return(all(vapply(x, .warm_start_portable, logical(1))))
  is.atomic(x) && !isS4(x) && !is.object(x)
}

.validate_warm_start_model <- function(model) {
  invalid <- function() rlang::abort("Invalid portable warm-start model contract.")
  required <- c("format_version", "schema", "features", "preprocessing", "coefficients",
    "intercept", "outcome", "calibration", "training", "tuning", "validation")
  if (!inherits(model, "pairwiseLLM_warm_model") || !is.list(model) ||
      anyDuplicated(names(model)) || !all(required %in% names(model)) ||
      !.warm_start_portable(model)) invalid()
  if (!identical(model$format_version, 1L) && !identical(model$format_version, 2L)) {
    rlang::abort(paste0("Unsupported warm-start model format version; supported: 1 and 2. ",
      "Update pairwiseLLM or re-export from a supported version."))
  }
  definition <- warm_start_feature_schema(model$schema)
  if (!identical(model$features, definition$feature)) invalid()
  .validate_warm_start_preprocess(model$preprocessing)
  if (!identical(model$features, model$preprocessing$features) ||
      !.warm_start_named_numeric(model$coefficients, model$preprocessing$retained) ||
      !.warm_start_number(model$intercept)) invalid()
  .validate_warm_start_outcome(model$outcome)
  .validate_warm_start_calibration(model$calibration)
  training <- model$training
  if (!is.list(training)) invalid()
  .warm_start_task_id(training$task_id)
  .warm_start_hyperparameters(training$alpha, training$lambda)
  if (!.warm_start_number(training$n, 3) || training$n != floor(training$n) ||
      training$n != model$preprocessing$n_training ||
      !.warm_start_number(training$n_nonzero, 0) ||
      training$n_nonzero != sum(model$coefficients != 0) ||
      !identical(training$engine, "glmnet")) invalid()
  for (field in c("engine_version", "package_version")) {
    if (!is.character(training[[field]]) || length(training[[field]]) != 1L ||
        is.na(training[[field]]) || !nzchar(training[[field]])) invalid()
  }
  if ("metadata" %in% names(model)) .validate_warm_start_metadata(model$metadata)
  if (identical(model$format_version, 2L)) {
    .validate_warm_start_reduced(model)
  } else if ("audit_status" %in% names(model)) {
    invalid()
  }
  if (model$calibration$status == "uncalibrated") {
    if (!is.null(model$tuning) || !is.null(model$validation)) {
      rlang::abort("Uncalibrated core models cannot contain tuning or validation results.")
    }
  } else if (identical(model$format_version, 1L)) {
    .validate_warm_start_development(model)
  }
  invisible(model)
}

.new_warm_start_model <- function(schema, preprocessing, coefficients, intercept, outcome, training,
                                   calibration = list(status = "uncalibrated", intercept = NULL, slope = NULL),
                                   tuning = NULL, validation = NULL, format_version = 1L,
                                   audit_status = NULL, metadata = NULL) {
  model <- structure(list(format_version = format_version, schema = schema,
    features = warm_start_feature_schema(schema)$feature, preprocessing = preprocessing,
    coefficients = coefficients, intercept = intercept, outcome = outcome,
    calibration = calibration, training = training, tuning = tuning, validation = validation),
    class = "pairwiseLLM_warm_model")
  if (!is.null(audit_status)) model$audit_status <- audit_status
  if (!is.null(metadata)) model$metadata <- metadata
  .validate_warm_start_model(model)
  model
}

# This boundary is used only during development, never by deployment validation/prediction.
.warm_start_glmnet_available <- function() requireNamespace("glmnet", quietly = TRUE)

.warm_start_require_glmnet <- function() {
  if (!.warm_start_glmnet_available()) {
    rlang::abort("Model development requires optional package 'glmnet'. Install it explicitly first.")
  }
}

.warm_start_glmnet_controls <- function(engine = glmnet::glmnet) {
  control <- list(thresh = 1e-12, maxit = 100000L)
  # glmnet 5 moved solver controls into a list; retain compatibility with 4.x.
  if ("control" %in% names(formals(engine))) return(list(control = control))
  control
}

.warm_start_glmnet_fit <- function(x, z, alpha, lambda) {
  .warm_start_matrix(x, missing = FALSE)
  .warm_start_outcome_values(z)
  .warm_start_hyperparameters(alpha, lambda)
  if (nrow(x) < 3L || length(z) != nrow(x) || !is.finite(stats::sd(z)) || stats::sd(z) <= 0) {
    rlang::abort("Elastic-net fitting requires at least three aligned rows and a nonconstant outcome.")
  }
  .warm_start_require_glmnet()
  exclude <- NULL
  if (ncol(x) == 1L) {
    # Equal penalty factors preserve the real predictor's penalty normalization.
    x <- cbind(x, 0)
    colnames(x)[2] <- make.unique(c(colnames(x)[1], ".excluded_zero"))[2]
    exclude <- 2L
  }
  fit <- tryCatch(
    do.call(glmnet::glmnet, c(list(x = x, y = z, family = "gaussian", alpha = alpha,
      lambda = lambda, standardize = FALSE, intercept = TRUE, exclude = exclude),
      .warm_start_glmnet_controls())),
    error = function(e) rlang::abort("Elastic-net fitting failed.", parent = e)
  )
  if (!identical(fit$jerr, 0L) && !identical(fit$jerr, 0)) {
    rlang::abort("Elastic-net fitting did not converge; no deployment model was created.")
  }
  if (length(fit$lambda) != 1L || !isTRUE(all.equal(as.numeric(fit$lambda), as.numeric(lambda),
      tolerance = 1e-12)) || any(!is.finite(fit$a0)) || any(!is.finite(fit$beta))) {
    rlang::abort("Elastic-net fit did not return finite coefficients at the requested lambda.")
  }
  fit
}

.fit_warm_start_fixed <- function(features, ids, theta, task_id, alpha, lambda,
                                  schema = "writing_features_v1", missing_threshold = 0.20,
                                  unique_threshold = 0.10, frequency_ratio = 19) {
  ids <- .warm_start_ids(ids)
  features <- .validate_warm_start_features(features, ids, schema)
  .warm_start_outcome_values(theta)
  if (length(theta) != length(ids) || length(ids) < 3L) {
    rlang::abort("Supply at least three IDs and one theta value per ID, in the supplied ID order.")
  }
  .warm_start_task_id(task_id)
  .warm_start_hyperparameters(alpha, lambda)
  outcome <- .warm_start_outcome_fit(theta)
  x <- as.matrix(features[, -1, drop = FALSE])
  preprocessing <- .warm_start_preprocess_fit(x, missing_threshold, unique_threshold, frequency_ratio)
  scaled <- .warm_start_preprocess_apply(x, preprocessing)
  fit <- .warm_start_glmnet_fit(scaled, .warm_start_outcome_apply(theta, outcome), alpha, lambda)
  coefficients <- as.matrix(fit$beta)[preprocessing$retained, 1]
  coefficients <- stats::setNames(as.numeric(coefficients), preprocessing$retained)
  training <- list(task_id = task_id, n = length(ids), alpha = alpha, lambda = lambda,
    n_nonzero = sum(coefficients != 0), engine = "glmnet",
    engine_version = as.character(utils::packageVersion("glmnet")),
    package_version = as.character(utils::packageVersion("pairwiseLLM")))
  .new_warm_start_model(schema, preprocessing, coefficients, unname(fit$a0[1]), outcome, training)
}

#' @rdname pairwiseLLM_warm_model
#' @export
summary.pairwiseLLM_warm_model <- function(object, ...) {
  rlang::check_dots_empty()
  .validate_warm_start_model(object)
  list(task_id = object$training$task_id, target = object$outcome, n = object$training$n,
    schema = object$schema, retained_predictors = length(object$coefficients),
    removed_predictors = object$preprocessing$removed, nonzero_coefficients = object$training$n_nonzero,
    alpha = object$training$alpha, lambda = object$training$lambda,
    calibration = object$calibration$status,
    audit_status = if (object$format_version == 2L) "summary_only" else "full",
    validation = if (is.null(object$validation)) "not performed" else object$validation$metrics)
}

#' @rdname pairwiseLLM_warm_model
#' @export
print.pairwiseLLM_warm_model <- function(x, ...) {
  rlang::check_dots_empty()
  info <- summary(x)
  cat("Task-specific warm-start model:", info$task_id, "\n")
  cat("Target: within-task standardized BT/BTL theta (sample SD)\n")
  cat("Training rows:", info$n, "| Retained predictors:", info$retained_predictors,
    "| Nonzero coefficients:", info$nonzero_coefficients, "\n")
  cat("Alpha:", info$alpha, "| Lambda:", info$lambda, "\n")
  cat("Calibration:", info$calibration, "| Audit:", info$audit_status, "\n")
  if (is.list(info$validation)) {
    cat("Nested validation: Pearson r =", info$validation$pearson_r,
      "| RMSE =", info$validation$rmse, "| MAE =", info$validation$mae, "\n")
  } else {
    cat("Validation: not performed\n")
  }
  invisible(x)
}
