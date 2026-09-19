# Engine boundaries leave shared preprocessing, scales, folds and calibration in
# the CV orchestrator. New backends must implement these same numeric boundaries.
.warm_start_engine_control <- function(engine, control) {
  if (engine != "glmnet") rlang::abort(paste0("Engine '", engine, "' is not yet implemented."))
  if (!is.null(control) && (!is.list(control) || length(control))) {
    rlang::abort("glmnet does not accept nonempty `engine_control`; use alpha_grid and lambda_rule.")
  }
}

.warm_start_engine_tune <- function(engine, x, z, foldid, alpha_grid, lambda_rule) {
  .warm_start_engine_control(engine, NULL)
  .warm_start_tune(x, z, foldid, alpha_grid, lambda_rule)
}

.warm_start_engine_refit <- function(engine, x, z, selected) {
  .warm_start_engine_control(engine, NULL)
  fit <- .warm_start_glmnet_fit(x, z, selected$alpha, selected$lambda)
  coefficients <- as.matrix(fit$beta)[colnames(x), 1]
  list(type = "linear", coefficients = stats::setNames(as.numeric(coefficients), colnames(x)),
    intercept = unname(fit$a0[1]))
}

.validate_warm_start_engine_payload <- function(payload, engine, retained) {
  if (!identical(engine, "glmnet") || !is.list(payload) ||
      !identical(names(payload), c("type", "coefficients", "intercept")) ||
      !identical(payload$type, "linear") || !.warm_start_portable(payload) ||
      !.warm_start_named_numeric(payload$coefficients, retained) ||
      !.warm_start_number(payload$intercept)) {
    rlang::abort("Invalid warm-start engine payload.")
  }
  invisible(payload)
}

.warm_start_engine_predict <- function(payload, x) {
  if (!identical(payload$type, "linear")) rlang::abort("Unsupported warm-start prediction payload.")
  as.numeric(payload$intercept + x %*% payload$coefficients)
}

# Retain only documented numeric payload fields; do not use the general plain
# list copier for payloads, which would discard matrix dimensions in future engines.
.warm_start_engine_copy <- function(payload) {
  list(type = unname(payload$type),
    coefficients = stats::setNames(as.numeric(payload$coefficients), names(payload$coefficients)),
    intercept = as.numeric(payload$intercept))
}
