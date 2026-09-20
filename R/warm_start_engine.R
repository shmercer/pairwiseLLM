# Engine boundaries leave shared preprocessing, scales, folds and calibration in
# the CV orchestrator. New backends must implement these same numeric boundaries.
.warm_start_engine_control <- function(engine, control) {
  if (engine == "svr_rbf") return(.warm_start_svr_control(control))
  if (engine == "pls") return(.warm_start_pls_control(control))
  if (engine != "glmnet") rlang::abort(paste0("Engine '", engine, "' is not yet implemented."))
  if (!is.null(control) && (!is.list(control) || length(control))) {
    rlang::abort("glmnet does not accept nonempty `engine_control`; use alpha_grid and lambda_rule.")
  }
}

.warm_start_engine_tune <- function(engine, x, z, foldid, alpha_grid, lambda_rule, control = NULL) {
  if (engine == "svr_rbf") return(.warm_start_svr_tune(x, z, foldid, control))
  if (engine == "pls") return(.warm_start_pls_tune(x, z, foldid, control))
  .warm_start_engine_control(engine, NULL)
  .warm_start_tune(x, z, foldid, alpha_grid, lambda_rule)
}

.warm_start_engine_refit <- function(engine, x, z, selected) {
  if (engine == "svr_rbf") {
    label <- paste("SVR context refit, cost", selected$cost, "gamma_multiplier", selected$gamma_multiplier)
    return(.warm_start_cv_context(label, function() {
      .warm_start_svr_fit(x, z, selected$cost, selected$gamma_multiplier)
    }))
  }
  if (engine == "pls") {
    return(.warm_start_cv_context(paste("PLS context refit, ncomp", selected$ncomp), function() {
      .warm_start_pls_payload(.warm_start_pls_fit(x, z, selected$ncomp), selected$ncomp, colnames(x))
    }))
  }
  .warm_start_engine_control(engine, NULL)
  fit <- .warm_start_glmnet_fit(x, z, selected$alpha, selected$lambda)
  coefficients <- as.matrix(fit$beta)[colnames(x), 1]
  list(type = "linear", coefficients = stats::setNames(as.numeric(coefficients), colnames(x)),
    intercept = unname(fit$a0[1]))
}

.validate_warm_start_engine_payload <- function(payload, engine, retained) {
  if (identical(engine, "svr_rbf")) return(.validate_warm_start_svr_payload(payload, retained))
  if (!.warm_start_string(engine) || !engine %in% c("glmnet", "pls") || !is.list(payload) ||
      !identical(names(payload), c("type", "coefficients", "intercept")) ||
      !identical(payload$type, "linear") || !.warm_start_portable(payload) ||
      !.warm_start_named_numeric(payload$coefficients, retained) ||
      !.warm_start_number(payload$intercept)) {
    rlang::abort("Invalid warm-start engine payload.")
  }
  invisible(payload)
}

.warm_start_engine_predict <- function(payload, x) {
  if (identical(payload$type, "rbf_svr")) return(.warm_start_svr_predict(payload, x))
  if (!identical(payload$type, "linear")) rlang::abort("Unsupported warm-start prediction payload.")
  as.numeric(payload$intercept + x %*% payload$coefficients)
}

# Retain only documented numeric payload fields; do not use the general plain
# list copier for payloads, which would discard matrix dimensions in future engines.
.warm_start_engine_copy <- function(payload) {
  if (identical(payload$type, "rbf_svr")) {
    sv <- payload$support_vectors
    return(list(type = "rbf_svr", support_vectors = matrix(as.numeric(sv),
      nrow = nrow(sv), ncol = ncol(sv), dimnames = list(NULL, colnames(sv))),
      dual = as.numeric(payload$dual), rho = as.numeric(payload$rho), gamma = as.numeric(payload$gamma)))
  }
  list(type = unname(payload$type),
    coefficients = stats::setNames(as.numeric(payload$coefficients), names(payload$coefficients)),
    intercept = as.numeric(payload$intercept))
}
