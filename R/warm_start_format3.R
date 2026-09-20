# Format 3 is independently validated; legacy contracts remain strict and retain
# their original constructors. Projection reuses their glmnet statistical audit.
.warm_start_format3 <- function(model, plan, payload) {
  model$format_version <- 3L
  model$audit_status <- "full"
  model$cv_plan <- plan
  model$cv_identity <- .warm_start_cv_identity(plan)
  model$engine_payload <- payload
  if (model$training$engine == "glmnet") {
    model$training$hyperparameters <- list(alpha = model$training$alpha, lambda = model$training$lambda)
  }
  .validate_warm_start_model(model)
  model
}

.warm_start_legacy_view <- function(model) {
  model$format_version <- if (identical(model$audit_status, "full")) 1L else 2L
  if (model$format_version == 1L) model$audit_status <- NULL
  model$cv_plan <- NULL
  model$cv_identity <- NULL
  model$engine_payload <- NULL
  model$training$hyperparameters <- NULL
  model
}

.validate_warm_start_format3 <- function(model) {
  invalid <- function() rlang::abort("Invalid format-3 warm-start model contract or CV evidence.")
  required <- c("audit_status", "cv_plan", "cv_identity", "engine_payload")
  if (!all(required %in% names(model)) ||
      !identical(class(model), "pairwiseLLM_warm_model") ||
      !.warm_start_string(model$audit_status) || !model$audit_status %in% c("full", "summary_only") ||
      !.warm_start_string(model$training$engine) || !model$training$engine %in% c("glmnet", "pls", "svr_rbf") ||
      !identical(model$calibration$status, "oof_linear")) invalid()
  .validate_warm_start_engine_payload(model$engine_payload, model$training$engine, model$preprocessing$retained)
  if (!identical(model$coefficients, model$engine_payload$coefficients) ||
      !identical(model$intercept, model$engine_payload$intercept)) invalid()
  if (model$training$engine == "glmnet") {
    if (!identical(model$training$hyperparameters,
        list(alpha = model$training$alpha, lambda = model$training$lambda))) invalid()
    .validate_warm_start_model(.warm_start_legacy_view(model))
  } else if (model$training$engine == "svr_rbf") {
    .validate_warm_start_svr_model(model)
  } else {
    .validate_warm_start_pls_model(model)
  }
  identity <- model$cv_identity
  fields <- c("format_version", "digest", "task_id", "n", "outcome_digest", "seed",
    "outer_folds", "inner_folds", "rng_kind")
  if (!is.list(identity) || !identical(names(identity), fields) ||
      !identical(identity$format_version, 1L) ||
      !identical(identity$task_id, model$training$task_id) ||
      !identical(identity$n, model$training$n) || !identical(identity$seed, model$tuning$seed) ||
      !identical(identity$outer_folds, model$validation$outer_folds) ||
      !identical(identity$inner_folds, model$validation$inner_folds)) invalid()
  for (field in c("digest", "outcome_digest")) {
    if (!.warm_start_string(identity[[field]]) || !grepl("^[a-f0-9]{32}$", identity[[field]])) invalid()
  }
  .warm_start_plan_rng(identity$rng_kind)
  if (model$audit_status == "summary_only") {
    if (!is.null(model$cv_plan) || !identical(model, .warm_start_reduced3(model))) invalid()
    return(invisible(model))
  }
  plan <- model$cv_plan
  .validate_warm_start_cv_plan(plan, model$tuning$ids, task_id = model$training$task_id)
  if (!identical(identity, .warm_start_cv_identity(plan)) ||
      !identical(unname(plan$outer_foldid), model$validation$predictions$fold) ||
      !identical(unname(plan$full_inner_foldid), model$tuning$foldid) ||
      !.warm_start_audit_equal(model$outcome, .warm_start_outcome_fit(plan$theta)) ||
      !.warm_start_audit_equal(model$tuning$oof$observed, .warm_start_outcome_apply(plan$theta, model$outcome))) {
    invalid()
  }
  for (fold in seq_len(plan$outer_folds)) {
    record <- model$validation$folds[[fold]]
    train <- which(plan$outer_foldid != fold)
    test <- which(plan$outer_foldid == fold)
    .validate_warm_start_engine_payload(record$engine_payload, model$training$engine, record$preprocessing$retained)
    if (!identical(unname(plan$outer_inner_foldid[[fold]]), record$tuning$foldid) ||
        !identical(record$coefficients, record$engine_payload$coefficients) ||
        !identical(record$intercept, record$engine_payload$intercept) ||
        !.warm_start_audit_equal(record$outcome, .warm_start_outcome_fit(plan$theta[train])) ||
        !.warm_start_audit_equal(record$predictions$observed,
          .warm_start_outcome_apply(plan$theta[test], record$outcome))) invalid()
  }
  invisible(model)
}

.warm_start_reduced3 <- function(model) {
  if (identical(model$training$engine, "svr_rbf")) return(.warm_start_svr_reduced(model))
  if (identical(model$training$engine, "pls")) return(.warm_start_pls_reduced(model))
  out <- .warm_start_reduced(.warm_start_legacy_view(model))
  out$format_version <- 3L
  out["cv_plan"] <- list(NULL)
  out$cv_identity <- .warm_start_plain(model$cv_identity)
  out$engine_payload <- .warm_start_engine_copy(model$engine_payload)
  out$training$hyperparameters <- list(alpha = out$training$alpha, lambda = out$training$lambda)
  out
}
