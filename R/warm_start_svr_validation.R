# SVR audit reconstruction and deployment validation never load e1071.
.validate_warm_start_svr_tuning <- function(x, n) {
  invalid <- function() rlang::abort("Invalid SVR tuning audit.")
  if (!is.list(x) || !all(c("control", "grid", "retained_p", "actual_gamma", "foldid", "conventions",
      "reference_preprocessing", "inner_preprocessing", "traces", "selected", "oof") %in% names(x)) ||
      !is.integer(x$foldid) || length(x$foldid) != n || anyNA(x$foldid) || min(x$foldid) != 1L ||
      max(x$foldid) < 2L || !identical(sort(unique(x$foldid)), seq_len(max(x$foldid))) ||
      !identical(x$conventions, .warm_start_svr_conventions()) ||
      !identical(x$control, .warm_start_svr_control(x$control))) invalid()
  grid <- .warm_start_svr_grid(x$control)
  if (!identical(x$grid, grid)) invalid()
  sizes <- tabulate(x$foldid)
  k <- length(sizes)
  .validate_warm_start_preprocess(x$reference_preprocessing)
  if (x$reference_preprocessing$n_training != n || !is.list(x$inner_preprocessing) ||
      length(x$inner_preprocessing) != k) invalid()
  preprocessing <- c(list(x$reference_preprocessing), x$inner_preprocessing)
  counts <- c(n, n - sizes)
  for (i in seq_along(preprocessing)) {
    p <- preprocessing[[i]]
    .validate_warm_start_preprocess(p)
    if (p$n_training != counts[i] || p$n_training < 3L ||
        !identical(p$features, x$reference_preprocessing$features)) invalid()
  }
  retained <- stats::setNames(vapply(preprocessing, function(p) length(p$retained), integer(1)),
    c("context_refit", paste0("inner_", seq_len(k))))
  gamma <- outer(as.numeric(retained), grid$gamma_multiplier, function(p, m) m / p)
  dimnames(gamma) <- list(names(retained), NULL)
  if (!identical(x$retained_p, retained) || !identical(x$actual_gamma, gamma) ||
      any(!is.finite(gamma)) || any(gamma <= 0)) invalid()
  t <- x$traces
  oof <- x$oof
  if (!is.list(t) || !identical(t$fold_sizes, sizes) ||
      !is.matrix(t$fold_mse) || !is.numeric(t$fold_mse) ||
      !identical(dim(t$fold_mse), c(k, nrow(grid))) ||
      !is.matrix(t$candidate_oof) || !is.numeric(t$candidate_oof) ||
      !identical(dim(t$candidate_oof), c(as.integer(n), nrow(grid))) ||
      any(!is.finite(t$candidate_oof)) || !is.data.frame(oof) || nrow(oof) != n ||
      !identical(names(oof), c("row", "fold", "observed", "raw_prediction")) ||
      !identical(oof$row, seq_len(n)) || !identical(oof$fold, x$foldid) ||
      !is.numeric(oof$observed) || length(oof$observed) != n || any(!is.finite(oof$observed))) invalid()
  losses <- matrix(NA_real_, k, nrow(grid))
  for (fold in seq_len(k)) {
    rows <- which(x$foldid == fold)
    losses[fold, ] <- colMeans((t$candidate_oof[rows, , drop = FALSE] - oof$observed[rows])^2)
  }
  if (!.warm_start_audit_equal(t$fold_mse, losses)) invalid()
  errors <- .warm_start_loss_summary(losses, sizes)
  if (!.warm_start_audit_equal(t$cvm, errors$cvm) ||
      !.warm_start_audit_equal(t$cvsd, errors$cvsd)) invalid()
  choice <- .warm_start_svr_choice(errors$cvm, errors$cvsd)
  selected <- list(cost = grid$cost[choice$index], gamma_multiplier = grid$gamma_multiplier[choice$index],
    error = errors$cvm[choice$index_min], oof = as.numeric(t$candidate_oof[, choice$index]))
  if (!.warm_start_audit_equal(t[names(choice)], choice) ||
      !is.list(x$selected) || !identical(names(x$selected), names(selected)) ||
      !identical(x$selected[c("cost", "gamma_multiplier", "oof")], selected[c("cost", "gamma_multiplier", "oof")]) ||
      !.warm_start_audit_equal(x$selected$error, selected$error) ||
      !identical(oof$raw_prediction, selected$oof)) invalid()
  invisible(x)
}

.validate_warm_start_svr_refit <- function(record) {
  invalid <- function() rlang::abort("Invalid SVR context refit contract.")
  if (!all(c("coefficients", "intercept", "engine_payload") %in% names(record)) ||
      !is.null(record$coefficients) || !is.null(record$intercept) ||
      "n_nonzero" %in% names(record)) invalid()
  .validate_warm_start_svr_payload(record$engine_payload, record$preprocessing$retained)
  selected <- record$tuning$selected
  if (!.warm_start_number(selected$gamma_multiplier) ||
      !identical(record$engine_payload$gamma, selected$gamma_multiplier / length(record$preprocessing$retained)) ||
      nrow(record$engine_payload$support_vectors) > record$preprocessing$n_training) invalid()
  invisible(record)
}

.validate_warm_start_svr_model <- function(model) {
  invalid <- function() rlang::abort("Invalid format-3 SVR model contract.")
  if (!identical(model$features, warm_start_feature_schema(model$schema)$feature)) invalid()
  .validate_warm_start_preprocess(model$preprocessing)
  .validate_warm_start_outcome(model$outcome)
  .validate_warm_start_calibration(model$calibration)
  t <- model$training
  h <- t$hyperparameters
  if (!is.list(t) || !identical(model$features, model$preprocessing$features) ||
      !is.null(model$coefficients) || !is.null(model$intercept) ||
      !.warm_start_number(t$n, 3) || t$n != floor(t$n) || t$n != model$preprocessing$n_training ||
      !is.list(h) || !identical(names(h), c("cost", "gamma_multiplier", "epsilon", "gamma")) ||
      !.warm_start_number(h$cost) || h$cost <= 0 || !.warm_start_number(h$gamma_multiplier) ||
      h$gamma_multiplier <= 0 ||
      !identical(h, .warm_start_svr_hyperparameters(h, length(model$preprocessing$retained))) ||
      !identical(h$gamma, model$engine_payload$gamma) ||
      nrow(model$engine_payload$support_vectors) > t$n ||
      any(c("alpha", "lambda", "n_nonzero") %in% names(t)) ||
      !.warm_start_string(t$engine_version) || !.warm_start_string(t$package_version)) invalid()
  .warm_start_task_id(t$task_id)
  if ("metadata" %in% names(model)) .validate_warm_start_metadata(model$metadata)
  if (model$audit_status == "full") {
    .validate_warm_start_svr_refit(model)
    .validate_warm_start_development(model, "svr_rbf")
  } else {
    .validate_warm_start_svr_reduced(model)
  }
  invisible(model)
}

.warm_start_svr_reduced <- function(model) {
  out <- .warm_start_deployment_fields(model)
  out$training <- model$training[c("task_id", "n", "engine", "engine_version",
    "package_version", "hyperparameters")]
  out$tuning <- model$tuning[c("seed", "control", "conventions")]
  out$validation <- model$validation[c("method", "outer_folds", "inner_folds", "metrics")]
  out$validation$metrics <- model$validation$metrics[c("pearson_r", "squared_pearson_r",
    "spearman_rho", "rmse", "mae", "calibration_intercept", "calibration_slope", "undefined_reasons")]
  out$validation$warning_count <- if (model$audit_status == "full") {
    length(model$validation$warnings)
  } else {
    model$validation$warning_count
  }
  out$audit_status <- "summary_only"
  out["cv_plan"] <- list(NULL)
  out$cv_identity <- model$cv_identity
  if (!is.null(model$metadata)) out$metadata <- model$metadata
  out <- structure(.warm_start_plain(out), class = "pairwiseLLM_warm_model")
  out$engine_payload <- .warm_start_engine_copy(model$engine_payload)
  out
}

.validate_warm_start_svr_reduced <- function(model) {
  invalid <- function() rlang::abort("Invalid summary-only SVR artifact contract.")
  t <- model$tuning
  v <- model$validation
  if (!identical(model, .warm_start_svr_reduced(model)) || !is.list(t) || !is.list(v) ||
      !.warm_start_number(t$seed, 0, .Machine$integer.max) || t$seed != floor(t$seed) ||
      !identical(t$conventions, .warm_start_svr_conventions()) ||
      !identical(t$control, .warm_start_svr_control(t$control)) ||
      !model$training$hyperparameters$cost %in% t$control$cost ||
      !model$training$hyperparameters$gamma_multiplier %in% t$control$gamma_multiplier ||
      !identical(v$method, "nested_cv") || model$calibration$n != model$training$n) invalid()
  for (field in c("outer_folds", "inner_folds", "warning_count")) {
    value <- v[[field]]
    lower <- if (field == "warning_count") 0 else 2
    upper <- if (field == "warning_count") Inf else model$training$n
    if (!.warm_start_number(value, lower, upper) || value != floor(value)) invalid()
  }
  .validate_warm_start_summary_metrics(v$metrics)
  invisible(model)
}
