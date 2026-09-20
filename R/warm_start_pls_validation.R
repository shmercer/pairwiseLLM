# PLS audits use only portable values and shared statistical helpers, never pls.
.validate_warm_start_pls_tuning <- function(x, n) {
  invalid <- function() rlang::abort("Invalid PLS tuning audit.")
  if (!is.list(x) || !all(c("ncomp_requested", "ncomp_grid", "bounds", "foldid", "conventions",
      "reference_preprocessing", "inner_preprocessing", "traces", "selected", "oof") %in% names(x)) ||
      !is.integer(x$foldid) || length(x$foldid) != n || anyNA(x$foldid) || min(x$foldid) != 1L ||
      max(x$foldid) < 2L || !identical(sort(unique(x$foldid)), seq_len(max(x$foldid))) ||
      !identical(x$conventions, .warm_start_pls_conventions())) invalid()
  requested <- if (is.null(x$ncomp_requested)) {
    NULL
  } else {
    .warm_start_pls_control(list(ncomp = x$ncomp_requested))$ncomp
  }
  if (!identical(requested, x$ncomp_requested)) invalid()
  sizes <- tabulate(x$foldid)
  k <- length(sizes)
  .validate_warm_start_preprocess(x$reference_preprocessing)
  if (x$reference_preprocessing$n_training != n || !is.list(x$inner_preprocessing) ||
      length(x$inner_preprocessing) != k || !is.list(x$bounds) ||
      !identical(names(x$bounds), c("context_refit", paste0("inner_", seq_len(k))))) invalid()
  preprocessing <- c(list(x$reference_preprocessing), x$inner_preprocessing)
  counts <- c(n, n - sizes)
  for (i in seq_along(preprocessing)) {
    p <- preprocessing[[i]]
    .validate_warm_start_preprocess(p)
    b <- x$bounds[[i]]
    if (p$n_training != counts[i] || p$n_training < 3L ||
        !identical(p$features, x$reference_preprocessing$features) || !is.list(b) ||
        !identical(names(b), c("n", "p", "rank", "max_ncomp")) ||
        !identical(b$n, as.integer(p$n_training)) || !identical(b$p, length(p$retained)) ||
        !.warm_start_number(b$rank, 1, min(b$p, b$n - 1L)) || b$rank != floor(b$rank) ||
        !identical(b$max_ncomp, as.integer(min(10L, b$rank, b$p, b$n - 1L)))) invalid()
  }
  grid <- .warm_start_pls_grid(x$bounds, requested)
  if (!identical(x$ncomp_grid, grid)) invalid()
  t <- x$traces
  oof <- x$oof
  if (!is.list(t) || !identical(t$ncomp, grid) || !identical(t$fold_sizes, sizes) ||
      !is.matrix(t$fold_mse) || !is.numeric(t$fold_mse) ||
      !identical(dim(t$fold_mse), c(k, length(grid))) ||
      !is.matrix(t$candidate_oof) || !is.numeric(t$candidate_oof) ||
      !identical(dim(t$candidate_oof), c(as.integer(n), length(grid))) ||
      any(!is.finite(t$candidate_oof)) || !is.data.frame(oof) || nrow(oof) != n ||
      !identical(names(oof), c("row", "fold", "observed", "raw_prediction")) ||
      !identical(oof$row, seq_len(n)) || !identical(oof$fold, x$foldid) ||
      !is.numeric(oof$observed) || length(oof$observed) != n || any(!is.finite(oof$observed))) invalid()
  losses <- matrix(NA_real_, k, length(grid))
  for (fold in seq_len(k)) {
    rows <- which(x$foldid == fold)
    losses[fold, ] <- colMeans((t$candidate_oof[rows, , drop = FALSE] - oof$observed[rows])^2)
  }
  if (!.warm_start_audit_equal(t$fold_mse, losses)) invalid()
  errors <- .warm_start_loss_summary(t$fold_mse, sizes)
  if (!.warm_start_audit_equal(t$cvm, errors$cvm) ||
      !.warm_start_audit_equal(t$cvsd, errors$cvsd)) invalid()
  choice <- .warm_start_pls_choice(grid, errors$cvm, errors$cvsd)
  selected <- list(ncomp = grid[choice$index], error = errors$cvm[choice$index_min],
    oof = as.numeric(t$candidate_oof[, choice$index]))
  if (!.warm_start_audit_equal(t[names(choice)], choice) ||
      !identical(x$selected, selected) || !identical(oof$raw_prediction, selected$oof)) invalid()
  invisible(x)
}

.validate_warm_start_pls_model <- function(model) {
  invalid <- function() rlang::abort("Invalid format-3 PLS model contract.")
  if (!identical(model$features, warm_start_feature_schema(model$schema)$feature)) invalid()
  .validate_warm_start_preprocess(model$preprocessing)
  .validate_warm_start_outcome(model$outcome)
  .validate_warm_start_calibration(model$calibration)
  t <- model$training
  if (!is.list(t) || !identical(model$features, model$preprocessing$features) ||
      !.warm_start_number(t$n, 3) || t$n != floor(t$n) || t$n != model$preprocessing$n_training ||
      !identical(t$n_nonzero, sum(model$coefficients != 0)) ||
      !is.list(t$hyperparameters) || !identical(names(t$hyperparameters), "ncomp") ||
      !identical(t$hyperparameters, .warm_start_pls_control(t$hyperparameters)) ||
      length(t$hyperparameters$ncomp) != 1L || any(c("alpha", "lambda") %in% names(t)) ||
      !.warm_start_string(t$engine_version) || !.warm_start_string(t$package_version)) invalid()
  .warm_start_task_id(t$task_id)
  if ("metadata" %in% names(model)) .validate_warm_start_metadata(model$metadata)
  if (model$audit_status == "full") {
    .validate_warm_start_development(model, "pls")
  } else {
    .validate_warm_start_pls_reduced(model)
  }
  invisible(model)
}

.warm_start_pls_reduced <- function(model) {
  out <- .warm_start_deployment_fields(model)
  out$training <- model$training[c("task_id", "n", "n_nonzero", "engine", "engine_version",
    "package_version", "hyperparameters")]
  out$tuning <- model$tuning[c("seed", "ncomp_requested", "ncomp_grid", "conventions")]
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

.validate_warm_start_pls_reduced <- function(model) {
  invalid <- function() rlang::abort("Invalid summary-only PLS artifact contract.")
  t <- model$tuning
  v <- model$validation
  if (!identical(model, .warm_start_pls_reduced(model)) || !is.list(t) || !is.list(v) ||
      !.warm_start_number(t$seed, 0, .Machine$integer.max) || t$seed != floor(t$seed) ||
      !identical(t$conventions, .warm_start_pls_conventions()) ||
      !identical(t$ncomp_grid, .warm_start_pls_control(list(ncomp = t$ncomp_grid))$ncomp) ||
      !model$training$hyperparameters$ncomp %in% t$ncomp_grid ||
      !identical(v$method, "nested_cv") || model$calibration$n != model$training$n) invalid()
  if (is.null(t$ncomp_requested)) {
    if (!identical(t$ncomp_grid, seq_len(max(t$ncomp_grid)))) invalid()
  } else if (!identical(t$ncomp_requested, t$ncomp_grid)) {
    invalid()
  }
  if (max(t$ncomp_grid) > min(length(model$preprocessing$retained), model$training$n - 1L)) invalid()
  for (field in c("outer_folds", "inner_folds", "warning_count")) {
    value <- v[[field]]
    lower <- if (field == "warning_count") 0 else 2
    upper <- if (field == "warning_count") Inf else model$training$n
    if (!.warm_start_number(value, lower, upper) || value != floor(value)) invalid()
  }
  .validate_warm_start_summary_metrics(v$metrics)
  invisible(model)
}
