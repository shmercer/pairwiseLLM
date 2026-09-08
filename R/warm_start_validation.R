# Portable audit validation uses base values only; no development engine is loaded.
.warm_start_audit_equal <- function(x, y) isTRUE(all.equal(x, y, tolerance = 1e-10))

.validate_warm_start_tuning <- function(x, n) {
  invalid <- function() rlang::abort("Invalid warm-start tuning contract.")
  if (!is.list(x) || !is.numeric(x$alpha_grid) || !length(x$alpha_grid) ||
      any(!is.finite(x$alpha_grid)) || any(x$alpha_grid < 0 | x$alpha_grid > 1) ||
      any(diff(x$alpha_grid) <= 0) || !is.character(x$lambda_rule) || length(x$lambda_rule) != 1L ||
      is.na(x$lambda_rule) || !x$lambda_rule %in% c("lambda.1se", "lambda.min") ||
      !is.integer(x$foldid) || length(x$foldid) != n || anyNA(x$foldid) ||
      min(x$foldid) != 1L || !identical(sort(unique(x$foldid)), seq_len(max(x$foldid))) ||
      max(x$foldid) < 2L || !identical(x$conventions, .warm_start_tuning_conventions()) ||
      !is.list(x$traces) || length(x$traces) != length(x$alpha_grid) ||
      !is.list(x$inner_preprocessing) || length(x$inner_preprocessing) != max(x$foldid)) invalid()
  sizes <- tabulate(x$foldid)
  .validate_warm_start_preprocess(x$reference_preprocessing)
  if (x$reference_preprocessing$n_training != n ||
      !identical(x$lambda_min_ratio,
        if (length(x$reference_preprocessing$retained) > n) 0.01 else 0.0001)) invalid()
  for (fold in seq_along(sizes)) {
    p <- x$inner_preprocessing[[fold]]
    .validate_warm_start_preprocess(p)
    if (p$n_training != n - sizes[fold] || p$n_training < 3L ||
        !identical(p$features, x$reference_preprocessing$features)) invalid()
  }
  errors <- numeric(length(x$traces))
  for (i in seq_along(x$traces)) {
    t <- x$traces[[i]]
    if (!is.list(t) || !identical(t$alpha, x$alpha_grid[i]) || !is.numeric(t$lambda) ||
        !length(t$lambda) || any(!is.finite(t$lambda)) || any(t$lambda < 0) ||
        any(diff(t$lambda) >= 0) || !is.matrix(t$fold_mse) ||
        !identical(dim(t$fold_mse), c(length(sizes), length(t$lambda))) ||
        !identical(t$fold_sizes, sizes)) invalid()
    summary <- .warm_start_loss_summary(t$fold_mse, sizes)
    if (!.warm_start_audit_equal(t$cvm, summary$cvm) ||
        !.warm_start_audit_equal(t$cvsd, summary$cvsd)) invalid()
    choice <- .warm_start_lambda_choice(t$lambda, t$cvm, t$cvsd, x$lambda_rule)
    if (!.warm_start_audit_equal(t[names(choice)], choice)) invalid()
    errors[i] <- t$cvm[t$index_min]
  }
  index <- .warm_start_alpha_choice(errors)
  selected <- x$selected
  trace <- x$traces[[index]]
  if (!is.list(selected) || !identical(selected$alpha_index, index) ||
      !identical(selected$alpha, x$alpha_grid[index]) ||
      !identical(selected$lambda, trace$lambda[trace$index]) ||
      !identical(selected$error, errors[index]) || !is.numeric(selected$oof) ||
      length(selected$oof) != n || any(!is.finite(selected$oof)) ||
      !is.data.frame(x$oof) || nrow(x$oof) != n ||
      !identical(x$oof$row, seq_len(n)) || !identical(x$oof$fold, x$foldid) ||
      !identical(x$oof$raw_prediction, selected$oof) || !is.numeric(x$oof$observed) ||
      length(x$oof$observed) != n || any(!is.finite(x$oof$observed))) invalid()
  invisible(x)
}

.validate_warm_start_development <- function(model) {
  invalid <- function() rlang::abort("Invalid warm-start nested validation contract.")
  n <- model$training$n
  t <- model$tuning
  .validate_warm_start_tuning(t, n)
  if (!identical(.warm_start_ids(t$ids), t$ids) || length(t$ids) != n ||
      !.warm_start_number(t$seed, 0, .Machine$integer.max) || t$seed != floor(t$seed) ||
      !identical(model$preprocessing, t$reference_preprocessing) ||
      !identical(model$training$alpha, t$selected$alpha) ||
      !identical(model$training$lambda, t$selected$lambda) ||
      !.warm_start_audit_equal(model$calibration,
        .warm_start_calibration_fit(t$oof$raw_prediction, t$oof$observed))) invalid()
  v <- model$validation
  if (!is.list(v) || !identical(v$method, "nested_cv") ||
      !.warm_start_number(v$outer_folds, 2, n) || v$outer_folds != floor(v$outer_folds) ||
      !identical(v$inner_folds, max(t$foldid)) || !is.list(v$folds) ||
      length(v$folds) != v$outer_folds || !is.character(v$warnings) || anyNA(v$warnings)) invalid()
  p <- v$predictions
  if (!is.data.frame(p) || nrow(p) != n || !identical(p$item_id, t$ids) ||
      !is.integer(p$fold) || anyNA(p$fold) ||
      !identical(sort(unique(p$fold)), seq_len(v$outer_folds))) invalid()
  for (name in c("observed", "raw_prediction", "calibrated_prediction")) {
    if (!is.numeric(p[[name]]) || length(p[[name]]) != n || any(!is.finite(p[[name]]))) invalid()
  }
  theta <- t$oof$observed * model$outcome$sd + model$outcome$mean
  if (!.warm_start_audit_equal(model$outcome, .warm_start_outcome_fit(theta))) invalid()
  for (fold in seq_len(v$outer_folds)) {
    record <- v$folds[[fold]]
    train <- which(p$fold != fold)
    test <- which(p$fold == fold)
    if (!is.list(record)) invalid()
    .validate_warm_start_tuning(record$tuning, length(train))
    .validate_warm_start_outcome(record$outcome)
    .validate_warm_start_preprocess(record$preprocessing)
    .validate_warm_start_calibration(record$calibration)
    if (!identical(record$train_ids, t$ids[train]) || !identical(record$test_ids, t$ids[test]) ||
        !identical(record$tuning$alpha_grid, t$alpha_grid) ||
        !identical(record$tuning$lambda_rule, t$lambda_rule) ||
        !identical(max(record$tuning$foldid), v$inner_folds) ||
        !identical(record$preprocessing, record$tuning$reference_preprocessing) ||
        !.warm_start_audit_equal(record$outcome, .warm_start_outcome_fit(theta[train])) ||
        !.warm_start_audit_equal(record$tuning$oof$observed,
          .warm_start_outcome_apply(theta[train], record$outcome)) ||
        !.warm_start_audit_equal(p$observed[test], .warm_start_outcome_apply(theta[test], record$outcome)) ||
        !.warm_start_named_numeric(record$coefficients, record$preprocessing$retained) ||
        !.warm_start_number(record$intercept) ||
        !identical(record$n_nonzero, sum(record$coefficients != 0)) ||
        !.warm_start_audit_equal(record$calibration, .warm_start_calibration_fit(
          record$tuning$oof$raw_prediction, record$tuning$oof$observed)) ||
        !.warm_start_audit_equal(unname(as.matrix(record$predictions[, -1])),
          unname(as.matrix(p[test, c("observed", "raw_prediction", "calibrated_prediction")])))) invalid()
    if (!identical(record$predictions$item_id, t$ids[test]) ||
        !.warm_start_audit_equal(p$calibrated_prediction[test],
          .warm_start_calibration_apply(p$raw_prediction[test], record$calibration))) invalid()
  }
  if (!.warm_start_audit_equal(v$metrics,
      .warm_start_validation_metrics(p$calibrated_prediction, p$observed))) invalid()
  invisible(model)
}
