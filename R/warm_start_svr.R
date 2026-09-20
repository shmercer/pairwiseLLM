# RBF-SVR development uses e1071; all deployment operations use numeric values.
.warm_start_svr_available <- function() requireNamespace("e1071", quietly = TRUE)

.warm_start_require_svr <- function() {
  if (!.warm_start_svr_available()) {
    rlang::abort("Model development requires optional package 'e1071'. Install it explicitly first.")
  }
}

.warm_start_svr_control <- function(control) {
  defaults <- list(cost = 2^(-2:4), gamma_multiplier = 2^(-2:2))
  if (is.null(control) || identical(control, list())) return(defaults)
  if (!is.list(control) || !.warm_start_names(names(control)) ||
      !all(names(control) %in% names(defaults))) {
    rlang::abort("SVR `engine_control` accepts only named `cost` and `gamma_multiplier` vectors.")
  }
  for (name in names(control)) {
    value <- control[[name]]
    if (!is.numeric(value) || is.object(value) || !is.null(dim(value)) || !length(value) ||
        any(!is.finite(value)) || any(value <= 0) || anyDuplicated(value)) {
      rlang::abort(paste0("SVR `", name, "` must contain unique finite positive values."))
    }
    defaults[[name]] <- sort(as.numeric(value))
  }
  defaults
}

.warm_start_svr_conventions <- function() {
  list(type = "eps-regression", kernel = "radial", scale = FALSE, cross = 0,
    probability = FALSE, epsilon = 0.10, gamma = "multiplier_per_fit_retained_p",
    loss = "squared_error", aggregation = "fold_MSE_weighted_by_holdout_n",
    se = "sqrt(weighted_mean((fold_MSE-cvm)^2)/(K-1))",
    tie_tolerance = 1e-10, tie_scale = "max(1,abs(a),abs(b))",
    selection = "1se", candidate_tie = "lower_cost_then_lower_gamma_multiplier")
}

.warm_start_svr_grid <- function(control) {
  data.frame(cost = rep(control$cost, each = length(control$gamma_multiplier)),
    gamma_multiplier = rep(control$gamma_multiplier, times = length(control$cost)))
}

.warm_start_svr_choice <- function(cvm, cvsd) {
  # Candidates are in ascending cost, then ascending gamma-multiplier order.
  index_min <- which(.warm_start_near(cvm, min(cvm)))[1]
  threshold <- cvm[index_min] + cvsd[index_min]
  index_1se <- which(cvm <= threshold | .warm_start_near(cvm, threshold))[1]
  list(index_min = index_min, index_1se = index_1se, index = index_1se)
}

.warm_start_svr_fit <- function(x, z, cost, gamma_multiplier) {
  .warm_start_matrix(x, missing = FALSE)
  .warm_start_outcome_values(z)
  if (!.warm_start_number(cost) || cost <= 0 || !.warm_start_number(gamma_multiplier) ||
      gamma_multiplier <= 0 || nrow(x) < 3L || length(z) != nrow(x) ||
      !is.finite(stats::sd(z)) || stats::sd(z) <= 0) {
    rlang::abort("SVR requires positive cost/multiplier, three aligned rows and nonconstant outcomes.")
  }
  gamma <- gamma_multiplier / ncol(x)
  if (!is.finite(gamma) || gamma <= 0) rlang::abort("SVR actual gamma must be finite and positive.")
  .warm_start_require_svr()
  fit <- e1071::svm(x = x, y = z, type = "eps-regression", kernel = "radial",
    cost = cost, gamma = gamma, epsilon = 0.10, scale = FALSE, cross = 0, probability = FALSE)
  .warm_start_svr_payload(fit, colnames(x))
}

.warm_start_svr_payload <- function(fit, retained) {
  # Copy only deployment values, including one-column/one-support-vector matrices.
  if (!is.matrix(fit$SV) || ncol(fit$SV) != length(retained) ||
      (!identical(colnames(fit$SV), retained) &&
        !(length(retained) == 1L && is.null(colnames(fit$SV)))) || !is.matrix(fit$coefs) ||
      !identical(dim(fit$coefs), c(nrow(fit$SV), 1L))) {
    rlang::abort("SVR returned an incomplete support-vector fit.")
  }
  payload <- list(type = "rbf_svr", support_vectors = matrix(as.numeric(fit$SV),
    nrow = nrow(fit$SV), ncol = length(retained), dimnames = list(NULL, retained)),
    dual = as.numeric(fit$coefs), rho = as.numeric(fit$rho), gamma = as.numeric(fit$gamma))
  .validate_warm_start_svr_payload(payload, retained)
  payload
}

.validate_warm_start_svr_payload <- function(payload, retained) {
  invalid <- function() rlang::abort("Invalid numeric RBF-SVR deployment payload.")
  if (!is.list(payload) || !identical(names(payload), c("type", "support_vectors", "dual", "rho", "gamma")) ||
      !identical(payload$type, "rbf_svr") || !.warm_start_portable(payload)) invalid()
  sv <- payload$support_vectors
  if (!is.matrix(sv) || !is.numeric(sv) || nrow(sv) < 1L ||
      !identical(colnames(sv), retained) || ncol(sv) != length(retained) ||
      any(!is.finite(sv)) || !is.numeric(payload$dual) || is.object(payload$dual) ||
      !is.null(dim(payload$dual)) || length(payload$dual) != nrow(sv) ||
      any(!is.finite(payload$dual)) || !.warm_start_number(payload$rho) ||
      !.warm_start_number(payload$gamma) || payload$gamma <= 0) invalid()
  invisible(payload)
}

.warm_start_svr_predict <- function(payload, x) {
  .warm_start_matrix(x, missing = FALSE)
  .validate_warm_start_svr_payload(payload, colnames(x))
  sv <- payload$support_vectors
  # Direct differences avoid cancellation from subtracting two large squared norms.
  distance <- matrix(0, nrow(x), nrow(sv))
  for (j in seq_len(ncol(x))) distance <- distance + outer(x[, j], sv[, j], "-")^2
  if (any(!is.finite(distance))) rlang::abort("Nonfinite SVR squared distances.")
  value <- as.numeric(exp(-payload$gamma * distance) %*% payload$dual - payload$rho)
  if (any(!is.finite(value))) rlang::abort("Nonfinite SVR predictions.")
  value
}

.warm_start_svr_tune <- function(x, z, foldid, control) {
  grid <- .warm_start_svr_grid(control)
  k <- max(foldid)
  splits <- lapply(seq_len(k), function(fold) {
    .warm_start_cv_context(paste("Inner fold", fold), function() {
      train <- which(foldid != fold)
      test <- which(foldid == fold)
      preprocessing <- .warm_start_preprocess_fit(x[train, , drop = FALSE])
      list(train = train, test = test, preprocessing = preprocessing,
        x_train = .warm_start_preprocess_apply(x[train, , drop = FALSE], preprocessing),
        x_test = .warm_start_preprocess_apply(x[test, , drop = FALSE], preprocessing))
    })
  })
  reference <- .warm_start_preprocess_fit(x)
  retained_p <- c(context_refit = length(reference$retained),
    stats::setNames(vapply(splits, function(s) ncol(s$x_train), integer(1)), paste0("inner_", seq_len(k))))
  actual_gamma <- outer(as.numeric(retained_p), grid$gamma_multiplier, function(p, m) m / p)
  dimnames(actual_gamma) <- list(names(retained_p), NULL)
  oof <- matrix(NA_real_, nrow(x), nrow(grid))
  loss <- matrix(NA_real_, k, nrow(grid))
  for (fold in seq_len(k)) {
    s <- splits[[fold]]
    for (j in seq_len(nrow(grid))) {
      label <- paste("Inner fold", fold, "SVR cost", grid$cost[j], "gamma_multiplier", grid$gamma_multiplier[j])
      predicted <- .warm_start_cv_context(label, function() {
        payload <- .warm_start_svr_fit(s$x_train, z[s$train], grid$cost[j], grid$gamma_multiplier[j])
        .warm_start_engine_predict(payload, s$x_test)
      })
      oof[s$test, j] <- predicted
      loss[fold, j] <- mean((predicted - z[s$test])^2)
    }
  }
  sizes <- tabulate(foldid)
  errors <- .warm_start_loss_summary(loss, sizes)
  choice <- .warm_start_svr_choice(errors$cvm, errors$cvsd)
  selected <- list(cost = grid$cost[choice$index], gamma_multiplier = grid$gamma_multiplier[choice$index],
    error = errors$cvm[choice$index_min], oof = as.numeric(oof[, choice$index]))
  list(control = control, grid = grid, retained_p = retained_p, actual_gamma = actual_gamma,
    foldid = foldid, conventions = .warm_start_svr_conventions(), reference_preprocessing = reference,
    inner_preprocessing = lapply(splits, `[[`, "preprocessing"),
    traces = c(list(fold_mse = loss, fold_sizes = sizes, candidate_oof = oof), errors, choice),
    selected = selected, oof = data.frame(row = seq_len(nrow(x)), fold = foldid,
      observed = z, raw_prediction = selected$oof))
}

.warm_start_svr_hyperparameters <- function(selected, p) {
  list(cost = selected$cost, gamma_multiplier = selected$gamma_multiplier,
    epsilon = 0.10, gamma = selected$gamma_multiplier / p)
}
