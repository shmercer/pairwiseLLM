# PLS development only. Deployment uses the shared numeric linear payload.
.warm_start_pls_available <- function() requireNamespace("pls", quietly = TRUE)

.warm_start_require_pls <- function() {
  if (!.warm_start_pls_available()) {
    rlang::abort("Model development requires optional package 'pls'. Install it explicitly first.")
  }
}

.warm_start_pls_control <- function(control) {
  if (is.null(control) || identical(control, list())) return(list(ncomp = NULL))
  if (!is.list(control) || !identical(names(control), "ncomp")) {
    rlang::abort("PLS `engine_control` accepts only a named `ncomp` candidate vector.")
  }
  ncomp <- control$ncomp
  if (!is.numeric(ncomp) || is.object(ncomp) || !is.null(dim(ncomp)) || !length(ncomp) ||
      any(!is.finite(ncomp)) || any(ncomp < 1 | ncomp > 10 | ncomp != floor(ncomp)) ||
      anyDuplicated(ncomp)) {
    rlang::abort("PLS `ncomp` must contain unique positive integers no greater than 10.")
  }
  list(ncomp = sort(as.integer(ncomp)))
}

.warm_start_pls_conventions <- function() {
  list(method = "kernelpls", scale = FALSE, validation = "none", center = TRUE,
    rank_tolerance = 1e-7, rank_method = "centered_QR_non_LAPACK", cap = 10L,
    grid = "common_inner_and_context_refit", loss = "squared_error",
    aggregation = "fold_MSE_weighted_by_holdout_n",
    se = "sqrt(weighted_mean((fold_MSE-cvm)^2)/(K-1))",
    tie_tolerance = 1e-10, tie_scale = "max(1,abs(a),abs(b))",
    selection = "1se", component_tie = "fewest_components")
}

.warm_start_pls_bound <- function(x) {
  .warm_start_matrix(x, missing = FALSE)
  rank <- qr(scale(x, center = TRUE, scale = FALSE), tol = 1e-7, LAPACK = FALSE)$rank
  list(n = nrow(x), p = ncol(x), rank = rank,
    max_ncomp = as.integer(min(10L, rank, ncol(x), nrow(x) - 1L)))
}

.warm_start_pls_grid <- function(bounds, requested) {
  limit <- min(vapply(bounds, `[[`, integer(1), "max_ncomp"))
  if (limit < 1L) rlang::abort("PLS has no legal components across inner fits and the context refit.")
  if (is.null(requested)) return(seq_len(limit))
  if (any(requested > limit)) {
    rlang::abort(paste0("PLS requested ncomp exceeds the common inner/refit bound of ", limit, "."))
  }
  requested
}

.warm_start_pls_choice <- function(ncomp, cvm, cvsd) {
  index_min <- which(.warm_start_near(cvm, min(cvm)))[1]
  threshold <- cvm[index_min] + cvsd[index_min]
  index_1se <- which(cvm <= threshold | .warm_start_near(cvm, threshold))[1]
  list(index_min = index_min, index_1se = index_1se, index = index_1se,
    ncomp_min = ncomp[index_min], ncomp_1se = ncomp[index_1se])
}

.warm_start_pls_fit <- function(x, z, ncomp) {
  .warm_start_matrix(x, missing = FALSE)
  .warm_start_outcome_values(z)
  if (!.warm_start_number(ncomp, 1, .warm_start_pls_bound(x)$max_ncomp) ||
      ncomp != floor(ncomp) || nrow(x) < 3L || length(z) != nrow(x) || stats::sd(z) <= 0) {
    rlang::abort("PLS requires legal components, three aligned rows and nonconstant outcomes.")
  }
  .warm_start_require_pls()
  fit <- pls::plsr(z ~ x, ncomp = ncomp, method = "kernelpls", scale = FALSE,
    validation = "none", center = TRUE)
  fields <- c("coefficients", "Xmeans", "Ymeans", "scores", "loadings", "loading.weights",
    "Yscores", "Yloadings", "projection", "fitted.values", "residuals", "Xvar", "Xtotvar")
  if (any(vapply(fields, function(name) {
    value <- fit[[name]]
    !is.numeric(value) || !length(value) || any(!is.finite(value))
  }, logical(1))) || !identical(dim(fit$coefficients), c(ncol(x), 1L, as.integer(ncomp)))) {
    rlang::abort("PLS returned a nonfinite or incomplete fit (zero covariance or latent saturation).")
  }
  fit
}

.warm_start_pls_payload <- function(fit, ncomp, retained) {
  beta <- as.numeric(fit$coefficients[, 1L, ncomp])
  payload <- list(type = "linear", coefficients = stats::setNames(beta, retained),
    intercept = as.numeric(fit$Ymeans - fit$Xmeans %*% beta))
  .validate_warm_start_engine_payload(payload, "pls", retained)
  payload
}

.warm_start_pls_tune <- function(x, z, foldid, control) {
  # Control was normalized at the public boundary; default NULL remains distinct
  # from an explicit vector so legality can be checked separately in each context.
  requested <- control$ncomp
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
  reference_x <- .warm_start_preprocess_apply(x, reference)
  bounds <- c(list(context_refit = .warm_start_pls_bound(reference_x)),
    stats::setNames(lapply(splits, function(s) .warm_start_pls_bound(s$x_train)), paste0("inner_", seq_len(k))))
  ncomp <- .warm_start_pls_grid(bounds, requested)
  oof <- matrix(NA_real_, nrow(x), length(ncomp))
  loss <- matrix(NA_real_, k, length(ncomp))
  for (fold in seq_len(k)) {
    s <- splits[[fold]]
    predicted <- .warm_start_cv_context(paste("Inner fold", fold, "PLS ncomp", max(ncomp)), function() {
      fit <- .warm_start_pls_fit(s$x_train, z[s$train], max(ncomp))
      values <- vapply(ncomp, function(count) {
        .warm_start_engine_predict(.warm_start_pls_payload(fit, count, colnames(s$x_train)), s$x_test)
      }, numeric(length(s$test)))
      values <- matrix(values, nrow = length(s$test), ncol = length(ncomp))
      if (any(!is.finite(values))) rlang::abort("Nonfinite PLS inner predictions.")
      values
    })
    oof[s$test, ] <- predicted
    loss[fold, ] <- colMeans((predicted - z[s$test])^2)
  }
  sizes <- tabulate(foldid)
  errors <- .warm_start_loss_summary(loss, sizes)
  choice <- .warm_start_pls_choice(ncomp, errors$cvm, errors$cvsd)
  selected <- list(ncomp = ncomp[choice$index], error = errors$cvm[choice$index_min],
    oof = as.numeric(oof[, choice$index]))
  list(ncomp_requested = requested, ncomp_grid = ncomp, bounds = bounds, foldid = foldid,
    conventions = .warm_start_pls_conventions(), reference_preprocessing = reference,
    inner_preprocessing = lapply(splits, `[[`, "preprocessing"),
    traces = c(list(ncomp = ncomp, fold_mse = loss, fold_sizes = sizes, candidate_oof = oof), errors, choice),
    selected = selected, oof = data.frame(row = seq_len(nrow(x)), fold = foldid,
      observed = z, raw_prediction = selected$oof))
}
