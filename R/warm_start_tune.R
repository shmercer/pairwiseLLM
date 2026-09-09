.warm_start_cv_context <- function(label, fn) {
  tryCatch(withCallingHandlers(fn(), warning = function(w) {
    rlang::warn(paste0(label, ": ", conditionMessage(w)))
    invokeRestart("muffleWarning")
  }), error = function(e) rlang::abort(paste0(label, ": ", conditionMessage(e)), parent = e))
}

.warm_start_glmnet_path <- function(x, z, alpha, lambda = NULL, lambda_min_ratio = NULL) {
  .warm_start_matrix(x, missing = FALSE)
  .warm_start_outcome_values(z)
  if (!.warm_start_number(alpha, 0, 1) || nrow(x) < 3L || length(z) != nrow(x) ||
      !is.finite(stats::sd(z)) || stats::sd(z) <= 0) {
    rlang::abort("Elastic-net path requires valid alpha, three aligned rows and nonconstant outcomes.")
  }
  if (!is.null(lambda) && (!is.numeric(lambda) || !length(lambda) ||
      any(!is.finite(lambda)) || any(lambda < 0) || any(diff(lambda) >= 0))) {
    rlang::abort("Requested lambda path must be finite, nonnegative and strictly decreasing.")
  }
  if (is.null(lambda_min_ratio)) lambda_min_ratio <- if (ncol(x) > nrow(x)) 0.01 else 0.0001
  .warm_start_require_glmnet()
  exclude <- NULL
  if (ncol(x) == 1L) {
    x <- cbind(x, 0)
    colnames(x)[2] <- make.unique(c(colnames(x)[1], ".excluded_zero"))[2]
    exclude <- 2L
  }
  fit <- do.call(glmnet::glmnet, c(list(x = x, y = z, family = "gaussian", alpha = alpha,
    lambda = lambda, nlambda = 100L, lambda.min.ratio = lambda_min_ratio,
    standardize = FALSE, intercept = TRUE, exclude = exclude), .warm_start_glmnet_controls()))
  if (!isTRUE(fit$jerr == 0)) rlang::abort("Elastic-net path did not converge.")
  if (!length(fit$lambda) || any(!is.finite(fit$lambda)) || any(fit$lambda < 0) ||
      any(diff(fit$lambda) >= 0) || any(!is.finite(fit$a0)) || any(!is.finite(fit$beta)) ||
      length(fit$a0) != length(fit$lambda) ||
      !identical(dim(fit$beta), c(ncol(x), length(fit$lambda))) ||
      (!is.null(lambda) && (length(fit$lambda) != length(lambda) ||
        !isTRUE(all.equal(as.numeric(fit$lambda), as.numeric(lambda), tolerance = 1e-12))))) {
    rlang::abort("Elastic-net path did not return finite coefficients at every requested lambda.")
  }
  fit
}

.warm_start_near <- function(a, b) abs(a - b) <= 1e-10 * pmax(1, abs(a), abs(b))

.warm_start_alpha_choice <- function(errors) which(.warm_start_near(errors, min(errors)))[1]

.warm_start_loss_summary <- function(loss, sizes) {
  if (!is.matrix(loss) || !is.numeric(loss) || nrow(loss) < 2L || ncol(loss) < 1L ||
      any(!is.finite(loss)) || any(loss < 0) || !is.numeric(sizes) ||
      length(sizes) != nrow(loss) || any(!is.finite(sizes)) || any(sizes <= 0)) {
    rlang::abort("CV loss requires finite nonnegative fold errors and positive aligned fold sizes.")
  }
  weights <- sizes / sum(sizes)
  cvm <- colSums(loss * weights)
  cvsd <- sqrt(colSums(sweep(loss, 2, cvm)^2 * weights) / (nrow(loss) - 1L))
  list(cvm = cvm, cvsd = cvsd)
}

.warm_start_lambda_choice <- function(lambda, cvm, cvsd, rule) {
  minimum <- min(cvm)
  index_min <- which(.warm_start_near(cvm, minimum))[1]
  threshold <- cvm[index_min] + cvsd[index_min]
  index_1se <- which(cvm <= threshold | .warm_start_near(cvm, threshold))[1]
  list(index_min = index_min, index_1se = index_1se,
    index = if (rule == "lambda.1se") index_1se else index_min,
    lambda_min = lambda[index_min], lambda_1se = lambda[index_1se])
}

.warm_start_tuning_conventions <- function() {
  list(path = "per_alpha_reference_on_tuning_dataset_exact_inner_lambdas", nlambda = 100L,
    lambda_min_ratio = "0.01_if_retained_p_gt_n_else_0.0001", loss = "squared_error",
    aggregation = "fold_MSE_weighted_by_holdout_n",
    se = "sqrt(weighted_mean((fold_MSE-cvm)^2)/(K-1))",
    tie_tolerance = 1e-10, tie_scale = "max(1,abs(a),abs(b))",
    lambda_tie = "largest_lambda", alpha_tie = "smallest_alpha",
    alpha_selection = "cvm_at_lambda_min", prediction = "exact_fitted_coefficients")
}

.warm_start_tune <- function(x, z, foldid, alpha_grid, lambda_rule) {
  k <- max(foldid)
  splits <- lapply(seq_len(k), function(fold) {
    .warm_start_cv_context(paste("Inner fold", fold), function() {
      train <- which(foldid != fold)
      test <- which(foldid == fold)
      if (length(train) < 3L || !length(test) || stats::sd(z[train]) <= 0) {
        rlang::abort("Requested split requires a nonempty holdout and three nonconstant training rows.")
      }
      p <- .warm_start_preprocess_fit(x[train, , drop = FALSE])
      list(train = train, test = test, preprocessing = p,
        x_train = .warm_start_preprocess_apply(x[train, , drop = FALSE], p),
        x_test = .warm_start_preprocess_apply(x[test, , drop = FALSE], p))
    })
  })
  reference <- .warm_start_preprocess_fit(x)
  reference_x <- .warm_start_preprocess_apply(x, reference)
  ratio <- if (ncol(reference_x) > nrow(reference_x)) 0.01 else 0.0001
  traces <- vector("list", length(alpha_grid))
  candidates <- vector("list", length(alpha_grid))
  alpha_errors <- numeric(length(alpha_grid))
  sizes <- vapply(splits, function(s) length(s$test), integer(1))
  for (i in seq_along(alpha_grid)) {
    alpha <- alpha_grid[i]
    result <- .warm_start_cv_context(paste("Alpha", alpha), function() {
      ref <- .warm_start_glmnet_path(reference_x, z, alpha, lambda_min_ratio = ratio)
      lambda <- as.numeric(ref$lambda)
      oof <- matrix(NA_real_, nrow(x), length(lambda))
      loss <- matrix(NA_real_, k, length(lambda))
      for (fold in seq_len(k)) {
        s <- splits[[fold]]
        predicted <- .warm_start_cv_context(paste("Inner fold", fold), function() {
          fit <- .warm_start_glmnet_path(s$x_train, z[s$train], alpha, lambda)
          coefficients <- as.matrix(fit$beta)[colnames(s$x_train), , drop = FALSE]
          sweep(s$x_test %*% coefficients, 2, fit$a0, "+")
        })
        if (any(!is.finite(predicted))) rlang::abort("Nonfinite inner predictions.")
        oof[s$test, ] <- predicted
        loss[fold, ] <- colMeans((predicted - z[s$test])^2)
      }
      errors <- .warm_start_loss_summary(loss, sizes)
      choice <- .warm_start_lambda_choice(lambda, errors$cvm, errors$cvsd, lambda_rule)
      list(trace = c(list(alpha = alpha, lambda = lambda, fold_mse = loss,
        fold_sizes = sizes), errors, choice), oof = as.numeric(oof[, choice$index]))
    })
    traces[[i]] <- result$trace
    alpha_errors[i] <- result$trace$cvm[result$trace$index_min]
    candidates[[i]] <- result$oof
  }
  index <- .warm_start_alpha_choice(alpha_errors)
  selected_trace <- traces[[index]]
  best <- list(alpha = alpha_grid[index], lambda = selected_trace$lambda[selected_trace$index],
    alpha_index = index, error = alpha_errors[index], oof = candidates[[index]])
  list(alpha_grid = alpha_grid, lambda_rule = lambda_rule, foldid = foldid,
    conventions = .warm_start_tuning_conventions(), reference_preprocessing = reference,
    lambda_min_ratio = ratio,
    inner_preprocessing = lapply(splits, `[[`, "preprocessing"), traces = traces,
    selected = best, oof = data.frame(row = seq_len(nrow(x)), fold = foldid,
      observed = z, raw_prediction = best$oof))
}
