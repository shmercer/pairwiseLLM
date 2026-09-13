# Shape-constrained cumulative-logit calibration on accepted CJ point locations.

.rubric_mgcv_available <- function() {
  requireNamespace("mgcv", quietly = TRUE) && utils::packageVersion("mgcv") >= "1.9.4"
}

.rubric_monotone_rng_available <- function() requireNamespace("withr", quietly = TRUE)

.rubric_monotone_dependencies <- function(fitting = FALSE) {
  if (!.rubric_mgcv_available()) {
    rlang::abort(paste0("Monotone ordinal fitting and prediction require optional 'mgcv' >= 1.9-4. ",
      "Install or update it with install.packages(\"mgcv\")."),
      class = "pairwiseLLM_rubric_dependency_missing")
  }
  if (fitting && !.rubric_monotone_rng_available()) {
    rlang::abort(paste0("Monotone ordinal fitting requires optional 'withr' to preserve RNG state. ",
      "Install it with install.packages(\"withr\")."),
      class = "pairwiseLLM_rubric_dependency_missing")
  }
}

.rubric_monotone_controls <- function(...) {
  dots <- rlang::list2(...)
  if (length(dots) && (is.null(names(dots)) || any(!nzchar(names(dots))) ||
    anyDuplicated(names(dots)) || any(!names(dots) %in% c("k", "sp")))) {
    rlang::abort("Monotone ordinal `...` accepts only uniquely named `k` and `sp` controls.")
  }
  k <- if ("k" %in% names(dots)) dots$k else 6L
  sp <- dots$sp
  if (!is.numeric(k) || !is.null(dim(k)) || length(k) != 1L || !is.finite(k) ||
    k < 5 || k > .Machine$integer.max || k != as.integer(k)) {
    rlang::abort("Monotone basis dimension `k` must be a single integer >= 5.")
  }
  if (!is.null(sp) && (!is.numeric(sp) || !is.null(dim(sp)) || length(sp) != 1L ||
    !is.finite(sp) || sp <= 0)) {
    rlang::abort("Monotone smoothing `sp` must be NULL or a single finite positive number.")
  }
  list(k = as.integer(k), sp = sp)
}

.rubric_fit_ordinal_monotone <- function(object, controls) {
  .rubric_monotone_dependencies(fitting = TRUE)
  data <- object$calibration_data[!is.na(object$calibration_data$category), ]
  center <- mean(data$theta)
  scale <- stats::sd(data$theta)
  if (!is.finite(center) || !is.finite(scale) || scale <= 0) {
    rlang::abort("Labeled calibration CJ scores must have a finite, nonzero standard deviation.")
  }
  n_unique <- length(unique(data$theta))
  if (n_unique < 5L) {
    rlang::abort("Monotone ordinal calibration requires at least five unique labeled CJ scores for its cubic basis.")
  }
  k <- min(controls$k, n_unique)
  data <- data.frame(category = as.integer(data$category), z = (data$theta - center) / scale)
  if (any(!is.finite(data$z))) rlang::abort("Standardized calibration scores must be finite.")
  conditions <- new.env(parent = emptyenv())
  conditions$warnings <- character()
  if (k < controls$k) {
    conditions$warnings <- paste0("Monotone basis dimension reduced from ", controls$k, " to ", k,
      " (the number of unique labeled CJ scores).")
  }
  # Literal controls and a namespace environment avoid serializing fit locals.
  formula <- stats::as.formula(paste0("category ~ s(z, bs = 'sc', xt = 'm+', k = ", k, ")"),
    env = asNamespace("mgcv"))
  model <- tryCatch(withCallingHandlers(
    .pairwiseLLM_with_seed(1L, function() {
      mgcv::scasm(formula, family = mgcv::ocat(R = object$K), data = data, sp = controls$sp, bs = 0)
    }), warning = function(w) {
      conditions$warnings <- c(conditions$warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) {
      rlang::abort(paste0("Monotone ordinal calibration could not be fitted. ",
        "Inspect sparse/separated labels, score orientation, and basis dimension; no alternative model was fitted."),
        class = "pairwiseLLM_rubric_monotone_fit_error", parent = e)
    })
  # scasm rebuilds these with .GlobalEnv even when the input is namespace-scoped.
  for (field in c("formula", "terms", "pred.formula", "pterms")) {
    environment(model[[field]]) <- asNamespace("mgcv")
  }
  environment(attr(model$model, "terms")) <- asNamespace("mgcv")
  object$transformation <- list(center = center, scale = scale)
  smooth <- model$smooth[[1L]]
  object$backend <- list(name = "mgcv::scasm", version = as.character(utils::packageVersion("mgcv")),
    link = "logit", latent_link = "identity", model = model,
    thresholds = model$family$getTheta(TRUE), intercept = unname(model$coefficients["(Intercept)"]),
    basis = list(bs = "sc", constraint = "m+", k_requested = controls$k, k = k,
      order = smooth$m, knots = smooth$knots),
    smoothing = list(method = "EFS", sp_requested = controls$sp, sp = model$full.sp %||% model$sp,
      penalties = smooth$S), edf = sum(model$edf),
    smooth_edf = sum(model$edf[seq.int(smooth$first.para, smooth$last.para)]),
    convergence = model$outer.info, threshold_standard_errors = c(0, rep(NA_real_, object$K - 2L)))
  object$status <- "fitted"
  .rubric_validate_ordinal_monotone(object)

  grid <- seq(min(data$z), max(data$z), length.out = 1001L)
  eta <- .rubric_monotone_eta(grid, object)
  monotonicity <- .rubric_monotone_grid_check(eta, object$backend$thresholds)
  boundaries <- .rubric_monotone_boundaries(object, grid, eta)
  object$backend$cutpoints_z <- boundaries$cutpoints
  object$backend$cutpoints_theta <- center + scale * boundaries$cutpoints
  object$backend$cutpoint_status <- boundaries$status
  warnings <- conditions$warnings
  converged <- isTRUE(model$outer.info$converged)
  if (!converged) warnings <- c(warnings, "Monotone ordinal convergence requires review; inspect backend$convergence.")
  if (monotonicity$essentially_flat) {
    warnings <- c(warnings,
      "The monotone ordinal relationship is essentially flat; check rubric coding and calibration.")
  }
  singletons <- names(object$category_counts)[object$category_counts == 1L]
  if (length(singletons)) {
    warnings <- c(warnings, paste0("Sparse calibration: only one labeled response in categories ",
      paste(singletons, collapse = ", "), ". Threshold uncertainty is unavailable."))
  }
  object$diagnostics$category_probabilities_available <- TRUE
  object$diagnostics$ordinal <- list(converged = converged, convergence = model$outer.info,
    singleton_categories = singletons, conditional_on_cj = TRUE,
    threshold_uncertainty_available = FALSE, monotonicity = monotonicity)
  object$warnings <- unique(c(object$warnings, warnings))
  if (length(warnings)) {
    rlang::warn(paste(unique(warnings), collapse = "\n"), class = "pairwiseLLM_rubric_ordinal_diagnostics")
  }
  object
}

.rubric_validate_ordinal_monotone <- function(object) {
  backend <- object$backend
  transformation <- object$transformation
  scalar <- function(x) is.numeric(x) && is.null(dim(x)) && length(x) == 1L && is.finite(x)
  if (object$calibration_design != "same_set" || !is.list(backend) ||
    !identical(backend$name, "mgcv::scasm") || !identical(backend$link, "logit") ||
    !identical(backend$latent_link, "identity") || !scalar(backend$intercept) ||
    !is.numeric(backend$thresholds) || !is.null(dim(backend$thresholds)) ||
    length(backend$thresholds) != object$K - 1L || any(!is.finite(backend$thresholds)) ||
    any(diff(backend$thresholds) <= 0) || backend$thresholds[[1L]] != -1 ||
    !is.list(backend$basis) || !identical(backend$basis$bs, "sc") ||
    !identical(backend$basis$constraint, "m+") || !scalar(backend$basis$k) || backend$basis$k < 5 ||
    !inherits(backend$model, "gam") || length(backend$model$smooth) != 1L ||
    !identical(backend$model$smooth[[1L]]$xt, "m+") ||
    !is.numeric(backend$model$coefficients) || any(!is.finite(backend$model$coefficients)) ||
    !is.list(transformation) || !scalar(transformation$center) ||
    !scalar(transformation$scale) || transformation$scale <= 0 ||
    !is.numeric(object$calibration_range) || length(object$calibration_range) != 2L ||
    any(!is.finite(object$calibration_range)) || diff(object$calibration_range) <= 0) {
    rlang::abort("Invalid fitted monotone ordinal model, thresholds, basis, transformation, range, or design.")
  }
  invisible(object)
}

.rubric_monotone_eta <- function(z, object) {
  if (any(!is.finite(z))) rlang::abort("Standardized prediction scores must be finite.")
  eta <- as.vector(mgcv::predict.gam(object$backend$model, newdata = data.frame(z = z), type = "link"))
  if (length(eta) != length(z) || any(!is.finite(eta))) {
    rlang::abort("Monotone ordinal predictions did not produce finite latent locations.")
  }
  eta
}

.rubric_monotone_cumulative <- function(eta, thresholds) {
  stats::plogis(outer(eta, thresholds, function(eta, threshold) threshold - eta))
}

.rubric_monotone_grid_check <- function(eta, thresholds) {
  cumulative <- .rubric_monotone_cumulative(eta, thresholds)
  probabilities <- cbind(cumulative, 1) - cbind(0, cumulative)
  .rubric_monotone_check_probabilities(probabilities, length(eta), length(thresholds) + 1L)
  min_increment <- min(diff(eta))
  max_cumulative_increment <- max(apply(cumulative, 2L, diff))
  if (min_increment < -1e-8 || max_cumulative_increment > 1e-8) {
    rlang::abort("Monotone ordinal calibration failed numerical monotonicity verification; no fallback was fitted.")
  }
  list(grid_size = length(eta), tolerance = 1e-8, min_latent_increment = min_increment,
    max_cumulative_increment = max_cumulative_increment, latent_range = range(eta),
    essentially_flat = diff(range(eta)) <= 1e-8)
}

.rubric_monotone_check_probabilities <- function(probabilities, n, K) {
  if (!is.matrix(probabilities) || !identical(dim(probabilities), as.integer(c(n, K))) ||
    any(!is.finite(probabilities)) || any(probabilities < 0 | probabilities > 1) ||
    any(abs(rowSums(probabilities) - 1) > 1e-12)) {
    rlang::abort("Monotone ordinal predictions did not produce valid category probabilities.")
  }
  invisible(probabilities)
}

.rubric_monotone_boundaries <- function(object, grid, eta) {
  cutpoints <- rep(NA_real_, object$K - 1L)
  status <- rep("outside_range", object$K - 1L)
  for (k in seq_along(cutpoints)) {
    threshold <- object$backend$thresholds[[k]]
    near <- abs(eta - threshold) <= 1e-8
    if (any(utils::head(near, -1L) & utils::tail(near, -1L))) {
      status[[k]] <- "flat_or_unresolved"
    } else if (threshold >= eta[[1L]] && threshold <= utils::tail(eta, 1L)) {
      root <- tryCatch(stats::uniroot(function(z) .rubric_monotone_eta(z, object) - threshold,
        interval = range(grid), tol = 1e-10)$root, error = function(e) NA_real_)
      cutpoints[[k]] <- root
      status[[k]] <- if (is.finite(root)) "unique" else "flat_or_unresolved"
    }
  }
  list(cutpoints = cutpoints, status = status)
}

.rubric_monotone_probabilities <- function(theta, object) {
  z <- (theta - object$transformation$center) / object$transformation$scale
  cumulative <- .rubric_monotone_cumulative(.rubric_monotone_eta(z, object), object$backend$thresholds)
  probabilities <- cbind(cumulative, 1) - cbind(0, cumulative)
  .rubric_monotone_check_probabilities(probabilities, length(theta), object$K)
  colnames(probabilities) <- as.character(object$levels)
  probabilities
}

.rubric_predict_ordinal_monotone <- function(object, newdata, hard_score) {
  .rubric_monotone_dependencies()
  items <- .rubric_same_set_prediction_items(object, newdata)
  probabilities <- .rubric_monotone_probabilities(items$theta, object)
  .rubric_ordinal_prediction_table(object, items, probabilities, hard_score)
}
