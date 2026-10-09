# Experimental model matching, not an automatic engine or Bayesian replacement.
.bt_lapse_fit <- function(kernel, control, comparisons = NULL, supplied = list()) {
  ids <- kernel$ids
  transform <- kernel$transform
  provenance <- list(engine = "lapse", requested_engine = "lapse", engine_package = "stats",
    engine_version = as.character(getNamespaceVersion("stats")),
    package_version = as.character(getNamespaceVersion("pairwiseLLM")),
    model_variant = normalize_model_variant("btl_e_b"), experimental = TRUE,
    supplied_arguments = supplied, requested_sirt_eps = NULL,
    adjustment = list(method = "none", log_penalty = 0), theta_finite = FALSE, se_finite = FALSE,
    effective_settings = list(solver = "stats::optim", algorithm = "L-BFGS-B", control = control,
      boundary_zero_algorithm = "BFGS",
      start_epsilon = c(0.001, 0.05, 0.2, 0.5, 0.9), start_theta = 0, start_beta = 0,
      epsilon_interval = c(0, 1), epsilon_parameterization = "natural_bounded"),
    identification = list(convention = "sum_to_zero", internal_reference = utils::tail(ids, 1L),
      transformation = transform, parameter_order = c(colnames(kernel$X), "epsilon")),
    convergence = list(status = "not_attempted", converged = FALSE),
    uncertainty = list(method = "inverse_joint_observed_information", valid = FALSE, status = "unavailable",
      coordinates = "sum_to_zero_theta_beta_epsilon", scope = "conditional_on_realized_comparison_graph",
      schedule_aware = FALSE), reliability_valid = FALSE, reliability_status = "prototype_ssr_unavailable",
    fallback_reason = NULL)
  diagnostics <- list(item_order = ids, parameter_order = c(colnames(kernel$X), "epsilon"))
  theta <- NULL
  beta <- epsilon <- NA_real_
  fail <- function(reason, message, parent = NULL) {
    diagnostics$failure_reason <- reason
    provenance$convergence$status <- "failed"
    provenance$convergence$converged <- FALSE
    rlang::abort(message, class = c("pairwiseLLM_bt_lapse_error", "pairwiseLLM_bt_validation_error"),
      failure_reason = reason, theta = theta, beta = beta, epsilon = epsilon,
      provenance = provenance, diagnostics = diagnostics, parent = parent)
  }
  diagnostics$design_rank <- qr(kernel$X)$rank
  if (length(ids) < 3L || nrow(kernel$X) < length(ids) + 1L ||
      diagnostics$design_rank != ncol(kernel$X)) {
    fail("unidentified_design", "Ordered comparisons cannot identify the full lapse/position model.")
  }
  diagnostics$optimization <- .bt_lapse_optimize(kernel, control)
  opt <- diagnostics$optimization
  if (is.na(opt$selected)) fail("optimizer_failure", "No finite lapse optimization candidate is available.")
  best <- opt$attempts[[opt$selected]]
  par <- best$par
  theta <- tibble::tibble(ID = ids, theta = as.vector(transform %*% utils::head(par, -2L)))
  provenance$theta_finite <- all(is.finite(theta$theta))
  beta <- par[length(ids)]
  epsilon <- utils::tail(par, 1L)
  surface <- .bt_lapse_surface(par, kernel)
  diagnostics <- c(diagnostics, surface)
  provenance$convergence <- list(status = "candidate", converged = FALSE,
    code = best$code, message = best$message, evaluations = best$evaluations)
  if (any(!is.finite(c(par, surface$value, surface$gradient, surface$hessian)))) {
    fail("nonfinite_surface", "Lapse estimates, objective, score and Hessian must be finite.")
  }
  tolerance <- 100 * .Machine$double.eps * max(1, abs(surface$value))
  zero <- opt$boundary_zero
  diagnostics$boundary_objective_tolerance <- tolerance
  # An unfinished boundary search cannot establish that the interior beats it.
  if (is.null(zero$par) || zero$code != 0L || !is.finite(zero$objective)) {
    fail("boundary_unresolved", "The epsilon-zero boundary optimization is unresolved.")
  }
  zero_surface <- .bt_lapse_surface(zero$par, kernel)
  zero_checks <- .bt_lapse_boundary_checks(zero_surface, kernel, control)
  diagnostics$boundary_zero_checks <- zero_checks
  if (!zero_checks$stationary) {
    fail("boundary_unresolved", "The epsilon-zero boundary failed independent stationarity/curvature checks.")
  }
  # Compare likelihoods before choosing an uncertainty convention. A numerical
  # tie can represent the same zero-boundary solution reached from an interior
  # start, but proximity to zero alone never establishes a boundary optimum.
  boundary <- zero_surface$value <= surface$value + tolerance && zero_checks$kkt_valid
  if (boundary) {
    best <- zero
    par <- zero$par
    surface <- zero_surface
    theta$theta <- as.vector(transform %*% utils::head(par, -2L))
    provenance$theta_finite <- all(is.finite(theta$theta))
    beta <- par[length(ids)]
    epsilon <- 0
    diagnostics[names(surface)] <- surface
    provenance$convergence <- list(status = "candidate", converged = FALSE,
      code = best$code, message = best$message, evaluations = best$evaluations)
  } else if (epsilon == 0 || zero_surface$value < surface$value - tolerance) {
    fail("boundary_kkt_failure", "The best epsilon-zero candidate fails the one-sided lapse score check.")
  }
  diagnostics$optimization$selected_source <- if (boundary) "boundary_zero" else "attempts"
  diagnostics$boundary <- if (boundary) "epsilon_zero" else "none"
  if (epsilon == 1 || opt$boundary_one_objective <= surface$value + tolerance) {
    fail("unidentified_information", "The epsilon-one likelihood leaves theta and beta unidentified.")
  }
  if (best$code != 0L) fail("not_converged", "The best likelihood candidate did not converge.")
  H <- .bt_alpha_matrix(surface$hessian)
  I <- .bt_alpha_matrix(.bt_lapse_information(par, kernel))
  diagnostics$hessian_checks <- H[setdiff(names(H), "chol")]
  diagnostics$information_checks <- I[setdiff(names(I), "chol")]
  if (!I$positive_definite || !is.finite(I$rcond) || I$rcond < control$min_rcond) {
    fail("unidentified_information", "Full-model information is not positive definite and well conditioned.")
  }
  # With a strictly positive one-sided score, curvature is required only along
  # the theta/beta face. At a zero score, require the full joint curvature too.
  require_joint <- !boundary || zero_checks$epsilon_score <= control$gradient_tol
  if (require_joint && (!H$positive_definite || !is.finite(H$rcond) || H$rcond < control$min_rcond)) {
    fail("invalid_hessian", "Joint observed Hessian is not positive definite and well conditioned.")
  }
  if (boundary) {
    diagnostics$gradient_max <- max(zero_checks$gradient_max, zero_checks$kkt_violation)
    diagnostics$newton_correction <- c(zero_checks$newton_correction, epsilon = 0)
  } else {
    correction <- as.vector(backsolve(H$chol, forwardsolve(t(H$chol), surface$gradient)))
    diagnostics$gradient_max <- max(abs(surface$gradient))
    diagnostics$newton_correction <- c(as.vector(transform %*% utils::head(correction, -2L)),
                                      utils::tail(correction, 2L))
  }
  diagnostics$step_max <- max(abs(diagnostics$newton_correction))
  if (diagnostics$gradient_max > control$gradient_tol || diagnostics$step_max > control$step_tol) {
    fail("not_stationary", "Lapse fit failed natural-coordinate score/Newton-correction checks.")
  }
  item_covariance <- joint <- NULL
  if (!boundary) {
    covariance <- chol2inv(H$chol)
    item_map <- cbind(transform, 0, 0)
    item_covariance <- tryCatch(.bt_item_covariance(covariance, item_map, ids, "Lapse"),
      error = function(e) fail("invalid_covariance", "Lapse item covariance validation failed.", e))
    joint_map <- rbind(item_map, c(rep(0, ncol(transform)), 1, 0), c(rep(0, ncol(transform)), 0, 1))
    joint <- joint_map %*% covariance %*% t(joint_map)
    joint_names <- c(paste0("theta:", ids), "beta", "epsilon")
    dimnames(joint) <- list(joint_names, joint_names)
    if (any(!is.finite(joint)) || any(diag(joint) <= 0) || !isSymmetric(joint, tol = 1e-10) ||
        min(eigen(joint, symmetric = TRUE, only.values = TRUE)$values) < -1e-10 * max(diag(joint))) {
      fail("invalid_covariance", "Joint natural-parameter covariance failed finite/PSD checks.")
    }
    theta$se <- unname(sqrt(diag(item_covariance)))
    provenance$uncertainty$valid <- TRUE
    provenance$uncertainty$status <- "valid"
    provenance$se_finite <- TRUE
  } else {
    theta$se <- rep(NA_real_, length(ids))
    provenance$uncertainty$method <- "none"
    provenance$uncertainty$status <- "nonregular_boundary"
  }
  provenance$convergence$status <- if (boundary) "converged_boundary" else "converged"
  provenance$convergence$converged <- TRUE
  structure(list(engine = "lapse", model_variant = "btl_e_b", fit = best, theta = theta,
    beta = unname(beta), epsilon = unname(epsilon), vcov = item_covariance, parameter_vcov = joint,
    log_likelihood = -surface$value, objective = surface$value, reliability = NA_real_,
    ssr = list(ssr = NA_real_, valid = FALSE, status = "prototype_ssr_unavailable"),
    provenance = provenance, diagnostics = diagnostics, comparisons = comparisons),
    class = c("pairwiseLLM_bt_lapse", "list"))
}

.bt_fit_lapse <- function(dat, verbose, dots) {
  if (any(dat[[3L]] == 0.5)) .bt_abort("Lapse estimation requires binary outcomes; ties are not supported.")
  control <- .bt_lapse_control(dots, verbose)
  .bt_lapse_fit(.bt_lapse_design(dat), control,
    comparisons = tibble::tibble(object1 = as.character(dat[[1L]]), object2 = as.character(dat[[2L]])),
    supplied = dots)
}

#' Predict ordered comparisons from an experimental lapse BTL fit
#'
#' Calculate `(1-epsilon) * plogis(theta1-theta2+beta) + epsilon/2`.
#' Positive beta favors the first presented item. Swapping the items generally
#' does not give complementary probabilities unless beta is zero. These are
#' plug-in probabilities, without integration over estimation uncertainty. Valid
#' epsilon-zero boundary fits also support prediction; their unavailable joint
#' uncertainty does not invalidate the point estimates.
#'
#' @param object A validated experimental fit from [fit_bt_model()] with
#'   `engine = "lapse"`.
#' @param newdata A data frame containing `object1` and `object2` IDs known to
#'   the fit. `NULL` uses the original comparisons in their original order.
#'   Repeated pairs and self-predictions are allowed; a self-prediction includes
#'   positional bias and need not equal 0.5. Empty input returns `numeric(0)`.
#' @param ... Reserved; additional arguments are rejected.
#' @return Numeric first-item win probabilities in input row order.
#' @seealso [fit_bt_model()],
#'   [integrated CJ workflow](https://shmercer.github.io/pairwiseLLM/articles/adaptive-cj-workflow.html)
#' @family frequentist models
#' @export
predict.pairwiseLLM_bt_lapse <- function(object, newdata = NULL, ...) {
  # Reuse input validation, but use contrasts directly rather than inverting
  # rounded simple-BT probabilities at extreme logits.
  .bt_predict_pairs(object, newdata, list(...), "Lapse")
  if (is.null(newdata)) newdata <- object$comparisons
  first <- match(as.character(newdata$object1), object$theta$ID)
  second <- match(as.character(newdata$object2), object$theta$ID)
  eta <- object$theta$theta[first] - object$theta$theta[second] + object$beta
  exp(.link_e1_log_probability(eta, object$epsilon))
}
