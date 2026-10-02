# Hamilton--Tawn alpha adjustment: symmetric pseudo-wins on EVERY item pair.
.bt_alpha_control <- function(dots, verbose) {
  if (length(dots) && (is.null(names(dots)) || any(names(dots) != "control") || length(dots) != 1L)) {
    .bt_abort("The alpha engine accepts only a named `control` list in `...`.")
  }
  defaults <- list(epsilon = 1e-12, maxit = 200L, gradient_tol = 1e-7,
                   step_tol = 1e-7, min_rcond = 1e-12, trace = FALSE)
  control <- dots$control %||% list()
  if (!is.list(control) || (length(control) && (is.null(names(control)) ||
      any(!names(control) %in% names(defaults)) || anyDuplicated(names(control))))) {
    .bt_abort("Alpha `control` must be a list of uniquely named supported numerical controls.")
  }
  control <- utils::modifyList(defaults, control, keep.null = TRUE)
  for (name in setdiff(names(defaults), "trace")) {
    x <- control[[name]]
    if (!.bt_real_vector(x) || length(x) != 1L || !is.finite(x) || x <= 0 ||
        (name == "maxit" && (x != floor(x) || x > .Machine$integer.max)) ||
        (name == "min_rcond" && x >= 1)) {
      .bt_abort(paste("Invalid positive finite alpha control:", name))
    }
  }
  if (!is.logical(control$trace) || length(control$trace) != 1L || is.na(control$trace)) {
    .bt_abort("Alpha control `trace` must be TRUE or FALSE.")
  }
  control$trace <- isTRUE(verbose) && control$trace
  control
}

# Ford's finite-MLE condition; ordinary undirected connectivity is insufficient
# when alpha is zero. Reachability in BOTH orientations tests strong connectivity.
.bt_alpha_mle_exists <- function(counts, n) {
  from <- c(counts$first[counts$wins > 0], counts$second[counts$losses > 0])
  to <- c(counts$second[counts$wins > 0], counts$first[counts$losses > 0])
  reachable <- function(from, to) {
    neighbors <- split(to, factor(from, levels = seq_len(n)))
    seen <- rep(FALSE, n)
    seen[1L] <- TRUE
    queue <- 1L
    head <- 1L
    while (head <= length(queue)) {
      next_ids <- neighbors[[queue[[head]]]]
      next_ids <- unique(next_ids[!seen[next_ids]])
      seen[next_ids] <- TRUE
      queue <- c(queue, next_ids)
      head <- head + 1L
    }
    all(seen)
  }
  reachable(from, to) && reachable(to, from)
}

.bt_alpha_kernel <- function(prepared, alpha) {
  pairs <- t(utils::combn(seq_along(prepared$ids), 2L))
  pairs <- data.frame(first = pairs[, 1L], second = pairs[, 2L])
  counts <- merge(pairs, prepared$counts, by = c("first", "second"), all.x = TRUE, sort = TRUE)
  counts$wins[is.na(counts$wins)] <- 0
  counts$losses[is.na(counts$losses)] <- 0
  list(counts = counts, pseudo_count = alpha / (length(prepared$ids) - 1L),
       ids = prepared$ids, transform = prepared$transform,
       design = prepared$transform[counts$first, , drop = FALSE] -
         prepared$transform[counts$second, , drop = FALSE])
}

.bt_alpha_glm <- function(kernel, control) {
  weights <- c(kernel$counts$wins, kernel$counts$losses) + kernel$pseudo_count
  used <- weights > 0
  n <- nrow(kernel$design)
  rows <- rep(seq_len(n), 2L)
  response <- rep(c(1, 0), each = n)
  # Quasibinomial supplies the identical binomial-logit IWLS equations without
  # warning about fractional pseudo-counts. No dispersion is estimated or used.
  # Weighted binary rows keep deviance equal to -2 times the objective, avoiding
  # cancellation in near-zero grouped-proportion deviance on well-fitting data.
  family <- stats::quasibinomial("logit")
  family$dispersion <- 1
  stats::glm.fit(kernel$design[rows[used], , drop = FALSE], response[used],
                 weights = weights[used], family = family, start = rep(0, ncol(kernel$design)),
                 intercept = FALSE, singular.ok = FALSE,
                 control = control[c("epsilon", "maxit", "trace")])
}

# Negative penalized log likelihood, its derivatives, and ORIGINAL-data information.
# Separate p and q evaluations avoid cancellation at large positive contrasts.
.bt_alpha_surface <- function(beta, kernel) {
  eta <- as.vector(kernel$design %*% beta)
  p <- stats::plogis(eta)
  q <- stats::plogis(-eta)
  lp <- stats::plogis(eta, log.p = TRUE)
  lq <- stats::plogis(-eta, log.p = TRUE)
  counts <- kernel$counts
  c0 <- kernel$pseudo_count
  log_likelihood <- sum(counts$wins * lp + counts$losses * lq)
  log_penalty <- c0 * sum(lp + lq)
  pair_score <- (counts$wins + c0) * q - (counts$losses + c0) * p
  score <- rowsum(c(pair_score, -pair_score), c(counts$first, counts$second), reorder = TRUE)
  information <- crossprod(kernel$design, kernel$design * ((counts$wins + counts$losses) * p * q))
  hessian <- information + crossprod(kernel$design, kernel$design * (2 * c0 * p * q))
  list(value = -(log_likelihood + log_penalty), log_likelihood = log_likelihood,
       log_penalty = log_penalty, score = stats::setNames(as.vector(score), kernel$ids),
       gradient = -as.vector(crossprod(kernel$design, pair_score)),
       hessian = hessian, information = information)
}

.bt_alpha_matrix <- function(x) {
  finite <- is.matrix(x) && is.numeric(x) && !is.complex(x) && all(is.finite(x))
  symmetric <- finite && isSymmetric(x, tol = 1e-10)
  upper <- if (symmetric) tryCatch(chol(x), error = function(e) NULL) else NULL
  list(finite = finite, symmetric = symmetric, positive_definite = !is.null(upper),
       rcond = if (is.null(upper)) NA_real_ else rcond(x), chol = upper)
}

.bt_alpha_fail <- function(reason, message, diagnostics, theta, provenance, parent = NULL) {
  diagnostics$failure_reason <- reason
  rlang::abort(message, class = c("pairwiseLLM_bt_alpha_error", "pairwiseLLM_bt_validation_error"),
               failure_reason = reason, diagnostics = diagnostics, theta = theta,
               provenance = provenance, parent = parent)
}

.bt_fit_alpha <- function(dat, alpha, verbose, dots) {
  if (!.bt_real_vector(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha < 0) {
    .bt_abort("`alpha` must be supplied explicitly as a finite nonnegative numeric scalar.")
  }
  if (any(dat[[3L]] == 0.5)) .bt_abort("Alpha estimation requires binary outcomes; ties are not supported here.")
  control <- .bt_alpha_control(dots, verbose)
  prepared <- .bt_binary_design(dat)
  ids <- prepared$ids
  transform <- prepared$transform
  provenance <- list(
    engine = "alpha", requested_engine = "alpha", engine_package = "stats",
    engine_version = as.character(getNamespaceVersion("stats")),
    package_version = as.character(getNamespaceVersion("pairwiseLLM")),
    supplied_arguments = c(list(alpha = alpha), dots), requested_sirt_eps = NULL,
    effective_settings = list(solver = "stats::glm.fit", algorithm = "IWLS",
                              working_family = "quasibinomial", link = "logit", dispersion = 1,
                              intercept = FALSE, start = rep(0, ncol(transform)), control = control),
    adjustment = list(method = "hamilton_alpha", alpha = alpha, pseudo_count = alpha / (length(ids) - 1L),
                      pairs = "all_unordered_pairs", objective = "log_likelihood + alpha/(N-1) * sum(log(p*(1-p)))",
                      reference = "doi:10.1111/jedm.70022, equation (3)"),
    identification = list(convention = "sum_to_zero", internal_reference = tail(ids, 1L),
                          parameter_order = head(ids, -1L), transformation = transform),
    convergence = list(status = "not_attempted", converged = FALSE, code = NA_integer_, message = ""),
    uncertainty = list(method = "inverse_unpenalized_observed_information", coordinates = "sum_to_zero",
                       scope = "conditional_on_realized_comparison_graph", schedule_aware = FALSE, valid = FALSE),
    theta_finite = FALSE, se_finite = FALSE, reliability_valid = FALSE,
    reliability_status = "unavailable", fallback_reason = NULL
  )
  diagnostics <- list(parameter_order = colnames(transform), item_order = ids, warnings = character())
  theta <- NULL
  fail <- function(reason, message, parent = NULL) {
    .bt_alpha_fail(reason, message, diagnostics, theta, provenance, parent)
  }
  if (alpha == 0 && !.bt_alpha_mle_exists(prepared$counts, length(ids))) {
    fail("no_finite_mle", "alpha = 0 requires a strongly connected directed win graph for a finite MLE.")
  }
  kernel <- .bt_alpha_kernel(prepared, alpha)
  total <- kernel$counts$wins + kernel$counts$losses + 2 * kernel$pseudo_count
  if (any(!is.finite(total)) || (alpha > 0 && kernel$pseudo_count == 0)) {
    fail("unrepresentable_penalty", "Alpha pseudo-counts are outside the representable numerical range.")
  }
  fit <- tryCatch(withCallingHandlers(.bt_alpha_glm(kernel, control), warning = function(w) {
    diagnostics$warnings <<- c(diagnostics$warnings, conditionMessage(w))
  }), error = function(e) fail("solver_error", "Alpha IWLS estimation failed.", e))
  provenance$convergence <- list(status = if (isTRUE(fit$converged)) "iwls_converged" else "not_converged",
    converged = isTRUE(fit$converged), code = if (isTRUE(fit$converged)) 0L else 1L,
    code_source = "mapped_glm_converged", message = if (isTRUE(fit$converged)) "IWLS converged." else "IWLS failed.",
    iterations = fit$iter)
  diagnostics$optimizer <- provenance$convergence
  beta <- fit$coefficients
  if (!.bt_real_vector(beta) || length(beta) != ncol(transform) || any(!is.finite(beta))) {
    fail("invalid_coefficients", "Alpha estimation must return finite item contrasts.")
  }
  # Equal total wins/losses for EVERY item proves that zero is the unique
  # stationary solution. Preserve that exact solution instead of interpreting
  # IWLS rounding noise as positive score variance for SSR. Never threshold theta.
  difference <- kernel$counts$wins - kernel$counts$losses
  imbalance <- rowsum(c(difference, -difference), c(kernel$counts$first, kernel$counts$second))
  diagnostics$exact_zero_solution <- all(imbalance == 0)
  if (diagnostics$exact_zero_solution) beta[] <- 0
  diagnostics$coefficients <- beta
  theta <- tibble::tibble(ID = ids, theta = as.vector(transform %*% beta))
  provenance$theta_finite <- all(is.finite(theta$theta))
  surface <- .bt_alpha_surface(beta, kernel)
  surface$gradient <- stats::setNames(surface$gradient, colnames(transform))
  diagnostics <- c(diagnostics, surface)
  diagnostics$gradient_max <- max(abs(surface$score))
  if (!provenance$theta_finite || any(!is.finite(c(surface$value, surface$gradient, surface$score)))) {
    fail("nonfinite_surface", "Alpha estimates, objective and score must be finite.")
  }
  if (!isTRUE(fit$converged) || !identical(fit$rank, ncol(transform))) {
    fail("not_converged", "Alpha estimation did not converge with full-rank contrasts.")
  }
  penalized <- .bt_alpha_matrix(surface$hessian)
  original <- .bt_alpha_matrix(surface$information)
  diagnostics$hessian_checks <- penalized[setdiff(names(penalized), "chol")]
  diagnostics$information_checks <- original[setdiff(names(original), "chol")]
  if (!penalized$positive_definite || !is.finite(penalized$rcond) || penalized$rcond < control$min_rcond) {
    fail("invalid_hessian", "Alpha penalized Hessian is not numerically positive definite and well conditioned.")
  }
  correction <- backsolve(penalized$chol, forwardsolve(t(penalized$chol), surface$gradient))
  diagnostics$newton_correction <- as.vector(transform %*% correction)
  diagnostics$step_max <- max(abs(diagnostics$newton_correction))
  diagnostics$stationary <- is.finite(diagnostics$step_max) &&
    diagnostics$gradient_max <= control$gradient_tol && diagnostics$step_max <= control$step_tol
  if (!diagnostics$stationary) {
    fail("not_stationary", "Alpha IWLS result failed independent score/Newton-correction convergence checks.")
  }
  provenance$convergence$status <- "converged"
  provenance$convergence$message <- "IWLS converged and independent stationarity checks passed."
  if (!original$positive_definite || !is.finite(original$rcond) || original$rcond < control$min_rcond) {
    fail("invalid_information",
         "Alpha unpenalized information is not numerically positive definite and well conditioned.")
  }
  covariance <- tryCatch(.bt_item_covariance(chol2inv(original$chol), transform, ids, "Alpha"),
    error = function(e) fail("invalid_covariance", "Alpha item covariance validation failed.", e))
  theta$se <- unname(sqrt(diag(covariance)))
  ssr <- tryCatch(.bt_centered_ssr(theta$theta, theta$se, "Alpha"),
    error = function(e) fail("invalid_reliability", "Alpha reliability arithmetic is invalid.", e))
  provenance$uncertainty$valid <- TRUE
  provenance$se_finite <- TRUE
  provenance$reliability_valid <- ssr$valid
  provenance$reliability_status <- ssr$status
  structure(list(engine = "alpha", fit = fit, theta = theta, alpha = alpha, vcov = covariance,
                 reliability = ssr$ssr, ssr = ssr, provenance = provenance, diagnostics = diagnostics,
                 comparisons = tibble::tibble(object1 = as.character(dat[[1L]]), object2 = as.character(dat[[2L]]))),
            class = c("pairwiseLLM_bt_alpha", "list"))
}

#' Predict pairwise win probabilities from an alpha-adjusted Bradley-Terry fit
#'
#' Calculate `plogis(theta1 - theta2)`, the probability that the first item
#' wins. These plug-in probabilities do not integrate over uncertainty and have
#' no tie, lapse, or positional parameter.
#'
#' @param object An alpha-adjusted fit from [fit_bt_model()] with `engine = "alpha"`.
#' @inheritParams predict.pairwiseLLM_bt_firth
#' @return A numeric vector of first-item win probabilities in input row order.
#' @examples
#' comparisons <- data.frame(object1 = c("a", "a", "b"),
#'                           object2 = c("b", "c", "c"), result = c(1, 1, 1))
#' fit <- fit_bt_model(comparisons, engine = "alpha", alpha = 0.5)
#' predict(fit)
#' predict(fit, data.frame(object1 = "c", object2 = "a"))
#' @seealso [fit_bt_model()], [summarize_bt_fit()]
#' @family frequentist models
#' @export
predict.pairwiseLLM_bt_alpha <- function(object, newdata = NULL, ...) {
  .bt_predict_pairs(object, newdata, list(...), "Alpha")
}
