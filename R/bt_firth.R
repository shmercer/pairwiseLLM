# Firth's mean-bias-reduced binomial-logit BT model for nonadaptive schedules.
.bt_firth_control <- function(dots, verbose) {
  if (length(dots) && (is.null(names(dots)) || any(names(dots) != "control") || length(dots) != 1L)) {
    .bt_abort("The brglm2 engine accepts only a named `control` list in `...`.")
  }
  control <- dots$control %||% list()
  allowed <- c("epsilon", "maxit", "slowit", "max_step_factor", "trace")
  if (!is.list(control) || (length(control) &&
      (is.null(names(control)) || any(!names(control) %in% allowed) || anyDuplicated(names(control))))) {
    .bt_abort(paste0("Firth `control` must be a list of uniquely named numerical controls: ",
                     "epsilon, maxit, slowit, max_step_factor, trace."))
  }
  settings <- utils::modifyList(
    list(epsilon = 1e-10, maxit = 200L, slowit = 1, max_step_factor = 12L, trace = FALSE),
    control, keep.null = TRUE
  )
  for (name in setdiff(allowed, "trace")) {
    value <- settings[[name]]
    if (!.bt_real_vector(value) || length(value) != 1L || !is.finite(value) || value <= 0 ||
        (name %in% c("maxit", "max_step_factor") && (value != floor(value) || value > .Machine$integer.max))) {
      .bt_abort(paste0("Firth control `", name, "` must be a positive finite ",
                       if (name %in% c("maxit", "max_step_factor")) "integer." else "number."))
    }
  }
  if (!is.logical(settings$trace) || length(settings$trace) != 1L || is.na(settings$trace)) {
    .bt_abort("Firth control `trace` must be TRUE or FALSE.")
  }
  settings$trace <- isTRUE(verbose) && settings$trace
  do.call(brglm2::brglmControl, c(settings, list(type = "AS_mean")))
}

# Kept separate so numerical failures can be tested without mocking base generics.
.bt_firth_glm <- function(data, design, control) {
  stats::glm(cbind(wins, losses) ~ design - 1, data = data,
             family = stats::binomial("logit"), method = brglm2::brglmFit,
             start = rep(0, ncol(design)), control = control, singular.ok = FALSE)
}

.bt_firth_covariance <- function(covariance, transform, ids) {
  size <- ncol(transform)
  if (!is.matrix(covariance) || !is.numeric(covariance) || is.complex(covariance) ||
      !identical(dim(covariance), c(size, size)) || any(!is.finite(covariance)) ||
      !isSymmetric(covariance, tol = 1e-10)) {
    .bt_abort("Firth covariance must be finite and symmetric in identifiable coordinates.")
  }
  # Test positive definiteness before mapping to the necessarily singular,
  # sum-to-zero item covariance. Do not repair an unidentified covariance.
  tryCatch(chol(covariance), error = function(e) .bt_abort("Firth covariance is not positive definite."))
  covariance <- transform %*% covariance %*% t(transform)
  dimnames(covariance) <- list(ids, ids)
  if (any(!is.finite(covariance)) || any(diag(covariance) <= 0)) {
    .bt_abort("Firth item covariance must be finite with positive variances.")
  }
  covariance
}

.bt_firth_ssr <- function(theta, se) {
  .bt_validate_estimates(theta, se)
  observed <- stats::var(theta)
  error <- mean(se^2)
  if (!is.finite(observed) || !is.finite(error)) {
    .bt_abort("Firth SSR requires finite score variance and mean squared SE.")
  }
  if (observed == 0) {
    return(list(observed_variance = observed, mean_squared_se = error,
                true_score_variance = -error, ssr = NA_real_, n_items = length(theta),
                n_finite = length(theta), valid = FALSE, status = "zero_score_variance"))
  }
  scale_separation_reliability(theta, se)
}

.bt_fit_firth <- function(dat, verbose, dots) {
  if (any(dat[[3L]] == 0.5)) .bt_abort("brglm2 requires binary outcomes; ties are not supported here.")
  if (!.require_ns("brglm2", quietly = TRUE)) {
    .bt_abort(paste0("Package 'brglm2' must be installed to use engine = 'brglm2'. ",
                     "Install it with install.packages('brglm2')."))
  }
  control <- .bt_firth_control(dots, verbose)
  ids <- sort(unique(c(as.character(dat[[1L]]), as.character(dat[[2L]]))), method = "radix")
  first <- match(as.character(dat[[1L]]), ids)
  second <- match(as.character(dat[[2L]]), ids)
  pairs <- data.frame(first = pmin(first, second), second = pmax(first, second),
                      wins = ifelse(first < second, dat[[3L]], 1 - dat[[3L]]))
  pairs$losses <- 1 - pairs$wins
  counts <- stats::aggregate(cbind(wins, losses) ~ first + second, data = pairs, FUN = sum)
  counts <- counts[order(counts$first, counts$second), , drop = FALSE]

  # Reference-coordinate coefficients are contrasts to the last sorted item.
  # Centering the full coordinate map gives item estimates AND their covariance
  # under the same deterministic sum-to-zero constraint.
  transform <- rbind(diag(length(ids) - 1L), 0)
  transform <- sweep(transform, 2L, colMeans(transform))
  rownames(transform) <- ids
  colnames(transform) <- paste0("contrast", seq_len(ncol(transform)))
  design <- transform[counts$first, , drop = FALSE] - transform[counts$second, , drop = FALSE]
  fit <- .bt_firth_glm(counts, design, control)
  if (!isTRUE(fit$converged)) .bt_abort("Firth estimation did not converge; inspect or increase numerical controls.")
  coefficients <- stats::coef(fit)
  if (!.bt_real_vector(coefficients) || length(coefficients) != ncol(transform) ||
      any(!is.finite(coefficients)) || !identical(fit$rank, ncol(transform))) {
    .bt_abort("Firth estimation must return finite, full-rank item contrasts.")
  }
  covariance <- .bt_firth_covariance(stats::vcov(fit), transform, ids)
  theta <- tibble::tibble(ID = ids, theta = as.vector(transform %*% coefficients),
                          se = unname(sqrt(diag(covariance))))
  ssr <- .bt_firth_ssr(theta$theta, theta$se)
  provenance <- list(
    engine = "brglm2", requested_engine = "brglm2",
    engine_version = as.character(getNamespaceVersion("brglm2")),
    package_version = as.character(getNamespaceVersion("pairwiseLLM")),
    supplied_arguments = dots, requested_sirt_eps = NULL,
    effective_settings = list(family = "binomial", link = "logit", intercept = FALSE,
                              start = rep(0, ncol(transform)), control = fit$control),
    adjustment = list(method = "firth", type = "AS_mean", log_determinant_multiplier = 0.5),
    identification = list(convention = "sum_to_zero", internal_reference = tail(ids, 1L),
                          transformation = transform),
    convergence = list(status = "converged", converged = fit$converged, iterations = fit$iter),
    uncertainty = list(method = "inverse_expected_information", coordinates = "sum_to_zero", valid = TRUE),
    theta_finite = TRUE, se_finite = TRUE, reliability_valid = ssr$valid,
    reliability_status = ssr$status, fallback_reason = NULL
  )
  structure(list(engine = "brglm2", fit = fit, theta = theta, reliability = ssr$ssr,
                 ssr = ssr, provenance = provenance, vcov = covariance,
                 comparisons = tibble::tibble(object1 = as.character(dat[[1L]]), object2 = as.character(dat[[2L]]))),
            class = c("pairwiseLLM_bt_firth", "list"))
}

#' Predict pairwise win probabilities from a Firth Bradley-Terry fit
#'
#' Calculate `plogis(theta1 - theta2)`, the probability that the first item
#' wins. Predictions use the fitted item strengths, without a positional,
#' lapse, or tie parameter. They do not integrate over estimation uncertainty.
#'
#' @param object A Firth fit returned by [fit_bt_model()] with `engine = "brglm2"`.
#' @param newdata A data frame containing `object1` and `object2` item IDs.
#'   `NULL` uses the original comparisons in their original order. IDs must
#'   be nonmissing and present in the fitted model. Repeated pairs are allowed;
#'   comparing an item with itself returns 0.5.
#' @param ... Reserved; additional arguments are rejected.
#' @return A numeric vector of first-item win probabilities in input row order.
#'   An empty data frame returns `numeric(0)`.
#' @examples
#' if (requireNamespace("brglm2", quietly = TRUE)) {
#'   comparisons <- data.frame(object1 = c("a", "a", "b"),
#'                             object2 = c("b", "c", "c"), result = c(1, 1, 1))
#'   fit <- fit_bt_model(comparisons, engine = "brglm2")
#'   predict(fit)
#'   predict(fit, data.frame(object1 = "c", object2 = "a"))
#' }
#' @seealso [fit_bt_model()], [summarize_bt_fit()]
#' @family frequentist models
#' @export
predict.pairwiseLLM_bt_firth <- function(object, newdata = NULL, ...) {
  if (length(list(...))) .bt_abort("Firth prediction does not accept additional arguments.")
  if (is.null(newdata)) newdata <- object$comparisons
  if (!is.data.frame(newdata) || !all(c("object1", "object2") %in% names(newdata))) {
    .bt_abort("`newdata` must be a data frame containing `object1` and `object2`.")
  }
  for (name in c("object1", "object2")) {
    ids <- newdata[[name]]
    if (!is.atomic(ids) || anyNA(ids) || any(!nzchar(as.character(ids))) ||
        any(!as.character(ids) %in% object$theta$ID)) {
      .bt_abort("Prediction IDs must be nonmissing item labels present in the Firth fit.")
    }
  }
  first <- match(as.character(newdata$object1), object$theta$ID)
  second <- match(as.character(newdata$object2), object$theta$ID)
  stats::plogis(object$theta$theta[first] - object$theta$theta[second])
}
