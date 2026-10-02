# Internal validation and provenance for the existing frequentist engines.
.bt_abort <- function(message) {
  rlang::abort(message, class = "pairwiseLLM_bt_validation_error")
}

.bt_real_vector <- function(x) {
  is.numeric(x) && !is.complex(x) && is.null(dim(x))
}

.bt_validate_estimates <- function(theta, se) {
  if (!.bt_real_vector(theta) || !.bt_real_vector(se) || length(theta) != length(se)) {
    .bt_abort("`theta` and `se` must be real numeric vectors of equal length.")
  }
  if (any(!is.finite(theta)) || any(!is.finite(se)) || any(se < 0)) {
    .bt_abort("`theta` and `se` must be finite, with nonnegative SEs; no items are dropped.")
  }
}

.bt_validate_data <- function(dat) {
  if (!nrow(dat)) .bt_abort("`bt_data` must contain comparisons.")
  for (j in 1:2) {
    if (!is.atomic(dat[[j]]) || anyNA(dat[[j]]) || any(!nzchar(as.character(dat[[j]])))) {
      .bt_abort("Object IDs must be nonmissing, nonempty atomic values.")
    }
  }
  if (any(as.character(dat[[1L]]) == as.character(dat[[2L]]))) {
    .bt_abort("Self-comparisons cannot identify BT scores.")
  }
  if (!.bt_real_vector(dat[[3L]]) || anyNA(dat[[3L]]) ||
      any(!dat[[3L]] %in% c(0, 0.5, 1))) {
    .bt_abort("Comparison results must be finite numeric 0, 0.5 (tie), or 1.")
  }
  .bt_check_connected(dat)
  invisible(NULL)
}

.bt_check_connected <- function(dat, ids = unique(c(as.character(dat[[1L]]), as.character(dat[[2L]])))) {
  from <- match(as.character(dat[[1L]]), ids)
  to <- match(as.character(dat[[2L]]), ids)
  neighbors <- split(c(to, from), factor(c(from, to), levels = seq_along(ids)))
  seen <- rep(FALSE, length(ids))
  queue <- 1L
  seen[1L] <- TRUE
  head <- 1L
  while (head <= length(queue)) {
    next_ids <- neighbors[[queue[[head]]]]
    next_ids <- unique(next_ids[!seen[next_ids]])
    seen[next_ids] <- TRUE
    queue <- c(queue, next_ids)
    head <- head + 1L
  }
  if (!all(seen)) {
    .bt_abort("Comparison graph is disconnected: global BT scores and SSR are not identified.")
  }
  invisible(NULL)
}

# Match positional and partially named legacy arguments exactly as the engine
# does. Integer placeholders map matched arguments back to already forced values.
.bt_match_args <- function(fun, args) {
  call <- as.call(c(list(as.name("engine")), stats::setNames(as.list(seq_along(args)), names(args))))
  as.list(match.call(fun, call = call))[-1L]
}

.bt_resolve_settings <- function(fun, args, omit) {
  matched <- .bt_match_args(fun, args)
  defaults <- formals(fun)
  defaults <- defaults[setdiff(names(defaults), c(omit, "..."))]
  settings <- lapply(defaults, eval, envir = environment(fun))
  for (name in setdiff(names(matched), omit)) {
    settings[name] <- args[matched[[name]]]
  }
  settings
}

.bt_validate_eps <- function(eps) {
  if (!.bt_real_vector(eps) || length(eps) != 1L || !is.finite(eps) || eps < 0) {
    .bt_abort("The effective sirt epsilon must be a finite, nonnegative numeric scalar.")
  }
}

.bt_sirt_settings <- function(dat, dots, sirt_eps) {
  settings <- .bt_resolve_settings(sirt::btm, c(list(data = dat), dots), "data")
  if (!is.null(sirt_eps)) {
    # Include positional and partially named legacy epsilon arguments.
    supplied <- .bt_match_args(sirt::btm, c(list(data = dat), dots))
    if ("eps" %in% names(supplied)) {
      .bt_abort("Supply only one of `sirt_eps` and legacy `eps`.")
    }
    settings$eps <- sirt_eps
  }
  .bt_validate_eps(settings$eps)
  if (isTRUE(as.logical(settings$ignore.ties))) {
    ids <- unique(c(as.character(dat[[1L]]), as.character(dat[[2L]])))
    .bt_check_connected(dat[dat[[3L]] != 0.5, , drop = FALSE], ids)
  }
  settings
}

.bt_ssr_sirt <- function(fit, theta) {
  ssr <- scale_separation_reliability(theta$theta, theta$se)
  raw <- fit$mle.rel
  if (!.bt_real_vector(raw) || length(raw) != 1L || !is.finite(raw)) {
    .bt_abort("sirt returned missing or nonfinite `mle.rel` reliability.")
  }
  tolerance <- 1e-12 * max(1, abs(raw), abs(ssr$ssr))
  difference <- abs(raw - ssr$ssr)
  if (difference > tolerance) {
    .bt_abort("sirt `mle.rel` disagrees with independently calculated SSR.")
  }
  c(ssr, list(engine_reliability = raw, agrees = TRUE,
              absolute_difference = difference, tolerance = tolerance))
}

.bt_provenance <- function(engine, requested, fit, settings, dots, sirt_eps, ssr, fallback = NULL) {
  if (engine == "sirt") {
    .bt_validate_eps(fit$eps)
    if (!isTRUE(all.equal(fit$eps, settings$eps, tolerance = 0))) {
      .bt_abort("Returned sirt epsilon does not match the requested effective setting.")
    }
    settings$eps <- fit$eps
    identification <- list(
      convention = if (is.null(settings$fix.theta)) "sum_to_zero" else "fixed_theta",
      fixed_theta = settings$fix.theta
    )
    # btm 4.2.133 accepts fix.delta but does not apply it. Do not claim that
    # a requested value was used: retain it separately from the returned pars.
    adjustment <- list(method = "epsilon", eps = fit$eps)
    status <- if (is.null(fit$iter)) {
      "unknown"
    } else if (fit$iter < settings$maxiter) {
      "stopping_criterion_met"
    } else {
      "iteration_limit_reached"
    }
    converged <- if (status == "stopping_criterion_met") TRUE else NA
    settings$fix.delta_requested <- settings$fix.delta
    settings$fix.delta <- NULL
    settings$fix.delta_application <- if (as.character(package_version(getNamespaceVersion("sirt"))) == "4.2.133") {
      "not_applied_by_engine"
    } else {
      "not_verified_for_engine_version"
    }
    settings$returned_parameters <- fit$pars
  } else {
    identification <- list(convention = "engine_contrasts", refcat = fit$refcat,
                           contrasts = fit$contrasts, player_levels = levels(fit$player1[[fit$id]]))
    adjustment <- list(method = if (isTRUE(settings$br)) "engine_bias_reduction" else "none",
                       br = settings$br)
    settings$control <- fit$control
    settings$family <- fit$family$family
    settings$link <- fit$family$link
    status <- if (is.null(fit$converged)) {
      "unknown"
    } else if (isTRUE(fit$converged)) {
      "converged"
    } else {
      "not_converged"
    }
    converged <- fit$converged %||% NA
  }
  list(
    engine = engine, requested_engine = requested,
    engine_version = as.character(package_version(getNamespaceVersion(engine))),
    package_version = as.character(package_version(getNamespaceVersion("pairwiseLLM"))),
    supplied_arguments = dots, requested_sirt_eps = sirt_eps,
    effective_settings = settings, adjustment = adjustment,
    identification = identification,
    convergence = list(status = status, converged = converged, iterations = fit$iter),
    theta_finite = TRUE, se_finite = TRUE,
    reliability_valid = ssr$valid, reliability_status = ssr$status,
    fallback_reason = fallback
  )
}
