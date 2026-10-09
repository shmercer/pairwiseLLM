# Fixed numerical algorithm; no changes to likelihood or starts after failure.
.bt_lapse_attempt <- function(epsilon, kernel, control) {
  boundary <- epsilon == 0
  start <- c(rep(0, ncol(kernel$X)), if (!boundary) epsilon)
  warnings <- character()
  tryCatch(withCallingHandlers({
    total <- sum(kernel$counts$wins + kernel$counts$losses)
    cache <- new.env(parent = emptyenv())
    cache$previous <- cache$surface <- NULL
    objective <- function(x) {
      # optim requests value and gradient at the same point. Cache only that
      # point; this changes neither evaluated values nor the optimization path.
      if (!identical(x, cache$previous)) {
        if (!boundary && (utils::tail(x, 1L) < 0 || utils::tail(x, 1L) > 1)) {
          stop("Lapse optimizer evaluated epsilon outside [0, 1].")
        }
        cache$surface <- if (boundary) .bt_lapse_objective(x, kernel, TRUE) else .bt_lapse_surface(x, kernel)
        cache$previous <- x
      }
      cache$surface
    }
    fit <- if (boundary) {
      stats::optim(start, function(x) objective(x)$value / total,
        function(x) objective(x)$gradient / total, method = "BFGS",
        control = control[c("maxit", "reltol", "trace")])
    } else {
      stats::optim(start, function(x) objective(x)$value / total,
        function(x) objective(x)$gradient / total, method = "L-BFGS-B",
        lower = c(rep(-Inf, length(start) - 1L), 0), upper = c(rep(Inf, length(start) - 1L), 1),
        control = list(maxit = control$maxit, trace = control$trace,
          factr = control$reltol / .Machine$double.eps, pgtol = control$gradient_tol / total))
    }
    # Polish in natural coordinates, retaining the existing tolerances and limit.
    # At exact zero only theta/beta are free; the one-sided epsilon score is
    # validated independently, never mistaken for an interior zero-score test.
    steps <- 0L
    if (fit$convergence == 0L) {
      for (i in seq_len(10L)) {
        obj <- objective(fit$par)
        free <- seq_along(fit$par)
        if (!boundary && utils::tail(fit$par, 1L) == 0) free <- utils::head(free, -1L)
        H <- .bt_alpha_matrix(obj$hessian[free, free, drop = FALSE])
        if (!H$positive_definite) break
        step <- numeric(length(fit$par))
        step[free] <- backsolve(H$chol, forwardsolve(t(H$chol), obj$gradient[free]))
        if (max(abs(obj$gradient[free])) <= control$gradient_tol && max(abs(step)) <= control$step_tol) break
        accepted <- FALSE
        for (fraction in 2^-(0:20)) {
          candidate <- fit$par - fraction * step
          if (!boundary && (utils::tail(candidate, 1L) < 0 || utils::tail(candidate, 1L) > 1)) next
          value <- objective(candidate)$value
          if (is.finite(value) && value <= obj$value + 1e-12 * max(1, abs(obj$value))) {
            fit$par <- candidate
            steps <- steps + 1L
            accepted <- TRUE
            break
          }
        }
        if (!accepted) break
      }
    }
    obj <- objective(fit$par)
    list(start_epsilon = epsilon, boundary = boundary, par = if (boundary) obj$natural else fit$par,
         objective = obj$value, code = fit$convergence, message = fit$message %||% "",
         evaluations = fit$counts, newton_steps = steps, warnings = warnings)
  }, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
  }), error = function(e) {
    list(start_epsilon = epsilon, boundary = boundary, par = NULL, objective = Inf,
         code = 1L, message = conditionMessage(e), warnings = warnings)
  })
}

.bt_lapse_boundary_checks <- function(surface, kernel, control) {
  k <- length(surface$gradient)
  H <- .bt_alpha_matrix(surface$hessian[-k, -k, drop = FALSE])
  score <- utils::head(surface$gradient, -1L)
  step <- if (H$positive_definite && all(is.finite(score))) {
    as.vector(backsolve(H$chol, forwardsolve(t(H$chol), score)))
  } else {
    rep(Inf, length(score))
  }
  centered <- c(as.vector(kernel$transform %*% utils::head(step, -1L)), utils::tail(step, 1L))
  epsilon_score <- utils::tail(surface$gradient, 1L)
  finite <- all(is.finite(c(surface$value, surface$gradient, surface$hessian, step, H$rcond)))
  list(gradient_max = max(abs(score)), step_max = max(abs(c(step, centered))),
    newton_correction = centered, hessian_rcond = H$rcond, epsilon_score = epsilon_score,
    kkt_violation = max(0, -epsilon_score),
    stationary = finite && H$rcond >= control$min_rcond &&
      max(abs(score)) <= control$gradient_tol && max(abs(c(step, centered))) <= control$step_tol,
    kkt_valid = is.finite(epsilon_score) && epsilon_score >= -control$gradient_tol)
}

.bt_lapse_optimize <- function(kernel, control) {
  starts <- c(0.001, 0.05, 0.2, 0.5, 0.9)
  attempts <- lapply(starts, .bt_lapse_attempt, kernel = kernel, control = control)
  values <- vapply(attempts, `[[`, numeric(1), "objective")
  # Select on objective, never on whether a worse solution has nicer uncertainty.
  selected <- if (any(is.finite(values))) which.min(values) else NA_integer_
  list(attempts = attempts, selected = selected,
       boundary_zero = .bt_lapse_attempt(0, kernel, control),
       boundary_one_objective = sum(kernel$counts$wins + kernel$counts$losses) * log(2))
}
