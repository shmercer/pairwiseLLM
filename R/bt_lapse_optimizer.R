# Fixed numerical algorithm; no changes to likelihood or starts after failure.
.bt_lapse_attempt <- function(epsilon, kernel, control) {
  boundary <- epsilon == 0
  start <- c(rep(0, ncol(kernel$X)), if (!boundary) stats::qlogis(epsilon))
  warnings <- character()
  tryCatch(withCallingHandlers({
    total <- sum(kernel$counts$wins + kernel$counts$losses)
    previous <- NULL
    cached <- NULL
    objective <- function(x) {
      # optim requests value and gradient at the same point. Cache only that
      # point; this changes neither evaluated values nor the optimization path.
      if (!identical(x, previous)) {
        cached <<- .bt_lapse_objective(x, kernel, boundary)
        previous <<- x
      }
      cached
    }
    fit <- stats::optim(start, function(x) objective(x)$value / total,
      function(x) objective(x)$gradient / total, method = "BFGS",
      control = control[c("maxit", "reltol", "trace")])
    # As in the existing Gaussian optimizer, polish relative-objective convergence
    # with at most ten damped Newton steps. Independent natural-coordinate gates
    # still decide whether this is a stationary, identified interior solution.
    steps <- 0L
    if (fit$convergence == 0L) {
      for (i in seq_len(10L)) {
        obj <- objective(fit$par)
        H <- .bt_alpha_matrix(obj$hessian)
        if (!H$positive_definite) break
        step <- as.vector(backsolve(H$chol, forwardsolve(t(H$chol), obj$gradient)))
        natural_score <- .bt_lapse_surface(obj$natural, kernel)$gradient
        if (boundary) natural_score <- head(natural_score, -1L)
        if (max(abs(natural_score)) <= control$gradient_tol && max(abs(step)) <= control$step_tol) break
        accepted <- FALSE
        for (fraction in 2^-(0:20)) {
          candidate <- fit$par - fraction * step
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
    list(start_epsilon = epsilon, boundary = boundary, par = obj$natural,
         objective = obj$value, code = fit$convergence, message = fit$message %||% "",
         evaluations = fit$counts, newton_steps = steps, warnings = warnings)
  }, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
  }), error = function(e) {
    list(start_epsilon = epsilon, boundary = boundary, par = NULL, objective = Inf,
         code = 1L, message = conditionMessage(e), warnings = warnings)
  })
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
