# Shared fixed-judge likelihood and Gaussian numerical routines.
# Estimator-specific evidence and priors are assembled by each backend.

.link_pair_surface <- function(pairs, input) {
  nh <- nrow(input$hub$items)
  endpoint <- function(set, item) {
    h <- match(item, input$hub$items$item_id)
    s <- nh + match(item, input$spoke$items$item_id)
    ifelse(set == input$hub$set_id, h, s)
  }
  input$item_transform[endpoint(pairs$A_set, pairs$A_item), , drop = FALSE] -
    input$item_transform[endpoint(pairs$B_set, pairs$B_item), , drop = FALSE]
}

.link_log_likelihood <- function(eta, epsilon) {
  p <- stats::plogis(eta)
  q <- stats::plogis(-eta)
  lp <- stats::plogis(eta, log.p = TRUE)
  if (epsilon == 0) return(list(logp = lp, gradient = q, hessian = -p * q))
  if (epsilon == 1) return(list(logp = rep(-log(2), length(eta)),
    gradient = rep(0, length(eta)), hessian = rep(0, length(eta))))
  a <- log1p(-epsilon) + lp
  b <- log(epsilon) - log(2)
  logp <- pmax(a, b) + log1p(exp(-abs(a - b)))
  responsibility <- exp(a - logp)
  gradient <- responsibility * q
  list(logp = logp, gradient = gradient, hessian = gradient * (1 - 2 * p) - gradient^2)
}

.link_gaussian_objective <- function(w, kernel, hessian = FALSE) {
  likelihood <- .link_log_likelihood(as.double(kernel$base + kernel$X %*% w), kernel$epsilon)
  out <- list(value = sum(w^2) / 2 + kernel$constant - sum(likelihood$logp),
    gradient = as.double(w - crossprod(kernel$X, likelihood$gradient)))
  if (hessian) out$hessian <- diag(length(w)) - crossprod(kernel$X, kernel$X * likelihood$hessian)
  out
}

.link_gaussian_optimize <- function(kernel, control, objective = .link_gaussian_objective) {
  attempts <- lapply(c(0, -1, 1, -2, 2), function(delta_start) {
    start <- c(delta_start, rep(0, length(kernel$mean) - 1L))
    tryCatch({
      fit <- stats::optim(start, function(w) objective(w, kernel)$value,
        function(w) objective(w, kernel)$gradient, method = "BFGS",
        control = list(maxit = control$maxit, reltol = control$rel_tol))
      # Relative objective convergence can precede gradient convergence. Polish
      # successful BFGS fits by at most ten damped observed-Hessian Newton steps.
      steps <- 0L
      if (fit$convergence == 0L) {
        for (i in seq_len(10L)) {
          obj <- objective(fit$par, kernel, TRUE)
          if (max(abs(obj$gradient)) <= control$gradient_tol) break
          upper <- tryCatch(chol(obj$hessian), error = function(e) NULL)
          if (is.null(upper)) break
          step <- as.double(backsolve(upper, forwardsolve(t(upper), obj$gradient)))
          accepted <- FALSE
          for (fraction in 2^-(0:20)) {
            next_par <- fit$par - fraction * step
            next_value <- objective(next_par, kernel)$value
            if (is.finite(next_value) && next_value <= obj$value + 1e-12 * max(1, abs(obj$value))) {
              fit$par <- next_par
              accepted <- TRUE
              steps <- steps + 1L
              break
            }
          }
          if (!accepted) break
        }
      }
      obj <- objective(fit$par, kernel, TRUE)
      list(start_delta_sd = delta_start, mode = fit$par, convergence_code = as.integer(fit$convergence),
        objective = obj$value, gradient = obj$gradient, gradient_max = max(abs(obj$gradient)),
        hessian = obj$hessian, evaluations = fit$counts, newton_steps = steps,
        message = fit$message %||% "")
    }, error = function(e) {
      list(start_delta_sd = delta_start, convergence_code = 1L,
        objective = NA_real_, gradient_max = NA_real_, message = conditionMessage(e))
    })
  })
  valid <- vapply(attempts, function(a) {
    a$convergence_code == 0L && is.finite(a$objective) && is.finite(a$gradient_max) &&
      a$gradient_max <= control$gradient_tol
  }, logical(1))
  selected <- if (any(valid)) {
    which(valid)[which.min(vapply(attempts[valid], `[[`, numeric(1), "objective"))]
  } else {
    NA_integer_
  }
  list(attempts = attempts, selected = as.integer(selected))
}

.link_gaussian_integrate <- function(mean, sd, control, fail) {
  if (sd == 0) return(stats::plogis(mean))
  # Split at the logistic transition and normal center so sharp transitions
  # cannot be missed by the adaptive integrator, even for broad contrasts.
  transition <- max(-8, min(8, -mean / sd))
  cuts <- sort(unique(c(-Inf, min(0, transition), max(0, transition), Inf)))
  values <- errors <- numeric(length(cuts) - 1L)
  for (i in seq_along(values)) {
    ans <- tryCatch(stats::integrate(function(z) stats::dnorm(z) * stats::plogis(mean + sd * z),
      cuts[i], cuts[i + 1L], rel.tol = control$prediction_rel_tol,
      abs.tol = control$prediction_abs_tol / length(values), subdivisions = control$subdivisions,
      stop.on.error = FALSE), error = function(e) NULL)
    if (is.null(ans) || ans$message != "OK" || !is.finite(ans$value) || !is.finite(ans$abs.error)) {
      fail("prediction_integration_failure", "Gaussian prediction integration failed.")
    }
    values[i] <- ans$value
    errors[i] <- ans$abs.error
  }
  value <- sum(values)
  if (sum(errors) > max(control$prediction_abs_tol, control$prediction_rel_tol * abs(value)) ||
    !is.finite(value) || value < 0 || value > 1 + control$prediction_abs_tol) {
    fail("prediction_integration_failure", "Gaussian prediction did not meet numerical tolerances.")
  }
  min(1, value)
}

.link_gaussian_predict <- function(state, pairs, input, integrate, fail) {
  coords <- colnames(input$item_transform)
  mean <- .link_align_numeric(state$mean, coords, "Gaussian prediction mean")
  covariance <- .link_covariance(state$covariance, coords)
  X <- .link_pair_surface(pairs, input)
  mu <- as.double(X %*% mean) + input$judge$beta
  variance <- rowSums((X %*% covariance) * X)
  if (any(!is.finite(c(mu, variance))) || any(variance < 0)) {
    fail("prediction_variance_invalid", "Gaussian contrast variance must be finite and nonnegative.")
  }
  if (input$judge$epsilon == 1) return(rep(.5, nrow(pairs)))
  vapply(seq_len(nrow(pairs)), function(i) {
    (1 - input$judge$epsilon) * integrate(mu[i], sqrt(variance[i]), state$controls) +
      input$judge$epsilon / 2
  }, numeric(1))
}
