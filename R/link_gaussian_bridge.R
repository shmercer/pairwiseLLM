# E2: full Gaussian Phase A bridges, cross-only MAP/Laplace, deterministic prediction.
.link_e2_control <- function(x) {
  .link_fields(x, c("maxit", "rel_tol", "gradient_tol", "prediction_rel_tol",
    "prediction_abs_tol", "subdivisions"), label = "E2 estimator controls")
  for (k in names(x)) {
    value <- .link_scalar(x[[k]], paste("E2", k), 0)
    .link_check(value > 0, paste0("E2 ", k, " must be positive."))
    if (k %in% c("maxit", "subdivisions")) {
      .link_check(value == floor(value) && value <= .Machine$integer.max,
        paste0("E2 ", k, " must be a positive integer."))
      value <- as.integer(value)
    }
    x[[k]] <- value
  }
  x
}

.link_e2_controls <- function(x) {
  utils::modifyList(list(maxit = 2000L, rel_tol = 1e-10, gradient_tol = 1e-6,
    prediction_rel_tol = 1e-9, prediction_abs_tol = 1e-11, subdivisions = 1000L), .link_e2_control(x))
}

.link_e2_fail <- function(code, message) {
  rlang::abort(message, class = "pairwiseLLM_e2_numerical_error", failure_code = code)
}

.link_e2_stabilize <- function(covariance) {
  if (!is.matrix(covariance) || !is.numeric(covariance) ||
    nrow(covariance) != ncol(covariance) || any(!is.finite(covariance))) {
    .link_e2_fail("bridge_covariance_invalid", "E2 bridge covariance must be finite and square.")
  }
  n <- nrow(covariance)
  if (!n) return(list(covariance = covariance, lower = covariance, scale = 1, jitter = 0))
  if (max(abs(covariance - t(covariance))) > 1e-10 * max(1, abs(covariance))) {
    .link_e2_fail("bridge_covariance_invalid", "E2 bridge covariance must be symmetric.")
  }
  covariance <- (covariance + t(covariance)) / 2
  scale <- max(diag(covariance))
  if (scale <= 0) scale <- 1
  for (relative in c(0, 1e-12, 1e-10, 1e-8, 1e-6)) {
    jitter <- relative * scale
    candidate <- covariance + diag(jitter, n)
    upper <- tryCatch(chol(candidate), error = function(e) NULL)
    if (!is.null(upper)) return(list(covariance = candidate, lower = t(upper), scale = scale, jitter = jitter))
  }
  .link_e2_fail("bridge_jitter_exhausted", "E2 bridge covariance requires more than 1e-6 times its variance scale.")
}

.link_e2_bridge <- function(draws, basis) {
  u <- .link_to_reduced(draws, basis)
  covariance <- if (ncol(u)) stats::cov(u) else matrix(numeric(), 0L, 0L)
  out <- .link_e2_stabilize(covariance)
  c(list(mean = colMeans(u), n_draws = nrow(u)), out)
}

.link_e2_surface <- function(pairs, input) {
  nh <- nrow(input$hub$items)
  endpoint <- function(set, item) {
    h <- match(item, input$hub$items$item_id)
    s <- nh + match(item, input$spoke$items$item_id)
    ifelse(set == input$hub$set_id, h, s)
  }
  input$item_transform[endpoint(pairs$A_set, pairs$A_item), , drop = FALSE] -
    input$item_transform[endpoint(pairs$B_set, pairs$B_item), , drop = FALSE]
}

.link_e2_kernel <- function(input, bridges) {
  prior <- input$control$delta_prior
  mean <- c(prior$mean, bridges$hub$mean, bridges$spoke$mean)
  d <- length(mean)
  lower <- matrix(0, d, d)
  lower[1L, 1L] <- prior$sd
  cursor <- 1L
  for (bridge in bridges) {
    n <- length(bridge$mean)
    if (n) {
      at <- cursor + seq_len(n)
      lower[at, at] <- bridge$lower
      cursor <- cursor + n
    }
  }
  # Sort only internal arithmetic, never alter the input evidence or its hashes.
  rows <- input$cross
  order <- order(rows$A_set, rows$A_item, rows$B_set, rows$B_item, rows$y_A, method = "radix")
  rows <- rows[order, , drop = FALSE]
  winner <- 2 * rows$y_A - 1
  surface <- .link_e2_surface(rows, input) * winner
  list(mean = unname(mean), lower = lower, X = surface %*% lower,
    base = as.double(surface %*% mean) + winner * input$judge$beta,
    epsilon = input$judge$epsilon,
    constant = sum(log(diag(lower))) + d * log(2 * pi) / 2)
}

# Derivatives of log P(observed outcome) in the winner-oriented linear predictor.
.link_e2_likelihood <- function(eta, epsilon) {
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

.link_e2_objective <- function(w, kernel, hessian = FALSE) {
  likelihood <- .link_e2_likelihood(as.double(kernel$base + kernel$X %*% w), kernel$epsilon)
  out <- list(value = sum(w^2) / 2 + kernel$constant - sum(likelihood$logp),
    gradient = as.double(w - crossprod(kernel$X, likelihood$gradient)))
  if (hessian) out$hessian <- diag(length(w)) - crossprod(kernel$X, kernel$X * likelihood$hessian)
  out
}

.link_e2_optimize <- function(kernel, control) {
  attempts <- lapply(c(0, -1, 1, -2, 2), function(delta_start) {
    start <- c(delta_start, rep(0, length(kernel$mean) - 1L))
    tryCatch({
      fit <- stats::optim(start, function(w) .link_e2_objective(w, kernel)$value,
        function(w) .link_e2_objective(w, kernel)$gradient, method = "BFGS",
        control = list(maxit = control$maxit, reltol = control$rel_tol))
      # Relative objective convergence can precede gradient convergence. Polish
      # successful BFGS fits by at most ten damped observed-Hessian Newton steps.
      steps <- 0L
      if (fit$convergence == 0L) {
        for (i in seq_len(10L)) {
          obj <- .link_e2_objective(fit$par, kernel, TRUE)
          if (max(abs(obj$gradient)) <= control$gradient_tol) break
          upper <- tryCatch(chol(obj$hessian), error = function(e) NULL)
          if (is.null(upper)) break
          step <- as.double(backsolve(upper, forwardsolve(t(upper), obj$gradient)))
          accepted <- FALSE
          for (fraction in 2^-(0:20)) {
            next_par <- fit$par - fraction * step
            next_value <- .link_e2_objective(next_par, kernel)$value
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
      obj <- .link_e2_objective(fit$par, kernel, TRUE)
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

.link_e2_fit <- function(input, initial = NULL) {
  started <- proc.time()
  control <- .link_e2_controls(input$control$estimator)
  diagnostics <- list(fit_attempted = TRUE, fit_valid = FALSE, uncertainty_scope = "joint_shapes_and_offset",
    bridge = list(), optimization = list(method = "BFGS_with_Newton_polishing", coordinates = "bridge_whitened",
      controls = control))
  result <- tryCatch({
    bridges <- list()
    for (k in c("hub", "spoke")) {
      bridges[[k]] <- .link_e2_bridge(input$phase_a[[k]]$value, input$basis[[k]])
      diagnostics$bridge[[k]] <- bridges[[k]][c("n_draws", "mean", "covariance", "scale", "jitter")]
    }
    diagnostics$covariance_jitter <- max(vapply(bridges, `[[`, numeric(1), "jitter"))
    kernel <- .link_e2_kernel(input, bridges)
    exact <- input$counts$cross == 0L || input$judge$epsilon == 1
    if (exact) {
      w <- rep(0, length(kernel$mean))
      obj <- .link_e2_objective(w, kernel, TRUE)
      diagnostics$optimization$method <- "exact_independent_bridges"
    } else {
      optimization <- .link_e2_optimize(kernel, control)
      diagnostics$optimization$attempts <- optimization$attempts
      diagnostics$optimization$selected <- optimization$selected
      if (is.na(optimization$selected)) {
        .link_e2_fail("optimizer_failure", "E2 found no converged stationary mode within the numerical controls.")
      }
      accepted <- optimization$attempts[[optimization$selected]]
      w <- accepted$mode
      obj <- .link_e2_objective(w, kernel, TRUE)
    }
    diagnostics$convergence_code <- 0L
    diagnostics$finite_objective <- is.finite(obj$value)
    diagnostics$finite_gradient <- all(is.finite(obj$gradient))
    diagnostics$optimization$objective <- obj$value
    diagnostics$optimization$gradient <- obj$gradient
    diagnostics$optimization$hessian <- obj$hessian
    diagnostics$optimization$gradient_max <- max(abs(obj$gradient))
    if (!diagnostics$finite_objective || !diagnostics$finite_gradient || any(!is.finite(obj$hessian))) {
      .link_e2_fail("nonfinite_mode", "E2 mode objective, gradient, or Hessian is not finite.")
    }
    upper <- tryCatch(chol(obj$hessian), error = function(e) NULL)
    diagnostics$hessian_pd <- !is.null(upper)
    if (is.null(upper)) .link_e2_fail("hessian_not_pd", "E2 observed Hessian is not positive definite.")
    V <- kernel$lower %*% chol2inv(upper) %*% t(kernel$lower)
    V <- (V + t(V)) / 2
    mode <- as.double(kernel$mean + kernel$lower %*% w)
    coords <- colnames(input$item_transform)
    names(mode) <- coords
    dimnames(V) <- list(coords, coords)
    diagnostics$optimization$hessian_rcond <- rcond(obj$hessian)
    diagnostics$optimization$covariance_rcond <- rcond(V)
    # These maps include hub/spoke/offset cross-covariances after updating.
    means <- as.double(input$item_transform %*% mode)
    variance <- diag(input$item_transform %*% V %*% t(input$item_transform))
    if (any(!is.finite(c(mode, V, means, variance))) || any(variance < 0) || V[1, 1] <= 0) {
      .link_e2_fail("invalid_laplace_covariance", "E2 Laplace summaries/covariance are not finite and valid.")
    }
    sd <- sqrt(variance)
    delta_sd <- sqrt(V[1, 1])
    diagnostics$covariance_valid <- TRUE
    diagnostics$fit_valid <- TRUE
    prediction <- list(method = "gaussian_contrast_quadrature", mean = mode, covariance = V, controls = control)
    prediction$hash <- .link_hash(prediction)
    list(theta_mean = means, theta_sd = sd, lower = means + stats::qnorm(.025) * sd,
      upper = means + stats::qnorm(.975) * sd,
      delta = list(mean = unname(mode[1]), sd = delta_sd,
        lower = unname(mode[1]) + stats::qnorm(.025) * delta_sd,
        upper = unname(mode[1]) + stats::qnorm(.975) * delta_sd,
        identification = if (input$counts$cross == 0L) "prior_only" else if (exact) "unidentified" else "cross_set"),
      covariance = V, mode = mode, prediction = prediction)
  }, error = function(e) {
    diagnostics$fit_valid <<- FALSE
    diagnostics$covariance_valid <<- FALSE
    diagnostics$failure_code <<- e$failure_code %||% "e2_numerical_failure"
    diagnostics$optimization$message <<- conditionMessage(e)
    list(theta_mean = rep(NA_real_, nrow(input$item_transform)),
      delta = list(mean = NA_real_, identification = "failed"))
  })
  elapsed <- proc.time() - started
  diagnostics$elapsed_seconds <- unname(elapsed[["elapsed"]])
  diagnostics$cpu_seconds <- unname(sum(elapsed[c("user.self", "sys.self")]))
  do.call(.link_new_result, c(list(input = input, diagnostics = diagnostics), result))
}

.link_e2_integrate <- function(mean, sd, control) {
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
      .link_e2_fail("prediction_integration_failure", "E2 Gaussian prediction integration failed.")
    }
    values[i] <- ans$value
    errors[i] <- ans$abs.error
  }
  value <- sum(values)
  if (sum(errors) > max(control$prediction_abs_tol, control$prediction_rel_tol * abs(value)) ||
    !is.finite(value) || value < 0 || value > 1 + control$prediction_abs_tol) {
    .link_e2_fail("prediction_integration_failure", "E2 Gaussian prediction did not meet numerical tolerances.")
  }
  min(1, value)
}

.link_e2_predict <- function(state, pairs, input) {
  .link_fields(state, c("method", "mean", "covariance", "controls", "hash"),
    c("method", "mean", "covariance", "controls", "hash"), "E2 prediction state")
  .link_check(identical(state$method, "gaussian_contrast_quadrature") &&
    identical(state$hash, .link_hash(state[names(state) != "hash"])) &&
    identical(state$controls, .link_e2_controls(input$control$estimator)), "Invalid E2 prediction state or controls.")
  coords <- colnames(input$item_transform)
  mean <- .link_align_numeric(state$mean, coords, "E2 prediction mean")
  covariance <- .link_covariance(state$covariance, coords)
  X <- .link_e2_surface(pairs, input)
  mu <- as.double(X %*% mean) + input$judge$beta
  variance <- rowSums((X %*% covariance) * X)
  if (any(!is.finite(c(mu, variance))) || any(variance < 0)) {
    .link_e2_fail("prediction_variance_invalid", "E2 Gaussian contrast variance must be finite and nonnegative.")
  }
  if (input$judge$epsilon == 1) return(rep(.5, nrow(pairs)))
  vapply(seq_len(nrow(pairs)), function(i) {
    (1 - input$judge$epsilon) * .link_e2_integrate(mu[i], sqrt(variance[i]), state$controls) +
      input$judge$epsilon / 2
  }, numeric(1))
}
