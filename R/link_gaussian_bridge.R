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

.link_e2_surface <- function(pairs, input) .link_pair_surface(pairs, input)

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
.link_e2_likelihood <- function(eta, epsilon) .link_log_likelihood(eta, epsilon)

.link_e2_objective <- function(w, kernel, hessian = FALSE) .link_gaussian_objective(w, kernel, hessian)

.link_e2_optimize <- function(kernel, control) .link_gaussian_optimize(kernel, control, .link_e2_objective)

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

.link_e2_integrate <- function(mean, sd, control) .link_gaussian_integrate(mean, sd, control, .link_e2_fail)

.link_e2_predict <- function(state, pairs, input) {
  .link_fields(state, c("method", "mean", "covariance", "controls", "hash"),
    c("method", "mean", "covariance", "controls", "hash"), "E2 prediction state")
  .link_check(identical(state$method, "gaussian_contrast_quadrature") &&
    identical(state$hash, .link_hash(state[names(state) != "hash"])) &&
    identical(state$controls, .link_e2_controls(input$control$estimator)), "Invalid E2 prediction state or controls.")
  .link_gaussian_predict(state, pairs, input, .link_e2_integrate, .link_e2_fail)
}
