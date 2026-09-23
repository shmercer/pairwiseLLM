# E3: one joint likelihood, ordinary centered-shape priors, explicit set offset.
.link_e3_control <- function(x) {
  numeric_fields <- c("maxit", "rel_tol", "gradient_tol", "prediction_rel_tol",
    "prediction_abs_tol", "subdivisions")
  .link_fields(x, c("engine", numeric_fields, "cmdstan"), label = "E3 estimator controls")
  if (!is.null(x$engine)) {
    .link_check(is.character(x$engine) && length(x$engine) == 1L && !is.na(x$engine) &&
      x$engine %in% c("map_laplace", "mcmc"), "E3 engine must be map_laplace or mcmc.")
  }
  for (k in intersect(names(x), numeric_fields)) {
    value <- .link_scalar(x[[k]], paste("E3", k), 0)
    .link_check(value > 0, paste0("E3 ", k, " must be positive."))
    if (k %in% c("maxit", "subdivisions")) {
      .link_check(value == floor(value) && value <= .Machine$integer.max,
        paste0("E3 ", k, " must be a positive integer."))
      value <- as.integer(value)
    }
    x[[k]] <- value
  }
  if (!is.null(x$cmdstan)) {
    .link_check(identical(x$engine, "mcmc"), "E3 cmdstan controls require engine = 'mcmc'.")
    x$cmdstan <- .link_e3_cmdstan_control(x$cmdstan)
  }
  x
}

.link_e3_controls <- function(x) {
  utils::modifyList(list(engine = "map_laplace", maxit = 2000L, rel_tol = 1e-10,
    gradient_tol = 1e-6, prediction_rel_tol = 1e-9, prediction_abs_tol = 1e-11,
    subdivisions = 1000L, cmdstan = list()), .link_e3_control(x))
}

.link_e3_fail <- function(code, message) {
  rlang::abort(message, class = "pairwiseLLM_e3_numerical_error", failure_code = code)
}

.link_e3_evidence <- function(input) {
  # No aggregation, filtering, posterior replay, or manufactured observations.
  rows <- dplyr::bind_rows(input$phase_a$hub$value, input$phase_a$spoke$value, input$cross)
  .link_check(nrow(rows) == sum(unlist(input$counts[c("phase_a_hub", "phase_a_spoke", "cross")])) &&
    !anyDuplicated(rows$observation_id), "E3 likelihood evidence does not reconcile.")
  # Canonical arithmetic order leaves the authoritative input/hash order intact.
  rows[order(rows$A_set, rows$A_item, rows$B_set, rows$B_item, rows$y_A,
    rows$observation_id, method = "radix"), , drop = FALSE]
}

.link_e3_kernel <- function(input) {
  rows <- .link_e3_evidence(input)
  d <- ncol(input$item_transform)
  prior <- input$control$delta_prior
  mean <- c(prior$mean, rep(0, d - 1L))
  lower <- diag(c(prior$sd, rep(1, d - 1L)), nrow = d)
  winner <- 2 * rows$y_A - 1
  surface <- .link_pair_surface(rows, input) * winner
  list(mean = mean, lower = lower, X = surface %*% lower,
    base = as.double(surface %*% mean) + winner * input$judge$beta,
    epsilon = input$judge$epsilon,
    constant = log(prior$sd) + d * log(2 * pi) / 2)
}

.link_e3_identification <- function(input) {
  if (input$counts$cross == 0L) "prior_only" else if (input$judge$epsilon == 1) "unidentified" else "cross_set"
}

.link_e3_fit <- function(input, initial = NULL) {
  control <- .link_e3_controls(input$control$estimator)
  if (control$engine == "mcmc") return(.link_e3_mcmc(input, control))
  .link_e3_map(input, control)
}

.link_e3_map <- function(input, control) {
  started <- proc.time()
  diagnostics <- list(fit_attempted = TRUE, fit_valid = FALSE,
    uncertainty_scope = "joint_shapes_and_offset", covariance_jitter = 0,
    optimization = list(engine = "map_laplace", method = "BFGS_with_Newton_polishing",
      coordinates = "prior_whitened", controls = control))
  result <- tryCatch({
    kernel <- .link_e3_kernel(input)
    if (!nrow(kernel$X) || input$judge$epsilon == 1) {
      w <- rep(0, length(kernel$mean))
      diagnostics$optimization$method <- "exact_independent_priors"
    } else {
      optimization <- .link_gaussian_optimize(kernel, control)
      diagnostics$optimization$attempts <- optimization$attempts
      diagnostics$optimization$selected <- optimization$selected
      if (is.na(optimization$selected)) {
        .link_e3_fail("optimizer_failure", "E3 found no converged stationary mode within the numerical controls.")
      }
      w <- optimization$attempts[[optimization$selected]]$mode
    }
    obj <- .link_gaussian_objective(w, kernel, TRUE)
    diagnostics$convergence_code <- 0L
    diagnostics$finite_objective <- is.finite(obj$value)
    diagnostics$finite_gradient <- all(is.finite(obj$gradient))
    diagnostics$optimization$objective <- obj$value
    diagnostics$optimization$gradient <- obj$gradient
    diagnostics$optimization$gradient_max <- max(abs(obj$gradient))
    diagnostics$optimization$hessian <- obj$hessian
    if (!diagnostics$finite_objective || !diagnostics$finite_gradient || any(!is.finite(obj$hessian))) {
      .link_e3_fail("nonfinite_mode", "E3 mode objective, gradient, or Hessian is not finite.")
    }
    if (max(abs(obj$gradient)) > control$gradient_tol) {
      .link_e3_fail("gradient_tolerance", "E3 mode gradient exceeds the requested tolerance.")
    }
    upper <- tryCatch(chol(obj$hessian), error = function(e) NULL)
    diagnostics$hessian_pd <- !is.null(upper)
    if (is.null(upper)) .link_e3_fail("hessian_not_pd", "E3 observed Hessian is not positive definite.")
    V <- kernel$lower %*% chol2inv(upper) %*% t(kernel$lower)
    V <- (V + t(V)) / 2
    coords <- colnames(input$item_transform)
    mode <- stats::setNames(as.double(kernel$mean + kernel$lower %*% w), coords)
    dimnames(V) <- list(coords, coords)
    inv_scale <- diag(1 / diag(kernel$lower), nrow = length(coords))
    diagnostics$optimization$free_hessian <- inv_scale %*% obj$hessian %*% inv_scale
    dimnames(diagnostics$optimization$free_hessian) <- list(coords, coords)
    diagnostics$optimization$hessian_rcond <- rcond(obj$hessian)
    diagnostics$optimization$covariance_rcond <- rcond(V)
    means <- as.double(input$item_transform %*% mode)
    variance <- diag(input$item_transform %*% V %*% t(input$item_transform))
    if (any(!is.finite(c(mode, V, means, variance))) || any(variance < 0) || V[1, 1] <= 0) {
      .link_e3_fail("invalid_laplace_covariance", "E3 Laplace summaries/covariance are not finite and valid.")
    }
    sd <- sqrt(variance)
    delta_sd <- sqrt(V[1, 1])
    diagnostics$covariance_valid <- TRUE
    diagnostics$fit_valid <- TRUE
    prediction <- list(method = "gaussian_contrast_quadrature", mean = mode,
      covariance = V, controls = control)
    prediction$hash <- .link_hash(prediction)
    list(theta_mean = means, theta_sd = sd, lower = means + stats::qnorm(.025) * sd,
      upper = means + stats::qnorm(.975) * sd,
      delta = list(mean = unname(mode[1]), sd = delta_sd,
        lower = unname(mode[1]) + stats::qnorm(.025) * delta_sd,
        upper = unname(mode[1]) + stats::qnorm(.975) * delta_sd,
        identification = .link_e3_identification(input)),
      covariance = V, mode = mode, prediction = prediction)
  }, error = function(e) {
    diagnostics$fit_valid <<- FALSE
    diagnostics$covariance_valid <<- FALSE
    diagnostics$failure_code <<- e$failure_code %||% "e3_numerical_failure"
    diagnostics$optimization$message <<- conditionMessage(e)
    list(theta_mean = rep(NA_real_, nrow(input$item_transform)),
      delta = list(mean = NA_real_, identification = "failed"))
  })
  elapsed <- proc.time() - started
  diagnostics$elapsed_seconds <- unname(elapsed[["elapsed"]])
  diagnostics$cpu_seconds <- unname(sum(elapsed[c("user.self", "sys.self")]))
  do.call(.link_new_result, c(list(input = input, diagnostics = diagnostics), result))
}

.link_e3_predict <- function(state, pairs, input) {
  control <- .link_e3_controls(input$control$estimator)
  if (control$engine == "mcmc") return(.link_e3_mcmc_predict(state, pairs, input))
  .link_fields(state, c("method", "mean", "covariance", "controls", "hash"),
    c("method", "mean", "covariance", "controls", "hash"), "E3 prediction state")
  .link_check(identical(state$method, "gaussian_contrast_quadrature") &&
    identical(state$hash, .link_hash(state[names(state) != "hash"])) &&
    identical(state$controls, control), "Invalid E3 prediction state or controls.")
  integrate <- function(mean, sd, control) .link_gaussian_integrate(mean, sd, control, .link_e3_fail)
  .link_gaussian_predict(state, pairs, input, integrate, .link_e3_fail)
}
