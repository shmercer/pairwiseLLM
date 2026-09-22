# E1: one-dimensional integration with fixed Phase A shapes. No adaptive state.
.link_e1_control <- function(x) {
  .link_fields(x, c("rel_tol", "abs_tol", "subdivisions", "quantile_tol"), label = "E1 estimator controls")
  for (k in names(x)) {
    value <- .link_scalar(x[[k]], paste("E1", k), 0)
    .link_check(value > 0, paste0("E1 ", k, " must be positive."))
    if (k == "subdivisions") {
      .link_check(value == floor(value) && value <= .Machine$integer.max, "E1 subdivisions must be a positive integer.")
      value <- as.integer(value)
    }
    x[[k]] <- value
  }
  x
}

.link_e1_controls <- function(x) {
  utils::modifyList(list(rel_tol = 1e-9, abs_tol = 1e-11, subdivisions = 1000L,
    quantile_tol = 1e-8), .link_e1_control(x))
}

.link_e1_fail <- function(code, message) {
  rlang::abort(message, class = "pairwiseLLM_e1_numerical_error", failure_code = code)
}

.link_e1_pair_surface <- function(pairs, input) {
  ah <- pairs$A_set == input$hub$set_id
  hub <- input$phase_a$hub$value
  spoke <- input$phase_a$spoke$value
  h <- ifelse(ah, pairs$A_item, pairs$B_item)
  s <- ifelse(ah, pairs$B_item, pairs$A_item)
  sign <- ifelse(ah, -1, 1)
  list(base = unname(sign * (spoke[s] - hub[h]) + input$judge$beta), sign = sign)
}

.link_e1_kernel <- function(input) {
  surface <- .link_e1_pair_surface(input$cross, input)
  winner <- 2 * input$cross$y_A - 1
  base <- surface$base * winner
  slope <- surface$sign * winner
  # Canonical summation order gives row permutation invariance without changing
  # evidence order, hashes, or the multiplicity of legitimate repeated rows.
  index <- order(slope, base, method = "radix")
  list(base = base[index], slope = slope[index], epsilon = input$judge$epsilon,
    prior = input$control$delta_prior)
}

.link_e1_log_probability <- function(eta, epsilon) {
  lp <- stats::plogis(eta, log.p = TRUE)
  if (epsilon == 0) return(lp)
  if (epsilon == 1) return(rep(-log(2), length(eta)))
  a <- log1p(-epsilon) + lp
  b <- log(epsilon) - log(2)
  pmax(a, b) + log1p(exp(-abs(a - b)))
}

.link_e1_log_density <- function(x, kernel) {
  vapply(x, function(z) {
    delta <- kernel$prior$mean + kernel$prior$sd * z
    stats::dnorm(z, log = TRUE) + sum(.link_e1_log_probability(
      kernel$base + kernel$slope * delta, kernel$epsilon))
  }, numeric(1))
}

.link_e1_log_mass_bound <- function(a, b, kernel) {
  # Each observed-outcome probability is monotone in delta. Its endpoint maximum
  # times exact Normal interval mass bounds this panel, including infinite tails.
  if (a >= 0) {
    hi <- stats::pnorm(a, lower.tail = FALSE, log.p = TRUE)
    lo <- stats::pnorm(b, lower.tail = FALSE, log.p = TRUE)
  } else {
    hi <- stats::pnorm(b, log.p = TRUE)
    lo <- stats::pnorm(a, log.p = TRUE)
  }
  log_mass <- hi + log(-expm1(lo - hi))
  eta_a <- kernel$base + kernel$slope * (kernel$prior$mean + kernel$prior$sd * a)
  eta_b <- kernel$base + kernel$slope * (kernel$prior$mean + kernel$prior$sd * b)
  log_mass + sum(.link_e1_log_probability(pmax(eta_a, eta_b), kernel$epsilon))
}

.link_e1_nodes <- function() {
  # Standard embedded Gauss 7 / Kronrod 15 rule, tabulated in QUADPACK dqk15:
  # https://www.netlib.org/quadpack/dqk15.f . Coefficients only; implementation
  # below uses log weights and explicit rational maps for both infinite tails.
  positive <- c(.9914553711208126, .9491079123427585, .8648644233597691,
    .7415311855993944, .5860872354676911, .4058451513773972, .2077849550078985)
  wk <- c(.02293532201052922, .06309209262997855, .1047900103222502,
    .1406532597155259, .1690047266392679, .1903505780647854, .2044329400752989)
  wg <- c(0, .1294849661688697, 0, .2797053914892767, 0, .3818300505051189, 0)
  nodes <- c(-positive, 0, rev(positive))
  wk <- c(wk, .2094821410847278, rev(wk))
  wg <- c(wg, .4179591836734694, rev(wg))
  list(nodes = nodes, kronrod = wk, gauss = wg)
}

.link_e1_panel <- function(a, b, kernel) {
  rule <- .link_e1_nodes()
  nodes <- rule$nodes
  wk <- rule$kronrod
  wg <- rule$gauss
  if (is.finite(a) && is.finite(b)) {
    half <- (b - a) / 2
    x <- a + half * (1 + nodes)
    log_measure <- log(half)
  } else {
    t <- (1 + nodes) / 2
    x <- if (is.finite(a)) a + t / (1 - t) else b - t / (1 - t)
    log_measure <- -log(2) - 2 * log1p(-t)
  }
  density <- .link_e1_log_density(x, kernel)
  if (anyNA(density) || any(density == Inf) || any(!is.finite(x))) {
    .link_e1_fail("nonfinite_integrand", "E1 encountered a nonfinite integration surface.")
  }
  list(lower = a, upper = b, x = x, log_k = density + log_measure + log(wk),
    log_g = density + log_measure + log(wg),
    log_bound = .link_e1_log_mass_bound(a, b, kernel))
}

.link_e1_split <- function(panel) {
  a <- panel$lower
  b <- panel$upper
  mid <- if (!is.finite(a)) {
    b - max(1, abs(b))
  } else if (!is.finite(b)) {
    a + max(1, abs(a))
  } else {
    a + (b - a) / 2
  }
  if (!is.finite(mid) || mid <= a || mid >= b) {
    .link_e1_fail("quadrature_roundoff", "E1 cannot subdivide the integration interval further.")
  }
  mid
}

.link_e1_errors <- function(integrands, weights, gauss_weights) {
  wk <- .link_e1_nodes()$kronrod
  discrepancy <- vapply(seq_len(length(weights) / 15L), function(i) {
    rows <- (i - 1L) * 15L + seq_len(15L)
    values <- integrands[rows, , drop = FALSE]
    difference <- abs(colSums(values * (weights[rows] - gauss_weights[rows])))
    transformed <- values * (weights[rows] / wk)
    integral <- colSums(transformed * wk)
    asc <- colSums(abs(sweep(transformed, 2L, integral / 2, `-`)) * wk)
    error <- difference
    nonzero <- asc > 0 & difference > 0
    error[nonzero] <- asc[nonzero] * pmin(1, (200 * difference[nonzero] / asc[nonzero])^1.5)
    pmax(error, 50 * .Machine$double.eps * colSums(abs(values) * weights[rows]))
  }, numeric(ncol(integrands)))
  matrix(discrepancy, ncol = ncol(integrands), byrow = TRUE,
    dimnames = list(NULL, colnames(integrands)))
}

.link_e1_rule <- function(panels, control, prediction = NULL) {
  x <- unlist(lapply(panels, `[[`, "x"), use.names = FALSE)
  log_k <- unlist(lapply(panels, `[[`, "log_k"), use.names = FALSE)
  log_g <- unlist(lapply(panels, `[[`, "log_g"), use.names = FALSE)
  shift <- max(log_k)
  if (!is.finite(shift)) .link_e1_fail("normalization_failure", "E1 has no finite posterior mass at quadrature nodes.")
  k <- exp(log_k - shift)
  g <- exp(log_g - shift)
  total <- sum(k)
  weights <- k / total
  mean <- sum(weights * x)
  variance <- sum(weights * (x - mean)^2)
  integrands <- cbind(mass = 1, mean = x, variance = (x - mean)^2)
  if (!is.null(prediction)) integrands <- cbind(integrands, prediction = prediction(x))
  estimates <- colSums(integrands * weights)
  # Sum absolute panel discrepancies rather than allowing errors to cancel.
  discrepancy <- .link_e1_errors(integrands, weights, g / total)
  tolerance <- pmax(control$abs_tol, control$rel_tol * abs(estimates))
  error <- colSums(discrepancy)
  scores <- apply(sweep(discrepancy, 2L, tolerance, `/`), 1L, max)
  log_total <- shift + log(total)
  # A tiny sampled integral must not hide a distant/narrow mode. Refine panels
  # whose monotone envelope could exceed their sampled mass by exp(8), until
  # either mass is resolved or its bound is below the requested tolerance.
  bounds <- vapply(panels, `[[`, numeric(1), "log_bound")
  masses <- vapply(seq_along(panels), function(i) sum(weights[(i - 1L) * 15L + seq_len(15L)]), numeric(1))
  unresolved <- bounds - log_total > log(min(tolerance)) & bounds - log_total > log(masses) + 8
  if (anyNA(unresolved)) .link_e1_fail("normalization_failure", "E1 could not bound integration mass.")
  scores[unresolved] <- Inf
  list(x = x, weights = weights, gauss_weights = g / total, mean = mean,
    variance = variance, estimates = estimates, errors = error, scores = scores,
    converged = all(error <= tolerance) && !any(unresolved), log_normalizer = log_total,
    panel_mass = masses)
}

.link_e1_quadrature <- function(kernel, control, panels = NULL, prediction = NULL) {
  if (is.null(panels)) {
    cuts <- c(-Inf, -4, -2, -1, 0, 1, 2, 4, Inf)
    panels <- lapply(seq_len(length(cuts) - 1L), function(i) .link_e1_panel(cuts[i], cuts[i + 1L], kernel))
  }
  repeat {
    if (length(panels) > control$subdivisions) {
      .link_e1_fail("quadrature_subdivisions", "E1 quadrature exceeded the configured subdivision limit.")
    }
    rule <- .link_e1_rule(panels, control, prediction)
    if (rule$converged) break
    i <- which.max(rule$scores)
    old <- panels[[i]]
    mid <- .link_e1_split(old)
    panels <- append(panels[-i], list(.link_e1_panel(old$lower, mid, kernel),
      .link_e1_panel(mid, old$upper, kernel)), after = i - 1L)
  }
  list(panels = panels, rule = rule)
}

.link_e1_quantiles <- function(quadrature, kernel, control) {
  panels <- quadrature$panels
  masses <- quadrature$rule$panel_mass
  cumulative <- c(0, cumsum(masses))
  log_z <- quadrature$rule$log_normalizer
  max_error <- 0
  intervals <- matrix(NA_real_, 2L, 2L)
  quantiles <- vapply(seq_along(c(.025, .975)), function(j) {
    target <- c(.025, .975)[j]
    i <- which(cumulative[-1L] >= target)[1L]
    panel <- panels[[i]]
    residual <- target - cumulative[i]
    cdf <- function(x) {
      if (x == panel$lower) return(-residual)
      integral <- stats::integrate(function(z) exp(.link_e1_log_density(z, kernel) - log_z),
        lower = panel$lower, upper = x, rel.tol = control$rel_tol,
        abs.tol = control$abs_tol, subdivisions = control$subdivisions, stop.on.error = FALSE)
      if (!identical(integral$message, "OK") || !is.finite(integral$value) ||
        integral$abs.error > max(control$abs_tol, control$rel_tol * abs(integral$value))) {
        .link_e1_fail("quantile_integration_failure", "E1 CDF integration failed its tolerance.")
      }
      max_error <<- max(max_error, integral$abs.error)
      integral$value - residual
    }
    a <- panel$lower
    b <- panel$upper
    # Quantile bracketing changes only the root bracket; integration always uses
    # the original panel, including its infinite endpoint when present.
    for (attempt in seq_len(control$subdivisions)) {
      if (!is.finite(a)) a <- b - max(1, abs(b)) else if (!is.finite(b)) b <- a + max(1, abs(a))
      fa <- cdf(a)
      fb <- cdf(b)
      if (fa <= 0 && fb >= 0) break
      width <- b - a
      if (fa > 0) a <- a - width
      if (fb < 0) b <- b + width
    }
    if (!is.finite(a) || !is.finite(b) || fa > 0 || fb < 0) {
      .link_e1_fail("quantile_bracket_failure", "E1 could not bracket a posterior quantile.")
    }
    intervals[j, ] <<- c(a, b)
    stats::uniroot(cdf, c(a, b), f.lower = fa, f.upper = fb,
      tol = control$quantile_tol / kernel$prior$sd, maxiter = control$subdivisions,
      check.conv = TRUE)$root
  }, numeric(1))
  list(value = kernel$prior$mean + kernel$prior$sd * quantiles,
    error = max_error, brackets = intervals)
}

.link_e1_fit <- function(input, initial = NULL) {
  started <- proc.time()
  control <- .link_e1_controls(input$control$estimator)
  kernel <- .link_e1_kernel(input)
  prior <- kernel$prior
  diagnostics <- list(fit_attempted = TRUE, fit_valid = FALSE,
    uncertainty_scope = "offset_only_conditional_on_fixed_shapes",
    quadrature = list(method = "adaptive_gauss_kronrod_15_7", domain = c(-Inf, Inf),
      coordinate = "(delta - prior_mean) / prior_sd", controls = control))
  # No mode search is required. Ignoring numerical hints makes continuation and
  # a fresh fit use exactly the same quadrature with the same cumulative evidence.
  result <- tryCatch({
    q <- .link_e1_quadrature(kernel, control)
    diagnostics$quadrature$partitions <- vapply(q$panels, function(p) c(p$lower, p$upper), numeric(2))
    diagnostics$quadrature$subdivisions <- as.integer(length(q$panels))
    diagnostics$quadrature$log_normalizer <- q$rule$log_normalizer
    diagnostics$quadrature$errors <- q$rule$errors
    exact_prior <- input$counts$cross == 0L || kernel$epsilon == 1
    if (exact_prior) {
      mean <- prior$mean
      sd <- prior$sd
      interval <- stats::qnorm(c(.025, .975), mean, sd)
      diagnostics$quadrature$summary_method <- "exact_normal_prior"
    } else {
      mean <- prior$mean + prior$sd * q$rule$mean
      sd <- prior$sd * sqrt(q$rule$variance)
      quantiles <- .link_e1_quantiles(q, kernel, control)
      interval <- quantiles$value
      diagnostics$quadrature$cdf_abs_error <- quantiles$error
      diagnostics$quadrature$quantile_brackets <- quantiles$brackets
      diagnostics$quadrature$summary_method <- "adaptive_quadrature"
    }
    if (any(!is.finite(c(mean, sd, interval, sd^2))) || sd <= 0) {
      .link_e1_fail("nonfinite_summary", "E1 posterior summaries are not finite with positive variance.")
    }
    diagnostics$fit_valid <- TRUE
    diagnostics$convergence_code <- 0L
    diagnostics$finite_objective <- TRUE
    diagnostics$covariance_valid <- TRUE
    diagnostics$quadrature$status <- "OK"
    h <- unname(input$phase_a$hub$value)
    s <- unname(input$phase_a$spoke$value)
    list(theta_mean = c(h, s + mean), theta_sd = c(rep(0, length(h)), rep(sd, length(s))),
      lower = c(h, s + interval[1L]), upper = c(h, s + interval[2L]),
      delta = list(mean = mean, sd = sd, lower = interval[1L], upper = interval[2L],
        identification = if (input$counts$cross == 0L) {
          "prior_only"
        } else if (kernel$epsilon == 1) {
          "unidentified"
        } else {
          "cross_set"
        }),
      covariance = matrix(sd^2, 1L, 1L, dimnames = list("delta", "delta")),
      prediction = list(method = "posterior_quadrature", panels = q$panels,
        controls = control, log_normalizer = q$rule$log_normalizer,
        panel_hash = .link_hash(q$panels)))
  }, error = function(e) {
    diagnostics$failure_code <<- e$failure_code %||% "quadrature_failure"
    diagnostics$convergence_code <<- 1L
    diagnostics$quadrature$status <<- conditionMessage(e)
    list(theta_mean = rep(NA_real_, nrow(input$item_transform)),
      delta = list(mean = NA_real_, identification = "failed"))
  })
  elapsed <- proc.time() - started
  diagnostics$elapsed_seconds <- unname(elapsed[["elapsed"]])
  diagnostics$cpu_seconds <- unname(sum(elapsed[c("user.self", "sys.self")]))
  do.call(.link_new_result, c(list(input = input, diagnostics = diagnostics), result))
}

.link_e1_predict <- function(state, pairs, input) {
  .link_check(identical(state$method, "posterior_quadrature") && is.list(state$panels) && length(state$panels) > 0L,
    "E1 prediction requires its serialized quadrature state.")
  .link_check(identical(state$panel_hash, .link_hash(state$panels)), "E1 prediction quadrature state was modified.")
  if (nrow(pairs) == 0L) return(numeric())
  control <- .link_e1_controls(input$control$estimator)
  .link_check(identical(state$controls, control), "E1 prediction controls do not match the fit.")
  rule <- .link_e1_rule(state$panels, control)
  .link_check(isTRUE(all.equal(rule$log_normalizer, state$log_normalizer, tolerance = 1e-12)),
    "E1 prediction normalizer does not match its quadrature state.")
  surface <- .link_e1_pair_surface(pairs, input)
  kernel <- .link_e1_kernel(input)
  out <- numeric(nrow(pairs))
  delta <- kernel$prior$mean + kernel$prior$sd * rule$x
  # Batch pair-specific integrands over the shared nodes; avoid reevaluating
  # all likelihood rows for every requested pair. Bound temporary matrix size.
  for (start in seq.int(1L, nrow(pairs), by = 128L)) {
    index <- seq.int(start, min(nrow(pairs), start + 127L))
    eta <- sweep(outer(delta, surface$sign[index]), 2L, surface$base[index], `+`)
    probabilities <- (1 - kernel$epsilon) * stats::plogis(eta) + kernel$epsilon / 2
    values <- colSums(probabilities * rule$weights)
    errors <- colSums(.link_e1_errors(probabilities, rule$weights, rule$gauss_weights))
    refine <- which(errors > pmax(control$abs_tol, control$rel_tol * abs(values)))
    for (j in refine) {
      probability <- function(x) {
        eta <- surface$base[index[j]] + surface$sign[index[j]] *
          (kernel$prior$mean + kernel$prior$sd * x)
        (1 - kernel$epsilon) * stats::plogis(eta) + kernel$epsilon / 2
      }
      q <- .link_e1_quadrature(kernel, control, state$panels, probability)
      values[j] <- unname(q$rule$estimates[["prediction"]])
    }
    out[index] <- values
  }
  if (any(!is.finite(out)) || any(out < -64 * .Machine$double.eps | out > 1 + 64 * .Machine$double.eps)) {
    .link_e1_fail("prediction_integration_failure", "E1 integrated probabilities are invalid.")
  }
  pmin(1, pmax(0, out))
}
