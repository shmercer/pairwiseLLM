# Ordered binary likelihood matching btl_e_b, without its Bayesian priors.
.bt_lapse_control <- function(dots, verbose) {
  if (length(dots) && (length(dots) != 1L || !identical(names(dots), "control"))) {
    .bt_abort("The lapse engine accepts only a named `control` list in `...`.")
  }
  defaults <- list(maxit = 2000L, reltol = 1e-12, gradient_tol = 1e-7,
                   step_tol = 1e-7, min_rcond = 1e-12, trace = FALSE)
  control <- dots$control %||% list()
  if (!is.list(control) || (length(control) && (is.null(names(control)) || anyNA(names(control)) ||
      any(!names(control) %in% names(defaults)) || anyDuplicated(names(control))))) {
    .bt_abort("Lapse `control` must be a list of uniquely named supported numerical controls.")
  }
  control <- utils::modifyList(defaults, control, keep.null = TRUE)
  for (name in setdiff(names(defaults), "trace")) {
    x <- control[[name]]
    if (!.bt_real_vector(x) || length(x) != 1L || !is.finite(x) || x <= 0 ||
        (name == "maxit" && (x != floor(x) || x > .Machine$integer.max)) ||
        (name == "min_rcond" && x >= 1)) {
      .bt_abort(paste("Invalid positive finite lapse control:", name))
    }
  }
  if (!is.logical(control$trace) || length(control$trace) != 1L || is.na(control$trace)) {
    .bt_abort("Lapse control `trace` must be TRUE or FALSE.")
  }
  control$trace <- isTRUE(verbose) && control$trace
  control
}

.bt_lapse_design <- function(dat) {
  # Reuse exactly the existing item identification map, but NOT its unordered counts.
  prepared <- .bt_binary_design(dat)
  rows <- data.frame(first = match(as.character(dat[[1L]]), prepared$ids),
                     second = match(as.character(dat[[2L]]), prepared$ids), wins = dat[[3L]])
  rows$losses <- 1 - rows$wins
  counts <- stats::aggregate(cbind(wins, losses) ~ first + second, rows, sum)
  counts <- counts[order(counts$first, counts$second), , drop = FALSE]
  X <- cbind(prepared$transform[counts$first, , drop = FALSE] -
               prepared$transform[counts$second, , drop = FALSE], beta = 1)
  list(ids = prepared$ids, transform = prepared$transform, counts = counts, X = X)
}

# Derivatives in NATURAL epsilon coordinates, including every nuisance cross term.
.bt_lapse_surface <- function(par, kernel) {
  k <- ncol(kernel$X)
  epsilon <- par[k + 1L]
  eta <- as.vector(kernel$X %*% par[seq_len(k)])
  weights <- c(kernel$counts$wins, kernel$counts$losses)
  used <- weights > 0
  signed_eta <- c(eta, -eta)[used]
  design <- rbind(kernel$X, -kernel$X)[used, , drop = FALSE]
  weights <- weights[used]
  likelihood <- .link_log_likelihood(signed_eta, epsilon)
  # 0.5 - plogis(eta) = -tanh(eta / 2) / 2 avoids cancellation near zero.
  epsilon_score <- -0.5 * tanh(signed_eta / 2) * exp(-likelihood$logp)
  cross <- -exp(stats::plogis(signed_eta, log.p = TRUE) +
                 stats::plogis(-signed_eta, log.p = TRUE) - likelihood$logp) -
    likelihood$gradient * epsilon_score
  gradient <- -c(as.vector(crossprod(design, weights * likelihood$gradient)),
                  sum(weights * epsilon_score))
  H <- -crossprod(design, design * (weights * likelihood$hessian))
  cross <- -as.vector(crossprod(design, weights * cross))
  H <- rbind(cbind(H, cross), c(cross, sum(weights * epsilon_score^2)))
  dimnames(H) <- list(c(colnames(kernel$X), "epsilon"), c(colnames(kernel$X), "epsilon"))
  list(value = -sum(weights * likelihood$logp), gradient = gradient, hessian = H,
       probabilities = exp(.link_e1_log_probability(eta, epsilon)))
}

.bt_lapse_objective <- function(par, kernel, boundary = FALSE) {
  natural <- if (boundary) c(par, 0) else c(head(par, -1L), stats::plogis(tail(par, 1L)))
  surface <- .bt_lapse_surface(natural, kernel)
  k <- length(natural)
  if (boundary) {
    surface$gradient <- head(surface$gradient, -1L)
    surface$hessian <- surface$hessian[-k, -k, drop = FALSE]
  } else {
    epsilon <- natural[k]
    jacobian <- c(rep(1, k - 1L), epsilon * (1 - epsilon))
    surface$hessian <- surface$hessian * outer(jacobian, jacobian)
    surface$hessian[k, k] <- surface$hessian[k, k] +
      surface$gradient[k] * jacobian[k] * (1 - 2 * epsilon)
    surface$gradient <- surface$gradient * jacobian
  }
  surface$natural <- natural
  surface
}

.bt_lapse_information <- function(par, kernel) {
  eta <- as.vector(kernel$X %*% head(par, -1L))
  epsilon <- tail(par, 1L)
  p <- exp(.link_e1_log_probability(eta, epsilon))
  q <- exp(.link_e1_log_probability(-eta, epsilon))
  derivative <- cbind(kernel$X * ((1 - epsilon) * stats::plogis(eta) * stats::plogis(-eta)),
                       -0.5 * tanh(eta / 2))
  crossprod(derivative, derivative * ((kernel$counts$wins + kernel$counts$losses) / (p * q)))
}
