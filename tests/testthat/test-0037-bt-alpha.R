# Public deterministic fixtures: integer counts, no study outcomes or providers.
alpha_counts <- function(wins = c(7, 8, 6), losses = c(3, 2, 4),
                         pairs = data.frame(a = c("a", "a", "b"), b = c("b", "c", "c"))) {
  rows <- rep(seq_len(nrow(pairs)), wins + losses)
  data.frame(pairs[rows, ], y = unlist(Map(function(w, l) c(rep(1, w), rep(0, l)), wins, losses)),
             row.names = NULL)
}

alpha_fit <- function(dat = alpha_counts(), alpha = 0.5, ...) {
  pairwiseLLM::fit_bt_model(dat, engine = "alpha", alpha = alpha, verbose = FALSE, ...)
}

alpha_error <- function(expr) tryCatch(expr, pairwiseLLM_bt_alpha_error = identity)

test_that("two-item alpha estimates, predictions and original-data covariance match an oracle", {
  for (alpha in c(0.3, 0.5, 1, 2.75)) for (wins in c(0, 3, 5, 10)) {
    dat <- alpha_counts(wins, 10 - wins, data.frame(a = "left", b = "right"))
    fit <- alpha_fit(dat, alpha)
    contrast <- log((wins + alpha) / (10 - wins + alpha))
    p <- (wins + alpha) / (10 + 2 * alpha)
    expect_equal(fit$theta$theta, c(contrast, -contrast) / 2, tolerance = 1e-9)
    expect_equal(predict(fit), rep(p, 10), tolerance = 1e-10)
    expected <- matrix(c(1, -1, -1, 1), 2) / (4 * 10 * p * (1 - p))
    expect_equal(unname(fit$vcov), expected, tolerance = 1e-9)
    expect_equal(fit$theta$se^2, diag(expected), tolerance = 1e-9)
    expect_true(fit$provenance$convergence$converged)
    expect_identical(fit$alpha, alpha)
  }
})

test_that("alpha recovers known scores and approaches ordinary MLE with information", {
  pairs <- t(combn(4, 2))
  truth <- log(c(1, 2, 4, 8))
  truth <- truth - mean(truth)
  p <- plogis(truth[pairs[, 1]] - truth[pairs[, 2]])
  for (alpha in c(0.3, 0.5)) {
    errors <- numeric(3)
    for (k in seq_along(errors)) {
      total <- 45 * c(1, 10, 100)[k]
      wins <- round(total * p)
      dat <- alpha_counts(wins, total - wins,
                          data.frame(a = letters[pairs[, 1]], b = letters[pairs[, 2]]))
      fit <- alpha_fit(dat, alpha)
      errors[k] <- max(abs(fit$theta$theta - truth))
      mle <- alpha_fit(dat, 0)
      expect_equal(mle$theta$theta, truth, tolerance = 1e-9)
      expect_true(fit$diagnostics$stationary)
    }
    expect_true(all(diff(errors) < 0))
    expect_lt(tail(errors, 1), 2e-4)
  }
})

test_that("high-information alpha agrees with existing frequentist engines", {
  dat <- alpha_counts(c(7000, 8000, 6000), c(3000, 2000, 4000))
  fit <- alpha_fit(dat)
  if (requireNamespace("sirt", quietly = TRUE)) {
    sirt <- pairwiseLLM::fit_bt_model(dat, engine = "sirt", verbose = FALSE,
                                     sirt_eps = 0.3, fix.eta = 0, ignore.ties = TRUE,
                                     conv = 1e-10, maxiter = 1000)
    index <- match(fit$theta$ID, sirt$theta$ID)
    expect_equal(fit$theta$theta, sirt$theta$theta[index], tolerance = 2e-4)
  }
  if (requireNamespace("BradleyTerry2", quietly = TRUE)) {
    bt2 <- pairwiseLLM::fit_bt_model(dat, engine = "BradleyTerry2", verbose = FALSE)
    values <- bt2$theta$theta[match(fit$theta$ID, bt2$theta$ID)]
    expect_equal(fit$theta$theta, unname(values - mean(values)), tolerance = 2e-4)
  }
})

test_that("separation and sparse adaptive-like comparisons have finite deterministic fits", {
  # Near-neighbor comparisons dominate; path plus bridges fixes observed connectivity.
  pairs <- data.frame(a = c(letters[1:7], "a", "b"), b = c(letters[2:8], "d", "f"))
  for (alpha in c(0.3, 0.5)) {
    fixtures <- list(alpha_counts(c(10, 10, 10), c(0, 0, 0)),
                     alpha_counts(c(10, 10, 5), c(0, 0, 5)),
                     alpha_counts(c(rep(3, 7), 1, 2), c(rep(0, 7), 1, 0), pairs))
    for (dat in fixtures) {
      fit <- alpha_fit(dat, alpha)
      expect_true(all(is.finite(fit$theta$theta)))
      expect_true(all(is.finite(fit$theta$se) & fit$theta$se > 0))
      expect_equal(sum(fit$theta$theta), 0, tolerance = 1e-12)
      expect_identical(alpha_fit(dat, alpha)$theta, fit$theta)
      expect_identical(alpha_fit(dat, alpha)$vcov, fit$vcov)
      expect_true(fit$diagnostics$stationary)
    }
  }
})

test_that("all unobserved pairs receive exactly the reference penalty and score adjustment", {
  dat <- alpha_counts(c(3, 1), c(1, 2), data.frame(a = c("a", "b"), b = c("b", "c")))
  prepared <- pairwiseLLM:::.bt_binary_design(dat)
  kernel <- pairwiseLLM:::.bt_alpha_kernel(prepared, 0.3)
  beta <- c(0.8, -0.4)
  theta <- as.vector(prepared$transform %*% beta)
  s <- pairwiseLLM:::.bt_alpha_surface(beta, kernel)
  p <- plogis(outer(theta, theta, "-"))
  diag(p) <- 0
  adjustment <- 0.3 * (1 - 2 * rowSums(p) / 2)
  raw_score <- numeric(3)
  for (i in seq_len(nrow(dat))) {
    first <- match(dat$a[i], prepared$ids)
    second <- match(dat$b[i], prepared$ids)
    residual <- dat$y[i] - p[first, second]
    raw_score[first] <- raw_score[first] + residual
    raw_score[second] <- raw_score[second] - residual
  }
  observed_eta <- theta[match(dat$a, prepared$ids)] - theta[match(dat$b, prepared$ids)]
  loglik <- sum(plogis(ifelse(dat$y == 1, observed_eta, -observed_eta), log.p = TRUE))
  penalty <- 0.15 * sum(log(p[upper.tri(p)] * (1 - p[upper.tri(p)])))
  expect_equal(unname(s$score), raw_score + adjustment, tolerance = 1e-12)
  expect_equal(s$value, -loglik - penalty, tolerance = 1e-12)
  expect_equal(s$log_likelihood, loglik, tolerance = 1e-12)
  expect_equal(s$log_penalty, penalty, tolerance = 1e-12)
  expect_equal(nrow(kernel$counts), 3L)
  expect_equal(kernel$counts$wins, c(3, 0, 1))
  expect_equal(kernel$pseudo_count, 0.15)
})

test_that("analytic alpha gradients and Hessians agree with finite differences", {
  prepared <- pairwiseLLM:::.bt_binary_design(alpha_counts())
  for (alpha in c(0, 0.3, 0.5)) for (beta in list(c(0, 0), c(-0.8, 0.6), c(4, -3))) {
    kernel <- pairwiseLLM:::.bt_alpha_kernel(prepared, alpha)
    surface <- function(b) pairwiseLLM:::.bt_alpha_surface(b, kernel)
    h <- 1e-5
    steps <- diag(2) * h
    g <- vapply(1:2, function(j) {
      (surface(beta + steps[, j])$value - surface(beta - steps[, j])$value) / (2 * h)
    }, numeric(1))
    H <- vapply(1:2, function(j) {
      (surface(beta + steps[, j])$gradient - surface(beta - steps[, j])$gradient) / (2 * h)
    }, numeric(2))
    expect_equal(surface(beta)$gradient, g, tolerance = 1e-8)
    expect_equal(unname(surface(beta)$hessian), H, tolerance = 1e-8)
  }
})

test_that("item labels, row order and full or partial pair reversal preserve fits", {
  dat <- alpha_counts()
  reverse <- dat[c(2, 1, 3)]
  reverse[[3]] <- 1 - reverse[[3]]
  partial <- dat
  flip <- seq(1, nrow(dat), 2)
  partial[flip, 1:2] <- dat[flip, 2:1]
  partial$y[flip] <- 1 - dat$y[flip]
  labels <- c(a = "zebra", b = "item with spaces", c = "alpha")
  renamed <- data.frame(a = unname(labels[dat$a]), b = unname(labels[dat$b]), y = dat$y)
  for (alpha in c(0.3, 0.5)) {
    original <- alpha_fit(dat, alpha)
    for (changed in list(dat[rev(seq_len(nrow(dat))), ], reverse, partial, renamed)) {
      fit <- alpha_fit(changed, alpha)
      mapped <- if (identical(changed, renamed)) unname(labels[original$theta$ID]) else original$theta$ID
      index <- match(mapped, fit$theta$ID)
      expect_equal(fit$theta$theta[index], original$theta$theta, tolerance = 1e-9)
      expect_equal(fit$theta$se[index], original$theta$se, tolerance = 1e-9)
      expect_equal(unname(fit$vcov[index, index]), unname(original$vcov), tolerance = 1e-9)
      expect_equal(fit$reliability, original$reliability, tolerance = 1e-9)
    }
    expect_equal(predict(alpha_fit(reverse, alpha)), 1 - predict(original), tolerance = 1e-10)
  }
})

test_that("covariance in an independent basis uses observed information, not penalty curvature", {
  dat <- alpha_counts()
  fit <- alpha_fit(dat)
  ids <- fit$theta$ID
  incidence <- diag(3)[match(dat$a, ids), ] - diag(3)[match(dat$b, ids), ]
  basis <- qr.Q(qr(contr.helmert(3)))
  X <- incidence %*% basis
  p <- predict(fit)
  V <- basis %*% solve(crossprod(X, X * (p * (1 - p)))) %*% t(basis)
  B <- fit$provenance$identification$transformation
  penalized <- B %*% solve(fit$diagnostics$hessian) %*% t(B)
  expect_equal(unname(fit$vcov), V, tolerance = 1e-10)
  expect_gt(max(abs(fit$vcov - penalized)), 1e-4)
  expect_true(isSymmetric(fit$vcov))
  expect_equal(qr(fit$vcov)$rank, 2L)
  expect_equal(unname(rowSums(fit$vcov)), rep(0, 3), tolerance = 1e-12)
  expect_identical(rownames(fit$vcov), ids)
  expect_identical(colnames(fit$vcov), ids)
  expect_equal(fit$theta$se^2, unname(diag(fit$vcov)), tolerance = 1e-12)
  expect_equal(fit$theta$theta, as.vector(B %*% fit$fit$coefficients), tolerance = 1e-12)
  expect_equal(unname(fit$fit$coefficients), fit$theta$theta[1:2] - fit$theta$theta[3], tolerance = 1e-12)
  expect_equal(fit$ssr, pairwiseLLM::scale_separation_reliability(fit$theta$theta, fit$theta$se))
  expect_identical(fit$provenance$uncertainty$method, "inverse_unpenalized_observed_information")
  expect_false(fit$provenance$uncertainty$schedule_aware)
  expect_true(fit$provenance$uncertainty$valid)
})

test_that("zero score variance retains the fit and negative SSR is not clipped", {
  equal <- alpha_fit(alpha_counts(rep(5, 3), rep(5, 3)))
  expect_identical(equal$theta$theta, c(0, 0, 0))
  expect_true(equal$diagnostics$exact_zero_solution)
  expect_equal(unname(equal$diagnostics$coefficients), c(0, 0))
  expect_true(all(is.finite(equal$vcov)))
  expect_true(is.na(equal$reliability))
  expect_identical(equal$ssr$status, "zero_score_variance")
  expect_false(equal$provenance$reliability_valid)
  expect_equal(predict(equal), rep(0.5, 30))
  expect_error(pairwiseLLM::scale_separation_reliability(equal$theta$theta, equal$theta$se),
               "positive finite score variance")
  negative <- alpha_fit(alpha_counts(c(6, 5, 5), c(4, 5, 5)))
  expect_lt(negative$reliability, 0)
  expect_identical(negative$ssr$status, "negative_true_score_variance")
})

test_that("alpha prediction, provenance, summaries and serialization retain the public contract", {
  fit <- alpha_fit()
  expect_s3_class(fit, "pairwiseLLM_bt_alpha")
  expect_identical(fit$provenance$engine_package, "stats")
  expect_identical(fit$provenance$engine_version, as.character(getNamespaceVersion("stats")))
  expect_identical(fit$provenance$adjustment$method, "hamilton_alpha")
  expect_identical(fit$provenance$identification$parameter_order, c("a", "b"))
  expect_identical(names(fit$diagnostics$gradient), c("contrast1", "contrast2"))
  expect_identical(colnames(fit$diagnostics$information), c("contrast1", "contrast2"))
  expect_identical(fit$provenance$convergence$code, 0L)
  expect_match(fit$provenance$convergence$message, "stationarity checks passed")
  expect_null(fit$provenance$fallback_reason)
  expect_identical(fit$provenance$effective_settings$solver, "stats::glm.fit")
  expect_identical(fit$provenance$effective_settings$dispersion, 1)
  summary <- pairwiseLLM::summarize_bt_fit(fit)
  expect_identical(names(summary), c("ID", "theta", "se", "rank", "engine", "reliability"))
  expect_identical(summary$theta, fit$theta$theta)
  pairs <- data.frame(object1 = c("c", "a", "b", "c"), object2 = c("a", "a", "c", "a"))
  expect_equal(predict(fit, pairs), plogis(fit$theta$theta[c(3, 1, 2, 3)] - fit$theta$theta[c(1, 1, 3, 1)]))
  expect_equal(predict(fit, pairs)[2], 0.5)
  expect_identical(predict(fit, pairs[FALSE, ]), numeric())
  expect_equal(predict(fit, transform(pairs, object1 = factor(object1))), predict(fit, pairs))
  expect_error(predict(fit, type = "link"), "additional arguments")
  expect_error(predict(fit, data.frame(a = "a", b = "b")), "newdata")
  for (bad in list(NA_character_, "unknown", "", list("a"))) {
    broken <- pairs[1, ]
    broken$object1 <- bad
    expect_error(predict(fit, broken), "Prediction IDs")
  }
  path <- file.path(withr::local_tempdir(), "alpha.rds")
  saveRDS(fit, path)
  restored <- readRDS(path)
  expect_identical(restored$diagnostics, fit$diagnostics)
  expect_identical(restored$provenance, fit$provenance)
  expect_identical(predict(restored), predict(fit))
})

test_that("alpha zero requires a finite MLE, and all alphas reject disconnected data", {
  pairs <- data.frame(a = c("a", "b"), b = c("b", "c"))
  regular <- alpha_counts(c(3, 4), c(2, 1), pairs)
  expect_true(alpha_fit(regular, 0)$provenance$convergence$converged)
  for (wins in list(c(1, 1), c(0, 0), c(1, 0))) {
    dat <- alpha_counts(wins, 1 - wins, pairs)
    err <- alpha_error(alpha_fit(dat, 0))
    expect_identical(err$failure_reason, "no_finite_mle")
    expect_identical(err$provenance$convergence$status, "not_attempted")
  }
  # Directed cycle has a finite MLE despite one-direction-only edges.
  cycle <- data.frame(a = c("a", "b", "c"), b = c("b", "c", "a"), y = rep(1, 3))
  expect_equal(alpha_fit(cycle, 0)$theta$theta, rep(0, 3))
  disconnected <- data.frame(a = c("a", "c"), b = c("b", "d"), y = c(0, 1))
  for (alpha in c(0, 0.3, 0.5)) {
    expect_error(alpha_fit(disconnected, alpha), "graph is disconnected", class = "pairwiseLLM_bt_validation_error")
  }
  tied <- regular
  tied$y[1] <- 0.5
  expect_error(alpha_fit(tied), "binary outcomes")
})

test_that("alpha and its numerical controls are explicit, validated and never change engine defaults", {
  dat <- alpha_counts()
  for (bad in list(NULL, numeric(), NA_real_, NaN, Inf, -1, 1i, "0.5", TRUE, c(0.3, 0.5), matrix(0.5))) {
    expect_error(alpha_fit(alpha = bad), "finite nonnegative numeric scalar")
  }
  expect_error(pairwiseLLM::fit_bt_model(dat, engine = "alpha"), "supplied explicitly")
  for (engine in c("auto", "sirt", "BradleyTerry2", "brglm2")) {
    expect_error(pairwiseLLM::fit_bt_model(dat, engine = engine, alpha = 0.3), "requires engine")
  }
  expect_error(alpha_fit(sirt_eps = 0.3), "requires engine")
  for (bad in list(list(method = "BFGS"), list(alpha = 0.3), list(epsilon = 1e-6, epsilon = 1e-8),
                  list(1), 1, list(trace = NA), list(trace = 1))) {
    expect_error(alpha_fit(control = bad), "control")
  }
  for (name in c("epsilon", "maxit", "gradient_tol", "step_tol", "min_rcond")) {
    for (bad in list(0, -1, Inf, NA_real_, "1", numeric(), 1i, NULL, c(1, 2))) {
      expect_error(alpha_fit(control = setNames(list(bad), name)), "control")
    }
  }
  for (control in list(list(maxit = 1.5), list(maxit = 1e20), list(min_rcond = 1))) {
    expect_error(alpha_fit(control = control), "control")
  }
  expect_error(alpha_fit(dat, 0.5, 1), "only a named")
  expect_error(alpha_fit(eps = 0.3), "only a named")
  expect_error(alpha_fit(control = list(), control = list()), "only a named")
  expect_identical(alpha_fit(control = NULL)$theta, alpha_fit()$theta)
  expect_false(alpha_fit(control = list(trace = TRUE))$provenance$effective_settings$control$trace)
  expect_output(pairwiseLLM::fit_bt_model(dat, "alpha", alpha = 0.3, control = list(trace = TRUE)), "Deviance")
  # The alpha route is available even with all optional packages unavailable.
  testthat::local_mocked_bindings(.require_ns = function(...) FALSE, .package = "pairwiseLLM")
  expect_true(alpha_fit()$provenance$convergence$converged)
  expect_error(pairwiseLLM::fit_bt_model(dat), "Both sirt and BradleyTerry2 failed")
  expect_error(pairwiseLLM::fit_bt_model(dat, "a"), "Both sirt and BradleyTerry2 failed")
})

test_that("numerical failures retain diagnostics without a retry or theta-only fallback", {
  dat <- alpha_counts(10, 0, data.frame(a = "a", b = "b"))
  err <- alpha_error(alpha_fit(dat, 1e-100))
  expect_identical(err$failure_reason, "not_stationary")
  expect_true(err$diagnostics$optimizer$converged)
  expect_true(all(is.finite(err$theta$theta)))
  expect_gt(err$diagnostics$step_max, 0.1)
  err <- alpha_error(suppressWarnings(alpha_fit(control = list(maxit = 1L))))
  expect_identical(err$failure_reason, "not_converged")
  expect_length(err$diagnostics$warnings, 1)
  expect_false(err$diagnostics$optimizer$converged)
  expect_true(all(is.finite(err$theta$theta)))
  expect_true(all(is.finite(err$diagnostics$hessian)))
  expect_identical(alpha_error(alpha_fit(dat, .Machine$double.xmax))$failure_reason, "unrepresentable_penalty")
  tiny <- .Machine$double.xmin * .Machine$double.eps
  expect_identical(alpha_error(alpha_fit(alpha = tiny))$failure_reason, "unrepresentable_penalty")
  calls <- 0L
  testthat::local_mocked_bindings(.bt_alpha_glm = function(...) {
    calls <<- calls + 1L
    stop("synthetic numerical failure")
  }, .package = "pairwiseLLM")
  err <- alpha_error(alpha_fit())
  expect_identical(calls, 1L)
  expect_identical(err$failure_reason, "solver_error")
  expect_match(conditionMessage(err$parent), "synthetic numerical failure")
  expect_identical(err$provenance$adjustment$alpha, 0.5)
})

test_that("invalid returned contrasts and surfaces are rejected with an audit condition", {
  raw <- alpha_fit()$fit
  for (change in list(list(coefficients = c(Inf, 0)), list(coefficients = c(NA_real_, 0)),
                     list(coefficients = 0), list(rank = 1L))) {
    broken <- utils::modifyList(raw, change)
    testthat::with_mocked_bindings({
      err <- alpha_error(alpha_fit())
      expect_s3_class(err, "pairwiseLLM_bt_validation_error")
      expect_match(err$failure_reason, "invalid_coefficients|not_converged")
    }, .bt_alpha_glm = function(...) broken, .package = "pairwiseLLM")
  }
  surface <- pairwiseLLM:::.bt_alpha_surface
  testthat::with_mocked_bindings({
    err <- alpha_error(alpha_fit())
    expect_identical(err$failure_reason, "nonfinite_surface")
    expect_true(all(is.finite(err$theta$theta)))
  }, .bt_alpha_surface = function(...) {
    out <- surface(...)
    out$value <- Inf
    out
  }, .package = "pairwiseLLM")
})

test_that("invalid uncertainty rejects the fit but preserves converged theta and matrices", {
  surface <- pairwiseLLM:::.bt_alpha_surface
  for (target in c("hessian", "information")) {
    for (bad in list(matrix(NA_real_, 2, 2), matrix(c(1, 0, 2, 1), 2),
                    diag(c(-1, 1)), diag(c(1, 1e-15)))) {
      testthat::with_mocked_bindings({
        err <- alpha_error(alpha_fit())
        expect_s3_class(err, "pairwiseLLM_bt_alpha_error")
        expect_identical(err$failure_reason, paste0("invalid_", target))
        expect_true(err$provenance$convergence$converged)
        expect_true(all(is.finite(err$theta$theta)))
        expect_identical(err$diagnostics[[target]], bad)
        expect_false(err$provenance$uncertainty$valid)
        expect_false(err$provenance$reliability_valid)
      }, .bt_alpha_surface = function(...) {
        out <- surface(...)
        out[[target]] <- bad
        out
      }, .package = "pairwiseLLM")
    }
  }
  for (target in c(".bt_item_covariance", ".bt_centered_ssr")) {
    bindings <- setNames(list(function(...) stop("synthetic validation failure")), target)
    testthat::with_mocked_bindings({
      err <- alpha_error(alpha_fit())
      expect_match(err$failure_reason, "invalid_covariance|invalid_reliability")
      expect_true(err$provenance$convergence$converged)
      expect_true(err$diagnostics$stationary)
      expect_true(all(is.finite(err$theta$theta)))
      expect_match(conditionMessage(err$parent), "synthetic validation failure")
    }, !!!bindings, .package = "pairwiseLLM")
  }
})
