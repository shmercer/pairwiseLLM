test_that("population recovery and zero position bias hold across graph sizes", {
  for (n in c(4L, 8L, 12L)) for (beta in c(0, 0.3)) {
    case <- lapse_case(n, beta, 0.2, if (n == 4L) "complete" else "cycle_chords")
    fit <- lapse_fit_case(case)
    expect_equal(fit$theta$theta, case$theta, tolerance = 1e-5)
    expect_equal(fit$beta, beta, tolerance = 1e-5)
    expect_equal(fit$epsilon, 0.2, tolerance = 1e-5)
    expect_identical(fit$model_variant, "btl_e_b")
    expect_identical(fit$provenance$adjustment$method, "none")
    expect_true(fit$provenance$convergence$converged)
    expect_true(fit$provenance$uncertainty$valid)
    expect_true(is.na(fit$reliability))
    expect_false(fit$ssr$valid)
  }
})

test_that("joint covariance includes beta and epsilon uncertainty and matches an independent basis", {
  case <- lapse_case()
  fit <- lapse_fit_case(case)
  n <- nrow(fit$theta)
  counts <- case$kernel$counts
  Q <- qr.Q(qr(contr.helmert(n)))
  D <- cbind(Q[counts$first, ] - Q[counts$second, ], 1)
  q <- plogis(case$theta[counts$first] - case$theta[counts$second] + case$beta)
  p <- (1 - case$epsilon) * q + case$epsilon / 2
  derivative <- cbind(D * ((1 - case$epsilon) * q * (1 - q)), 0.5 - q)
  information <- crossprod(derivative, derivative * ((counts$wins + counts$losses) / (p * (1 - p))))
  map <- rbind(cbind(Q, 0, 0), c(rep(0, n - 1), 1, 0), c(rep(0, n - 1), 0, 1))
  expected <- map %*% solve(information) %*% t(map)
  expect_equal(unname(fit$parameter_vcov), expected, tolerance = 1e-8)
  expect_equal(unname(fit$vcov), expected[seq_len(n), seq_len(n)], tolerance = 1e-8)
  expect_equal(fit$theta$se^2, unname(diag(fit$vcov)), tolerance = 1e-12)
  expect_equal(unname(rowSums(fit$vcov)), rep(0, n), tolerance = 1e-12)
  expect_equal(qr(fit$parameter_vcov)$rank, n + 1L)
  expect_gte(min(eigen(fit$parameter_vcov, symmetric = TRUE)$values), -1e-12)
  conditional <- Q %*% solve(information[seq_len(n - 1L), seq_len(n - 1L)]) %*% t(Q)
  expect_gt(max(abs(fit$vcov - conditional)), 1e-4)
})

test_that("public fits retain predictions, summaries and invariance under relabeling and reversal", {
  case <- lapse_case(seed = 30501, total = 2000)
  dat <- lapse_binary_data(case)
  fit <- pairwiseLLM::fit_bt_model(dat, engine = "lapse", verbose = FALSE)
  expect_equal(fit$theta, lapse_fit_case(case)$theta)
  expect_equal(fit$log_likelihood, -fit$objective)
  expect_equal(nrow(pairwiseLLM::summarize_bt_fit(fit)), 4L)
  original <- predict(fit)
  p <- (1 - fit$epsilon) * plogis(fit$theta$theta[match(dat$object1, fit$theta$ID)] -
                                  fit$theta$theta[match(dat$object2, fit$theta$ID)] + fit$beta) + fit$epsilon / 2
  expect_equal(original, p, tolerance = 1e-14)
  labels <- c(a = "zebra", b = "item with spaces", c = "beta", d = "alpha")
  renamed <- dat
  renamed$object1 <- unname(labels[dat$object1])
  renamed$object2 <- unname(labels[dat$object2])
  reversed <- dat[c(2, 1, 3)]
  names(reversed) <- names(dat)
  reversed$result <- 1 - dat$result
  for (which in c("row_order", "labels", "reverse")) {
    changed <- switch(which, row_order = dat[rev(seq_len(nrow(dat))), ], labels = renamed, reverse = reversed)
    other <- pairwiseLLM::fit_bt_model(changed, engine = "lapse", verbose = FALSE)
    index <- match(if (which == "labels") unname(labels[fit$theta$ID]) else fit$theta$ID, other$theta$ID)
    expect_equal(other$theta$theta[index], fit$theta$theta, tolerance = 1e-7)
    expect_equal(other$epsilon, fit$epsilon, tolerance = 1e-7)
    expect_equal(other$beta, if (which == "reverse") -fit$beta else fit$beta, tolerance = 1e-7)
    expect_equal(unname(other$vcov[index, index]), unname(fit$vcov), tolerance = 1e-7)
    if (which == "reverse") {
      expect_equal(predict(other), 1 - original, tolerance = 1e-8)
      sign <- c(rep(1, nrow(fit$theta)), -1, 1)
      expect_equal(other$parameter_vcov, fit$parameter_vcov * outer(sign, sign), tolerance = 1e-7)
    }
  }
  expect_gt(max(abs(predict(fit, reversed) - (1 - original))), 0.01)
  expect_equal(predict(fit, data.frame(object1 = "a", object2 = "a")),
                 (1 - fit$epsilon) * plogis(fit$beta) + fit$epsilon / 2)
  expect_identical(predict(fit, dat[FALSE, ]), numeric())
  expect_error(predict(fit, dat, extra = TRUE), "additional arguments")
  expect_error(predict(fit, list()), "data frame")
  expect_error(predict(fit, data.frame(object1 = "unknown", object2 = "a")), "Prediction IDs")
  expect_error(pairwiseLLM::bootstrap_bt_model(fit, mode = "fixed", n_rep = 2, seed = 305,
    estimator = "alpha", estimator_args = list(alpha = 0.5)), "outside the simple-BT")
})

test_that("identified zero-lapse population optima retain points without regular uncertainty", {
  for (n in c(4L, 8L, 12L)) for (beta in c(0, 0.3)) {
    case <- lapse_case(n, beta, 0, if (n == 4L) "complete" else "cycle_chords")
    fit <- lapse_fit_case(case)
    expect_equal(fit$theta$theta, case$theta, tolerance = 1e-5)
    expect_equal(fit$beta, beta, tolerance = 1e-5)
    expect_identical(fit$epsilon, 0)
    expect_identical(fit$provenance$convergence$status, "converged_boundary")
    expect_true(fit$provenance$convergence$converged)
    expect_identical(fit$diagnostics$boundary, "epsilon_zero")
    expect_identical(fit$diagnostics$optimization$selected_source, "boundary_zero")
    expect_true(fit$diagnostics$boundary_zero_checks$stationary)
    expect_true(fit$diagnostics$boundary_zero_checks$kkt_valid)
    expect_lte(fit$diagnostics$gradient_max, 1e-7)
    expect_lte(fit$diagnostics$step_max, 1e-7)
    expect_true(all(is.na(fit$theta$se)))
    expect_null(fit$vcov)
    expect_null(fit$parameter_vcov)
    expect_false(fit$provenance$se_finite)
    expect_false(fit$provenance$uncertainty$valid)
    expect_identical(fit$provenance$uncertainty$method, "none")
    expect_identical(fit$provenance$uncertainty$status, "nonregular_boundary")
    expect_false(fit$ssr$valid)
    expect_true(is.na(fit$reliability))
    expect_equal(fit$log_likelihood, -fit$objective)
  }
})

test_that("seeded boundary points match an independent binomial GLM and retain public behavior", {
  case <- lapse_case(beta = 0, epsilon = 0, seed = 30501)
  dat <- lapse_binary_data(case)
  fit <- pairwiseLLM::fit_bt_model(dat, engine = "lapse", verbose = FALSE)
  oracle <- glm.fit(case$kernel$X, with(case$kernel$counts, cbind(wins, losses)),
    family = binomial(), intercept = FALSE, control = glm.control(epsilon = 1e-12, maxit = 100))
  expect_true(oracle$converged)
  expect_identical(fit$epsilon, 0)
  expect_gt(fit$diagnostics$boundary_zero_checks$epsilon_score, 1e-7)
  expect_equal(fit$theta$theta, as.vector(case$kernel$transform %*% head(oracle$coefficients, -1L)),
               tolerance = 1e-7)
  expect_equal(fit$beta, unname(tail(oracle$coefficients, 1L)), tolerance = 1e-7)
  p <- plogis(case$kernel$X %*% oracle$coefficients)
  expect_equal(fit$objective, -sum(case$kernel$counts$wins * log(p) +
                                   case$kernel$counts$losses * log1p(-p)), tolerance = 1e-12)
  summary <- pairwiseLLM::summarize_bt_fit(fit, verbose = FALSE)
  expect_equal(nrow(summary), 4L)
  expect_true(all(is.na(summary$se)))
  predictions <- predict(fit)
  expect_equal(predictions, plogis(fit$theta$theta[match(dat$object1, fit$theta$ID)] -
    fit$theta$theta[match(dat$object2, fit$theta$ID)] + fit$beta), tolerance = 1e-14)
  changed <- dat[c(2, 1, 3)]
  names(changed) <- names(dat)
  changed$result <- 1 - dat$result
  reverse <- pairwiseLLM::fit_bt_model(changed, engine = "lapse", verbose = FALSE)
  expect_equal(reverse$theta$theta, fit$theta$theta, tolerance = 1e-7)
  expect_equal(reverse$beta, -fit$beta, tolerance = 1e-7)
  expect_identical(reverse$epsilon, 0)
  expect_equal(predict(reverse), 1 - predictions, tolerance = 1e-8)
  labels <- c(a = "zebra", b = "item with spaces", c = "beta", d = "alpha")
  changed <- dat[rev(seq_len(nrow(dat))), ]
  changed$object1 <- unname(labels[changed$object1])
  changed$object2 <- unname(labels[changed$object2])
  renamed <- pairwiseLLM::fit_bt_model(changed, engine = "lapse", verbose = FALSE)
  index <- match(unname(labels[fit$theta$ID]), renamed$theta$ID)
  expect_equal(renamed$theta$theta[index], fit$theta$theta, tolerance = 1e-7)
  expect_equal(renamed$beta, fit$beta, tolerance = 1e-7)
  expect_identical(renamed$epsilon, 0)
  expect_equal(predict(renamed), rev(predictions), tolerance = 1e-8)
  expect_error(pairwiseLLM::bootstrap_bt_model(fit, mode = "fixed", n_rep = 2, seed = 305,
    estimator = "alpha", estimator_args = list(alpha = 0.5)), "outside the simple-BT")
})

test_that("bounded optimization resolves near-zero regression cases with unchanged gates", {
  cases <- list(lapse_case(n = 12L, beta = 0, epsilon = 0.001, seed = 30501),
                lapse_case(n = 12L, beta = 0, epsilon = 0, graph = "cycle_chords", seed = 30501))
  for (case in cases) {
    fit <- lapse_fit_case(case)
    expect_gt(fit$epsilon, 0)
    expect_identical(fit$provenance$convergence$status, "converged")
    expect_identical(fit$provenance$uncertainty$status, "valid")
    expect_identical(fit$provenance$effective_settings$algorithm, "L-BFGS-B")
    expect_lte(fit$diagnostics$gradient_max, 1e-7)
    expect_lte(fit$diagnostics$step_max, 1e-7)
    expect_gte(fit$diagnostics$hessian_checks$rcond, 1e-12)
    expect_gte(fit$diagnostics$information_checks$rcond, 1e-12)
    expect_lte(max(abs(fit$theta$theta - case$theta)), 0.35)
    expect_lte(abs(fit$beta - case$beta), 0.15)
    expect_lte(abs(fit$epsilon - case$epsilon), 0.08)
    expect_true(all(is.finite(fit$theta$se)))
  }
})

test_that("a likelihood tie alone cannot turn a positive lapse optimum into a boundary fit", {
  case <- lapse_case(epsilon = 1e-7)
  fit <- lapse_fit_case(case)
  gap <- fit$diagnostics$optimization$boundary_zero$objective - fit$objective
  expect_lte(gap, fit$diagnostics$boundary_objective_tolerance)
  expect_lt(fit$diagnostics$boundary_zero_checks$epsilon_score, -1e-7)
  expect_false(fit$diagnostics$boundary_zero_checks$kkt_valid)
  expect_gt(fit$epsilon, 0)
  expect_lte(abs(fit$epsilon - case$epsilon), 1e-10)
  expect_identical(fit$provenance$convergence$status, "converged")
  expect_true(fit$provenance$uncertainty$valid)
  expect_lte(fit$diagnostics$gradient_max, 1e-7)
})
