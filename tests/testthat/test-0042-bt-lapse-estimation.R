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
