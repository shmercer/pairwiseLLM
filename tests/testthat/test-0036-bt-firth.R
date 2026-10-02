# Fixed counts represent nonadaptive schedules; no private data or live engines.
firth_counts <- function(wins = c(7, 8, 6), totals = c(10, 10, 10)) {
  pairs <- data.frame(object1 = c("a", "a", "b"), object2 = c("b", "c", "c"))
  rows <- rep(seq_len(nrow(pairs)), totals)
  data.frame(pairs[rows, ], result = unlist(Map(function(w, n) c(rep(1, w), rep(0, n - w)), wins, totals)),
             row.names = NULL)
}

firth_fit <- function(dat = firth_counts(), ...) {
  pairwiseLLM::fit_bt_model(dat, engine = "brglm2", verbose = FALSE, ...)
}

test_that("Firth matches the two-item analytic solution, including separation", {
  skip_if_not_installed("brglm2")
  for (wins in c(0, 3, 5, 10)) {
    dat <- data.frame(a = rep("a", 10), b = rep("b", 10), y = c(rep(1, wins), rep(0, 10 - wins)))
    fit <- firth_fit(dat)
    contrast <- log((wins + 0.5) / (10 - wins + 0.5))
    probability <- plogis(contrast)
    variance <- 1 / (10 * probability * (1 - probability))
    expect_equal(fit$theta$theta, c(contrast, -contrast) / 2, tolerance = 1e-8)
    expect_equal(fit$vcov, matrix(c(1, -1, -1, 1), 2, dimnames = list(c("a", "b"), c("a", "b"))) *
                   variance / 4, tolerance = 1e-8)
    expect_equal(predict(fit), rep(probability, 10), tolerance = 1e-8)
    expect_true(fit$provenance$convergence$converged)
  }
})

test_that("Firth approaches ordinary MLE in high-information data", {
  skip_if_not_installed("brglm2")
  dat <- firth_counts(c(7000, 8000, 6000), rep(10000, 3))
  fit <- firth_fit(dat)
  ids <- c("a", "b", "c")
  incidence <- diag(3)[match(dat$object1, ids), ] - diag(3)[match(dat$object2, ids), ]
  design <- incidence[, 1:2]
  mle <- glm(dat$result ~ design - 1, family = binomial())
  mle_theta <- c(coef(mle), 0)
  mle_theta <- mle_theta - mean(mle_theta)
  expect_equal(fit$theta$theta, unname(mle_theta), tolerance = 2e-4)
  expect_lt(max(abs(fit$theta$theta - mle_theta)), 2e-4)
})

test_that("complete and quasi separation, undefeated and winless objects remain finite", {
  skip_if_not_installed("brglm2")
  for (wins in list(c(10, 10, 10), c(10, 10, 5), c(5, 10, 10))) {
    fit <- firth_fit(firth_counts(wins))
    expect_true(all(is.finite(fit$theta$theta)))
    expect_true(all(is.finite(fit$theta$se)))
    expect_true(all(fit$theta$se > 0))
    expect_equal(sum(fit$theta$theta), 0, tolerance = 1e-12)
    expect_true(fit$provenance$convergence$converged)
    expect_gt(fit$theta$theta[1], fit$theta$theta[3])
  }
})

test_that("a sparse connected random schedule has valid estimates and covariance", {
  skip_if_not_installed("brglm2")
  withr::local_seed(307)
  # Schedule is fixed before outcomes; the path guarantees connectivity.
  pairs <- data.frame(a = c(1:7, 1, 3, 4), b = c(2:8, 5, 8, 7))
  dat <- pairs[rep(seq_len(nrow(pairs)), each = 3), ]
  dat$y <- rbinom(nrow(dat), 1, 0.5)
  fit <- firth_fit(dat)
  expect_equal(nrow(fit$theta), 8)
  expect_true(all(is.finite(fit$theta$theta)))
  expect_true(all(fit$theta$se > 0))
  expect_equal(rowSums(fit$vcov), setNames(rep(0, 8), fit$theta$ID), tolerance = 1e-10)
})

test_that("Firth fits and covariance are invariant to relabeling, order and pair orientation", {
  skip_if_not_installed("brglm2")
  dat <- firth_counts()
  original <- firth_fit(dat)
  reversed <- dat[c(2, 1, 3)]
  reversed[[3]] <- 1 - reversed[[3]]
  partial <- dat
  flip <- seq(1, nrow(dat), by = 2)
  partial[flip, 1:2] <- dat[flip, 2:1]
  partial$result[flip] <- 1 - dat$result[flip]
  labels <- c(a = "zebra", b = "item with spaces", c = "alpha")
  renamed <- dat
  renamed$object1 <- unname(labels[dat$object1])
  renamed$object2 <- unname(labels[dat$object2])
  for (changed in list(dat[rev(seq_len(nrow(dat))), ], reversed, partial, renamed)) {
    fit <- firth_fit(changed)
    mapped <- if (identical(changed, renamed)) unname(labels[original$theta$ID]) else original$theta$ID
    index <- match(mapped, fit$theta$ID)
    expect_equal(fit$theta$theta[index], original$theta$theta, tolerance = 1e-8)
    expect_equal(fit$theta$se[index], original$theta$se, tolerance = 1e-8)
    expect_equal(unname(fit$vcov[index, index]), unname(original$vcov), tolerance = 1e-8)
    expect_equal(fit$reliability, original$reliability, tolerance = 1e-8)
  }
  expect_identical(firth_fit(dat)$theta, original$theta)
  expect_identical(firth_fit(dat)$vcov, original$vcov)
  expect_equal(predict(firth_fit(reversed)), 1 - predict(original), tolerance = 1e-10)
})

test_that("centered covariance agrees with independent expected information", {
  skip_if_not_installed("brglm2")
  fit <- firth_fit()
  dat <- firth_counts()
  ids <- fit$theta$ID
  incidence <- diag(3)[match(dat$object1, ids), ] - diag(3)[match(dat$object2, ids), ]
  # Independent orthonormal sum-zero basis, not the implementation's reference basis.
  basis <- qr.Q(qr(contr.helmert(3)))
  design <- incidence %*% basis
  p <- predict(fit)
  expected <- basis %*% solve(crossprod(design, design * (p * (1 - p)))) %*% t(basis)
  expect_equal(unname(fit$vcov), expected, tolerance = 1e-9)
  expect_equal(fit$theta$se^2, unname(diag(fit$vcov)), tolerance = 1e-12)
  expect_true(isSymmetric(fit$vcov))
  expect_equal(qr(fit$vcov)$rank, 2L)
  expect_equal(rownames(fit$vcov), ids)
  expect_equal(colnames(fit$vcov), ids)
  expect_equal(fit$ssr, pairwiseLLM::scale_separation_reliability(fit$theta$theta, fit$theta$se))
  expect_identical(fit$reliability, fit$ssr$ssr)
})

test_that("SSR preserves valid zero-variance fits and retains negative coefficients", {
  skip_if_not_installed("brglm2")
  equal <- firth_fit(firth_counts(c(5, 5, 5)))
  expect_identical(equal$theta$theta, c(0, 0, 0))
  expect_true(all(is.finite(equal$vcov)))
  expect_true(all(equal$theta$se > 0))
  expect_true(is.na(equal$reliability))
  expect_false(equal$ssr$valid)
  expect_identical(equal$ssr$status, "zero_score_variance")
  expect_identical(equal$provenance$reliability_status, "zero_score_variance")
  expect_false(equal$provenance$reliability_valid)
  expect_equal(predict(equal), rep(0.5, 30))
  expect_error(pairwiseLLM::scale_separation_reliability(equal$theta$theta, equal$theta$se),
               "positive finite score variance")
  negative <- firth_fit(firth_counts(c(6, 5, 5)))
  expect_lt(negative$reliability, 0)
  expect_identical(negative$ssr$status, "negative_true_score_variance")
})

test_that("Firth provenance, summaries and serialization preserve the fit contract", {
  skip_if_not_installed("brglm2")
  fit <- firth_fit()
  p <- fit$provenance
  expect_identical(fit$engine, "brglm2")
  expect_s3_class(fit, "pairwiseLLM_bt_firth")
  expect_s3_class(fit$fit, "brglmFit")
  expect_identical(p$engine_version, as.character(packageVersion("brglm2")))
  expect_identical(p$package_version, as.character(getNamespaceVersion("pairwiseLLM")))
  expect_identical(p$adjustment$method, "firth")
  expect_identical(p$adjustment$type, "AS_mean")
  expect_equal(p$adjustment$log_determinant_multiplier, 0.5)
  expect_identical(p$identification$convention, "sum_to_zero")
  expect_identical(p$uncertainty$method, "inverse_expected_information")
  expect_true(p$uncertainty$valid)
  expect_true(p$theta_finite)
  expect_true(p$se_finite)
  expect_null(p$fallback_reason)
  expect_null(p$requested_sirt_eps)
  expect_identical(p$effective_settings$control$type, "AS_mean")
  expect_identical(p$effective_settings$start, c(0, 0))
  expect_no_warning(summary <- pairwiseLLM::summarize_bt_fit(fit))
  expect_identical(names(summary), c("ID", "theta", "se", "rank", "engine", "reliability"))
  expect_identical(summary$theta, fit$theta$theta)
  expect_equal(summary$reliability, rep(fit$reliability, 3))
  path <- file.path(withr::local_tempdir(), "firth.rds")
  saveRDS(fit, path)
  restored <- readRDS(path)
  expect_identical(restored$provenance, fit$provenance)
  expect_identical(restored$vcov, fit$vcov)
  expect_identical(predict(restored), predict(fit))
})

test_that("predictions preserve pair order and validate labels", {
  skip_if_not_installed("brglm2")
  fit <- firth_fit()
  pairs <- data.frame(object1 = c("c", "a", "b", "c"), object2 = c("a", "a", "c", "a"))
  predictions <- predict(fit, pairs)
  expect_equal(predictions[2], 0.5)
  expect_equal(predictions[1], predictions[4])
  expect_equal(predictions, plogis(fit$theta$theta[c(3, 1, 2, 3)] - fit$theta$theta[c(1, 1, 3, 1)]))
  expect_identical(predict(fit, pairs[FALSE, ]), numeric())
  expect_equal(predict(fit, transform(pairs, object1 = factor(object1))), predictions)
  expect_error(predict(fit, type = "link"), "additional arguments")
  for (bad in list(1, list(), data.frame(a = "a", b = "b"))) {
    expect_error(predict(fit, bad), "data frame containing")
  }
  for (bad in list(NA_character_, "unknown", "", list("a"))) {
    broken <- pairs[1, ]
    broken$object1 <- bad
    expect_error(predict(fit, broken), "Prediction IDs")
  }
})

test_that("disconnected data and incompatible Firth inputs fail without engine fallback", {
  disconnected <- data.frame(a = c("a", "c"), b = c("b", "d"), y = c(1, 0))
  expect_error(firth_fit(disconnected), "graph is disconnected", class = "pairwiseLLM_bt_validation_error")
  tied <- firth_counts()
  tied$result[1] <- 0.5
  expect_error(firth_fit(tied), "binary outcomes")
  expect_error(firth_fit(sirt_eps = 0.3), "requires engine")
  testthat::local_mocked_bindings(.require_ns = function(...) FALSE, .package = "pairwiseLLM")
  expect_error(firth_fit(), "Package 'brglm2' must be installed")
})

test_that("only validated numerical controls can alter the Firth solver", {
  skip_if_not_installed("brglm2")
  for (bad in list(list(type = "ML"), list(a = 1), list(epsilon = 1e-6, epsilon = 1e-8),
                  list(1), 1, list(trace = NA), list(trace = 1))) {
    expect_error(firth_fit(control = bad), "Firth.*control")
  }
  for (name in c("epsilon", "maxit", "slowit", "max_step_factor")) {
    for (bad in list(0, -1, Inf, NA_real_, "1", numeric(), 1i, NULL, c(1, 2))) {
      expect_error(firth_fit(control = setNames(list(bad), name)), "positive finite")
    }
  }
  expect_error(firth_fit(control = list(maxit = 1.5)), "integer")
  expect_error(firth_fit(control = list(maxit = 1e20)), "integer")
  expect_error(firth_fit(firth_counts(), 1), "only a named")
  expect_error(firth_fit(eps = 0.3), "only a named")
  expect_error(firth_fit(control = list(), control = list()), "only a named")
  expect_error(firth_fit(control = list(), extra = TRUE), "only a named")
  fit <- firth_fit(control = list(epsilon = 1e-9, maxit = 300L, slowit = 0.9, max_step_factor = 15L, trace = TRUE))
  expect_false(fit$provenance$effective_settings$control$trace)
  expect_equal(fit$provenance$effective_settings$control$epsilon, 1e-9)
  expect_equal(fit$provenance$effective_settings$control$maxit, 300L)
  expect_identical(firth_fit(control = NULL)$theta, firth_fit()$theta)
  expect_output(pairwiseLLM::fit_bt_model(firth_counts(), "brglm2", control = list(trace = TRUE)), "iteration")
  expect_error(suppressWarnings(firth_fit(control = list(maxit = 1L))), "did not converge")
})

test_that("invalid numerical engine results are rejected", {
  skip_if_not_installed("brglm2")
  raw <- firth_fit()$fit
  for (change in list(list(converged = FALSE), list(coefficients = c(Inf, 0)),
                     list(coefficients = c(NA_real_, 0)), list(coefficients = 0), list(rank = 1L))) {
    broken <- utils::modifyList(raw, change)
    testthat::with_mocked_bindings(
      expect_error(firth_fit(), "did not converge|finite, full-rank", class = "pairwiseLLM_bt_validation_error"),
      .bt_firth_glm = function(...) broken, .package = "pairwiseLLM"
    )
  }
  covariance <- pairwiseLLM:::.bt_firth_covariance
  transform <- rbind(diag(2), c(-1, -1))
  for (bad in list(1, matrix(1, 1), matrix(Inf, 2, 2), matrix(1i, 2, 2),
                  matrix(c(1, 0, 2, 1), 2), matrix(NA_real_, 2, 2))) {
    expect_error(covariance(bad, transform, letters[1:3]), "finite and symmetric")
  }
  expect_error(covariance(diag(c(-1, 1)), transform, letters[1:3]), "not positive definite")
  expect_error(covariance(diag(2), transform * 0, letters[1:3]), "positive variances")
  expect_error(covariance(diag(2), transform * 1e200, letters[1:3]), "must be finite")
  expect_error(pairwiseLLM:::.bt_firth_ssr(c(0, Inf), c(1, 1)), "must be finite")
  expect_error(pairwiseLLM:::.bt_firth_ssr(c(0, 1), c(1, Inf)), "must be finite")
  expect_error(pairwiseLLM:::.bt_firth_ssr(c(-1e308, 1e308), c(1, 1)), "finite score variance")
  expect_error(pairwiseLLM:::.bt_firth_ssr(c(0, 1), c(1e308, 1)), "finite score variance")
})

test_that("adding Firth does not change default dispatch or existing fit classes", {
  testthat::local_mocked_bindings(
    .bt_fit_firth = function(...) stop("Firth dispatch must not be reached"),
    .require_ns = function(...) FALSE, .package = "pairwiseLLM"
  )
  expect_error(pairwiseLLM::fit_bt_model(firth_counts(), verbose = FALSE), "Both sirt and BradleyTerry2 failed")
  expect_error(pairwiseLLM::fit_bt_model(firth_counts(), "sirt", FALSE), "Package 'sirt'")
  expect_error(pairwiseLLM::fit_bt_model(firth_counts(), "BradleyTerry2", FALSE), "Package 'BradleyTerry2'")
})
