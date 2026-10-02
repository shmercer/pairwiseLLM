# Deterministic comparison counts, with every pair observed in both directions.
ssr_fixture <- function() {
  data.frame(
    object1 = rep(c("1", "1", "2", "2", "3", "3"), each = 10),
    object2 = rep(c("2", "3", "1", "3", "1", "2"), each = 10),
    result = unlist(lapply(c(7, 8, 3, 6, 2, 4), function(w) c(rep(1, w), rep(0, 10 - w))))
  )
}

ssr_fit <- function(dat = ssr_fixture(), ...) {
  pairwiseLLM::fit_bt_model(dat, engine = "sirt", verbose = FALSE,
                          ignore.ties = TRUE, fix.eta = 0, maxiter = 500, conv = 1e-10, ...)
}

test_that("SSR exposes the hand-calculated sample-variance decomposition", {
  x <- pairwiseLLM::scale_separation_reliability(c(-1, 0, 1), c(0.2, 0.3, 0.4))
  expect_equal(x$observed_variance, 1)
  expect_equal(x$mean_squared_se, 29 / 300)
  expect_equal(x$true_score_variance, 271 / 300)
  expect_equal(x$ssr, 271 / 300)
  expect_identical(x$n_items, 3L)
  expect_identical(x$n_finite, 3L)
  expect_true(x$valid)
  expect_identical(x$status, "ok")
  zero <- pairwiseLLM::scale_separation_reliability(1:3, c(0, 0, 0))
  expect_equal(zero$ssr, 1)
  negative <- pairwiseLLM::scale_separation_reliability(1:3, c(2, 2, 2))
  expect_equal(negative$ssr, -3)
  expect_equal(negative$true_score_variance, -3)
  expect_true(negative$valid)
  expect_identical(negative$status, "negative_true_score_variance")
})

test_that("SSR has translation, order, and consistent scale invariance", {
  theta <- c(-2, -0.5, 1, 3)
  se <- c(0.1, 0.2, 0.4, 0.3)
  calc <- pairwiseLLM::scale_separation_reliability
  original <- calc(theta, se)
  expect_equal(calc(theta + 100, se), original)
  order <- c(4, 2, 1, 3)
  expect_equal(calc(theta[order], se[order]), original)
  expect_equal(calc(setNames(theta, letters[1:4]), setNames(se, LETTERS[1:4])), original)
  for (scale in c(-3, 0.1, 10)) {
    scaled <- calc(theta * scale, se * abs(scale))
    expect_equal(scaled$ssr, original$ssr)
    expect_equal(scaled$observed_variance, original$observed_variance * scale^2)
    expect_equal(scaled$mean_squared_se, original$mean_squared_se * scale^2)
  }
})

test_that("SSR rejects invalid inputs without dropping or coercing items", {
  calc <- pairwiseLLM::scale_separation_reliability
  for (bad in list(NULL, "1", TRUE, 1 + 1i, list(1, 2), matrix(1:2))) {
    expect_error(calc(bad, c(1, 1)), "real numeric vectors")
    expect_error(calc(c(1, 2), bad), "real numeric vectors")
  }
  expect_error(calc(1:3, c(1, 1)), "equal length")
  expect_error(calc(numeric(), numeric()), "at least two")
  expect_error(calc(1, 0), "at least two")
  for (bad in c(NA_real_, NaN, Inf, -Inf)) {
    expect_error(calc(c(1, bad), c(1, 1)), "must be finite")
    expect_error(calc(c(1, 2), c(1, bad)), "must be finite")
  }
  expect_error(calc(1:2, c(0, -1)), "nonnegative")
  expect_error(calc(c(1, 1), c(0, 0)), "positive finite score variance")
  expect_error(calc(c(-1e308, 1e308), c(1, 1)), "positive finite score variance")
  expect_error(calc(1:2, c(1e308, 1)), "finite mean squared SE")
  expect_error(calc(c(0, 1e-150), c(1e100, 1e100)), "nonfinite variance components")
})

test_that("comparison validation precedes either engine and rejects disconnected graphs", {
  # Even namespace availability must not be consulted for invalid input.
  testthat::local_mocked_bindings(
    .require_ns = function(...) stop("engine dispatch reached"), .package = "pairwiseLLM"
  )
  disconnected <- data.frame(a = c("1", "3"), b = c("2", "4"), result = c(1, 0))
  for (engine in c("auto", "sirt", "BradleyTerry2")) {
    expect_error(pairwiseLLM::fit_bt_model(disconnected, engine),
                 "disconnected: global BT scores and SSR are not identified")
  }
  dat <- ssr_fixture()
  expect_error(pairwiseLLM::fit_bt_model(dat[FALSE, ]), "contain comparisons")
  for (id in c(NA_character_, "")) {
    bad <- dat
    bad[1, 1] <- id
    expect_error(pairwiseLLM::fit_bt_model(bad), "Object IDs")
  }
  bad <- dat
  bad$object1 <- as.list(bad$object1)
  expect_error(pairwiseLLM::fit_bt_model(bad), "Object IDs")
  bad <- dat
  bad[1, 2] <- bad[1, 1]
  expect_error(pairwiseLLM::fit_bt_model(bad), "Self-comparisons")
  for (value in c(NA, NaN, Inf, -1, 0.2, 2)) {
    bad <- dat
    bad[1, 3] <- value
    expect_error(pairwiseLLM::fit_bt_model(bad), "Comparison results")
  }
  bad <- dat
  bad$result <- as.character(bad$result)
  expect_error(pairwiseLLM::fit_bt_model(bad), "Comparison results")
})

test_that("sirt SSR matches an independent direct-engine oracle tightly", {
  skip_if_not_installed("sirt")
  for (epsilon in c(0, 0.3, 0.5)) {
    invisible(capture.output(raw <- sirt::btm(ssr_fixture(), eps = epsilon,
      ignore.ties = TRUE, fix.eta = 0, maxiter = 500, conv = 1e-10)))
    fit <- ssr_fit(sirt_eps = epsilon)
    independent <- 1 - mean(raw$effects$se.theta^2) / stats::var(raw$effects$theta)
    expect_equal(fit$ssr$ssr, independent, tolerance = 1e-12)
    expect_equal(fit$ssr$ssr, raw$mle.rel, tolerance = 1e-12)
    expect_identical(fit$reliability, fit$fit$mle.rel)
    expect_true(fit$ssr$agrees)
    expect_lte(fit$ssr$absolute_difference, fit$ssr$tolerance)
    expect_identical(fit$provenance$effective_settings$eps, epsilon)
    expect_identical(fit$provenance$adjustment, list(method = "epsilon", eps = epsilon))
    expect_equal(fit$theta$theta, raw$effects$theta, tolerance = 1e-12)
    expect_equal(fit$theta$se, raw$effects$se.theta, tolerance = 1e-12)
    expect_identical(fit$provenance$convergence$status, "stopping_criterion_met")
    expect_true(fit$provenance$convergence$converged)
  }
})

test_that("existing sirt defaults and named, partial, and positional eps calls are preserved", {
  skip_if_not_installed("sirt")
  dat <- ssr_fixture()
  direct <- NULL
  invisible(capture.output(direct <- sirt::btm(dat)))
  default <- pairwiseLLM::fit_bt_model(dat, "sirt", FALSE)
  expect_equal(default$theta$theta, direct$effects$theta, tolerance = 1e-12)
  expect_equal(default$theta$se, direct$effects$se.theta, tolerance = 1e-12)
  expect_equal(default$reliability, direct$mle.rel, tolerance = 1e-12)
  expect_equal(default$provenance$effective_settings$eps, formals(sirt::btm)$eps)
  expect_equal(default$provenance$effective_settings$maxiter, 100)
  expect_identical(default$provenance$convergence$status, "iteration_limit_reached")
  expect_true(is.na(default$provenance$convergence$converged))
  legacy <- ssr_fit(eps = 0.2)
  explicit <- ssr_fit(sirt_eps = 0.2)
  expect_equal(legacy$theta, explicit$theta)
  expect_equal(legacy$ssr, explicit$ssr)
  partial <- ssr_fit(ep = 0.2)
  expect_equal(partial$theta, explicit$theta)
  positional <- pairwiseLLM::fit_bt_model(dat, "sirt", FALSE,
                                        NULL, TRUE, 0, NULL, NULL, 500, 1e-10, 0.2)
  expect_equal(positional$theta, explicit$theta)
  expect_equal(positional$provenance$effective_settings$eps, 0.2)
  expect_identical(explicit$provenance$engine_version, as.character(packageVersion("sirt")))
  expect_identical(explicit$provenance$package_version, as.character(getNamespaceVersion("pairwiseLLM")))
  expect_identical(explicit$provenance$identification$convention, "sum_to_zero")
  expect_true(explicit$provenance$theta_finite)
  expect_true(explicit$provenance$se_finite)
  expect_true(explicit$provenance$reliability_valid)
  expect_null(explicit$provenance$fallback_reason)
  old <- explicit[c("engine", "fit", "theta", "reliability")]
  expect_equal(pairwiseLLM::summarize_bt_fit(old, verbose = FALSE),
               pairwiseLLM::summarize_bt_fit(explicit, verbose = FALSE))
  path <- file.path(withr::local_tempdir(), "fit.rds")
  saveRDS(explicit, path)
  expect_identical(readRDS(path)$provenance, explicit$provenance)
})

test_that("sirt estimates and SSR are invariant to labels, row order, and side reversal", {
  skip_if_not_installed("sirt")
  original <- ssr_fit()
  dat <- ssr_fixture()
  reordered <- ssr_fit(dat[rev(seq_len(nrow(dat))), ])
  reversed <- dat[c(2, 1, 3)]
  reversed[[3]] <- 1 - reversed[[3]]
  swapped <- ssr_fit(reversed)
  labels <- c("zebra", "alpha", "middle")
  dat$object1 <- labels[as.integer(dat$object1)]
  dat$object2 <- labels[as.integer(dat$object2)]
  renamed <- ssr_fit(dat)
  for (fit in list(reordered, swapped, renamed)) {
    ids <- as.character(fit$theta$ID)
    if (identical(fit, renamed)) ids <- as.character(match(ids, labels))
    index <- match(as.character(original$theta$ID), ids)
    expect_equal(fit$theta$theta[index], original$theta$theta, tolerance = 1e-10)
    expect_equal(fit$theta$se[index], original$theta$se, tolerance = 1e-10)
    expect_equal(fit$ssr$ssr, original$ssr$ssr, tolerance = 1e-12)
  }
})

test_that("epsilon errors and result errors never silently trigger auto fallback", {
  skip_if_not_installed("sirt")
  dat <- ssr_fixture()
  for (bad in list(-1, Inf, NA_real_, numeric(), c(0.1, 0.2), "0.3", 1i)) {
    expect_error(pairwiseLLM::fit_bt_model(dat, sirt_eps = bad), "epsilon")
    expect_error(pairwiseLLM::fit_bt_model(dat, eps = bad), "epsilon")
  }
  expect_error(ssr_fit(sirt_eps = 0.3, eps = 0.3), "only one")
  expect_error(ssr_fit(sirt_eps = 0.3, eps = NULL), "only one")
  expect_error(ssr_fit(sirt_eps = 0.3, ep = 0.3), "only one")
  expect_error(pairwiseLLM::fit_bt_model(dat, "BradleyTerry2", sirt_eps = 0.3), "requires engine")
  fixture <- list(effects = data.frame(individual = c("1", "2", "3"),
                                      theta = c(-1, 0, 1), se.theta = rep(0.5, 3)),
                  mle.rel = 0.75, eps = 0.3, iter = 5L)
  run_bad <- function(bad, message) {
    testthat::local_mocked_bindings(.sirt_btm = function(...) bad, .package = "pairwiseLLM")
    expect_error(pairwiseLLM::fit_bt_model(dat, "auto", FALSE), message)
    expect_error(pairwiseLLM::fit_bt_model(dat, "sirt", FALSE), message)
  }
  for (value in list(NULL, NA_real_, Inf, c(0.75, 0.75), "0.75", 0.6)) {
    bad <- fixture
    bad$mle.rel <- value
    run_bad(bad, "reliability|disagrees")
  }
  bad <- fixture
  bad$effects$se.theta[1] <- Inf
  run_bad(bad, "must be finite")
  bad <- fixture
  bad$effects$theta <- rep(1, 3)
  run_bad(bad, "positive finite score variance")
  bad <- fixture
  bad$eps <- NULL
  run_bad(bad, "epsilon")
  bad$eps <- 0.5
  run_bad(bad, "does not match")
})

test_that("extreme-score and fixed-theta sirt fits cannot hide undefined SEs", {
  skip_if_not_installed("sirt")
  dat <- ssr_fixture()
  dat$result[dat$object1 == "1" | dat$object2 == "3"] <- 1
  dat$result[dat$object2 == "1" | dat$object1 == "3"] <- 0
  regularized <- ssr_fit(dat, sirt_eps = 0.3)
  expect_true(all(is.finite(regularized$theta$se)))
  expect_true(regularized$ssr$valid)
  expect_error(ssr_fit(dat, sirt_eps = 0), "must be finite")
  expect_error(ssr_fit(fix.theta = c("1" = 0)), "must be finite")
  equal <- ssr_fixture()
  equal$result <- rep(c(0, 1), 30)
  expect_error(ssr_fit(equal), "positive finite score variance")
})

test_that("ties removed by sirt cannot be the only connection between components", {
  skip_if_not_installed("sirt")
  dat <- data.frame(a = c("1", "3", "2"), b = c("2", "4", "3"), result = c(1, 0, 0.5))
  testthat::local_mocked_bindings(.sirt_btm = function(...) stop("engine reached"), .package = "pairwiseLLM")
  expect_error(pairwiseLLM::fit_bt_model(dat, ignore.ties = TRUE), "graph is disconnected")
  expect_error(pairwiseLLM::fit_bt_model(dat, ignore.ties = 1), "graph is disconnected")
  expect_error(pairwiseLLM::fit_bt_model(dat, "sirt", ignore.ties = FALSE), "engine reached")
})

test_that("BradleyTerry2 retains its legacy result and exposes effective conventions", {
  skip_if_not_installed("BradleyTerry2")
  dat <- ssr_fixture()
  fit <- pairwiseLLM::fit_bt_model(dat, "BradleyTerry2", FALSE)
  expect_true(is.na(fit$reliability))
  expect_true(is.na(fit$ssr$ssr))
  expect_false(fit$ssr$valid)
  expect_identical(fit$ssr$status, "unavailable_se_convention")
  expect_identical(fit$provenance$effective_settings$br, FALSE)
  expect_identical(fit$provenance$effective_settings$link, "logit")
  expect_identical(fit$provenance$identification$player_levels, c("1", "2", "3"))
  expect_identical(fit$provenance$convergence$status, "converged")
  expect_true(fit$provenance$convergence$converged)
  ref <- pairwiseLLM::fit_bt_model(dat, "BradleyTerry2", FALSE, refcat = "2")
  expect_identical(ref$provenance$identification$refcat, "2")
  expect_equal(unname(ref$theta$theta[ref$theta$ID == "2"]), 0)
  expect_equal(ref$theta$theta - mean(ref$theta$theta),
               fit$theta$theta - mean(fit$theta$theta), tolerance = 1e-10)
  testthat::local_mocked_bindings(
    .require_ns = function(pkg, ...) pkg == "BradleyTerry2", .package = "pairwiseLLM"
  )
  fallback <- pairwiseLLM::fit_bt_model(dat, "auto", FALSE, sirt_eps = 0.4)
  expect_identical(fallback$engine, "BradleyTerry2")
  expect_identical(fallback$provenance$requested_engine, "auto")
  expect_identical(fallback$provenance$requested_sirt_eps, 0.4)
  expect_match(fallback$provenance$fallback_reason, "sirt.*must be installed")
  expect_identical(fallback$theta, fit$theta)
  expect_error(pairwiseLLM::fit_bt_model(dat, "sirt", FALSE), "sirt.*must be installed")
})

test_that("BradleyTerry2 cannot fit graphs disconnected by subsets or zero weights", {
  skip_if_not_installed("BradleyTerry2")
  dat <- ssr_fixture()
  stub <- BradleyTerry2::BTm
  body(stub) <- quote(stop("engine reached"))
  testthat::local_mocked_bindings(BTm = stub, .package = "BradleyTerry2")
  # Keep the real formals for provenance argument matching.
  # These preflight checks happen before invoking the substituted engine body.
  expect_error(pairwiseLLM::fit_bt_model(dat, "BradleyTerry2", FALSE, subset = c(1, 3)), "disconnected")
  expect_error(pairwiseLLM::fit_bt_model(dat, "BradleyTerry2", FALSE, weights = rep(0, 6)), "disconnected")
  expect_error(pairwiseLLM::fit_bt_model(dat, "BradleyTerry2", FALSE, subset = NA), "missing comparisons")
  dat[1, 3] <- 0.5
  expect_error(pairwiseLLM::fit_bt_model(dat, "BradleyTerry2", FALSE), "ties are not supported")
})

test_that("explicit execution failures never switch engines and auto records the failure", {
  skip_if_not_installed("sirt")
  skip_if_not_installed("BradleyTerry2")
  testthat::local_mocked_bindings(
    .sirt_btm = function(...) stop("deliberate sirt execution failure"), .package = "pairwiseLLM"
  )
  expect_error(pairwiseLLM::fit_bt_model(ssr_fixture(), "sirt", FALSE), "deliberate sirt")
  fit <- pairwiseLLM::fit_bt_model(ssr_fixture(), "auto", FALSE)
  expect_identical(fit$engine, "BradleyTerry2")
  expect_match(fit$provenance$fallback_reason, "deliberate sirt execution failure")
})

test_that("engine-specific legacy dots can still select the auto fallback", {
  skip_if_not_installed("sirt")
  skip_if_not_installed("BradleyTerry2")
  fit <- pairwiseLLM::fit_bt_model(ssr_fixture(), "auto", FALSE, refcat = "2")
  expect_identical(fit$engine, "BradleyTerry2")
  expect_identical(fit$provenance$identification$refcat, "2")
  expect_match(fit$provenance$fallback_reason, "unused argument")
})

test_that("provenance separates requested delta from audited sirt behavior", {
  skip_if_not_installed("sirt")
  fit <- ssr_fit(fix.delta = -20)
  expect_identical(fit$provenance$effective_settings$fix.delta_requested, -20)
  expect_identical(fit$provenance$effective_settings$returned_parameters, fit$fit$pars)
  if (packageVersion("sirt") == "4.2.133") {
    expect_identical(fit$provenance$effective_settings$fix.delta_application, "not_applied_by_engine")
    expect_equal(fit$fit$pars$est[fit$fit$pars$par == "delta"], -99)
  } else {
    expect_identical(fit$provenance$effective_settings$fix.delta_application, "not_verified_for_engine_version")
  }
})

test_that("sirt supports connected tie data without changing tie defaults", {
  skip_if_not_installed("sirt")
  dat <- ssr_fixture()
  dat$result[c(1, 11, 21, 31, 41, 51)] <- 0.5
  fit <- pairwiseLLM::fit_bt_model(dat, "sirt", FALSE)
  invisible(capture.output(direct <- sirt::btm(dat)))
  expect_equal(fit$ssr$ssr, direct$mle.rel, tolerance = 1e-12)
  expect_identical(fit$provenance$effective_settings$ignore.ties, FALSE)
  expect_equal(fit$provenance$effective_settings$wgt.ties, 0.5)
})

test_that("BradleyTerry2 nonconvergence remains visible even in quiet mode", {
  skip_if_not_installed("BradleyTerry2")
  fit <- pairwiseLLM::fit_bt_model(ssr_fixture(), "BradleyTerry2", FALSE,
                                  control = stats::glm.control(maxit = 1))
  expect_identical(fit$provenance$convergence$status, "not_converged")
  expect_false(fit$provenance$convergence$converged)
  expect_identical(fit$provenance$effective_settings$control$maxit, 1)
})
