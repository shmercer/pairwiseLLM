test_that("D020 production contrasts meet the original global bound and independent references", {
  cases <- read.csv(test_path("fixtures", "gaussian-prediction-294", "d020-scalars.csv"))
  expect_equal(nrow(cases), 20L)
  # Independent Golub-Welsch standard-Normal quadrature, never the adaptive integrator.
  gh <- function(n) {
    J <- matrix(0, n, n)
    J[cbind(seq_len(n - 1L), 2:n)] <- sqrt(seq_len(n - 1L))
    eig <- eigen(J + t(J), symmetric = TRUE)
    list(nodes = eig$values, weights = eig$vectors[1L, ]^2)
  }
  gh256 <- gh(256L)
  gh512 <- gh(512L)
  reference <- function(mu, sd, rule) sum(rule$weights * plogis(mu + sd * rule$nodes))
  original <- stats::integrate
  calls <- list()
  local_mocked_bindings(integrate = function(...) {
    args <- list(...)
    ans <- original(...)
    calls[[length(calls) + 1L]] <<- list(args = args, result = ans)
    ans
  }, .package = "stats")
  for (i in seq_len(nrow(cases))) {
    x <- cases[i, ]
    control <- pairwiseLLM:::.link_e2_controls(list(prediction_rel_tol = x$prediction_rel_tol,
      prediction_abs_tol = x$prediction_abs_tol, subdivisions = x$subdivisions))
    calls <- list()
    value <- pairwiseLLM:::.link_e2_integrate(x$mu, x$sd, control)
    # All D020 contrasts have three segments; inspect the accepted attempt.
    accepted <- tail(calls, 3L)
    error <- sum(vapply(accepted, function(call) call$result$abs.error, numeric(1)))
    raw_value <- sum(vapply(accepted, function(call) call$result$value, numeric(1)))
    expect_lte(error, max(control$prediction_abs_tol, control$prediction_rel_tol * abs(raw_value)))
    expect_identical(value, min(1, raw_value))
    expect_lte(abs(value - x$gh512_value), 1e-9)
    g256 <- reference(x$mu, x$sd, gh256)
    g512 <- reference(x$mu, x$sd, gh512)
    expect_lte(abs(g256 - g512), 1e-9)
    expect_lte(abs(value - g512), 1e-9)
    for (call in accepted) {
      expect_identical(call$args$subdivisions, control$subdivisions)
      expect_lte(call$args$rel.tol, control$prediction_rel_tol / 12)
      expect_lte(call$args$abs.tol, control$prediction_abs_tol / 12)
    }
    expect_identical(value, pairwiseLLM:::.link_gaussian_integrate(
      x$mu, x$sd, control, pairwiseLLM:::.link_e3_fail))
  }
})

test_that("refinement tries the frozen ladder and accepts only the original global bound", {
  control <- pairwiseLLM:::.link_e2_controls(list())
  # Both split counts, both branches of the global bound, every possible stopping step.
  for (m in 2:3) for (value in c(0, .5)) for (step in 1:4) {
    trace <- new.env(parent = emptyenv())
    trace$calls <- 0L
    checked <- testthat::with_mocked_bindings({
      pairwiseLLM:::.link_e2_integrate(if (m == 2) 0 else 1, 1, control)
    }, integrate = function(f, lower, upper, rel.tol, abs.tol, subdivisions, stop.on.error) {
      trace$calls <- trace$calls + 1L
      attempt <- ceiling(trace$calls / m)
      divisor <- c(4, 16, 64, 256)[attempt]
      expect_equal(rel.tol, control$prediction_rel_tol / (divisor * m))
      expect_equal(abs.tol, control$prediction_abs_tol / (divisor * m))
      expect_identical(subdivisions, control$subdivisions)
      expect_false(stop.on.error)
      bound <- max(control$prediction_abs_tol, control$prediction_rel_tol * value)
      list(value = value / m, abs.error = bound / m * if (attempt < step) 2 else .5,
        message = "OK")
    }, .package = "stats")
    expect_equal(checked, value)
    expect_equal(trace$calls, m * step)
  }
})

test_that("unacceptable numerical results remain explicit bounded failures", {
  control <- pairwiseLLM:::.link_e2_controls(list())
  bad <- list(
    list(value = .25, abs.error = 1e-8, message = "OK"),
    list(value = -.1, abs.error = 0, message = "OK"),
    list(value = .6, abs.error = 0, message = "OK"),
    list(value = Inf, abs.error = 0, message = "OK"),
    list(value = .25, abs.error = Inf, message = "OK"),
    list(value = .25, abs.error = 0, message = "roundoff error"),
    NULL)
  for (answer in bad) {
    calls <- 0L
    with_mocked_bindings({
      for (estimator in c("e2", "e3")) {
        fail <- getFromNamespace(paste0(".link_", estimator, "_fail"), "pairwiseLLM")
        expect_error(pairwiseLLM:::.link_gaussian_integrate(0, 1, control, fail),
          class = paste0("pairwiseLLM_", estimator, "_numerical_error"))
      }
    }, integrate = function(...) {
      calls <<- calls + 1L
      if (is.null(answer)) stop("forced integration failure")
      answer
    }, .package = "stats")
    expect_lte(calls, 16L)
    expect_gte(calls, 8L)
  }
})

test_that("a failed segment can refine, and exact global-bound equality is accepted", {
  control <- pairwiseLLM:::.link_e2_controls(list())
  calls <- 0L
  local_mocked_bindings(integrate = function(...) {
    calls <<- calls + 1L
    if (calls == 1L) stop("first attempt fails")
    list(value = .25, abs.error = control$prediction_rel_tol * .5 / 2, message = "OK")
  }, .package = "stats")
  expect_equal(pairwiseLLM:::.link_e2_integrate(0, 1, control), .5)
  expect_equal(calls, 3L)
})

test_that("old scalar predictions and serialized fits remain compatible without mutation or refitting", {
  baseline <- readRDS(test_path("fixtures", "gaussian-prediction-294", "baseline-03cc16c.rds"))
  expect_identical(baseline$commit, "03cc16c5953d9b1a74345c97324ebd6578248040")
  control <- pairwiseLLM:::.link_e2_controls(list())
  x <- baseline$scalars
  value <- mapply(function(mu, sd) pairwiseLLM:::.link_e2_integrate(mu, sd, control), x$mu, x$sd)
  expect_lte(max(abs(value - x$value)), 1e-9)
  withr::local_seed(294)
  rng <- .Random.seed
  local_mocked_bindings(fit_link = function(...) stop("prediction must not refit"),
    .link_gaussian_optimize = function(...) stop("prediction must not optimize"),
    .package = "pairwiseLLM")
  for (estimator in names(baseline$saved)) {
    x <- baseline$saved[[estimator]]
    before <- serialize(x$fit, NULL, version = 3L)
    hashes <- x$fit$provenance$hashes
    p <- predict_link(x$fit, x$pairs)
    expect_identical(p, predict_link(x$fit, x$pairs))
    expect_identical(rev(p), predict_link(x$fit, x$pairs[rev(seq_len(nrow(x$pairs))), ]))
    expect_lte(max(abs(p - x$probabilities)), if (estimator == "E1") 1e-12 else 1e-9)
    expect_identical(serialize(x$fit, NULL, version = 3L), before)
    expect_identical(x$fit$provenance$hashes, hashes)
  }
  expect_identical(.Random.seed, rng)
})
