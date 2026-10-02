test_that("lapse inputs and controls cannot change the scientific model", {
  dat <- lapse_case()$skeleton
  for (bad in list(list(penalty = 1), list(alpha = 1), list(control = list(), control = list()), list(1))) {
    expect_error(do.call(pairwiseLLM::fit_bt_model, c(list(bt_data = dat, engine = "lapse"), bad)))
  }
  for (bad in list(1, list(nope = 1), list(maxit = 1, maxit = 2), list(NA))) {
    expect_error(pairwiseLLM:::.bt_lapse_control(list(control = bad), FALSE), "uniquely named")
  }
  for (name in c("maxit", "reltol", "gradient_tol", "step_tol", "min_rcond")) {
    for (bad in list(NULL, NA_real_, Inf, -1, 0, "bad", 1 + 1i)) {
      expect_error(pairwiseLLM:::.bt_lapse_control(list(control = setNames(list(bad), name)), FALSE),
                     "Invalid positive finite")
    }
  }
  for (bad in list(0.5, .Machine$integer.max + 1)) {
    expect_error(pairwiseLLM:::.bt_lapse_control(list(control = list(maxit = bad)), FALSE))
  }
  expect_error(pairwiseLLM:::.bt_lapse_control(list(control = list(min_rcond = 1)), FALSE))
  expect_error(pairwiseLLM:::.bt_lapse_control(list(control = list(trace = NA)), FALSE))
  expect_false(pairwiseLLM:::.bt_lapse_control(list(control = list(trace = TRUE)), FALSE)$trace)
  expect_true(pairwiseLLM:::.bt_lapse_control(list(control = list(trace = TRUE)), TRUE)$trace)
  dat$result[1] <- 0.5
  expect_error(pairwiseLLM::fit_bt_model(dat, engine = "lapse"), "binary outcomes")
  expect_error(pairwiseLLM::fit_bt_model(lapse_case()$skeleton, engine = "lapse", sirt_eps = 0.2), "sirt_eps")
})

test_that("nonidentified designs and epsilon-one likelihoods retain failures without SEs", {
  cases <- list(lapse_case(n = 2L), lapse_case(epsilon = 1),
                 lapse_case(n = 8L, beta = 0, graph = "tree"))
  for (case in cases) {
    failure <- lapse_error(lapse_fit_case(case))
    expect_s3_class(failure, "pairwiseLLM_bt_lapse_error")
    expect_s3_class(failure, "pairwiseLLM_bt_validation_error")
    expect_false(failure$provenance$uncertainty$valid)
    expect_false(failure$provenance$reliability_valid)
    expect_false("se" %in% names(failure$theta))
    expect_true(nzchar(failure$failure_reason))
  }
  one_orientation <- lapse_case()$skeleton
  one_orientation <- one_orientation[one_orientation$object1 < one_orientation$object2, ]
  # A presented path aliases the positional intercept with item contrasts.
  selected <- paste0(one_orientation$object1, one_orientation$object2) %in% c("ab", "bc", "cd")
  one_orientation <- one_orientation[selected, ]
  failure <- lapse_error(pairwiseLLM::fit_bt_model(one_orientation, engine = "lapse"))
  expect_identical(failure$failure_reason, "unidentified_design")
})

test_that("optimizer and matrix failures remain inspectable and never fall back", {
  failure <- lapse_error(lapse_fit_case(control = list(maxit = 1L)))
  expect_s3_class(failure, "pairwiseLLM_bt_lapse_error")
  expect_length(failure$diagnostics$optimization$attempts, 5L)
  expect_null(failure$provenance$fallback_reason)
  expect_false("se" %in% names(failure$theta))
  failure <- lapse_error(lapse_fit_case(control = list(min_rcond = 0.9)))
  expect_true(failure$failure_reason %in% c("boundary_unresolved", "unidentified_information"))
  expect_true(all(is.finite(failure$theta$theta)))
  expect_true(is.finite(failure$diagnostics$value))
  expect_true(is.matrix(failure$diagnostics$hessian))
})

test_that("failed searches retain every attempt and numerical warnings", {
  testthat::local_mocked_bindings(optim = function(...) stop("synthetic optimizer failure"), .package = "stats")
  failure <- lapse_error(lapse_fit_case())
  expect_identical(failure$failure_reason, "optimizer_failure")
  expect_length(failure$diagnostics$optimization$attempts, 5L)
  expect_match(failure$diagnostics$optimization$attempts[[1L]]$message, "synthetic optimizer failure")
})

test_that("optimizer warnings and unfinished attempts are retained", {
  testthat::local_mocked_bindings(optim = function(par, ...) {
    warning("synthetic warning")
    list(par = par, convergence = 1L, counts = c(`function` = 1L, gradient = 1L), message = "iteration limit")
  }, .package = "stats")
  expect_warning(attempt <- pairwiseLLM:::.bt_lapse_attempt(0.2, lapse_case()$kernel,
    pairwiseLLM:::.bt_lapse_control(list(), FALSE)), "synthetic warning")
  expect_identical(attempt$warnings, "synthetic warning")
  expect_identical(attempt$code, 1L)
})

test_that("the best likelihood candidate cannot evade independent validity gates", {
  baseline <- lapse_fit_case()
  opt <- baseline$diagnostics$optimization
  testthat::local_mocked_bindings(.bt_lapse_optimize = function(...) opt, .package = "pairwiseLLM")
  selected <- opt$selected
  original <- opt
  opt$attempts[[selected]]$code <- 1L
  expect_identical(lapse_error(lapse_fit_case())$failure_reason, "not_converged")
  opt <- original
  opt$attempts[[selected]]$par[1L] <- Inf
  failure <- lapse_error(lapse_fit_case())
  expect_identical(failure$failure_reason, "nonfinite_surface")
  expect_false("se" %in% names(failure$theta))
  opt <- original
  opt$boundary_zero$code <- 1L
  expect_identical(lapse_error(lapse_fit_case())$failure_reason, "boundary_unresolved")
  opt <- original
  opt$boundary_zero$par[1L] <- opt$boundary_zero$par[1L] + 1
  expect_identical(lapse_error(lapse_fit_case())$failure_reason, "boundary_unresolved")
})

test_that("invalid curvature and covariance never produce conventional uncertainty", {
  baseline <- lapse_fit_case()
  original_surface <- pairwiseLLM:::.bt_lapse_surface
  mode <- "hessian"
  testthat::local_mocked_bindings(
    .bt_lapse_optimize = function(...) baseline$diagnostics$optimization,
    .bt_lapse_surface = function(par, kernel) {
      out <- original_surface(par, kernel)
      if (tail(par, 1L) > 0) {
        if (mode == "hessian") out$hessian[1L, 1L] <- -1
        if (mode == "score") out$gradient[1L] <- out$gradient[1L] + 1
      } else if (mode == "boundary") {
        out$hessian[1L, 1L] <- -1
      }
      out
    }, .package = "pairwiseLLM")
  expect_identical(lapse_error(lapse_fit_case())$failure_reason, "invalid_hessian")
  mode <- "score"
  expect_identical(lapse_error(lapse_fit_case())$failure_reason, "not_stationary")
  mode <- "boundary"
  expect_identical(lapse_error(lapse_fit_case())$failure_reason, "boundary_unresolved")
  mode <- "unchanged"
  testthat::local_mocked_bindings(.bt_item_covariance = function(...) stop("invalid covariance"),
                                 .package = "pairwiseLLM")
  failure <- lapse_error(lapse_fit_case())
  expect_identical(failure$failure_reason, "invalid_covariance")
  expect_false(failure$provenance$uncertainty$valid)
  expect_false("se" %in% names(failure$theta))
})

test_that("local lapse confounding is rejected even with finite observed curvature", {
  baseline <- lapse_fit_case()
  testthat::local_mocked_bindings(
    .bt_lapse_optimize = function(...) baseline$diagnostics$optimization,
    .bt_lapse_information = function(par, kernel) matrix(0, length(par), length(par)), .package = "pairwiseLLM")
  failure <- lapse_error(lapse_fit_case())
  expect_identical(failure$failure_reason, "unidentified_information")
  expect_false(failure$provenance$uncertainty$valid)
})

test_that("separation and disconnected data never trigger regularization or fallback", {
  dat <- lapse_case()$skeleton
  dat$result <- 1
  failure <- lapse_error(pairwiseLLM::fit_bt_model(dat, engine = "lapse", verbose = FALSE))
  expect_s3_class(failure, "pairwiseLLM_bt_lapse_error")
  expect_false("se" %in% names(failure$theta))
  expect_null(failure$provenance$fallback_reason)
  dat <- data.frame(a = c("a", "c"), b = c("b", "d"), y = c(1, 0))
  expect_error(pairwiseLLM::fit_bt_model(dat, engine = "lapse"), "disconnected")
})

test_that("boundary candidates require KKT, stationarity, curvature and identification", {
  case <- lapse_case(epsilon = 0)
  baseline <- lapse_fit_case(case)
  original_surface <- pairwiseLLM:::.bt_lapse_surface
  opt <- baseline$diagnostics$optimization
  mode <- "kkt"
  testthat::local_mocked_bindings(
    .bt_lapse_optimize = function(...) opt,
    .bt_lapse_surface = function(par, kernel) {
      out <- original_surface(par, kernel)
      if (tail(par, 1L) == 0) {
        k <- length(par)
        if (mode == "kkt") out$gradient[k] <- -1
        if (mode == "score") out$gradient[1L] <- 1
        if (mode == "face_curvature") out$hessian[1L, 1L] <- -1
        if (mode == "joint_curvature") out$hessian[k, k] <- -1
      }
      out
    }, .package = "pairwiseLLM")
  # Make the selected joint candidate exact zero too: it cannot masquerade as
  # an interior fit when the independently evaluated boundary KKT check fails.
  opt$attempts[[opt$selected]] <- opt$boundary_zero
  expect_identical(lapse_error(lapse_fit_case(case))$failure_reason, "boundary_kkt_failure")
  mode <- "score"
  expect_identical(lapse_error(lapse_fit_case(case))$failure_reason, "boundary_unresolved")
  mode <- "face_curvature"
  expect_identical(lapse_error(lapse_fit_case(case))$failure_reason, "boundary_unresolved")
  mode <- "joint_curvature"
  expect_identical(lapse_error(lapse_fit_case(case))$failure_reason, "invalid_hessian")
  mode <- "unchanged"
  opt$boundary_zero$code <- 1L
  expect_identical(lapse_error(lapse_fit_case(case))$failure_reason, "boundary_unresolved")
  opt$boundary_zero$code <- 0L
  testthat::local_mocked_bindings(.bt_lapse_information = function(par, kernel) matrix(0, length(par), length(par)),
                                 .package = "pairwiseLLM")
  failure <- lapse_error(lapse_fit_case(case))
  expect_identical(failure$failure_reason, "unidentified_information")
  expect_identical(failure$provenance$convergence$status, "failed")
  expect_false(failure$provenance$convergence$converged)
  expect_false("se" %in% names(failure$theta))
})

test_that("strict boundary KKT needs face curvature and never produces an inverse joint covariance", {
  case <- lapse_case(beta = 0, epsilon = 0, seed = 30501)
  baseline <- lapse_fit_case(case)
  original_surface <- pairwiseLLM:::.bt_lapse_surface
  testthat::local_mocked_bindings(
    .bt_lapse_optimize = function(...) baseline$diagnostics$optimization,
    .bt_lapse_surface = function(par, kernel) {
      out <- original_surface(par, kernel)
      if (tail(par, 1L) == 0) out$hessian[length(par), length(par)] <- -1
      out
    },
    .bt_item_covariance = function(...) stop("A boundary fit must not construct joint uncertainty."),
    .package = "pairwiseLLM")
  fit <- lapse_fit_case(case)
  expect_identical(fit$epsilon, 0)
  expect_gt(fit$diagnostics$boundary_zero_checks$epsilon_score, 1e-7)
  expect_false(fit$diagnostics$hessian_checks$positive_definite)
  expect_true(fit$diagnostics$boundary_zero_checks$stationary)
  expect_null(fit$parameter_vcov)
  expect_identical(fit$provenance$uncertainty$status, "nonregular_boundary")
})

test_that("bounded attempts reject out-of-domain evaluations without clipping epsilon", {
  testthat::local_mocked_bindings(optim = function(par, fn, ...) {
    par[length(par)] <- -.Machine$double.eps
    fn(par)
  }, .package = "stats")
  attempt <- pairwiseLLM:::.bt_lapse_attempt(0.2, lapse_case()$kernel,
    pairwiseLLM:::.bt_lapse_control(list(), FALSE))
  expect_identical(attempt$code, 1L)
  expect_identical(attempt$objective, Inf)
  expect_null(attempt$par)
  expect_match(attempt$message, "outside \\[0, 1\\]")
})
