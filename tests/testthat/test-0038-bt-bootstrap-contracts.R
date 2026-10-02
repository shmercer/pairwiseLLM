test_that("fixed bootstrap retains schedules and reports independently calculable summaries", {
  fit <- bootstrap_fit()
  fit$comparisons <- fit$comparisons[c(2, 1, 3:8), ]
  fit$comparisons[2, ] <- list("b", "a")
  out <- bootstrap_bt_model(fit, "fixed", 12, 77, keep = "full")
  expect_s3_class(out, "pairwiseLLM_bt_bootstrap")
  expect_identical(out$status, "complete")
  expect_identical(out$n_success, 12L)
  expect_identical(out$n_failed, 0L)
  for (artifact in out$artifacts) {
    expect_equal(artifact$comparisons[1:2], as.data.frame(fit$comparisons))
    expect_true(all(artifact$comparisons$result %in% 0:1))
    expect_equal(bootstrap_two_item(artifact$comparisons, c("a", "b")),
      pairwiseLLM:::.bt_bootstrap_refit(artifact$comparisons, c("a", "b"), out$provenance$refit))
  }
  expect_true(length(unique(vapply(out$artifacts, function(x) paste(x$comparisons$result, collapse = ""), ""))) > 1L)
  expect_equal(out$theta$bootstrap_mean, unname(colMeans(out$draws)))
  expect_equal(out$theta$bias, unname(colMeans(out$draws)) - fit$theta$theta)
  expect_equal(out$theta$theta_corrected, 2 * fit$theta$theta - unname(colMeans(out$draws)))
  expect_equal(out$theta$bootstrap_sd, unname(apply(out$draws, 2, sd)))
  expect_equal(out$theta$mcse_bias, out$theta$bootstrap_sd / sqrt(12))
  expect_equal(rowSums(out$draws), rep(0, 12), tolerance = 1e-12)
  expect_true(all(out$replicates$connected))
  expect_true(all(out$replicates$n_comparisons == 8L))
  expect_true(all(out$replicates$n_repeated_comparisons == 7L))
  expect_identical(out$provenance$refit$args, fit$provenance$supplied_arguments)
})

test_that("theta inputs and refits are aligned and centered without changing scale", {
  schedule <- data.frame(A_id = rep("a", 8), B_id = rep("b", 8), Y = rep(999, 8))
  fun <- function(bt_data, item_ids, shift = 0) {
    rev(bootstrap_two_item(bt_data, item_ids)) + shift
  }
  a <- bootstrap_bt_model(c(b = -0.4, a = 0.4), "fixed", 10, 8,
    schedule = schedule, estimator = fun, estimator_args = list(shift = 30), keep = "theta")
  b <- bootstrap_bt_model(c(a = 12.4, b = 11.6), "fixed", 10, 8,
    schedule = schedule, estimator = bootstrap_two_item, keep = "theta")
  expect_equal(a$theta, b$theta)
  expect_equal(a$draws, b$draws)
  expect_identical(a$theta$ID, c("a", "b"))
  expect_equal(a$theta$theta_initial, c(.4, -.4))
  zero <- bootstrap_bt_model(c(a = 0, b = 0), "fixed", 2, 1,
    schedule = schedule, estimator = "alpha", estimator_args = list(alpha = .5))
  expect_identical(zero$n_success, 2L)
})

test_that("bootstrap validates counts, theta, connected schedules and estimator specifications", {
  for (name in c("n_rep", "seed", "min_success", "workers", "budget")) {
    for (bad in list(NULL, NA_real_, Inf, -1, 1.5, "2", TRUE, c(2, 3), matrix(2), 2^31)) {
      args <- list(object = bootstrap_fit(), mode = "fixed", n_rep = 2, seed = 3)
      args[name] <- list(bad)
      # NULL budget means recover schedule length.
      if (name == "budget" && is.null(bad)) next
      expect_error(do.call(bootstrap_bt_model, args), name, info = paste(name, toString(bad)))
    }
  }
  expect_error(bootstrap_run(min_success = 9), "cannot exceed")
  expect_error(bootstrap_run(budget = 7), "schedule rows")
  expect_error(bootstrap_run(initial_state = bootstrap_state()), "fixed mode")
  expect_error(bootstrap_run(btl_config = list()), "fixed mode")
  expect_error(bootstrap_run(schedule_fit_fn = identity), "fixed mode")
  for (bad in list(c(0, 1), c(a = 0, a = 1), c(a = NA, b = 1), c(a = Inf, b = 1),
                  c(a = 0), matrix(1, 2), list(theta = 1), list(theta = data.frame(ID = 1:2, theta = c(0, Inf))))) {
    expect_error(bootstrap_bt_model(bad, "fixed", 2, 1), "Theta|theta")
  }
  for (bad in list(NULL, 1, data.frame(a = "a", b = "b"), data.frame(A_id = "a", B_id = "a"),
                  data.frame(A_id = "a", B_id = "x"), data.frame(A_id = NA, B_id = "b"))) {
    expect_error(bootstrap_bt_model(c(a = 0, b = 1), "fixed", 2, 1, schedule = bad,
      estimator = "alpha", estimator_args = list(alpha = .5)), "[Ss]chedule")
  }
  expect_error(bootstrap_bt_model(c(a = 0, b = 1, c = 2), "fixed", 2, 1,
    schedule = bootstrap_data(), estimator = "alpha", estimator_args = list(alpha = .5)), "disconnected")
  expect_error(bootstrap_run(estimator = "brglm2"), "unchanged")
  expect_error(bootstrap_run(estimator_args = list(alpha = .5)), "unchanged")
  for (estimator in list(NULL, "auto", "sirt", c("alpha", "brglm2"), NA_character_, 1)) {
    expect_error(bootstrap_bt_model(c(a = 0, b = 1), "fixed", 2, 1, estimator = estimator), "Supply estimator")
  }
  for (args in list(1, list(1), list(a = 1, a = 2), list(alpha = -1), list(alpha = NA),
                   list(alpha = .5, engine = "alpha"))) {
    expect_error(bootstrap_bt_model(c(a = 0, b = 1), "fixed", 2, 1,
      estimator = "alpha", estimator_args = args))
  }
  expect_error(bootstrap_bt_model(c(a = 0, b = 1), "fixed", 2, 1,
    estimator = bootstrap_two_item, estimator_args = list(item_ids = 1:2)), "reserved")
})

test_that("initial invalid fits and optional dependency failures abort before replication", {
  bad <- bootstrap_fit()
  bad$provenance$convergence$converged <- FALSE
  expect_error(bootstrap_bt_model(bad, "fixed", 2, 1), "convergence")
  bad$provenance$convergence$converged <- TRUE
  bad$provenance$uncertainty$valid <- FALSE
  expect_error(bootstrap_bt_model(bad, "fixed", 2, 1), "uncertainty")
  testthat::local_mocked_bindings(.require_ns = function(pkg, ...) FALSE, .package = "pairwiseLLM")
  expect_error(bootstrap_run(), "withr")
  expect_error(pairwiseLLM:::.bt_bootstrap_estimator(c(a = 0, b = 1), "brglm2", list()), "brglm2")
})

test_that("explicit Firth bootstrap recovers settings and keeps optional engine explicit", {
  skip_if_not_installed("brglm2")
  f <- fit_bt_model(bootstrap_data(), engine = "brglm2", verbose = FALSE, control = list(maxit = 300L))
  b <- bootstrap_bt_model(f, "fixed", 4, 1)
  expect_identical(b$provenance$refit$estimator, "brglm2")
  expect_identical(b$provenance$refit$args, list(control = list(maxit = 300L)))
  expect_identical(b$n_success, 4L)
})
