test_that("restart and retention choices preserve scientific outputs and caller RNG", {
  withr::local_seed(5304)
  withr::local_rng_version("4.4.0")
  RNGkind("L'Ecuyer-CMRG")
  before <- .Random.seed
  kinds <- RNGkind()
  full <- bootstrap_run()
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), kinds)
  for (keep in c("summary", "theta", "full")) {
    out <- bootstrap_run(keep = keep)
    expect_identical(out$theta, full$theta)
    expect_identical(out$replicates, full$replicates)
    if (keep == "summary") expect_null(out$draws) else expect_identical(out$draws, full$draws)
    if (keep != "full") expect_null(out$artifacts) else expect_identical(out, full)
  }
  path <- tempfile(fileext = ".rds", tmpdir = withr::local_tempdir())
  saveRDS(full, path)
  expect_identical(readRDS(path), full)
})

test_that("failed refits are excluded as whole replicates with explicit thresholds", {
  refit <- function(bt_data, item_ids) {
    if (bt_data$result[1] == 1) stop("controlled failure")
    bootstrap_two_item(bt_data, item_ids)
  }
  run <- function(min_success) {
    bootstrap_bt_model(c(a = .4, b = -.4), "fixed", 24, 99,
      schedule = bootstrap_data(), estimator = refit, min_success = min_success, keep = "full")
  }
  expect_warning(out <- run(2), "excluded")
  expect_identical(out$status, "partial")
  expect_gt(out$n_success, 1L)
  expect_gt(out$n_failed, 0L)
  expect_identical(out$n_success + out$n_failed, 24L)
  failed <- !out$replicates$success
  expect_true(all(is.na(out$draws[failed, ])))
  expect_true(all(out$replicates$failure_phase[failed] == "estimator"))
  expect_true(all(out$replicates$message[failed] == "controlled failure"))
  expect_equal(out$theta$bootstrap_mean, unname(colMeans(out$draws[!failed, ])))
  err <- bootstrap_error(run(24))
  expect_s3_class(err, "pairwiseLLM_bt_bootstrap_error")
  expect_identical(err$result$replicates, out$replicates)
  expect_true(all(is.na(err$result$theta$theta_corrected)))
  expect_identical(err$result$status, "insufficient_success")
  for (refit in list(function(...) stop("all failed"), function(...) c(a = 1, b = Inf),
                    function(...) c(a = 1, other = 0),
                    function(...) {
                      list(theta = data.frame(ID = c("a", "b"), theta = c(0, 1)),
                        provenance = list(convergence = list(converged = FALSE)))
                    },
                    function(...) {
                      list(theta = data.frame(ID = c("a", "b"), theta = c(0, 1)),
                        provenance = list(uncertainty = list(valid = FALSE)))
                    })) {
    err <- bootstrap_error(bootstrap_bt_model(c(a = 1, b = 0), "fixed", 2, 1,
      schedule = bootstrap_data(), estimator = refit))
    expect_identical(err$result$n_success, 0L)
    expect_true(all(is.na(err$result$theta$bootstrap_mean)))
    expect_true(all(is.na(err$result$theta$mcse_bias)))
  }
})

test_that("real no-finite-MLE failures retain alpha diagnostic evidence", {
  out <- bootstrap_error(bootstrap_bt_model(c(a = 0, b = 0), "fixed", 3, 18,
    schedule = bootstrap_data(total = 1, wins = 1), estimator = "alpha", estimator_args = list(alpha = 0),
    keep = "full"))
  expect_identical(out$result$n_failed, 3L)
  expect_true(all(out$result$replicates$failure_reason == "no_finite_mle"))
  expect_true(all(vapply(out$result$failures, function(x) is.list(x$diagnostics), logical(1))))
})

test_that("fit warnings remain visible without being silently discarded", {
  fun <- function(bt_data, item_ids) {
    warning("controlled numeric warning")
    bootstrap_two_item(bt_data, item_ids)
  }
  expect_warning(out <- bootstrap_bt_model(c(a = .4, b = -.4), "fixed", 2, 99,
    schedule = bootstrap_data(), estimator = fun), "numerical warnings")
  expect_identical(out$replicates$warnings, rep(list("controlled numeric warning"), 2))
})

test_that("serial and multisession execution agree and restore the future plan", {
  skip_if_not_installed("future.apply")
  skip_if_not_installed("future")
  skip_if_no_psock()
  # Workers must load this source version, not an older user-library install.
  # covr/check already run against an installed package and need no extra install.
  if (pkgload::is_dev_package("pairwiseLLM")) {
    lib <- withr::local_tempdir()
    output <- system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", "--no-byte-compile",
      "--no-test-load", "--no-docs", paste0("--library=", shQuote(lib)),
      shQuote(getNamespaceInfo("pairwiseLLM", "path"))), stdout = TRUE, stderr = TRUE)
    expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
    withr::local_libpaths(c(lib, .libPaths()))
  }
  withr::local_seed(199)
  before <- .Random.seed
  plan <- future::plan()
  serial <- bootstrap_run()
  parallel <- bootstrap_run(workers = 2L)
  expect_identical(parallel, serial)
  expect_identical(.Random.seed, before)
  expect_identical(class(future::plan()), class(plan))
  expect_identical(body(future::plan()), body(plan))
  expect_identical(formals(future::plan()), formals(plan))
  for (strategy in c("trueskill_p50", "hybrid")) {
    state <- bootstrap_state(strategy, if (strategy == "hybrid") letters[1:12] else letters[1:6])
    budget <- if (strategy == "hybrid") 14L else 10L
    serial <- bootstrap_adaptive(state, n_rep = 2L, budget = budget,
      btl_config = list(refit_pairs_target = 100L), keep = "theta")
    parallel <- bootstrap_adaptive(state, n_rep = 2L, budget = budget, btl_config = list(refit_pairs_target = 100L),
      workers = 2L, keep = "theta")
    expect_identical(parallel$theta, serial$theta)
    expect_identical(parallel$draws, serial$draws)
    expect_identical(parallel$replicates, serial$replicates)
  }
  # Exercise independent scheduling-refit RNGs as well as outcome RNGs.
  fitter <- function(state, config) {
    draws <- outer(stats::rnorm(20, sd = .001), state$trueskill_state$items$mu, "+")
    colnames(draws) <- state$item_ids
    list(btl_posterior_draws = draws, theta_mean = colMeans(draws),
      theta_sd = apply(draws, 2, stats::sd), model_variant = "btl",
      diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000))
  }
  environment(fitter) <- baseenv()
  state <- bootstrap_state("hybrid", letters[1:12])
  run <- function(workers) {
    bootstrap_adaptive(state, n_rep = 2L, budget = 14L,
      btl_config = list(refit_pairs_target = 6L, model_variant = "btl"),
      schedule_fit_fn = fitter, workers = workers, keep = "theta")
  }
  serial <- run(1L)
  parallel <- run(2L)
  expect_identical(parallel$theta, serial$theta)
  expect_identical(parallel$draws, serial$draws)
  expect_identical(parallel$replicates, serial$replicates)
  failing <- function(bt_data, item_ids) {
    if (bt_data$result[1] == 1) stop("deterministic failure")
    stats::setNames(stats::runif(length(item_ids)), item_ids)
  }
  environment(failing) <- baseenv()
  run <- function(workers) {
    bootstrap_bt_model(c(a = .4, b = -.4), "fixed", 24, 99, schedule = bootstrap_data(),
      estimator = failing, min_success = 2L, workers = workers, keep = "theta")
  }
  expect_warning(serial <- run(1L), "excluded")
  expect_warning(parallel <- run(2L), "excluded")
  expect_identical(parallel$theta, serial$theta)
  expect_identical(parallel$draws, serial$draws)
  expect_identical(parallel$replicates, serial$replicates)
  expect_identical(.Random.seed, before)
})

test_that("bootstrap preserves absence of a caller seed", {
  withr::local_preserve_seed()
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  bootstrap_run(keep = "summary")
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("summary arithmetic overflow cannot produce usable corrected scores", {
  out <- bootstrap_error(bootstrap_bt_model(c(a = 1e308, b = -1e308), "fixed", 2, 1,
    schedule = bootstrap_data(), estimator = function(...) c(a = -1e308, b = 1e308)))
  expect_identical(out$result$status, "invalid_summary")
  expect_true(all(is.na(out$result$theta$theta_corrected)))
})
