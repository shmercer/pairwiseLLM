test_that("canonical log fields and legacy freeze aliases stay explicit", {
  step <- .adaptive_step_log_transform_only_fields()
  stage <- .adaptive_link_stage_transform_only_fields()
  expect_true(all(step %in% names(schema_step_log)))
  expect_true(all(stage %in% names(schema_link_stage_log)))
  expect_true(all(c("delta_spoke_mean", "log_alpha_spoke_sd") %in% stage))
  out <- append_link_stage_log(new_link_stage_log(), list(
    transform_frozen = TRUE, transform_frozen_refit_id = 7L))
  expect_true(out$link_state_frozen)
  expect_identical(out$link_state_frozen_refit_id, 7L)
  expect_false(any(c("transform_frozen", "transform_frozen_refit_id") %in% names(out)))
})

test_that("offline calibration primitives reproduce truth and preserve RNG state", {
  withr::local_seed(5096)
  rng <- .Random.seed
  items <- .adaptive_calibration_build_items(c(3L, 2L))
  truth <- .adaptive_calibration_truth(items, 123L, -0.5, 2)
  expect_identical(.Random.seed, rng)
  expect_equal(truth$theta_global[1:3], truth$theta_raw[1:3])
  expect_equal(truth$theta_global[4:5], 2 * truth$theta_raw[4:5] - 0.5)
  expect_identical(truth, .adaptive_calibration_truth(items, 123L, -0.5, 2))
  judge <- .adaptive_calibration_judge(truth$theta_global, 0.2, 0.1, 12L)
  args <- list(items[1, ], items[4, ], list(step_log = tibble::tibble()))
  outcome <- do.call(judge, args)
  expect_true(outcome$is_valid)
  expect_true(outcome$Y %in% 0:1)
  expect_identical(outcome, do.call(judge, args))
  fit_fn <- .adaptive_calibration_fit_fn(truth$theta_raw, 321L, n_draws = 64L)
  state <- list(item_ids = items$item_id)
  fit <- fit_fn(state, list(chains = 2L))
  expect_identical(fit, fit_fn(state, list(chains = 2L)))
  expect_identical(colnames(fit$btl_posterior_draws), items$item_id)
  expect_equal(fit$theta_mean, colMeans(fit$btl_posterior_draws))
  expect_equal(fit$theta_sd, apply(fit$btl_posterior_draws, 2, sd))
  expect_identical(.Random.seed, rng)
})

test_that("calibration fingerprints describe functions without serializing their closures", {
  for (env in list(emptyenv(), baseenv(), globalenv(), asNamespace("stats"), new.env())) {
    fn <- function(x = 1) x + 1
    environment(fn) <- env
    surface <- .adaptive_calibration_canonicalize(fn)
    expect_identical(surface$kind, "function")
    expect_identical(surface$formals, list(x = 1))
    expect_match(surface$body, "x \\+ 1")
    expected <- if (identical(env, emptyenv())) "emptyenv" else
      if (identical(env, baseenv())) "baseenv" else
        if (identical(env, globalenv())) "globalenv" else
          if (isNamespace(env)) "namespace:stats" else "closure"
    expect_identical(surface$environment, expected)
  }
  expect_identical(.adaptive_calibration_hash_object(list(z = list(2, 3), a = 1)),
    .adaptive_calibration_hash_object(list(a = 1, z = list(2, 3))))
  expect_false(identical(.adaptive_calibration_hash_object(list(a = 1)),
    .adaptive_calibration_hash_object(list(a = 2))))
})

test_that("calibration summaries count eligible replicates and round-trip artifacts", {
  log <- tibble::tibble(refit_id = 1:4, spoke_id = 2L,
    ppc_brier_cross_active = c(.1, .3, .9, NA_real_),
    n_cross_edges_active_since_last_refit = c(2L, 2L, 0L, 2L))
  metrics <- .adaptive_calibration_extract_replicate_metrics(list(link_stage_log = log), 1L)
  expect_identical(metrics$eligible, c(TRUE, TRUE, FALSE, FALSE))
  out <- .adaptive_calibration_summarize(metrics, 2L, 12L, list(size = 3L))
  expect_equal(out$summary$quantile_p95, .29)
  expect_equal(out$summary$mean, .2)
  expect_identical(out$summary$n_eligible_rows, 2L)
  expect_identical(out$summary$n_replicates_without_eligible, 1L)
  paths <- .adaptive_calibration_write_artifacts(out$summary, metrics, out$sidecar,
    file.path(withr::local_tempdir(), "nested"))
  expect_true(all(file.exists(unlist(paths))))
  expect_equal(read.csv(paths$summary_csv)$quantile_p95, .29)
  expect_equal(nrow(read.csv(paths$replicates_csv)), 4L)
  expect_equal(jsonlite::read_json(paths$sidecar_json)$cross_set_ppc_brier_max, .29)
})

test_that("calibration replicate driver can run an offline Phase A fixture", {
  # Phase B remains gated. Exercise the retained driver through supported Phase A.
  out <- .adaptive_calibration_run_replicate(1L, 12L, c(3L, 3L), -.5, 1,
    0, .05, 1L, adaptive_config = list(link_estimation_mode = "fixed_shape_offset"))
  expect_s3_class(out$state, "adaptive_state")
  expect_equal(nrow(out$state$step_log), 1L)
  expect_equal(nrow(out$metrics), 0L)
})

test_that("default calibration path falls back to the source tree or absence", {
  directory <- withr::local_tempdir()
  withr::local_dir(directory)
  # pkgload supplies a system.file shim in the development imports environment.
  imports <- parent.env(asNamespace("pairwiseLLM"))
  target <- if (exists("system.file", imports, inherits = FALSE)) "pairwiseLLM" else "base"
  local_mocked_bindings(system.file = function(...) "", .package = target)
  expect_identical(.adaptive_calibration_default_artifact_path(), NA_character_)
  dir.create(file.path("inst", "extdata"), recursive = TRUE)
  path <- file.path("inst", "extdata", "adaptive_linking_calibration_default.json")
  writeLines("{}", path)
  expect_identical(.adaptive_calibration_default_artifact_path(), path)
})

test_that("Gaussian optimization exposes objective failures and rejects non-descent polishing", {
  kernel <- list(mean = 0)
  control <- list(maxit = 20L, rel_tol = 1e-8, gradient_tol = 1e-8)
  broken <- function(...) stop("objective unavailable")
  out <- .link_gaussian_optimize(kernel, control, broken)
  expect_true(is.na(out$selected))
  expect_true(all(vapply(out$attempts, function(x) x$message == "objective unavailable", logical(1))))
  # A reported convergence with a nonzero gradient must not become a valid fit.
  local_mocked_bindings(optim = function(par, ...) list(par = par, convergence = 0L, counts = c(1L, 1L)),
    .package = "stats")
  inconsistent <- function(w, kernel, hessian = FALSE) {
    list(value = sum(w^2), gradient = rep(1, length(w)), hessian = diag(length(w)))
  }
  out <- .link_gaussian_optimize(kernel, control, inconsistent)
  expect_true(is.na(out$selected))
  expect_true(out$attempts[[1]]$newton_steps < 10L)
  indefinite <- function(w, kernel, hessian = FALSE) {
    list(value = 0, gradient = 1, hessian = matrix(-1))
  }
  out <- .link_gaussian_optimize(kernel, control, indefinite)
  expect_true(is.na(out$selected))
  expect_true(all(vapply(out$attempts, function(x) x$newton_steps == 0L, logical(1))))
})

test_that("Gaussian predictions fail when accumulated quadrature error exceeds tolerance", {
  local_mocked_bindings(integrate = function(...) list(message = "OK", value = .25, abs.error = .1),
    .package = "stats")
  expect_error(.link_gaussian_integrate(0, 1,
    list(prediction_abs_tol = 1e-8, prediction_rel_tol = 1e-8, subdivisions = 100L),
    .link_e2_fail), "numerical tolerances")
})

test_that("E3 Stan cache is writable, content-bound and reused without sampling", {
  skip_if_not_installed("cmdstanr")
  directory <- withr::local_tempdir()
  local_mocked_bindings(R_user_dir = function(...) directory, .package = "tools")
  local_mocked_bindings(cmdstan_version = function(...) package_version("2.38.0"), .package = "cmdstanr")
  path <- .link_e3_model_file()
  expect_true(file.exists(path))
  expect_identical(readLines(path), readLines(system.file("stan", "link_joint_offset.stan", package = "pairwiseLLM")))
  expect_identical(.link_e3_model_file(), path)
  local_mocked_bindings(cmdstan_version = function(...) package_version("2.39.0"), .package = "cmdstanr")
  expect_false(identical(.link_e3_model_file(), path))
})

test_that("E3 convolution handles a constant shape and missing posterior diagnostics", {
  expect_equal(.link_e3_prior_convolution_quantile(rep(2, 10), list(mean = 1, sd = 3), .975),
    3 + 3 * qnorm(.975))
  skip_if_not_installed("posterior")
  local_mocked_bindings(summarise_draws = function(...) stop("summary unavailable"), .package = "posterior")
  draws <- array(0, c(10L, 2L, 1L), dimnames = list(NULL, NULL, "delta"))
  out <- .link_e3_sampler_diagnostics(list(diagnostic_summary = function(...) list()), draws, list())
  expect_false(out$audit_gate$passed)
  expect_true(all(is.na(out$parameters$mean)))
  expect_true(all(is.na(out$parameters$mcse_sd_ratio)))
})
