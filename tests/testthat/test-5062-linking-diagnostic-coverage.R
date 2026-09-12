test_that("linking diagnostics preserve missing, finite, and nonfinite evidence", {
  f <- pairwiseLLM:::.adaptive_link_cmdstan_collect_diagnostics
  fit <- list(diagnostic_summary = function() data.frame(num_divergent = c(0, 2)),
    summary = function(variables) data.frame(rhat = c(1, 1.02), ess_bulk = c(400, 900)))
  expect_equal(f(fit, "delta"), list(divergences = 2L, max_rhat = 1.02, min_ess_bulk = 400))
  fit$diagnostic_summary <- function() data.frame(num_divergent = Inf)
  fit$summary <- function(variables) data.frame(rhat = NA_real_, ess_bulk = Inf)
  out <- f(fit, "delta")
  expect_true(is.na(out$divergences))
  expect_length(out$notes, 3)
  fit$summary <- function(variables) data.frame(mean = 1)
  expect_match(paste(f(fit, "delta")$notes, collapse = " "), "missing rhat.*missing ess_bulk")
  fit$diagnostic_summary <- function() stop("missing")
  fit$summary <- function(variables) stop("missing")
  expect_length(f(fit, "delta")$notes, 2)
  expect_error(pairwiseLLM:::.adaptive_link_cmdstan_draws_matrix(
    list(draws = function(...) stop("broken draw file")), "delta"), "broken draw file")
})

test_that("linking fit contracts distinguish HMC, deterministic, and reused fits", {
  f <- pairwiseLLM:::.adaptive_link_diagnostics_contract
  hmc <- list(diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 500,
    diagnostics_divergences_pass = TRUE, diagnostics_rhat_pass = TRUE,
    diagnostics_ess_pass = TRUE))
  expect_true(f(hmc)$link_diagnostics_pass)
  expect_identical(f(hmc)$link_uncertainty_approximation, "cmdstan_posterior_draws")
  hmc$diagnostics$diagnostics_ess_pass <- FALSE
  expect_false(f(hmc)$link_diagnostics_pass)
  deterministic <- list(diagnostics = list(converged = TRUE, hessian_posdef = TRUE),
    theta_hub_post = c(a = 0), theta_spoke_post = c(b = 1), theta_spoke_sd_post = c(b = 0.2))
  expect_true(f(deterministic)$link_diagnostics_pass)
  expect_identical(f(deterministic)$link_fit_method, "map_laplace")
  for (field in c("theta_hub_post", "theta_spoke_post", "delta_mean", "log_alpha_mean")) {
    bad <- deterministic
    bad[[field]] <- Inf
    expect_false(f(bad)$link_diagnostics_finite_summary_pass, info = field)
  }
  deterministic$diagnostics$hessian_posdef <- FALSE
  expect_false(f(deterministic)$link_diagnostics_uncertainty_pass)
  deterministic$diagnostics$hessian_posdef <- NULL
  expect_error(f(deterministic), "requires.*converged")
  deterministic$diagnostics$hessian_posdef <- TRUE
  deterministic$fit_contract <- list(uncertainty_approximation = "wrong")
  expect_error(f(deterministic), "laplace_hessian")
  reused <- list(fit_contract = list(contract_type = "link_refit_frozen_reuse"), delta_sd = 0.1)
  expect_true(f(reused)$link_diagnostics_pass)
  expect_identical(f(reused)$link_fit_method, "accepted_state_reuse")
  reused$delta_sd <- -1
  expect_false(f(reused)$link_diagnostics_uncertainty_pass)
  expect_error(f(list()), "undefined")
})

test_that("linking sampler adapter forwards settings without launching a sampler", {
  root <- withr::local_tempdir()
  captured <- new.env(parent = emptyenv())
  draws <- matrix(c(0, 1), ncol = 1, dimnames = list(NULL, "delta"))
  model_fn <- function(path, cpp_options) {
    expect_true(file.exists(path))
    expect_true(cpp_options$stan_threads)
    list(sample = function(...) {
      captured$args <- list(...)
      list(draws = function(variables, format) {
        expect_identical(variables, "delta")
        expect_identical(format, "matrix")
        draws
      }, diagnostic_summary = function() data.frame(num_divergent = 0L),
      summary = function(variables) data.frame(rhat = 1, ess_bulk = 1000))
    })
  }
  config <- list(chains = 2L, parallel_chains = 1L, threads_per_chain = 1L,
    iter_warmup = 10L, iter_sampling = 20L, output_dir = root)
  f <- pairwiseLLM:::.adaptive_link_fit_transform_cmdstan
  out <- f(list(N = 2L), "delta", config, 41L, model_fn)
  expect_identical(out$draws_matrix, draws)
  expect_identical(captured$args$seed, 41L)
  expect_identical(captured$args$data, list(N = 2L))
  expect_identical(captured$args$iter_warmup, 10L)
  expect_identical(captured$args$iter_sampling, 20L)
  expect_identical(captured$args$output_dir, root)
  expect_match(captured$args$output_basename, "^link_transform_refit-")
  expect_error(f(list(), "delta", config, 1L, "bad"), "model_fn")
  config$output_dir <- NA_character_
  expect_error(f(list(), "delta", config, 1L, model_fn), "output_dir")
})

test_that("spoke snapshots and merges isolate independent state", {
  state <- task09_link_state()
  f <- pairwiseLLM:::.adaptive_linking_refit_merge_spoke_state
  snapshot <- pairwiseLLM:::.adaptive_linking_refit_spoke_snapshot(state, 2L)
  expect_identical(snapshot$linking$phase_a$ready_spokes, 2L)
  expect_identical(snapshot$controller$current_link_spoke_id, 2L)
  expect_identical(state$linking$phase_a$ready_spokes, 2:3)
  state$controller$link_refit_stats_by_spoke <- list(`2` = list(value = 1), `3` = list(value = 3))
  snapshot$controller$link_refit_stats_by_spoke <- list(`2` = list(value = 2), `3` = list(value = 99))
  snapshot$controller$linking_identified_by_spoke <- list(`2` = TRUE)
  snapshot$linking$anchored_joint$accepted_state_by_spoke <- list(`2` = list(marker = 2))
  snapshot$linking$anchored_joint$fisher_t0_by_spoke <- list(`2` = list(marker = 3))
  snapshot$linking$probe$panels_by_spoke <- list(`2` = list(marker = 4))
  out <- f(state, snapshot, 2L)
  expect_identical(out$controller$link_refit_stats_by_spoke,
    list(`2` = list(value = 2), `3` = list(value = 3)))
  expect_true(out$controller$linking_identified)
  expect_identical(out$linking$anchored_joint$accepted_state_by_spoke[["2"]], list(marker = 2))
  expect_identical(out$linking$anchored_joint$fisher_t0_by_spoke[["2"]], list(marker = 3))
  expect_identical(out$linking$probe$panels_by_spoke[["2"]], list(marker = 4))
  expect_identical(out$history_pairs, state$history_pairs)
  expect_identical(out$trueskill_state, state$trueskill_state)
  state$config$btl_config$phase_b_refit_workers <- 4L
  expect_identical(pairwiseLLM:::.adaptive_phase_b_refit_parallel_workers(state, 2L), 2L)
  state$config$btl_config$phase_b_refit_workers <- 0L
  expect_error(pairwiseLLM:::.adaptive_phase_b_refit_parallel_workers(state, 2L), "positive integer")
})

test_that("stop reconstruction checks every required gate and historical reliability", {
  row <- tibble::tibble(link_stop_eligible = TRUE, reliability_link_global = 0.99,
    hub_anchored = TRUE, probe_brier = 0.01, probe_pred_rmse_lagged = 0.001,
    theta_global_rmse_lagged = 0.001, ts_btl_rank_spearman = 0.99)
  f <- function(x, diagnostics = TRUE) {
    pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(x, diagnostics, 0.1, list())
  }
  expect_true(f(row))
  expect_false(f(row, FALSE))
  expect_error(f(row[FALSE, ]), "exactly one row")
  for (field in c("reliability_link_global", "hub_anchored", "probe_brier",
    "probe_pred_rmse_lagged", "theta_global_rmse_lagged")) {
    bad <- row
    bad[[field]] <- NULL
    expect_false(f(bad), info = field)
  }
  row$reliability_EAP_link <- row$reliability_link_global
  row$reliability_link_global <- NULL
  expect_true(f(row))
  row$probe_quality_pass <- NA
  expect_true(f(row))
  row$probe_quality_pass <- FALSE
  expect_false(f(row))
  identified <- pairwiseLLM:::.adaptive_link_reconstruct_identified_from_logs
  expect_true(identified(row, list()))
  row$ts_btl_rank_spearman <- 0.1
  expect_false(identified(row, list()))
  expect_error(identified(row[FALSE, ], list()), "exactly one row")
})

test_that("retained phase-specific judge parameters enforce paired availability", {
  state <- task09_link_state()
  controller <- state$controller
  controller$judge_param_mode <- "phase_specific"
  f <- pairwiseLLM:::.adaptive_link_judge_params
  state$btl_fit$beta_within_mean <- 0.2
  state$btl_fit$epsilon_within_mean <- 0.1
  out <- f(state, controller, "link", TRUE, FALSE)
  expect_equal(out$beta, 0.2)
  expect_equal(out$epsilon, 0.1)
  expect_true(out$cold_start_fallback_used)
  expect_equal(f(state, controller, "within")$beta, 0.2)
  expect_error(f(state, controller), "beta_link_mean")
  state$btl_fit$beta_link_mean <- 0.4
  expect_error(f(state, controller, "link", TRUE, FALSE), "epsilon_link_mean")
  state$btl_fit$epsilon_link_mean <- 2
  expect_equal(f(state, controller)$epsilon, 1)
  state$btl_fit$beta_within_mean <- Inf
  expect_error(f(state, controller, "within"), "beta_within_mean")
  state$btl_fit$beta_link_mean <- NULL
  state$btl_fit$epsilon_link_mean <- NULL
  state$btl_fit$beta_mean <- Inf
  state$btl_fit$epsilon_mean <- Inf
  out <- f(state, controller, "link", TRUE, FALSE)
  expect_equal(out$beta, 0)
  expect_equal(out$epsilon, 0)
})
