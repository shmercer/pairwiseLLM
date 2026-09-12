test_that("marginal reconstruction preserves aligned means and sample uncertainty", {
  f <- pairwiseLLM:::.adaptive_phase_b_global_metric_marginal_quantile_draws
  mu <- c(a = -1, b = 2, c = 0)
  sigma <- c(c = 0, a = 0.2, b = 0.4)
  withr::local_seed(31)
  before <- .Random.seed
  draws <- f(mu, sigma, 8L, "fixture")
  expect_identical(.Random.seed, before)
  expect_identical(colnames(draws), names(mu))
  expect_equal(colMeans(draws), mu)
  expect_equal(apply(draws, 2, sd), sigma[names(mu)])
  expect_equal(draws[, "c"], rep(0, 8))
  expect_identical(f(mu, sigma, 8L, "fixture"), draws)
  expect_error(f(mu, sigma, 1L, "fixture"), "at least two")
  expect_error(f(numeric(), sigma, 4L, "fixture"), "named theta")
  expect_error(f(c(a = 1, a = 2), c(a = 1), 4L, "fixture"), "unique")
  expect_error(f(mu, c(a = 1), 4L, "fixture"), "missing required")
  expect_error(f(c(a = Inf), c(a = 1), 4L, "fixture"), "finite theta means")
  expect_error(f(c(a = 1), c(a = Inf), 4L, "fixture"), "finite theta SD")
  expect_error(f(c(a = 1), c(a = -1), 4L, "fixture"), "non-negative")
})

test_that("global reconstruction distinguishes retained uncertainty contracts", {
  f <- pairwiseLLM:::.adaptive_phase_b_global_metric_uncertainty_approximation
  expect_identical(f("transform", "plugin"), "plugin")
  expect_identical(f("transform"), NA_character_)
  expect_identical(f("anchored_joint", "cmdstan_posterior_draws"), "cmdstan_posterior_draws")
  expect_identical(f("anchored_joint", link_fit_method = "map_laplace"),
    "laplace_hessian_marginal_quantiles")
  expect_identical(f("anchored_joint", "accepted_state"), "accepted_state_marginal_quantiles")
  expect_identical(f("anchored_joint"), "accepted_state_marginal_quantiles")
  expect_error(f("anchored_joint", "unsupported"), "Unsupported")
})

test_that("artifact draw reconstruction rejects incompatible domains and shapes", {
  state <- task09_link_state()
  f <- pairwiseLLM:::.adaptive_phase_a_artifact_draws_for_phase_b_global
  expected <- state$linking$phase_a$artifacts[["1"]]$posterior_draws
  expect_equal(f(state, 1L), expected)
  colnames(state$linking$phase_a$artifacts[["1"]]$posterior_draws) <- NULL
  expect_equal(f(state, 1L), expected)
  state$linking$phase_a$artifacts[["1"]]$posterior_draws <- matrix(1, 4, 3)
  expect_error(f(state, 1L), "item count")
  state$linking$phase_a$artifacts[["1"]]$posterior_draws <- expected
  colnames(state$linking$phase_a$artifacts[["1"]]$posterior_draws) <- c("x", "y")
  expect_error(f(state, 1L), "missing required item ids")
  expect_error(pairwiseLLM:::.adaptive_phase_a_artifact_item_ids(state, list(), 99L),
    "No state items")
  expect_error(pairwiseLLM:::.adaptive_phase_a_artifact_item_field_map(state, 99L, "theta_raw_sd"),
    "Missing Phase A")
  expect_error(pairwiseLLM:::.adaptive_phase_a_artifact_item_field_map(state, 1L, "missing"),
    "missing required columns")
})

test_that("retained transform summaries validate the latest spoke evidence", {
  state <- task09_link_state()
  controller <- task09_transform_controller(state, "shift_scale")
  f <- pairwiseLLM:::.adaptive_phase_b_global_metric_transform_stats
  expect_equal(f(state, 2L, controller)$log_alpha_spoke_mean, log(2))
  controller$link_refit_stats_by_spoke <- list()
  state$link_stage_log <- tibble::tibble(spoke_id = c(2L, 3L, 2L), refit_id = c(2L, 4L, 1L),
    link_transform_state = "shift_scale", delta_spoke_mean = c(0.7, 9, 0.1),
    log_alpha_spoke_mean = log(2))
  expect_equal(f(state, 2L, controller)$delta_spoke_mean, 0.7)
  state$link_stage_log$link_transform_state[1] <- "bad"
  expect_error(f(state, 2L, controller), "valid transform state")
  state$link_stage_log$link_transform_state[1] <- "shift_scale"
  state$link_stage_log$delta_spoke_mean[1] <- NA_real_
  expect_error(f(state, 2L, controller), "finite delta")
  state$link_stage_log$delta_spoke_mean[1] <- 0
  state$link_stage_log$log_alpha_spoke_mean[1] <- NA_real_
  expect_error(f(state, 2L, controller), "finite log-alpha")
})

test_that("retained transformations propagate plugin and joint posterior uncertainty", {
  state <- task09_link_state()
  f <- pairwiseLLM:::.adaptive_link_global_score_stats_active
  ids <- c("a", "b", "c", "d", "unknown")
  out <- f(state, ids, 2L, 1L, "shift_scale", 0.3, log(2))
  expect_equal(out$mean_map[1:4], c(a = -1, b = 1, c = -0.7, d = 1.3))
  expect_equal(out$var_map[1:4], c(a = 0.04, b = 0.04, c = 0.16, d = 0.16))
  expect_true(is.na(out$mean_map[["unknown"]]))
  expect_equal(out$reliability, var(out$mean_map[1:4]) /
    (var(out$mean_map[1:4]) + mean(out$var_map[1:4])))
  expect_false(f(state, "a", 2L, 1L, "shift_only", 0)$defined)
  expect_false(f(state, ids, 2L, 1L, "shift_only", NA_real_)$defined)
  expect_false(f(state, ids, 2L, 1L, "shift_scale", 0)$defined)
  expect_equal(f(state, ids, 2L, 1L, "bad", 0)$mean_map[1:4],
    c(a = -1, b = 1, c = -0.5, d = 0.5))
  fit <- list(posterior_draws = list(delta = c(0, 1), log_alpha = log(c(1, 2)),
    theta_hub = matrix(c(-2, -1, 1, 2), 2, dimnames = list(NULL, c("a", "b"))),
    theta_spoke = matrix(c(-1, 0, 0, 1), 2, dimnames = list(NULL, c("c", "d")))))
  joint <- f(state, ids, 2L, 1L, "shift_scale", 0.5, log(2), fit = fit,
    refit_mode = "joint_refit")
  expect_equal(joint$mean_map[1:4], c(a = -1.5, b = 1.5, c = 0, d = 1.5))
  expect_equal(joint$var_map[1:4], c(a = 0.5, b = 0.5, c = 2, d = 4.5))
  locked <- f(state, ids, 2L, 1L, "shift_only", 0.5, fit = fit,
    refit_mode = "joint_refit", hub_lock_mode = "hard_lock")
  expect_equal(locked$mean_map[1:2], c(a = -1, b = 1))
  fit$posterior_draws$theta_spoke <- fit$posterior_draws$theta_spoke[1, , drop = FALSE]
  fit$posterior_draws$log_alpha <- numeric()
  fallback <- f(state, ids, 2L, 1L, "shift_scale", 0.5, log(2), fit = fit)
  expect_equal(fallback$mean_map[3:4], c(c = -0.5, d = 1.5))
  fixed <- f(state, ids, 2L, 1L, "shift_only", 0, shift_only_theta_treatment = "fixed")
  expect_equal(fixed$var_map[3:4], c(c = 0, d = 0))
})

test_that("retained rank stability requires aligned finite lagged scores", {
  state <- task09_link_state()
  f <- pairwiseLLM:::.adaptive_link_rank_stability_lagged
  args <- list(state = state, active_ids = letters[1:4], stability_lag = 1L,
    spoke_id = 2L, hub_id = 1L, transform_mode = "shift_only", delta_mean = 0,
    lag_row = tibble::tibble(delta_spoke_mean = 0, log_alpha_spoke_mean = 0))
  expect_false(do.call(f, args)$lag_eligible)
  means <- colMeans(state$btl_fit$btl_posterior_draws)
  args$state$refit_meta$theta_mean_history <- list(means, means)
  expect_equal(do.call(f, args)$rho_rank_lagged, 1)
  expect_true(do.call(f, args)$rho_rank_lagged_pass)
  args$state$refit_meta$theta_mean_history[[2]] <- -means
  expect_equal(do.call(f, args)$rho_rank_lagged, -1)
  args$state$refit_meta$theta_mean_history[[2]] <- "missing"
  expect_equal(do.call(f, args)$rho_rank_lagged, 1)
  args$active_ids <- "a"
  expect_false(do.call(f, args)$rho_rank_lagged_pass)
  args$active_ids <- letters[1:4]
  args$lag_row <- tibble::tibble()
  expect_false(do.call(f, args)$rho_rank_lagged_pass)
  args$state$refit_meta$theta_mean_history[[1]] <- unname(means)
  expect_false(do.call(f, args)$rho_rank_lagged_pass)
  transform <- pairwiseLLM:::.adaptive_link_transform_theta_mean_for_spoke
  expect_length(transform(state, numeric(), 2L, 1L, "shift_only", 0), 0)
  expect_true(all(is.na(transform(state, means, 2L, 1L, "shift_scale", 0))))
  expect_equal(transform(state, means, 2L, 1L, "bad", 0), means[1:4])
})
