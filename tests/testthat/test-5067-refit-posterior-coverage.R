test_that("global posterior reconstruction cannot invent a pooled common posterior", {
  state <- task09_link_state()
  expect_error(.adaptive_phase_b_global_metric_draws(state),
    class = "pairwiseLLM_link_selector_unvalidated")
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    session <- start_link_session(link_contract_input(id, 2L))
    result <- .link_session_results(session)[[1L]]
    expect_identical(.link_orchestration_view(result)$covariance, result$uncertainty$covariance)
    expect_true(result$diagnostics$fit_valid)
  }
})

test_that("unversioned probe caches cannot supply stopping metrics", {
  state <- task09_link_state()
  state$linking$probe$prediction_cache <- tibble::tibble(refit_id = c(1L, 1L, 2L, 2L),
    spoke_id = 2L, link_epoch_id = 1L, pair_key = rep(c("a:c", "b:d"), 2),
    pred_prob = c(0.2, 0.4, 0.3, 0.6))
  state$linking$probe$realized_edges <- tibble::tibble(spoke_id = 2L, link_epoch_id = 1L,
    pair_key = c("a:c", "b:d"), Y = c(1L, 0L))
  expect_identical(.adaptive_link_probe_metrics_current(state, 2L, 2L),
    list(probe_brier = NA_real_, realized_n = 0L))
  expect_true(is.na(.adaptive_link_probe_pred_rmse_lagged(state, 2L, 2L, 1L, 1L)))
})

test_that("posterior predictive Brier integrates draws and preserves presentation effects", {
  edges <- tibble::tibble(hub_item = c("a", "b"), spoke_item = c("c", "d"),
    spoke_in_A = c(TRUE, FALSE), y_spoke = c(1L, 0L))
  attr(edges, "judge_params") <- list(beta = 0.2, epsilon = 0.1)
  hub <- c(a = -1, b = 1)
  spoke <- c(c = -0.5, d = 0.5)
  posterior <- list(delta = c(-0.1, 0.3), log_alpha = log(c(1, 2)),
    theta_hub = rbind(hub, hub + 0.2), theta_spoke = rbind(spoke, spoke - 0.1))
  expected <- mean(vapply(1:2, function(k) {
    eta <- posterior$delta[k] + exp(posterior$log_alpha[k]) * posterior$theta_spoke[k, ] -
      posterior$theta_hub[k, ] + c(0.2, -0.2)
    mean((c(1, 0) - (0.9 * plogis(eta) + 0.05))^2)
  }, numeric(1)))
  expect_equal(.adaptive_link_ppc_brier_cross(edges, hub, spoke, 0, posterior_draws = posterior), expected)
  posterior <- list(delta = rep(0, 201), log_alpha = 1)
  expected <- mean((c(1, 0) - (0.9 * plogis(spoke - hub + c(0.2, -0.2)) + 0.05))^2)
  expect_equal(.adaptive_link_ppc_brier_cross(edges, hub, spoke, 0, posterior_draws = posterior), expected)
})
