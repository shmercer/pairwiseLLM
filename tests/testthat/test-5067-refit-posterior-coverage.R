test_that("global posterior reconstruction aligns artifacts and applies each saved transform", {
  state <- task09_link_state()
  controller <- task09_transform_controller(state, "shift_scale")
  draws <- .adaptive_phase_b_global_metric_draws(state, controller)
  expected <- do.call(cbind, lapply(state$linking$phase_a$artifacts, `[[`, "posterior_draws"))
  expected[, c("c", "d")] <- 0.3 + 2 * expected[, c("c", "d")]
  expected[, c("e", "f")] <- -0.1 + 0.5 * expected[, c("e", "f")]
  expect_equal(unname(as.vector(draws)), unname(as.vector(expected)))
  expect_identical(colnames(draws), state$item_ids)
  bad <- state
  bad$linking$phase_a$required_sets <- integer()
  expect_error(.adaptive_phase_b_global_metric_draws(bad, controller), "non-empty.*required_sets")
  bad <- state
  bad$item_ids <- c(bad$item_ids, "missing")
  expect_error(.adaptive_phase_b_global_metric_draws(bad, controller), "full runtime item domain")
  state <- .adaptive_anchored_joint_sync_scaffolding(state)
  anchored <- .adaptive_phase_b_global_metric_draws_anchored_joint(state, 2L, 4L)
  accepted <- state$linking$anchored_joint$accepted_state_by_spoke[["2"]]
  expect_equal(colMeans(anchored$draws), accepted$theta_spoke_global_mean)
  expect_equal(apply(anchored$draws, 2, stats::sd), accepted$theta_spoke_global_sd)
  state$controller$link_refit_stats_by_spoke[["2"]] <-
    list(link_uncertainty_approximation = "cmdstan_posterior_draws")
  expect_error(.adaptive_phase_b_global_metric_draws_anchored_joint(state, 2L, 4L),
    "persisted authoritative posterior draws")
})

test_that("heldout metrics match exact keyed outcomes and do not borrow other epochs", {
  state <- task09_link_state()
  state$linking$probe$prediction_cache <- tibble::tibble(refit_id = c(1L, 1L, 2L, 2L),
    spoke_id = 2L, link_epoch_id = 1L, pair_key = rep(c("a:c", "b:d"), 2),
    pred_prob = c(0.2, 0.4, 0.3, 0.6))
  state$linking$probe$realized_edges <- tibble::tibble(spoke_id = 2L, link_epoch_id = 1L,
    pair_key = c("a:c", "b:d"), Y = c(1L, 0L))
  expect_equal(.adaptive_link_probe_metrics_current(state, 2L, 2L),
    list(probe_brier = mean(c(0.3^2, 0.4^2)), realized_n = 2L))
  expect_equal(.adaptive_link_probe_pred_rmse_lagged(state, 2L, 2L, 1L, 1L),
    sqrt(mean(c(0.1^2, 0.2^2))))
  expect_true(is.na(.adaptive_link_probe_pred_rmse_lagged(state, 2L, 2L, 1L, 2L)))
  expect_identical(.adaptive_link_probe_metrics_current(state, 99L, 2L)$realized_n, 0L)
  state$linking$probe$realized_edges$link_epoch_id <- 2L
  expect_identical(.adaptive_link_probe_metrics_current(state, 2L, 2L)$realized_n, 0L)
  state$linking$probe$realized_edges$link_epoch_id <- 1L
  state$linking$probe$realized_edges$Y <- NA_integer_
  expect_identical(.adaptive_link_probe_metrics_current(state, 2L, 2L)$realized_n, 0L)
  state$linking$probe$prediction_cache$pred_prob <- NA_real_
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
