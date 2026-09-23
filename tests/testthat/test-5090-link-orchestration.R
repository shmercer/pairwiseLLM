# Provider-free contract fixtures for future orchestration; no adaptive selection.
orchestration_input <- function(id = "fixed_shape_offset", edges = 4L, spoke = "S") {
  args <- link_contract_args(id, edges)
  args$hub$items$global_item_id <- paste0("H_", args$hub$items$item_id)
  args$spoke$set_id <- spoke
  args$spoke$items$global_item_id <- paste0(spoke, "_", args$spoke$items$item_id)
  args$cross$B_set <- rep(spoke, edges)
  if (id == "joint_offset") {
    args$phase_a$spoke$observations$A_set <- spoke
    args$phase_a$spoke$observations$B_set <- spoke
  }
  do.call(prepare_link_input, args)
}
orchestration_probes <- function(spoke = "S") {
  data.frame(observation_id = c("probe-1", "probe-2"),
    A_set = c("H", spoke), A_item = c("b", "a"),
    B_set = c(spoke, "H"), B_item = c("a", "b"), y_A = c(1L, 0L))
}

test_that("common hooks expose each estimator's coordinates, scope and oriented prediction", {
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    fit <- fit_link(orchestration_input(id))
    probes <- orchestration_probes()
    pairs <- probes[, -6]
    view <- .link_orchestration_view(fit)
    expect_identical(view$covariance, fit$uncertainty$covariance)
    expect_identical(view$evidence_counts, fit$provenance$counts)
    expect_identical(view$evidence_hashes, fit$provenance$hashes)
    expect_identical(view$coordinates, colnames(view$covariance))
    expect_false(view$adaptive_d_optimal_supported)
    expect_identical(.link_orchestration_result(fit, "S"), fit)
    expect_error(.link_orchestration_result(fit, "T"), "identity mismatch")
    gradient <- .link_candidate_gradient(fit, pairs)
    expect_equal(gradient[1, ], -gradient[2, ])
    expect_equal(unname(gradient[, "delta"]), c(-1, 1))
    expect_identical(dim(.link_candidate_gradient(fit, pairs[0, ])), c(0L, ncol(gradient)))
    expected <- predict_link(fit, pairs)
    expect_identical(.adaptive_link_predictive_prob_oriented(fit, list(), "S",
      c("H_b", "S_a"), c("S_a", "H_b")), expected)
    expect_gt(abs(sum(expected) - 1), 1e-4)
    scores <- .adaptive_link_phase_b_routing_scores(fit, list(), c("S_a", "H_b"), "H", "S")
    expect_identical(unname(scores), fit$items$theta_link_mean[c(3, 2)])
    expect_identical(.adaptive_link_theta_global_map_for_items(fit, list(), c("S_a", "H_b"), "S"), scores)
    expect_identical(.adaptive_link_predictive_utility_context(fit, list(), "S"), view)
    expect_error(.adaptive_link_phase_b_routing_scores(fit, list(), "S_a", "T", "S"), "hub identity")
  }
})

test_that("routing and candidate endpoints fail explicitly instead of falling back", {
  fit <- fit_link(orchestration_input())
  expect_error(.link_routing_scores(fit, "missing"), "Unknown global")
  expect_error(.link_global_pairs(fit, "H_a", character()), "equal lengths")
  no_globals <- fit_link(link_contract_input(edges = 2L))
  expect_error(.link_routing_scores(no_globals, "a"), "explicit unique global")
  expect_error(.link_orchestration_result(list()), "common E1--E3")
  bad <- link_contract_result(valid = FALSE)
  expect_error(.link_routing_scores(bad, "a"), "invalid linking fit")
})

test_that("held-out metrics use the prediction API without changing any estimator evidence", {
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    input <- orchestration_input(id)
    fit <- fit_link(input)
    before <- serialize(fit, NULL)
    probes <- orchestration_probes()
    p <- predict_link(fit, probes[, -6])
    panel <- data.frame(observation_id = probes$observation_id,
      hub_bin = c(1L, 1L), spoke_bin = c(2L, 2L))
    metrics <- .link_probe_metrics(fit, probes, fit, panel)
    expect_equal(metrics$probe_brier, mean((p - probes$y_A)^2))
    expect_identical(metrics$probe_pred_rmse_lagged, 0)
    expect_identical(metrics$probe_rank_bins_hub_covered, 1L)
    expect_identical(metrics$probe_rank_bins_spoke_covered, 1L)
    probes$y_A <- 1L - probes$y_A
    altered <- .link_probe_metrics(fit, probes, fit)
    expect_false(identical(metrics$probe_brier, altered$probe_brier))
    expect_identical(metrics$input_hash, altered$input_hash)
    expect_identical(metrics$cross_evidence_hash, altered$cross_evidence_hash)
    expect_false(identical(metrics$probe_evidence_hash, altered$probe_evidence_hash))
    expect_identical(serialize(fit, NULL), before)
    expect_identical(fit$continuation$input, input)
    expect_identical(.link_routing_scores(fit, c("H_a", "S_b")),
      stats::setNames(fit$items$theta_link_mean[c(1, 4)], c("H_a", "S_b")))
    expect_identical(tibble::as_tibble(metrics)$estimator_id, id)
    expect_true(is.na(.link_probe_metrics(fit, probes[0, ])$probe_brier))
    expect_true(is.na(.link_probe_metrics(fit, probes)$probe_pred_rmse_lagged))
  }
})

test_that("probe separation rejects observation and base-pair overlap in either orientation", {
  fit <- fit_link(orchestration_input("joint_offset"))
  probes <- orchestration_probes()
  for (id in c("cross-1", "within-h", "within-s")) {
    bad <- probes; bad$observation_id[1] <- id
    expect_error(.link_probe_metrics(fit, bad), "IDs overlap")
  }
  bad <- probes; bad$A_item <- c("a", "b"); bad$B_item <- c("b", "a")
  expect_error(.link_probe_metrics(fit, bad), "base pairs overlap")
  expect_error(.link_probe_metrics(fit, bad[2, ]), "base pairs overlap")
  bad <- probes; bad$observation_id[] <- "duplicate"
  expect_error(.link_probe_metrics(fit, bad), "unique")
  panel <- data.frame(observation_id = "probe-1", hub_bin = -1, spoke_bin = 1)
  expect_error(.link_probe_metrics(fit, probes, panel = panel), "rank bins")
})

test_that("stopping preserves missingness, uncertainty scope, identification and lag gates", {
  probes <- orchestration_probes()
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    fit <- fit_link(orchestration_input(id))
    metrics <- .link_stop_metrics(fit, probes)
    expect_false(metrics$link_stop_pass_now)
    expect_match(metrics$link_stop_blocker_codes, "lag_not_eligible")
    expect_match(metrics$link_stop_blocker_codes, "min_refits_not_met")
    expect_identical(metrics$estimator_id, id)
    expect_identical(metrics$uncertainty_scope, fit$diagnostics$uncertainty_scope)
    if (id == "fixed_shape_offset") {
      expect_true(is.na(metrics$reliability_link_global))
      expect_match(metrics$link_stop_blocker_codes, "reliability_link_global")
    }
    assessed <- .link_stop_metrics(fit, probes, fit, refits_in_epoch = 3L, lag_eligible = TRUE)
    expect_identical(assessed$theta_global_rmse_lagged, 0)
    expect_identical(assessed$probe_pred_rmse_lagged, 0)
    expect_false(assessed$adaptive_d_optimal_supported)
    prior <- fit_link(orchestration_input(id, 0L))
    expect_match(.link_stop_metrics(prior, probes)$link_stop_blocker_codes, "diagnostics_failed")
    expect_error(.link_stop_metrics(fit, probes, refits_in_epoch = 0), "refits_in_epoch")
    expect_error(.link_stop_metrics(fit, probes, lag_eligible = NA), "lag_eligible")
    expect_error(.link_stop_metrics(fit, probes,
      controller = list(theta_global_rmse_scope = "wrong")), "scope")
    expect_error(.link_probe_metrics(fit, probes, fit_link(orchestration_input(id, spoke = "T"))),
      "changed spoke")
  }
  bad <- link_contract_result(link_contract_input("joint_offset", 2L), valid = FALSE)
  metrics <- .link_stop_metrics(bad, probes)
  expect_false(metrics$fit_valid)
  expect_false(metrics$link_stop_pass_now)
  expect_true(is.na(metrics$probe_brier))
  missing <- link_contract_result(link_contract_input("gaussian_posterior_bridge", 0L), covariance = FALSE)
  metrics <- .link_stop_metrics(missing, probes[0, ])
  expect_true(is.na(metrics$reliability_link_global))
})

test_that("multi-spoke hooks and serialization retain distinct hub posteriors and logs", {
  input <- orchestration_input("gaussian_posterior_bridge")
  other <- orchestration_input("gaussian_posterior_bridge", 2L, "T")
  session <- start_link_session(list(input, other))
  expect_error(.link_orchestration_result(session), "explicit spoke_id")
  expect_error(.link_orchestration_result(session, "U"), "Unknown")
  s <- .link_orchestration_result(session, "S")
  t <- .link_orchestration_result(session, "T")
  expect_false(identical(.link_routing_scores(s, "H_a"), .link_routing_scores(t, "H_a")))
  rows <- dplyr::bind_rows(.link_stop_metrics(s, orchestration_probes()),
    .link_stop_metrics(t, orchestration_probes("T")))
  expect_identical(rows$spoke_set_id, c("S", "T"))
  path <- file.path(withr::local_tempdir(), "session.rds")
  save_link_session(session, path)
  restored <- load_link_session(path)
  expect_identical(.link_orchestration_result(restored, "S"), s)
  resumed <- resume_link_session(restored, orchestration_input("gaussian_posterior_bridge", 6L))
  expect_identical(.link_orchestration_result(resumed, "T"), t)
  expect_identical(.link_probe_metrics(s, orchestration_probes()),
    .link_probe_metrics(.link_orchestration_result(restored, "S"), orchestration_probes()))
})

test_that("direct fitting and continuation are independent of selector configuration and calls", {
  testthat::local_mocked_bindings(
    select_next_pair = function(...) stop("selector called"),
    .adaptive_link_attach_predictive_utility = function(...) stop("utility called"),
    .adaptive_link_d_opt_state_get = function(...) stop("Fisher called"), .package = "pairwiseLLM")
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    input <- orchestration_input(id)
    state <- start_link_session(input)
    before <- .link_orchestration_result(state)
    for (config in list(list(), list(link_estimation_mode = "anchored_joint"),
                        list(pairing_strategy = "random", probe_panel_edges = 100L))) {
      expect_identical(.adaptive_link_predictive_utility_context(state, config, "S"),
        .link_orchestration_view(before))
      expect_identical(.link_orchestration_result(state)$continuation$input, input)
    }
    resumed <- resume_link_session(state, orchestration_input(id, 6L))
    fresh <- fit_link(orchestration_input(id, 6L))
    expect_identical(.link_orchestration_result(resumed)$items, fresh$items)
  }
})

test_that("D-optimal entrypoints reject every estimator and legacy alias", {
  error <- "pairwiseLLM_link_selector_unvalidated"
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset", "anchored_joint", "transform")) {
    expect_error(.adaptive_link_attach_predictive_utility(NULL, NULL,
      list(link_estimation_mode = id), 2L), class = error)
    expect_error(.adaptive_link_d_opt_state_get(list(), 1L, 2L, NULL, id), class = error)
    expect_error(.adaptive_link_d_opt_matrix_dim(NULL, id), class = error)
  }
  for (mode in c("linking_d_optimal", .adaptive_linking_d_optimal_utility_modes())) {
    expect_error(.adaptive_linking_selection_order(data.frame(), mode), class = error)
  }
  expect_error(.link_guard_adaptive_selection(start_link_session(orchestration_input())), class = error)
  expect_error(.link_guard_adaptive_selection(fit_link(orchestration_input())), class = error)
  expect_true(.link_guard_adaptive_selection(list(), list(run_mode = "within_set")))
  row <- data.frame(is_cross_set = TRUE, run_mode = "link_one_spoke", is_probe_step = FALSE)
  expect_error(.adaptive_link_d_opt_update_after_commit(NULL, list(), row), class = error)
  row$is_probe_step <- TRUE
  expect_identical(.adaptive_link_d_opt_update_after_commit(NULL, list(ok = TRUE), row), list(ok = TRUE))
  row$is_cross_set <- FALSE
  expect_identical(.adaptive_link_d_opt_update_after_commit(NULL, list(), row), list())
})

test_that("per-refit stopping respects score domains and cannot promote unidentifiable fits", {
  fit <- fit_link(orchestration_input("gaussian_posterior_bridge"))
  probes <- orchestration_probes()
  controls <- list(link_stop_reliability_min = 0, probe_edges_min_for_stop = 1L,
    probe_brier_max = 1, min_refits_in_phase_b = 1L)
  for (scope in c("direct_evidence_spoke", "all_spoke_items", "min_cross_set_edges_k")) {
    controls$theta_global_rmse_scope <- scope
    metrics <- .link_stop_metrics(fit, probes, fit, controller = controls, lag_eligible = TRUE)
    expect_true(metrics$link_stop_pass_now)
    expect_identical(metrics$theta_global_rmse_lagged, 0)
    expect_false(metrics$adaptive_d_optimal_supported)
  }
  controls$min_cross_set_edges_k <- 100L
  metrics <- .link_stop_metrics(fit, probes, fit, controller = controls, lag_eligible = TRUE)
  expect_false(metrics$link_stop_pass_now)
  expect_true(is.na(metrics$theta_global_rmse_lagged))
  args <- link_contract_args("gaussian_posterior_bridge", 2L)
  args$judge$epsilon <- 1
  flat <- fit_link(do.call(prepare_link_input, args))
  expect_identical(flat$offset$identification, "unidentified")
  metrics <- .link_stop_metrics(flat, probes, flat, controller = controls, lag_eligible = TRUE)
  expect_false(metrics$link_stop_pass_now)
  expect_match(metrics$link_stop_blocker_codes, "diagnostics_failed")
  longer <- fit_link(orchestration_input("gaussian_posterior_bridge", 6L))
  expect_error(.link_probe_metrics(fit, probes, longer), "unchanged old evidence prefix")
})

test_that("probe quality formulas and threshold boundaries are unchanged", {
  p <- c(.1, .4, .5, .6, .9)
  y <- c(0L, 1L, 0L, 1L, 1L)
  q <- .link_probe_quality(p, y, letters[1:5], LETTERS[1:5], 1:5, 1:5,
    list(probe_edges_min_for_stop = 5L))
  expect_equal(q$probe_near_boundary_frac, 3/5)
  expect_equal(q$probe_extreme_frac, 2/5)
  expect_equal(q$probe_midrange_frac, 3/5)
  expect_equal(q$probe_brier_near_boundary, mean((p[2:4] - y[2:4])^2))
  expect_true(q$probe_near_boundary_pass)
  expect_false(q$probe_extreme_frac_pass)
  expect_match(q$probe_quality_blocker_codes, "probe_extreme_frac")
  q <- .link_probe_quality(c(.4, .6), c(0L, 1L), c("h1", "h2"), c("s1", "s2"), 1:2, 1:2,
    list(probe_edges_min_for_stop = 2L, probe_ece_max = .5))
  expect_true(q$probe_quality_pass)
  expect_identical(q$probe_quality_blocker_codes, "none")
  q <- .link_probe_quality(c(.01, .99), c(0L, 1L), c("h1", "h2"), c("s1", "s2"))
  expect_true(is.na(q$probe_brier_near_boundary))
  expect_false(q$probe_brier_near_boundary_pass)
})

test_that("full-uncertainty reliability retains the active-item domain", {
  result <- fit_link(orchestration_input("gaussian_posterior_bridge"))
  # Frozen cross rows use H:a against S:b, so H:b is outside the active domain.
  items <- result$items
  active <- !(items$set_id == "H" & items$item_id == "b")
  expected <- .adaptive_link_reliability_decomposition(items$theta_link_mean[active],
    items$theta_link_sd[active]^2, 1e-6, 1e-6)$reliability
  metrics <- .link_stop_metrics(result, orchestration_probes())
  expect_equal(metrics$reliability_link_global, expected)
})
