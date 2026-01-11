# SPDX-License-Identifier: MIT

test_that("Mix guard blocks hybrid stage1 switch when bridge-edge frac is high", {
  samples <- tibble::tibble(
    ID = as.character(1:10),
    text = paste0("sample_", 1:10)
  )

  judge_fun <- function(pairs, ...) {
    # Deterministic outcomes.
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  # Minimal BT fit (runner requires a fit object when stage2 is possible).
  ids_local <- samples$ID
  fit_fun <- function(bt_data, ...) {
    list(
      engine = "bt",
      reliability = NA_real_,
      theta = tibble::tibble(ID = ids_local, theta = 0, se = 1),
      diagnostics = list()
    )
  }

  # RC fit with stable ranking across rounds.
  fit_rc_mock <- function(bt_data, ids = NULL, ...) {
    if (is.null(ids)) {
      ids <- sort(unique(c(as.character(bt_data$object1), as.character(bt_data$object2))))
    }
    list(
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), pi = rep(1 / length(ids), length(ids))),
      reliability = NA_real_,
      diagnostics = list()
    )
  }

  # Force the graph-state metrics to look connected but bottlenecked.
  graph_state_bottleneck <- function(pairs, ids, ...) {
    ids <- as.character(ids)
    deg <- rep(2, length(ids))
    names(deg) <- ids
    comp <- rep(1L, length(ids))
    names(comp) <- ids

    metrics <- tibble::tibble(
      n_nodes = as.integer(length(ids)),
      n_edges = as.integer(max(1L, length(ids) - 1L)),
      n_components = as.integer(1L),
      largest_component_frac = as.double(1),
      degree_min = as.double(2),
      degree_min_lcc = as.double(2),
      degree_median = as.double(2),
      degree_max = as.double(2),
      pct_nodes_with_degree_gt0 = as.double(1),
      bridge_edge_count = as.integer(1L),
      bridge_edge_frac = as.double(0.5)
    )

    list(metrics = metrics, degree = deg, component_id = comp)
  }

  testthat::local_mocked_bindings(
    .graph_state_from_pairs = graph_state_bottleneck,
    fit_rank_centrality = fit_rc_mock,
    .package = "pairwiseLLM"
  )

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_engine_running = "hybrid",
    fit_engine_final = "none",
    final_refit = FALSE,
    init_round_size = 4,
    round_size = 2,
    max_rounds = 3,
    min_rounds = 0,
    stage1_k_conn = 1L,
    stage1_k_stab = 1L,
    stage1_k_mix = 1L,
    stage1_check_mix_every = 1L,
    stage1_max_bridge_edge_frac = 0.02,
    stage1_stability_metric = "spearman",
    stage1_min_spearman = -1,
    stage1_allow_degenerate_stability = TRUE,
    fit_fun = fit_fun,
    verbose = FALSE
  )

  expect_true(is.data.frame(out$rounds))
  expect_true(all(out$rounds$stage == "stage1_rc"))
})


test_that("Mix guard permits hybrid stage1 switch when bridge-edge frac is low", {
  samples <- tibble::tibble(
    ID = as.character(1:10),
    text = paste0("sample_", 1:10)
  )

  judge_fun <- function(pairs, ...) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  ids_local <- samples$ID
  fit_fun <- function(bt_data, ...) {
    list(
      engine = "bt",
      reliability = NA_real_,
      theta = tibble::tibble(ID = ids_local, theta = 0, se = 1),
      diagnostics = list()
    )
  }

  fit_rc_mock <- function(bt_data, ids = NULL, ...) {
    if (is.null(ids)) {
      ids <- sort(unique(c(as.character(bt_data$object1), as.character(bt_data$object2))))
    }
    list(
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), pi = rep(1 / length(ids), length(ids))),
      reliability = NA_real_,
      diagnostics = list()
    )
  }

  graph_state_healthy <- function(pairs, ids, ...) {
    ids <- as.character(ids)
    deg <- rep(2, length(ids))
    names(deg) <- ids
    comp <- rep(1L, length(ids))
    names(comp) <- ids

    metrics <- tibble::tibble(
      n_nodes = as.integer(length(ids)),
      n_edges = as.integer(max(1L, length(ids) - 1L)),
      n_components = as.integer(1L),
      largest_component_frac = as.double(1),
      degree_min = as.double(2),
      degree_min_lcc = as.double(2),
      degree_median = as.double(2),
      degree_max = as.double(2),
      pct_nodes_with_degree_gt0 = as.double(1),
      bridge_edge_count = as.integer(0L),
      bridge_edge_frac = as.double(0)
    )

    list(metrics = metrics, degree = deg, component_id = comp)
  }

  testthat::local_mocked_bindings(
    .graph_state_from_pairs = graph_state_healthy,
    fit_rank_centrality = fit_rc_mock,
    .package = "pairwiseLLM"
  )

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_engine_running = "hybrid",
    fit_engine_final = "none",
    final_refit = FALSE,
    init_round_size = 4,
    round_size = 2,
    max_rounds = 4,
    min_rounds = 0,
    stage1_k_conn = 1L,
    stage1_k_stab = 1L,
    stage1_k_mix = 1L,
    stage1_check_mix_every = 1L,
    stage1_max_bridge_edge_frac = 0.02,
    stage1_stability_metric = "spearman",
    stage1_min_spearman = -1,
    stage1_allow_degenerate_stability = TRUE,
    stage2_min_rounds = 0L,
    fit_fun = fit_fun,
    verbose = FALSE
  )

  expect_true(any(out$rounds$stage == "stage2_bt"))
})


test_that("Mix guard blocks stopping when precision would otherwise stop", {
  samples <- tibble::tibble(
    ID = as.character(1:10),
    text = paste0("sample_", 1:10)
  )

  judge_fun <- function(pairs, ...) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  ids_local <- samples$ID
  fit_fun <- function(bt_data, ...) {
    list(
      engine = "bt",
      reliability = NA_real_,
      theta = tibble::tibble(ID = ids_local, theta = 0, se = 1),
      diagnostics = list()
    )
  }

  # Force precision stop to be TRUE each round.
  bt_should_stop_mock <- function(...) {
    list(stop = TRUE)
  }

  graph_state_bottleneck <- function(pairs, ids, ...) {
    ids <- as.character(ids)
    deg <- rep(2, length(ids))
    names(deg) <- ids
    comp <- rep(1L, length(ids))
    names(comp) <- ids

    metrics <- tibble::tibble(
      n_nodes = as.integer(length(ids)),
      n_edges = as.integer(max(1L, length(ids) - 1L)),
      n_components = as.integer(1L),
      largest_component_frac = as.double(1),
      degree_min = as.double(2),
      degree_min_lcc = as.double(2),
      degree_median = as.double(2),
      degree_max = as.double(2),
      pct_nodes_with_degree_gt0 = as.double(1),
      bridge_edge_count = as.integer(1L),
      bridge_edge_frac = as.double(0.5)
    )

    list(metrics = metrics, degree = deg, component_id = comp)
  }

  testthat::local_mocked_bindings(
    .graph_state_from_pairs = graph_state_bottleneck,
    bt_should_stop = bt_should_stop_mock,
    .package = "pairwiseLLM"
  )

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_engine_running = "bt",
    fit_engine_final = "none",
    final_refit = FALSE,
    init_round_size = 4,
    round_size = 2,
    max_rounds = 2,
    min_rounds = 0,
    stop_max_bridge_edge_frac = 0.02,
    stop_k_mix = 1L,
    stop_check_mix_every = 1L,
    stop_min_largest_component_frac = 0,
    stop_min_degree = 0L,
    fit_fun = fit_fun,
    verbose = FALSE
  )

  # We should not stop early despite precision wanting to stop.
  expect_true(is.data.frame(out$pairing_diagnostics))
  expect_true(any(out$pairing_diagnostics$stop_blocked_by == "mix_guard"))
  expect_true(is.na(out$stop_round) || out$stop_round > 1L)
})
