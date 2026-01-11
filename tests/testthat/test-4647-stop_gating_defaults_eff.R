# SPDX-License-Identifier: MIT

testthat::test_that("bt_run_adaptive applies effective stop gating defaults when stop_min_* are NA", {
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

  graph_state_unhealthy <- function(pairs, ids, ...) {
    ids <- as.character(ids)
    deg <- rep(0, length(ids))
    names(deg) <- ids
    comp <- rep(1L, length(ids))
    comp[seq_len(max(1L, floor(length(ids) / 2)))] <- 2L
    names(comp) <- ids

    metrics <- tibble::tibble(
      n_nodes = as.integer(length(ids)),
      n_edges = as.integer(max(1L, length(ids) - 1L)),
      n_components = as.integer(2L),
      largest_component_frac = as.double(0.9),
      degree_min = as.double(0),
      degree_min_lcc = as.double(0),
      degree_median = as.double(0),
      degree_max = as.double(0),
      pct_nodes_with_degree_gt0 = as.double(0),
      bridge_edge_count = as.integer(0L),
      bridge_edge_frac = as.double(0)
    )

    list(metrics = metrics, degree = deg, component_id = comp)
  }

  testthat::local_mocked_bindings(
    .graph_state_from_pairs = graph_state_unhealthy,
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
    max_rounds = 1,
    min_rounds = 0,
    fit_fun = fit_fun,
    verbose = FALSE
  )

  testthat::expect_true(is.data.frame(out$metrics))
  testthat::expect_true(all(c(
    "stop_min_degree_eff",
    "stop_min_largest_component_frac_eff",
    "stop_gating_active"
  ) %in% names(out$metrics)))

  testthat::expect_equal(out$metrics$stop_min_degree_eff[[1]], 1L)
  testthat::expect_equal(out$metrics$stop_min_largest_component_frac_eff[[1]], 0.98)
  testthat::expect_true(isTRUE(out$metrics$stop_gating_active[[1]]))
  testthat::expect_false(isTRUE(out$metrics$graph_healthy[[1]]))
})


testthat::test_that("bt_run_adaptive stop_min_* overrides effective defaults (can disable gating)", {
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

  graph_state_unhealthy <- function(pairs, ids, ...) {
    ids <- as.character(ids)
    deg <- rep(0, length(ids))
    names(deg) <- ids
    comp <- rep(1L, length(ids))
    comp[seq_len(max(1L, floor(length(ids) / 2)))] <- 2L
    names(comp) <- ids

    metrics <- tibble::tibble(
      n_nodes = as.integer(length(ids)),
      n_edges = as.integer(max(1L, length(ids) - 1L)),
      n_components = as.integer(2L),
      largest_component_frac = as.double(0.9),
      degree_min = as.double(0),
      degree_min_lcc = as.double(0),
      degree_median = as.double(0),
      degree_max = as.double(0),
      pct_nodes_with_degree_gt0 = as.double(0),
      bridge_edge_count = as.integer(0L),
      bridge_edge_frac = as.double(0)
    )

    list(metrics = metrics, degree = deg, component_id = comp)
  }

  testthat::local_mocked_bindings(
    .graph_state_from_pairs = graph_state_unhealthy,
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
    max_rounds = 1,
    min_rounds = 0,
    stop_min_degree = 0L,
    stop_min_largest_component_frac = 0,
    fit_fun = fit_fun,
    verbose = FALSE
  )

  testthat::expect_equal(out$metrics$stop_min_degree_eff[[1]], 0L)
  testthat::expect_equal(out$metrics$stop_min_largest_component_frac_eff[[1]], 0)
  testthat::expect_true(isTRUE(out$metrics$stop_gating_active[[1]]))
  testthat::expect_true(isTRUE(out$metrics$graph_healthy[[1]]))
})
