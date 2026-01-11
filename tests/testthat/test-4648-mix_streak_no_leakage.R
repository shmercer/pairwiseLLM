# SPDX-License-Identifier: MIT

test_that("Stage1 mixing streak does not leak into stop-stage mixing streak", {
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

  # Precision should be eligible immediately (mix guard will be the blocker).
  bt_should_stop_mock <- function(...) {
    list(stop = TRUE)
  }

  # Always a well-connected graph with a low bridge-edge fraction.
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
    bt_should_stop = bt_should_stop_mock,
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
    max_rounds = 6,
    min_rounds = 0,
    stage1_k_conn = 1L,
    stage1_k_stab = 1L,
    stage1_k_mix = 2L,
    stage1_check_mix_every = 1L,
    stage1_max_bridge_edge_frac = 0.02,
    stage1_stability_metric = "spearman",
    stage1_min_spearman = -1,
    stage1_allow_degenerate_stability = TRUE,
    stage2_min_rounds = 0L,
    stop_max_bridge_edge_frac = 0.02,
    stop_k_mix = 2L,
    stop_check_mix_every = 1L,
    fit_fun = fit_fun,
    verbose = FALSE
  )

  expect_true(is.data.frame(out$rounds))

  idx_stage2 <- which(out$rounds$stage == "stage2_bt")
  expect_true(length(idx_stage2) >= 1L)

  first_stage2 <- idx_stage2[[1]]
  # Mixing streak should be reset at the stage boundary.
  expect_equal(out$rounds$mix_streak[[first_stage2]], 0L)

  # Stop mixing guard requires 2 consecutive checks in stage2, so stopping cannot
  # occur on the first stage2 round.
  idx_stop <- which(isTRUE(out$rounds$stop))
  if (length(idx_stop) >= 1L) {
    expect_true(idx_stop[[1]] > first_stage2)
  }
})
