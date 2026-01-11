# SPDX-License-Identifier: MIT

test_that("Mix check scheduling is recorded and streak increments only on check rounds", {
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

  # Precision should always be eligible (mix guard will decide).
  bt_should_stop_mock <- function(...) {
    list(stop = TRUE)
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
    max_rounds = 6,
    min_rounds = 0,
    stop_max_bridge_edge_frac = 0.02,
    stop_k_mix = 3L,
    stop_check_mix_every = 2L,
    fit_fun = fit_fun,
    verbose = FALSE
  )

  rounds <- out$rounds
  expect_true(is.data.frame(rounds))

  # stop_k_mix=3 and stop_check_mix_every=2 => checks should occur on rounds 2, 4, 6.
  expect_equal(rounds$mix_checked_this_round[1:6], c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))

  # Streak should only advance on check rounds.
  expect_equal(rounds$mix_streak[1:6], c(0L, 1L, 1L, 2L, 2L, 3L))

  # Stop should not occur until the required number of checks have passed.
  expect_true(isTRUE(rounds$stop[[6]]))
})
