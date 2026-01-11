# SPDX-License-Identifier: MIT

test_that("Hybrid stage1: degenerate Spearman stability does not switch by default", {
  samples <- tibble::tibble(
    ID = as.character(1:12),
    text = paste0("sample_", 1:12)
  )

  initial_results <- tibble::tibble(
    ID1 = "1",
    ID2 = "2",
    better_id = "1"
  )

  judge_fun <- function(pairs, ...) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  # Deterministic pair selection: return a fixed pair each round.
  # Include text columns so judge helpers that validate inputs (e.g. simulate_bt_judge)
  # remain compatible if this binding is ever referenced.
  select_mock <- function(samples, ...) {
    t1 <- samples$text[match("1", samples$ID)]
    t2 <- samples$text[match("3", samples$ID)]
    tibble::tibble(ID1 = "1", text1 = t1, ID2 = "3", text2 = t2)
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

  calls <- new.env(parent = emptyenv())
  calls$rc <- 0L

  fit_rc_mock <- function(bt_data, ids = NULL, ...) {
    calls$rc <- calls$rc + 1L

    if (is.null(ids)) {
      ids <- sort(unique(c(as.character(bt_data$object1), as.character(bt_data$object2))))
    }

    # First two calls are completely degenerate (all equal), then become informative.
    theta <- if (calls$rc <= 2L) {
      rep(0, length(ids))
    } else {
      seq_along(ids)
    }

    list(
      theta = tibble::tibble(ID = ids, theta = theta, pi = rep(1 / length(ids), length(ids))),
      reliability = NA_real_,
      diagnostics = list()
    )
  }

  testthat::local_mocked_bindings(
    select_adaptive_pairs = select_mock,
    fit_rank_centrality = fit_rc_mock,
    .package = "pairwiseLLM"
  )

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_engine_running = "hybrid",
    fit_engine_final = "bt_mle",
    initial_results = initial_results,
    init_round_size = 0,
    round_size = 1,
    max_rounds = 4,
    min_rounds = 5,
    fit_fun = fit_fun,
    stage1_k_conn = 1,
    stage1_k_stab = 1,
    stage1_stability_metric = "spearman",
    stage1_min_spearman = 0.9,
    stage1_min_pct_nodes_with_degree_gt0 = 0,
    stage1_min_largest_component_frac = 0,
    stage1_min_degree_median = 0,
    stage1_min_degree_min_lcc = 0,
    stage1_min_degree_min = 0,
    verbose = FALSE
  )

  # Degenerate ranks produce NA Spearman and should not count as stable.
  expect_equal(out$rounds$pairing_stage[1:3], rep("stage1_rc", 3))
  expect_equal(out$rounds$pairing_stage[4], "stage2_bt")

  expect_true(is.na(out$rounds$rho_spearman_rc[2]))
  expect_true(is.na(out$rounds$rho_spearman_rc[3]))
  expect_false(out$rounds$stab_ok[2])
})

test_that("Hybrid stage1: stage1_allow_degenerate_stability preserves legacy behavior", {
  samples <- tibble::tibble(
    ID = as.character(1:12),
    text = paste0("sample_", 1:12)
  )

  initial_results <- tibble::tibble(
    ID1 = "1",
    ID2 = "2",
    better_id = "1"
  )

  judge_fun <- function(pairs, ...) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  select_mock <- function(samples, ...) {
    t1 <- samples$text[match("1", samples$ID)]
    t2 <- samples$text[match("3", samples$ID)]
    tibble::tibble(ID1 = "1", text1 = t1, ID2 = "3", text2 = t2)
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

  calls <- new.env(parent = emptyenv())
  calls$rc <- 0L

  fit_rc_mock <- function(bt_data, ids = NULL, ...) {
    calls$rc <- calls$rc + 1L

    if (is.null(ids)) {
      ids <- sort(unique(c(as.character(bt_data$object1), as.character(bt_data$object2))))
    }

    # Always degenerate.
    theta <- rep(0, length(ids))

    list(
      theta = tibble::tibble(ID = ids, theta = theta, pi = rep(1 / length(ids), length(ids))),
      reliability = NA_real_,
      diagnostics = list()
    )
  }

  testthat::local_mocked_bindings(
    select_adaptive_pairs = select_mock,
    fit_rank_centrality = fit_rc_mock,
    .package = "pairwiseLLM"
  )

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_engine_running = "hybrid",
    fit_engine_final = "bt_mle",
    initial_results = initial_results,
    init_round_size = 0,
    round_size = 1,
    max_rounds = 2,
    min_rounds = 3,
    fit_fun = fit_fun,
    stage1_k_conn = 1,
    stage1_k_stab = 1,
    stage1_stability_metric = "spearman",
    stage1_allow_degenerate_stability = TRUE,
    stage1_min_spearman = 0.9,
    stage1_min_pct_nodes_with_degree_gt0 = 0,
    stage1_min_largest_component_frac = 0,
    stage1_min_degree_median = 0,
    stage1_min_degree_min_lcc = 0,
    stage1_min_degree_min = 0,
    verbose = FALSE
  )

  # With legacy behavior, degenerate ranks count as perfectly stable and allow switching.
  expect_equal(out$rounds$pairing_stage[1], "stage1_rc")
  expect_equal(out$rounds$pairing_stage[2], "stage2_bt")
  expect_equal(out$rounds$rho_spearman_rc[2], 1)
  expect_true(out$rounds$stab_ok[2])
})
