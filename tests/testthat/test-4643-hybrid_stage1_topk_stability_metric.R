# SPDX-License-Identifier: MIT

test_that("Hybrid stage1: top-k overlap stability metric respects thresholds and streaks", {
  samples <- tibble::tibble(
    ID = as.character(1:20),
    text = paste0("sample_", 1:20)
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
  # Include text columns to stay compatible with judge helpers that validate inputs.
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

    # Create deterministic RC scores whose top-5 set changes once.
    # Round 1: top-5 are IDs 1:5.
    # Round 2+: top-5 are IDs c(1,2,3,4,6), so overlap with round 1 is 4/5 = 0.8.
    if (calls$rc == 1L) {
      theta <- rev(seq_along(ids))
    } else {
      theta <- rev(seq_along(ids))
      theta[match("5", ids)] <- -999
      theta[match("6", ids)] <- 999
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
    stage1_k_stab = 2,
    stage1_stability_metric = "topk_jaccard",
    stage1_topk = 5,
    stage1_topk_overlap = 0.85,
    stage1_min_pct_nodes_with_degree_gt0 = 0,
    stage1_min_largest_component_frac = 0,
    stage1_min_degree_median = 0,
    stage1_min_degree_min_lcc = 0,
    stage1_min_degree_min = 0,
    verbose = FALSE
  )

  expect_true(is.na(out$rounds$topk_overlap_rc[1]))
  expect_equal(out$rounds$topk_overlap_rc[2], 0.8, tolerance = 1e-12)
  expect_equal(out$rounds$topk_overlap_rc[3], 1, tolerance = 1e-12)
  expect_equal(out$rounds$topk_overlap_rc[4], 1, tolerance = 1e-12)

  # Streak should require two consecutive passing rounds before switching.
  expect_equal(out$rounds$stab_streak[1:4], c(0, 0, 1, 2))
  expect_equal(out$rounds$pairing_stage[1:3], rep("stage1_rc", 3))
  expect_equal(out$rounds$pairing_stage[4], "stage2_bt")
})
