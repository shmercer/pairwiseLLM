test_that("degree_min_lcc ignores unseen nodes for hybrid switching", {
  # 20 items, with one unseen item (degree 0). The other 19 items form a cycle.
  samples <- tibble::tibble(
    ID = sprintf("I%02d", 1:20),
    text = sprintf("text_%02d", 1:20)
  )

  seen_ids <- samples$ID[1:19]
  cycle_edges <- tibble::tibble(
    ID1 = seen_ids,
    ID2 = c(seen_ids[-1], seen_ids[1]),
    # Break perfect cycle symmetry so Rank Centrality isn't degenerate.
    better_id = ifelse(seen_ids == seen_ids[1], c(seen_ids[-1], seen_ids[1])[1], seen_ids)
  )

  gs <- pairwiseLLM:::.graph_state_from_pairs(cycle_edges, ids = samples$ID)
  gm <- gs$metrics

  expect_identical(as.double(gm$degree_min), 0)
  expect_true(as.double(gm$degree_min_lcc) >= 1)

  judge_fun <- function(pairs) {
    pairs <- tibble::as_tibble(pairs)
    tibble::tibble(
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = pairs$ID1
    )
  }

  # Minimal BT-like fit: theta = (wins - losses), constant se.
  fit_fun <- function(bt_data, engine = NULL, verbose = FALSE, return_diagnostics = FALSE, include_residuals = FALSE) {
    bt_data <- tibble::as_tibble(bt_data)

    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    wins <- stats::setNames(rep(0, length(ids)), ids)
    losses <- stats::setNames(rep(0, length(ids)), ids)

    for (i in seq_len(nrow(bt_data))) {
      o1 <- as.character(bt_data$object1[[i]])
      o2 <- as.character(bt_data$object2[[i]])
      res <- bt_data$result[[i]]
      if (isTRUE(res == 1)) {
        wins[[o1]] <- wins[[o1]] + 1
        losses[[o2]] <- losses[[o2]] + 1
      } else {
        wins[[o2]] <- wins[[o2]] + 1
        losses[[o1]] <- losses[[o1]] + 1
      }
    }

    theta <- wins - losses

    list(
      engine = "mock",
      theta = tibble::tibble(ID = ids, theta = as.double(theta[ids]), se = rep(1, length(ids))),
      reliability = NA_real_,
      diagnostics = NULL
    )
  }

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    initial_results = cycle_edges,
    fit_fun = fit_fun,
    build_bt_fun = build_bt_data,
    fit_engine_running = "hybrid",
    stage1_stability_metric = "spearman",
    init_round_size = 0,
    round_size = 1,
    max_rounds = 2,
    min_rounds = 0,
    stop_min_degree = 0,
    stop_min_largest_component_frac = 0,
    stage1_k_conn = 1L,
    stage1_k_stab = 1L,
    stage1_min_spearman = -1,
    seed = 123
  )

  expect_true("degree_min_lcc" %in% names(out$metrics))
  expect_true(any(out$rounds$pairing_stage == "stage2_bt"))
})
