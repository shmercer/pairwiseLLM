test_that("bt_run_adaptive hybrid switches from RC to BT after gates", {
  # Deterministic toy setup: 4 items, seeded selection, deterministic judge.
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("a", "b", "c", "d")
  )

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

  # Seeded initial results to avoid bootstrap randomness.
  initial_results <- tibble::tibble(
    ID1 = c("A", "B"),
    ID2 = c("C", "D"),
    better_id = c("A", "B")
  )

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    initial_results = initial_results,
    fit_fun = fit_fun,
    build_bt_fun = build_bt_data,
    fit_engine_running = "hybrid",
    init_round_size = 0,
    round_size = 1,
    max_rounds = 2,
    min_rounds = 0,
    stop_min_degree = 0,
    stop_min_largest_component_frac = 0,
    stage1_k_conn = 1L,
    stage1_k_stab = 1L,
    stage1_min_pct_nodes_with_degree_gt0 = 0,
    stage1_min_largest_component_frac = 0,
    stage1_min_degree_median = 0,
    stage1_min_degree_min = 0,
    stage1_min_spearman = -1,
    stage1_max_rounds = 10L,
    seed = 123
  )

  expect_true(is.data.frame(out$rounds))
  expect_true("stage" %in% names(out$rounds))
  expect_true(any(out$rounds$stage == "stage2_bt"))
})
