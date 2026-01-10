test_that("bt_run_adaptive enforces stage2_min_rounds before allowing precision/stability stops", {
  samples <- tibble::tibble(
    ID = LETTERS[1:4],
    text = paste0("t", LETTERS[1:4])
  )

  # Deterministic judge: always select the left item.
  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  # Mock fit: stable theta each round, high reliability/sepG so precision would
  # otherwise be satisfied immediately.
  fit_fun <- function(bt_data, ...) {
    ids <- sort(unique(c(as.character(bt_data[[1]]), as.character(bt_data[[2]]))))
    list(
      engine = "mock",
      reliability = 0.99,
      theta = tibble::tibble(ID = ids, theta = rep(0, length(ids)), se = rep(0.1, length(ids))),
      diagnostics = list(sepG = 10)
    )
  }

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    fit_engine_running = "hybrid",
    # Switch to stage2 as soon as possible.
    stage1_k_conn = 1L,
    stage1_k_stab = 1L,
    stage1_min_pct_nodes_with_degree_gt0 = 0,
    stage1_min_largest_component_frac = 0,
    stage1_min_degree_median = 0,
    stage1_min_degree_min = 0,
    stage1_min_spearman = -1,
    # Force a stage1->stage2 switch after the first round.
    stage1_max_rounds = 1L,
    # Enforce at least 2 BT rounds in stage2.
    stage2_min_rounds = 2L,
    round_size = 2,
    init_round_size = 2,
    max_rounds = 5,
    # Prevent early stopping inside stage1; we want to test the stage2 gate.
    min_rounds = 2L,
    # Make stability easy so that (absent the stage2 gate) we'd stop as soon as
    # we enter stage2.
    stop_stability_consecutive = 1L,
    stop_stability_rms = 1e9,
    stop_topk_overlap = 0,
    stop_topk = 2L,
    stop_min_largest_component_frac = NA_real_,
    stop_min_degree = NA_integer_,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    seed_pairs = 1
  )

  # Stage2 begins at round 2 (since stage1_max_rounds=1). With stage2_min_rounds=2,
  # the stop must be blocked until stage2_rounds >= 2, so earliest possible stop is round 3.
  expect_true(is.finite(out$stop_round))
  expect_gte(out$stop_round, 3L)

  # The rounds table should record stage2 progress.
  expect_true("stage2_rounds" %in% names(out$rounds))
  expect_true(max(out$rounds$stage2_rounds, na.rm = TRUE) >= 2L)
})
