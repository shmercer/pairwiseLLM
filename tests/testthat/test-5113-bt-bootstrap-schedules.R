test_that("all adaptive strategies use canonical execution with frozen initial comparisons", {
  for (strategy in c("random", "trueskill_p50", "trueskill_pollitt", "hybrid")) {
    input <- bootstrap_state(strategy, if (strategy == "hybrid") letters[1:12] else letters[1:6])
    budget <- if (strategy == "hybrid") 16L else 10L
    saved <- serialize(input, NULL)
    out <- bootstrap_adaptive(input, n_rep = 2L, budget = budget,
      btl_config = list(refit_pairs_target = 100L), keep = "full")
    expect_identical(serialize(input, NULL), saved)
    expect_true(all(out$replicates$n_comparisons == budget))
    expect_true(all(out$replicates$connected))
    expect_equal(out$provenance$initial_state$warm_start_pairs, input$warm_start_pairs)
    expect_length(unique(out$replicates$selector), 2L)
    init_rows <- seq_len(input$n_items - 1L)
    initial <- out$artifacts[[1]]$comparisons[init_rows, 1:2]
    for (i in seq_len(2L)) {
      art <- out$artifacts[[i]]
      expect_identical(art$comparisons[init_rows, 1:2], initial)
      expect_identical(art$state$controller$pairing_strategy, strategy)
      expect_true(all(art$state$history_state$pair_count <= art$state$controller$dup_max_obs_relaxed))
      state <- out$provenance$initial_state
      state$meta$seed <- out$replicates$selector[i]
      judge <- pairwiseLLM:::.bt_bootstrap_judge(out$provenance$theta, out$replicates$outcome[i], budget)
      canonical <- pairwiseLLM:::.bt_bootstrap_with_seed(out$replicates$selector[i], {
        while (nrow(state$history_pairs) < budget) {
          state <- adaptive_rank_run_live(state, judge, n_steps = 1L, progress = "none",
            adaptive_config = list(max_pairs_after_stop = budget), btl_config = list(refit_pairs_target = 100L))
        }
        state
      })
      fields <- c("A_id", "B_id", "Y", "round_stage", "pairing_strategy", "target_distance")
      expect_identical(art$state$step_log[, fields], canonical$step_log[, fields])
      expect_identical(art$state$trueskill_state, canonical$trueskill_state)
    }
  }
})

test_that("p50 and Pollitt schedules respond to simulated early outcomes at the same selector seed", {
  for (strategy in c("trueskill_p50", "trueskill_pollitt")) {
    initial <- bootstrap_state(strategy)
    schedules <- lapply(1:6, function(outcome_seed) {
      judge <- pairwiseLLM:::.bt_bootstrap_judge(setNames(rep(0, 6), letters[1:6]), outcome_seed, 12L)
      adaptive_rank_run_live(initial, judge, n_steps = 12L, progress = "none",
        btl_config = list(refit_pairs_target = 100L))$step_log
    })
    signatures <- vapply(schedules, function(x) paste(x$unordered_key[6:12], collapse = "|"), "")
    expect_gt(length(unique(signatures)), 1L)
    for (x in schedules) {
      expect_identical(x[1:5, c("A_id", "B_id")], schedules[[1]][1:5, c("A_id", "B_id")])
    }
    expect_gt(length(unique(vapply(schedules, function(x) paste(x$Y[1:5], collapse = ""), ""))), 1L)
  }
})

test_that("hybrid retains scheduled refits, changing identification, and fixed-budget continuation", {
  # Posterior contract fixture tests orchestration, not sampler correctness.
  fit_fn <- function(state, config) {
    ids <- state$item_ids
    mu <- state$trueskill_state$items$mu
    mu <- mu - mean(mu)
    draws <- outer(seq(-.01, .01, length.out = 20), mu, "+")
    make_test_btl_fit(ids, draws = draws, model_variant = "btl")
  }
  state <- bootstrap_state("hybrid", letters[1:12])
  cfg <- list(refit_pairs_target = 6L, model_variant = "btl", stability_lag = 1L,
    eap_reliability_min = -1, theta_corr_min = -1, theta_sd_rel_change_max = Inf,
    rank_spearman_min = -1)
  out <- bootstrap_adaptive(state, n_rep = 2L, budget = 18L, btl_config = cfg,
    schedule_fit_fn = fit_fn, keep = "full")
  expect_true(all(out$replicates$n_scheduling_refits == 3L))
  expect_true(all(out$replicates$n_comparisons == 18L))
  expect_true(all(out$replicates$global_identified))
  expect_true(all(!is.na(out$replicates$stop_boundary_step_id)))
  for (i in 1:2) {
    s <- out$provenance$initial_state
    s$meta$seed <- out$replicates$selector[i]
    judge <- pairwiseLLM:::.bt_bootstrap_judge(out$provenance$theta, out$replicates$outcome[i], 18L)
    fit <- pairwiseLLM:::.bt_bootstrap_schedule_fit(fit_fn, out$replicates$scheduling_refit[i])
    direct <- pairwiseLLM:::.bt_bootstrap_with_seed(out$replicates$selector[i], {
      while (nrow(s$history_pairs) < 18L) {
        s <- adaptive_rank_run_live(s, judge, n_steps = 1L, progress = "none", fit_fn = fit,
          adaptive_config = list(max_pairs_after_stop = 18L), btl_config = cfg)
      }
      s
    })
    expect_identical(out$artifacts[[i]]$state$step_log, direct$step_log)
    expect_identical(out$artifacts[[i]]$state$round_log, direct$round_log)
  }
})
