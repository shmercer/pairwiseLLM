test_that("benchmark metrics are report-only", {
  withr::local_seed(1)

  run <- pairwiseLLM:::.adaptive_simulation_run(
    scenario = "baseline",
    run_seed = 101L,
    judge_seed = 202L,
    n_steps = 12L
  )

  metrics <- pairwiseLLM:::.adaptive_benchmark_metrics(run$state)
  expect_true(nrow(metrics) > 0L)
  expect_true(all(metrics$report_only))
  expect_true(all(c("metric_group", "metric", "value", "report_only") %in% names(metrics)))
})

test_that("benchmark helpers do not alter stopping decisions", {
  items <- make_test_items(3)
  state <- pairwiseLLM:::new_adaptive_state(items)
  ids <- state$item_ids

  draws <- matrix(rep(seq_along(ids), each = 5L), nrow = 5L)
  colnames(draws) <- ids
  state$btl_fit <- make_test_btl_fit(
    ids,
    draws = draws,
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 500)
  )
  state$refit_meta$theta_mean_history <- list(
    stats::setNames(as.double(seq_along(ids)), ids),
    stats::setNames(as.double(seq_along(ids)), ids)
  )

  config <- list(
    ess_bulk_min = 100,
    ess_bulk_min_near_stop = 100,
    max_rhat = 1.01,
    divergences_max = 0L,
    eap_reliability_min = 0.90,
    stability_lag = 1L,
    theta_corr_min = 0.90,
    theta_sd_rel_change_max = 0.20,
    rank_spearman_min = 0.90
  )

  metrics_before <- pairwiseLLM:::compute_stop_metrics(state, config = config)
  decision_before <- pairwiseLLM:::should_stop(metrics_before, config = config)

  bench <- pairwiseLLM:::.adaptive_benchmark_metrics(state)
  expect_true(all(bench$report_only))

  metrics_after <- pairwiseLLM:::compute_stop_metrics(state, config = config)
  decision_after <- pairwiseLLM:::should_stop(metrics_after, config = config)

  expect_identical(decision_before, decision_after)
  expect_equal(metrics_before, metrics_after)
})

test_that("benchmark helpers can include report-only efficiency attribution rows", {
  state <- adaptive_rank_start(make_test_items(4), seed = 11L)
  state$warm_start_done <- TRUE
  state$round$staged_active <- TRUE
  state$step_log <- tibble::tibble(
    step_id = 1L,
    pair_id = NA_integer_,
    status = "starved",
    round_id = 1L,
    pair_type = "anchor_link",
    stage_quota = 1L,
    candidate_starved = TRUE,
    round_stage = "anchor_link",
    fallback_used = "global_safe",
    starvation_reason = "few_candidates_generated"
  )

  metrics <- pairwiseLLM:::.adaptive_benchmark_metrics(
    state,
    include_efficiency_profile = TRUE
  )

  expect_true(nrow(metrics) > 0L)
  expect_true(all(metrics$report_only))
  expect_true(any(metrics$metric_group == "efficiency_timing"))
  expect_true(any(metrics$metric_group == "efficiency_context"))
  expect_true(any(metrics$metric == "elapsed_seconds:select_next_pair"))
  expect_true(any(metrics$metric == "elapsed_seconds:phase_a_prepare"))
  expect_true(any(metrics$metric == "elapsed_seconds:phase_a_finalize_if_ready"))
  expect_true(any(metrics$metric == "elapsed_seconds:maybe_refit_btl"))
  expect_true(any(metrics$metric == "elapsed_seconds:round_starvation"))
  expect_true(any(metrics$metric == "latest_step_candidate_starved"))
  expect_true(any(grepl("^latest_step_round_stage:anchor_link$", metrics$metric)))
})

test_that("efficiency attribution benchmarking does not mutate state", {
  state <- adaptive_rank_start(make_test_items(4), seed = 19L)
  state$warm_start_done <- TRUE
  state$round$staged_active <- TRUE
  state$step_log <- tibble::tibble(
    step_id = 1L,
    pair_id = NA_integer_,
    status = "starved",
    round_id = 1L,
    pair_type = "anchor_link",
    stage_quota = 1L,
    candidate_starved = TRUE,
    round_stage = "anchor_link",
    fallback_used = "global_safe",
    starvation_reason = "few_candidates_generated"
  )

  step_log_before <- state$step_log
  round_before <- state$round
  phase_a_before <- state$linking$phase_a
  meta_before <- state$meta

  bench <- pairwiseLLM:::.adaptive_benchmark_metrics(
    state,
    include_efficiency_profile = TRUE
  )

  expect_true(all(bench$report_only))
  expect_equal(state$step_log, step_log_before)
  expect_equal(state$round, round_before)
  expect_equal(state$linking$phase_a, phase_a_before)
  expect_equal(state$meta, meta_before)
})
