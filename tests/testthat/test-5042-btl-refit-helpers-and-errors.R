test_that("adaptive BTL helper functions validate inputs and adapt contracts", {
  items <- make_test_items(3)
  state <- pairwiseLLM:::new_adaptive_state(items)

  expect_error(pairwiseLLM:::.adaptive_btl_defaults(1L), ">= 2")

  defaults <- pairwiseLLM:::.adaptive_btl_defaults(state$n_items)
  expect_true(is.list(defaults))
  expect_true(is.double(defaults$ess_bulk_min))

  expect_error(
    pairwiseLLM:::.adaptive_btl_resolve_config(state, "bad"),
    "must be a list"
  )

  merged <- pairwiseLLM:::.adaptive_btl_resolve_config(
    state,
    list(model_variant = "btl_e", stability_lag = 5L)
  )
  expect_identical(merged$model_variant, "btl_e")
  expect_identical(merged$stability_lag, 5L)

  expect_error(pairwiseLLM:::.adaptive_btl_adapt_fit("bad"), "must be a list")
  expect_error(
    pairwiseLLM:::.adaptive_btl_adapt_fit(list(a = 1L)),
    "must include"
  )

  adapted <- pairwiseLLM:::.adaptive_btl_adapt_fit(
    list(theta_draws = matrix(1, nrow = 2L, ncol = 2L))
  )
  expect_true("btl_posterior_draws" %in% names(adapted))
  expect_false("theta_draws" %in% names(adapted))

  expect_error(
    pairwiseLLM:::.adaptive_btl_fit_theta_mean(list(btl_posterior_draws = 1:3)),
    "numeric matrix"
  )

  theta <- pairwiseLLM:::.adaptive_btl_fit_theta_mean(
    list(btl_posterior_draws = matrix(c(1, 2, 3, 4), nrow = 2L))
  )
  expect_equal(theta, c(1.5, 3.5))
})

test_that("adaptive results extraction handles empty and committed-only logs", {
  items <- make_test_items(3)
  state <- pairwiseLLM:::new_adaptive_state(items)

  empty <- pairwiseLLM:::.adaptive_results_from_step_log(state)
  expect_equal(nrow(empty), 0L)

  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      pair_id = NA_integer_,
      A = 1L,
      B = 2L,
      Y = 1L
    )
  )
  out_na <- pairwiseLLM:::.adaptive_results_from_step_log(state)
  expect_equal(nrow(out_na), 0L)

  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 2L,
      timestamp = as.POSIXct("2026-01-01 00:01:00", tz = "UTC"),
      pair_id = 7L,
      A = 1L,
      B = 3L,
      Y = 0L
    )
  )
  out <- pairwiseLLM:::.adaptive_results_from_step_log(state)

  expect_equal(nrow(out), 1L)
  expect_identical(out$pair_uid[[1L]], "pair_7")
  expect_identical(out$ordered_key[[1L]], "1:3")
  expect_identical(out$better_id[[1L]], "3")
  expect_identical(out$winner_pos[[1L]], 2L)
})

test_that("adaptive results extraction maps linking phase and judge scope", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s1"),
    set_id = c(1L, 1L, 2L),
    global_item_id = c("gh1", "gh2", "gs1")
  )
  state <- adaptive_rank_start(
    items,
    seed = 3L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      judge_param_mode = "phase_specific"
    )
  )
  state$linking$phase_a$ready_for_phase_b <- TRUE
  state$linking$phase_a$phase <- "phase_b"
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      pair_id = 1L,
      A = 1L,
      B = 2L,
      Y = 1L,
      is_cross_set = FALSE
    )
  )
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 2L,
      timestamp = as.POSIXct("2026-01-01 00:01:00", tz = "UTC"),
      pair_id = 2L,
      A = 3L,
      B = 1L,
      Y = 1L,
      is_cross_set = TRUE
    )
  )

  out <- pairwiseLLM:::.adaptive_results_from_step_log(state)
  expect_identical(out$phase, c("phase2", "phase3"))
  expect_identical(out$judge_scope, c("within", "link"))
})

test_that("adaptive results extraction uses link judge scope for within-set rows in phase_b", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s1"),
    set_id = c(1L, 1L, 2L),
    global_item_id = c("gh1", "gh2", "gs1")
  )
  state <- adaptive_rank_start(
    items,
    seed = 8L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      judge_param_mode = "phase_specific"
    )
  )
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      pair_id = 1L,
      A = 3L,
      B = 1L,
      Y = 1L,
      is_cross_set = TRUE
    )
  )
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 2L,
      timestamp = as.POSIXct("2026-01-01 00:01:00", tz = "UTC"),
      pair_id = 2L,
      A = 1L,
      B = 2L,
      Y = 1L,
      is_cross_set = FALSE
    )
  )

  out <- pairwiseLLM:::.adaptive_results_from_step_log(state)
  expect_identical(out$phase, c("phase3", "phase3"))
  expect_identical(out$judge_scope, c("link", "link"))
})

test_that("ts-btl rank spearman returns NA for invalid inputs and finite value otherwise", {
  items <- make_test_items(3)
  state <- pairwiseLLM:::new_adaptive_state(items)

  expect_true(is.na(pairwiseLLM:::.adaptive_ts_btl_rank_spearman(state, c(1, 2, 3))))

  state$trueskill_state <- make_test_trueskill_state(items, mu = c(3, 2, 1))
  theta_bad_names <- c(3, 2, 1)
  expect_true(is.na(pairwiseLLM:::.adaptive_ts_btl_rank_spearman(state, theta_bad_names)))

  theta_nonfinite <- stats::setNames(c(3, Inf, 1), as.character(items$item_id))
  expect_true(is.na(pairwiseLLM:::.adaptive_ts_btl_rank_spearman(state, theta_nonfinite)))

  theta <- stats::setNames(c(3, 2, 1), as.character(items$item_id))
  rho <- pairwiseLLM:::.adaptive_ts_btl_rank_spearman(state, theta)
  expect_true(is.finite(rho))
})

test_that("default_btl_fit_fn validates state and requires committed comparisons", {
  expect_error(
    pairwiseLLM:::default_btl_fit_fn(list(), config = list()),
    "adaptive_state"
  )

  state <- pairwiseLLM:::new_adaptive_state(make_test_items(3))
  expect_error(
    pairwiseLLM:::default_btl_fit_fn(state, config = list()),
    "requires at least one committed comparison"
  )
})

test_that("maybe_refit_btl validates fit_fn and fit contract", {
  state <- pairwiseLLM:::new_adaptive_state(make_test_items(4))
  state$history_pairs <- tibble::tibble(A_id = "1", B_id = "2")

  expect_error(
    pairwiseLLM:::maybe_refit_btl(state, config = list(refit_pairs_target = 1L), fit_fn = 1L),
    "must be a function"
  )

  expect_error(
    pairwiseLLM:::maybe_refit_btl(
      state,
      config = list(refit_pairs_target = 1L),
      fit_fn = function(state, config) list(theta_mean = c(1, 2))
    ),
    "must return a list with `btl_posterior_draws`"
  )
})

test_that("Phase A committed-pair cache rebuilds, updates, and reconciles exactly", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s1", "s2"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs1", "gs2")
  )
  state <- pairwiseLLM:::new_adaptive_state(items)
  state$history_pairs <- tibble::tibble(
    A_id = c("h1", "h2", "h1", "s1"),
    B_id = c("h2", "h1", "s1", "s2")
  )

  rebuilt <- pairwiseLLM:::.adaptive_phase_a_committed_pairs_rebuild(state)
  expect_identical(rebuilt, c(`1` = 2L, `2` = 1L))

  state$refit_meta$phase_a_committed_pairs_by_set <- rebuilt
  state$refit_meta$phase_a_committed_pairs_history_n <- nrow(state$history_pairs)
  resolved <- pairwiseLLM:::.adaptive_phase_a_committed_pairs_resolve(
    state,
    validate_existing = TRUE,
    context = "test"
  )
  expect_identical(resolved, rebuilt)

  updated_same_set <- pairwiseLLM:::.adaptive_phase_a_committed_pairs_update(
    cache = rebuilt,
    state = state,
    A_id = "s1",
    B_id = "s2"
  )
  expect_identical(updated_same_set, c(`1` = 2L, `2` = 2L))

  updated_cross_set <- pairwiseLLM:::.adaptive_phase_a_committed_pairs_update(
    cache = rebuilt,
    state = state,
    A_id = "h1",
    B_id = "s1"
  )
  expect_identical(updated_cross_set, rebuilt)

  state$refit_meta$phase_a_committed_pairs_by_set <- c(`1` = 99L, `2` = 1L)
  state$refit_meta$phase_a_committed_pairs_history_n <- 0L
  expect_identical(
    pairwiseLLM:::.adaptive_phase_a_committed_pairs_resolve(state),
    rebuilt
  )
  state$refit_meta$phase_a_committed_pairs_history_n <- nrow(state$history_pairs)
  expect_error(
    pairwiseLLM:::.adaptive_phase_a_committed_pairs_resolve(
      state,
      validate_existing = TRUE,
      context = "test"
    ),
    "diverged from canonical committed history"
  )
})

test_that("compute_stop_metrics validates state and draw matrix shape", {
  expect_error(pairwiseLLM:::compute_stop_metrics(list(), list()), "adaptive_state")

  state <- pairwiseLLM:::new_adaptive_state(make_test_items(3))
  expect_null(pairwiseLLM:::compute_stop_metrics(state, config = list()))

  state$btl_fit <- list(btl_posterior_draws = 1:3)
  expect_error(
    pairwiseLLM:::compute_stop_metrics(state, config = list()),
    "numeric matrix"
  )

  state$btl_fit <- list(btl_posterior_draws = matrix(1, nrow = 1L, ncol = 3L))
  expect_error(
    pairwiseLLM:::compute_stop_metrics(state, config = list()),
    "at least two draws"
  )
})

test_that("phase3 and stopping gate branches are handled", {
  state <- pairwiseLLM:::new_adaptive_state(make_test_items(3))

  unchanged <- pairwiseLLM:::.adaptive_maybe_enter_phase3(
    state,
    metrics = list(diagnostics_pass = FALSE, reliability_EAP = 0.99),
    config = list(eap_reliability_min = 0.9)
  )
  expect_false(isTRUE(unchanged$refit_meta$near_stop))

  metrics <- list(
    diagnostics_pass = TRUE,
    reliability_EAP = 0.95,
    lag_eligible = TRUE,
    rho_theta = 0.99,
    delta_sd_theta = 0.01,
    rho_rank = 0.99
  )
  cfg <- list(
    eap_reliability_min = 0.9,
    theta_corr_min = 0.95,
    theta_sd_rel_change_max = 0.10,
    rank_spearman_min = 0.95
  )
  expect_false(pairwiseLLM:::should_stop(metrics, NULL))

  m <- metrics
  m$rho_theta <- 0.90
  expect_false(pairwiseLLM:::should_stop(m, cfg))
  m <- metrics
  m$delta_sd_theta <- 0.20
  expect_false(pairwiseLLM:::should_stop(m, cfg))
  m <- metrics
  m$rho_rank <- 0.90
  expect_false(pairwiseLLM:::should_stop(m, cfg))
})

test_that("round_log_row handles prior-round attribution and quota source selection", {
  state <- pairwiseLLM:::new_adaptive_state(make_test_items(3))
  state$history_pairs <- tibble::tibble(A_id = "1", B_id = "2")
  state$round <- utils::modifyList(
    state$round,
    list(
      round_id = 3L,
      round_committed = 0L,
      long_quota_raw = 4L,
      long_quota_effective = 2L,
      long_quota_removed = 2L,
      realloc_to_mid = 1L,
      realloc_to_local = 1L
    )
  )
  state$refit_meta$last_completed_round_summary <- list(
    round_id = 2L,
    long_quota_raw = 9L,
    long_quota_effective = 5L,
    long_quota_removed = 4L,
    realloc_to_mid = 2L,
    realloc_to_local = 2L
  )

  draws <- matrix(seq_len(30), nrow = 10L, ncol = 3L)
  state$btl_fit <- list(
    btl_posterior_draws = draws,
    theta_mean = c(a = 1, b = 2),
    model_variant = "btl_e_b"
  )

  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      pair_id = 1L,
      A = 1L,
      B = 2L,
      Y = 1L,
      n_candidates_scored = 5L,
      candidate_starved = FALSE,
      fallback_used = "base",
      n_candidates_after_duplicates = 4L,
      star_cap_rejects = 1L,
      round_id = 2L,
      round_stage = "local_link"
    )
  )

  row <- pairwiseLLM:::.adaptive_round_log_row(
    state = state,
    metrics = list(diagnostics_pass = TRUE),
    stop_decision = FALSE,
    stop_reason = "",
    refit_context = list(
      step_id_at_refit = 1L,
      timestamp = as.POSIXct("2026-01-01 00:01:00", tz = "UTC"),
      last_refit_M_done = 0L,
      last_refit_step = 0L
    ),
    config = list(near_tie_p_low = 0.4, near_tie_p_high = 0.6)
  )

  expect_identical(row$round_id_at_refit, 2L)
  expect_identical(row$long_quota_raw, 9L)
  expect_identical(row$long_quota_effective, 5L)
  expect_identical(row$long_quota_removed, 4L)
  expect_true(is.na(row$ts_btl_theta_corr))
  expect_true(is.na(row$ts_btl_rank_spearman))
  expect_true(is.na(row$ci95_theta_width_mean))
  expect_true(is.na(row$cov_trace_theta))
  expect_true(is.na(row$top20_boundary_entropy_mean))
  expect_true(is.na(row$nn_diff_sd_mean))
})

test_that("round_log_row suppresses global stop reasons during phase_b linking", {
  state <- adaptive_rank_start(
    tibble::tibble(
      item_id = c("h1", "h2", "s21", "s22"),
      set_id = c(1L, 1L, 2L, 2L),
      global_item_id = c("gh1", "gh2", "gs21", "gs22")
    ),
    seed = 17L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$linking$phase_a$phase <- "phase_b"
  state$linking$phase_a$ready_for_phase_b <- TRUE
  state$linking$phase_a$strict_ready_for_phase_b <- TRUE
  state$linking$phase_a$ready_spokes <- 2L
  state$linking$phase_a$set_status <- tibble::tibble(
    set_id = c(1L, 2L),
    source = c("run", "run"),
    status = c("ready", "ready"),
    validation_message = c("ok", "ok"),
    artifact_path = c(NA_character_, NA_character_)
  )
  state$trueskill_state <- NULL
  draws <- matrix(
    c(
      0.2, 0.1, -0.1, -0.2,
      0.3, 0.0, -0.2, -0.1
    ),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(NULL, c("h1", "h2", "s21", "s22"))
  )
  state$btl_fit <- list(
    btl_posterior_draws = draws,
    theta_mean = c(h1 = 0.25, h2 = 0.05, s21 = -0.15, s22 = -0.15),
    model_variant = "btl_e_b"
  )

  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      pair_id = 1L,
      i = 1L,
      j = 3L,
      A = 1L,
      B = 3L,
      Y = 1L,
      set_i = 1L,
      set_j = 2L,
      is_cross_set = TRUE,
      link_spoke_id = 2L,
      run_mode = "link_one_spoke",
      is_probe_step = FALSE,
      round_stage = "anchor_link"
    )
  )

  row <- pairwiseLLM:::.adaptive_round_log_row(
    state = state,
    metrics = list(diagnostics_pass = TRUE),
    stop_decision = TRUE,
    stop_reason = "btl_converged",
    refit_context = list(
      step_id_at_refit = 1L,
      timestamp = as.POSIXct("2026-01-01 00:01:00", tz = "UTC"),
      last_refit_M_done = 0L,
      last_refit_step = 0L
    ),
    config = list(near_tie_p_low = 0.4, near_tie_p_high = 0.6)
  )

  expect_false(isTRUE(row$stop_decision))
  expect_true(is.na(row$stop_reason))
  expect_identical(as.integer(row$new_active_pairs_since_last_refit), 1L)
  expect_identical(as.integer(row$new_probe_pairs_since_last_refit), 0L)
  expect_identical(as.integer(row$new_total_cross_pairs_since_last_refit), 1L)
})

test_that("default_btl_fit_fn scopes Phase A linking refits to active set ids", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s1", "s2"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs1", "gs2")
  )
  state <- adaptive_rank_start(
    items,
    seed = 13L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$linking$phase_a$set_status <- tibble::tibble(
    set_id = c(1L, 2L),
    source = c("run", "run"),
    status = c("ready", "pending_finalization"),
    validation_message = c("ok", "pending_finalization"),
    artifact_path = c(NA_character_, NA_character_)
  )

  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      pair_id = 1L,
      A = 1L,
      B = 2L,
      Y = 1L,
      is_cross_set = FALSE
    )
  )
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 2L,
      timestamp = as.POSIXct("2026-01-01 00:01:00", tz = "UTC"),
      pair_id = 2L,
      A = 3L,
      B = 4L,
      Y = 1L,
      is_cross_set = FALSE
    )
  )
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 3L,
      timestamp = as.POSIXct("2026-01-01 00:02:00", tz = "UTC"),
      pair_id = 3L,
      A = 1L,
      B = 3L,
      Y = 1L,
      is_cross_set = TRUE
    )
  )

  observed <- NULL
  testthat::with_mocked_bindings(
    fit_bayes_btl_mcmc = function(results, ids, model_variant, cmdstan) {
      observed <<- list(results = results, ids = as.character(ids))
      list(
        fit = make_test_btl_fit(
          ids = ids,
          draws = matrix(c(0.1, -0.1, 0.2, -0.2), nrow = 2L, byrow = TRUE),
          model_variant = model_variant
        )
      )
    },
    pairwiseLLM:::default_btl_fit_fn(state, config = list(model_variant = "btl_e_b")),
    .package = "pairwiseLLM"
  )

  expect_identical(observed$ids, c("s1", "s2"))
  expect_true(nrow(observed$results) >= 1L)
  expect_true(all(observed$results$A_id %in% observed$ids))
  expect_true(all(observed$results$B_id %in% observed$ids))
})

test_that("adaptive stop metric scope warns when Phase A scoped ids fallback to global", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s1", "s2"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs1", "gs2")
  )
  state <- adaptive_rank_start(
    items,
    seed = 9L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$linking$phase_a$set_status <- tibble::tibble(
    set_id = c(1L, 2L),
    source = c("run", "run"),
    status = c("ready", "pending_finalization"),
    validation_message = c("ok", "pending_finalization"),
    artifact_path = c(NA_character_, NA_character_)
  )

  scope <- NULL
  expect_warning(
    scope <- pairwiseLLM:::.adaptive_stop_metric_scope(state, ids = "s1"),
    "falling back to global scope"
  )
  expect_identical(scope$phase_scope, "global")
  expect_identical(scope$scope_ids, "s1")
})
