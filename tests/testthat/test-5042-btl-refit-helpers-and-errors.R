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
})
