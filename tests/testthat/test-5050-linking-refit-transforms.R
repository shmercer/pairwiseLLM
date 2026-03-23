make_linking_refit_state <- function(adaptive_config = list()) {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs31", "gs32")
  )
  base_cfg <- list(run_mode = "link_multi_spoke", hub_id = 1L)
  state <- adaptive_rank_start(
    items,
    seed = 123L,
    adaptive_config = utils::modifyList(base_cfg, adaptive_config)
  )
  state$config$btl_config <- test_link_btl_config(state$config$btl_config %||% list())

  draws <- matrix(
    c(
      1.00, 0.80, 0.60, -0.70, -0.90, -0.20, -0.40,
      1.05, 0.85, 0.65, -0.60, -0.85, -0.15, -0.35,
      0.95, 0.75, 0.55, -0.75, -0.95, -0.30, -0.45,
      1.10, 0.90, 0.70, -0.65, -0.80, -0.10, -0.30
    ),
    nrow = 4,
    byrow = TRUE
  )
  colnames(draws) <- as.character(state$item_ids)
  state$btl_fit <- make_test_btl_fit(state$item_ids, draws = draws, model_variant = "btl_e_b")

  art <- function(set_id, ids, thetas) {
    list(
      set_id = as.integer(set_id),
      items = tibble::tibble(
        global_item_id = ids,
        theta_raw_mean = as.double(thetas),
        theta_raw_sd = rep(0.15, length(ids))
      )
    )
  }
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L, 3L),
      source = c("run", "run", "run"),
      status = c("ready", "ready", "ready"),
      validation_message = c("ok", "ok", "ok"),
      artifact_path = c(NA_character_, NA_character_, NA_character_)
    ),
    artifacts = list(
      `1` = art(1L, c("gh1", "gh2", "gh3"), c(0.80, 0.40, 0.10)),
      `2` = art(2L, c("gs21", "gs22"), c(-0.30, -0.60)),
      `3` = art(3L, c("gs31", "gs32"), c(0.15, -0.10))
    ),
    ready_for_phase_b = TRUE,
    phase = "phase_b"
  )

  state
}

append_cross_step <- function(state, step_id, A_id, B_id, Y, spoke_id) {
  ids <- as.character(state$item_ids)
  A <- match(A_id, ids)
  B <- match(B_id, ids)
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = as.integer(step_id),
      timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + as.integer(step_id),
      pair_id = as.integer(step_id),
      i = as.integer(A),
      j = as.integer(B),
      A = as.integer(A),
      B = as.integer(B),
      Y = as.integer(Y),
      set_i = as.integer(state$set_ids[[A]]),
      set_j = as.integer(state$set_ids[[B]]),
      is_cross_set = TRUE,
      link_spoke_id = as.integer(spoke_id),
      run_mode = "link_multi_spoke",
      is_probe_step = FALSE
    )
  )
  state
}

append_probe_step <- function(state, step_id, hub_item_id, spoke_item_id, Y, spoke_id) {
  ids <- as.character(state$item_ids)
  hub_idx <- match(hub_item_id, ids)
  spoke_idx <- match(spoke_item_id, ids)
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = as.integer(step_id),
      timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + as.integer(step_id),
      pair_id = as.integer(step_id),
      i = as.integer(hub_idx),
      j = as.integer(spoke_idx),
      A = as.integer(hub_idx),
      B = as.integer(spoke_idx),
      Y = as.integer(Y),
      set_i = as.integer(state$set_ids[[hub_idx]]),
      set_j = as.integer(state$set_ids[[spoke_idx]]),
      is_cross_set = TRUE,
      link_spoke_id = as.integer(spoke_id),
      run_mode = "link_probe_holdout",
      is_probe_step = TRUE,
      link_stage = "probe_panel",
      round_stage = "probe_panel"
    )
  )
  state
}

current_link_epoch_signature <- function(state,
                                         spoke_id,
                                         transform_state = "shift_only",
                                         refit_mode = "shift_only",
                                         lock_mode = "soft_lock") {
  hub_id <- 1L
  pairwiseLLM:::.adaptive_link_epoch_signature_string(
    pairwiseLLM:::.adaptive_link_epoch_signature_components(
      transform_state = transform_state,
      refit_mode = refit_mode,
      lock_mode = lock_mode,
      hub_art = state$linking$phase_a$artifacts[[as.character(hub_id)]],
      spoke_art = state$linking$phase_a$artifacts[[as.character(spoke_id)]]
    )
  )
}

make_stable_epoch_stop_state <- function(probe_edges_min_for_stop = 2L,
                                         min_refits_in_phase_b = 3L,
                                         stability_lag = 2L) {
  state <- make_linking_refit_state(
    list(
      probe_edges_min_for_stop = probe_edges_min_for_stop,
      min_refits_in_phase_b = min_refits_in_phase_b
    )
  )
  state$config$btl_config$stability_lag <- as.integer(stability_lag)
  state$controller$probe_edges_min_for_stop <- as.integer(probe_edges_min_for_stop)
  state$controller$min_refits_in_phase_b <- as.integer(min_refits_in_phase_b)
  state$controller$link_epoch_id_by_spoke <- list(`2` = 4L)
  state$controller$link_epoch_signature_by_spoke <- list(
    `2` = current_link_epoch_signature(state, spoke_id = 2L)
  )
  state$controller$link_epoch_start_step_by_spoke <- list(`2` = 1L)
  state$controller$link_transform_state_by_spoke <- list(`2` = "shift_only")
  state$controller$link_refit_stats_by_spoke <- list(`2` = list(link_epoch_id = 4L))

  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "h2", "s22", 0L, spoke_id = 2L)
  state <- append_cross_step(state, 3L, "s21", "h3", 1L, spoke_id = 2L)

  panel <- tibble::tibble(
    probe_panel_id = c("panel_a", "panel_a"),
    link_epoch_id = c(4L, 4L),
    spoke_id = c(2L, 2L),
    hub_item_id = c("h1", "h2"),
    spoke_item_id = c("s21", "s22"),
    spoke_bin = c(1L, 2L),
    hub_bin = c(1L, 2L),
    planned_rank = c(1L, 2L),
    pair_key = c(
      pairwiseLLM:::make_unordered_key("h1", "s21"),
      pairwiseLLM:::make_unordered_key("h2", "s22")
    ),
    realized = c(TRUE, TRUE),
    realized_step_id = c(11L, 12L),
    realized_pair_id = c(11L, 12L),
    realized_run_mode = c("link_probe_holdout", "link_probe_holdout")
  )

  state$linking$probe <- pairwiseLLM:::.adaptive_link_probe_empty_state()
  state$linking$probe$panels_by_spoke[["2"]] <- panel
  state$linking$probe$realized_edges <- tibble::tibble(
    step_id = c(11L, 12L),
    pair_id = c(11L, 12L),
    run_mode = c("link_probe_holdout", "link_probe_holdout"),
    spoke_id = c(2L, 2L),
    link_epoch_id = c(4L, 4L),
    probe_panel_id = c("panel_a", "panel_a"),
    hub_item_id = c("h1", "h2"),
    spoke_item_id = c("s21", "s22"),
    pair_key = panel$pair_key,
    Y = c(1L, 0L)
  )

  state$round_log <- pairwiseLLM:::append_round_log(
    state$round_log,
    list(refit_id = 1L, diagnostics_pass = TRUE)
  )
  state$round_log <- pairwiseLLM:::append_round_log(
    state$round_log,
    list(refit_id = 2L, diagnostics_pass = TRUE)
  )

  stage_row <- list(
    spoke_id = 2L,
    hub_id = 1L,
    link_transform_policy = "auto",
    link_transform_state = "shift_only",
    link_refit_mode = "shift_only",
    hub_lock_mode = "soft_lock",
    link_epoch_id = 4L,
    probe_panel_id = "panel_a",
    delta_spoke_mean = 0.10,
    delta_spoke_sd = 0.01,
    reliability_link_global = 0.95,
    reliability_stop_pass = TRUE,
    linking_identified = TRUE,
    link_state_frozen = FALSE,
    probe_edges_planned = as.integer(probe_edges_min_for_stop),
    probe_edges_min_for_stop_used = as.integer(probe_edges_min_for_stop),
    probe_edges_realized_before_refit = 0L,
    probe_edges_realized_delta_since_last_refit = 0L,
    stop_blocker_codes = "lag_not_eligible,probe_pred_rmse_lagged,theta_global_rmse_lagged"
  )
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    state$link_stage_log,
    utils::modifyList(stage_row, list(
      refit_id = 1L,
      probe_edges_realized = 0L,
      stop_recent_pass_count = 0L,
      stop_recent_window_size = 0L,
      stability_window_refits_used = 3L,
      stability_passes_required_used = 2L,
      link_stop_pass = FALSE
    ))
  )
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    state$link_stage_log,
    utils::modifyList(stage_row, list(
      refit_id = 2L,
      probe_edges_realized_before_refit = 0L,
      probe_edges_realized_delta_since_last_refit = 1L,
      probe_edges_realized = 1L,
      stop_recent_pass_count = 0L,
      stop_recent_window_size = 0L,
      stability_window_refits_used = 3L,
      stability_passes_required_used = 2L,
      link_stop_pass = FALSE
    ))
  )

  state
}

run_mocked_stop_window_refit <- function(state, theta_rmse = 0.02) {
  testthat::with_mocked_bindings(
    .adaptive_link_fit_transform = function(cross_edges, hub_theta, spoke_theta, transform_mode) {
      list(
        delta_mean = 0.1,
        delta_sd = 0.01,
        log_alpha_mean = if (identical(transform_mode, "shift_scale")) 0.02 else NA_real_,
        log_alpha_sd = if (identical(transform_mode, "shift_scale")) 0.02 else NA_real_,
        theta_hub_post = hub_theta,
        theta_spoke_post = spoke_theta,
        posterior_draws = list(),
        diagnostics = list(
          converged = TRUE,
          hessian_posdef = TRUE
        ),
        fit_contract = list(
          estimation_method = "map_laplace",
          uncertainty_approximation = "laplace_hessian"
        )
      )
    },
    .adaptive_link_global_score_stats_active = function(...) {
      list(reliability = 0.95, V_mu = 1.2, V_post = 0.06)
    },
    .adaptive_link_reliability_transformed_active = function(...) 0.95,
    .adaptive_link_ts_btl_rank_spearman_active = function(...) 0.95,
    .adaptive_link_rank_stability_lagged = function(...) {
      list(lag_eligible = TRUE, rho_rank_lagged = 0.99, rho_rank_lagged_pass = TRUE)
    },
    .adaptive_link_probe_brier_for_fit = function(...) 0.10,
    .adaptive_link_probe_pred_rmse_lagged_for_fit = function(...) 0.01,
    .adaptive_link_theta_global_rmse_lagged = function(...) theta_rmse,
    .adaptive_linking_refit_update_state(state, list(last_refit_step = 3L)),
    .package = "pairwiseLLM"
  )
}

test_that("rolling pass windows retain the last 3 eligible outcomes without shortcutting", {
  window <- c(TRUE, FALSE)
  window <- pairwiseLLM:::.adaptive_link_result_window_append(window, TRUE, max_size = 3L)
  expect_identical(window, c(TRUE, FALSE, TRUE))
  expect_identical(pairwiseLLM:::.adaptive_link_result_window_pass_count(window), 2L)
  expect_true(length(window) == 3L)

  stale_window <- c(TRUE, FALSE, FALSE)
  stale_window <- pairwiseLLM:::.adaptive_link_result_window_append(
    stale_window,
    TRUE,
    max_size = 3L
  )
  expect_identical(stale_window, c(FALSE, FALSE, TRUE))
  expect_identical(pairwiseLLM:::.adaptive_link_result_window_pass_count(stale_window), 1L)
  expect_false(
    length(stale_window) == 3L &&
      pairwiseLLM:::.adaptive_link_result_window_pass_count(stale_window) >= 2L
  )
})

test_that("ineligible refits do not pollute stop or escalation rolling windows", {
  state <- make_stable_epoch_stop_state()
  state$controller$link_stop_recent_pass_window_by_spoke <- list(`2` = c(TRUE, TRUE))
  state$controller$link_escalation_recent_pass_window_by_spoke <- list(`2` = c(TRUE, FALSE))
  state$linking$probe$realized_edges <- state$linking$probe$realized_edges[0, , drop = FALSE]

  out <- run_mocked_stop_window_refit(state, theta_rmse = 0.045)
  stats <- out$controller$link_refit_stats_by_spoke[["2"]]

  expect_false(isTRUE(stats$link_stop_eligible))
  expect_false(isTRUE(stats$link_stop_pass))
  expect_identical(stats$stop_recent_pass_count, 2L)
  expect_identical(stats$stop_recent_window_size, 2L)
  expect_identical(out$controller$link_stop_recent_pass_window_by_spoke[["2"]], c(TRUE, TRUE))
  expect_identical(
    out$controller$link_escalation_recent_pass_window_by_spoke[["2"]],
    c(TRUE, FALSE)
  )
})

test_that("linking refit contract fields follow transform mode", {
  state_shift <- make_linking_refit_state(
    list(link_transform_mode = "shift_only", link_refit_mode = "shift_only")
  )
  state_shift <- append_cross_step(state_shift, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state_shift <- append_cross_step(state_shift, 2L, "h2", "s22", 0L, spoke_id = 2L)

  state_shift <- pairwiseLLM:::.adaptive_linking_refit_update_state(
    state_shift,
    refit_context = list(last_refit_step = 0L)
  )
  rows_shift <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state_shift,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row_shift <- rows_shift[rows_shift$spoke_id == 2L, , drop = FALSE]
  expect_true(is.finite(row_shift$delta_spoke_mean[[1L]]))
  expect_true(is.na(row_shift$log_alpha_spoke_mean[[1L]]))

  state_scale <- make_linking_refit_state(
    list(link_transform_mode = "shift_scale", link_refit_mode = "shift_only")
  )
  state_scale <- append_cross_step(state_scale, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state_scale <- append_cross_step(state_scale, 2L, "h2", "s22", 0L, spoke_id = 2L)
  state_scale <- pairwiseLLM:::.adaptive_linking_refit_update_state(
    state_scale,
    refit_context = list(last_refit_step = 0L)
  )
  rows_scale <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state_scale,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row_scale <- rows_scale[rows_scale$spoke_id == 2L, , drop = FALSE]
  expect_true(is.finite(row_scale$delta_spoke_mean[[1L]]))
  expect_true(is.finite(row_scale$log_alpha_spoke_mean[[1L]]))

  c_shift <- state_shift$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  expect_identical(c_shift$parameters, c("delta_s"))
  expect_identical(c_shift$link_transform_state, "shift_only")
  expect_false("link_transform_mode" %in% names(c_shift))
  expect_false("link_transform_mode" %in% names(state_shift$controller$link_refit_stats_by_spoke[["2"]]))

  c_scale <- state_scale$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  expect_identical(c_scale$parameters, c("delta_s", "log_alpha_s"))
  expect_identical(c_scale$link_transform_state, "shift_scale")
  expect_false("link_transform_mode" %in% names(c_scale))
})

test_that("soft lock with kappa=0 is rejected in joint refit", {
  base <- make_linking_refit_state(
    list(link_refit_mode = "joint_refit", link_transform_mode = "shift_only")
  )
  base <- append_cross_step(base, 1L, "s21", "h1", 1L, spoke_id = 2L)
  base <- append_cross_step(base, 2L, "h2", "s22", 0L, spoke_id = 2L)

  hard <- base
  hard <- pairwiseLLM:::.adaptive_apply_controller_config(
    hard,
    list(hub_lock_mode = "hard_lock", hub_lock_kappa = 0.75)
  )
  hard <- pairwiseLLM:::.adaptive_linking_refit_update_state(hard, list(last_refit_step = 0L))

  expect_error(
    pairwiseLLM:::.adaptive_apply_controller_config(
      base,
      list(hub_lock_mode = "soft_lock", hub_lock_kappa = 0)
    ),
    "strictly in \\(0, 1\\]"
  )

  d_hard <- hard$controller$link_refit_stats_by_spoke[["2"]]$delta_spoke_mean

  hard_contract <- hard$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  expect_equal(hard_contract$joint_refit$n_hub_items_estimated, 0L)
  expect_true(is.finite(d_hard))
})

test_that("soft lock uses artifact uncertainty and kappa strength", {
  base <- make_linking_refit_state(
    list(link_refit_mode = "joint_refit", link_transform_mode = "shift_only")
  )
  base <- append_cross_step(base, 1L, "s21", "h1", 1L, spoke_id = 2L)
  base <- append_cross_step(base, 2L, "h2", "s22", 0L, spoke_id = 2L)

  arts <- base$linking$phase_a$artifacts
  arts[["1"]]$items$theta_raw_sd <- c(0.02, 0.02, 0.02)
  base$linking$phase_a$artifacts <- arts

  soft_high <- pairwiseLLM:::.adaptive_apply_controller_config(
    base,
    list(hub_lock_mode = "soft_lock", hub_lock_kappa = 1)
  )
  soft_low <- pairwiseLLM:::.adaptive_apply_controller_config(
    base,
    list(hub_lock_mode = "soft_lock", hub_lock_kappa = 0.1)
  )

  out_high <- pairwiseLLM:::.adaptive_linking_refit_update_state(soft_high, list(last_refit_step = 0L))
  out_low <- pairwiseLLM:::.adaptive_linking_refit_update_state(soft_low, list(last_refit_step = 0L))

  sd_high <- out_high$controller$link_refit_stats_by_spoke[["2"]]$delta_spoke_sd
  sd_low <- out_low$controller$link_refit_stats_by_spoke[["2"]]$delta_spoke_sd
  expect_true(is.finite(sd_high))
  expect_true(is.finite(sd_low))
  c_high <- out_high$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  c_low <- out_low$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  expect_identical(c_high$lock$hub_lock_mode, "soft_lock")
  expect_equal(c_high$lock$hub_lock_kappa, 1, tolerance = 1e-12)
  expect_equal(c_low$lock$hub_lock_kappa, 0.1, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(
    out_high$controller$link_refit_stats_by_spoke[["2"]]$delta_spoke_mean,
    out_low$controller$link_refit_stats_by_spoke[["2"]]$delta_spoke_mean,
    tolerance = 1e-10
  )))
})

test_that("free hub lock skips soft-lock priors in transform joint refit", {
  state <- make_linking_refit_state(
    list(link_refit_mode = "joint_refit")
  )
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "h2", "s22", 0L, spoke_id = 2L)

  cross_edges <- pairwiseLLM:::.adaptive_link_cross_edges(
    state,
    spoke_id = 2L,
    last_refit_step = NULL
  )
  attr(cross_edges, "judge_params") <- list(
    mode = "global_shared",
    scope = "link",
    beta = 0,
    epsilon = 0
  )
  attr(cross_edges, "within_hub_edges") <- pairwiseLLM:::.adaptive_link_within_edges(
    state,
    set_id = 1L
  )
  attr(cross_edges, "within_spoke_edges") <- pairwiseLLM:::.adaptive_link_within_edges(
    state,
    set_id = 2L
  )

  captured <- new.env(parent = emptyenv())
  capture_fit_fn <- function(stan_data, variable_names, cmdstan, seed, model_fn = NULL) {
    captured$stan_data <- stan_data
    make_test_link_cmdstan_fit_fn()(stan_data, variable_names, cmdstan, seed, model_fn)
  }
  attr(cross_edges, "refit_contract") <- list(
    link_refit_mode = "joint_refit",
    hub_lock_mode = "free",
    hub_lock_kappa = 0.75,
    link_transform_policy = "auto",
    shift_only_theta_treatment = "fixed_eap_plugin_var",
    cmdstan = list(chains = 4L, parallel_chains = 4L, threads_per_chain = 1L),
    cmdstan_fit_fn = capture_fit_fn
  )

  hub_theta <- pairwiseLLM:::.adaptive_link_phase_a_theta_map(state, 1L, "theta_raw_mean")
  attr(hub_theta, "theta_sd") <- pairwiseLLM:::.adaptive_link_phase_a_theta_map(
    state,
    1L,
    "theta_raw_sd"
  )
  attr(hub_theta, "theta_prior_center") <- hub_theta
  attr(hub_theta, "theta_init") <- stats::setNames(c(10, 9, 8), names(hub_theta))

  spoke_theta <- pairwiseLLM:::.adaptive_link_phase_a_theta_map(state, 2L, "theta_raw_mean")
  attr(spoke_theta, "theta_sd") <- pairwiseLLM:::.adaptive_link_phase_a_theta_map(
    state,
    2L,
    "theta_raw_sd"
  )
  attr(spoke_theta, "theta_init") <- spoke_theta

  fit <- pairwiseLLM:::.adaptive_link_fit_transform(
    cross_edges = cross_edges,
    hub_theta = hub_theta,
    spoke_theta = spoke_theta,
    transform_mode = "shift_only"
  )

  expect_identical(captured$stan_data$estimate_hub, 1L)
  expect_identical(captured$stan_data$hub_prior_active, 0L)
  expect_identical(fit$fit_contract$lock$hub_lock_mode, "free")
  expect_true(is.na(fit$fit_contract$lock$hub_lock_kappa))
  expect_identical(
    fit$fit_contract$joint_refit$n_hub_items_estimated,
    length(hub_theta)
  )
})

test_that("joint_refit fit contract records joint theta estimation", {
  state <- make_linking_refit_state(
    list(link_refit_mode = "joint_refit", link_transform_mode = "shift_only")
  )
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "h2", "s22", 0L, spoke_id = 2L)

  out <- pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
  contract <- out$controller$link_refit_stats_by_spoke[["2"]]$fit_contract

  expect_identical(contract$link_refit_mode, "joint_refit")
  expect_true(all(c("theta_hub", "theta_spoke", "delta_s") %in% contract$parameters))
  expect_true(isTRUE(contract$joint_refit$used))
  expect_true(contract$joint_refit$n_hub_items_estimated >= 1L)
  expect_true(contract$joint_refit$n_spoke_items_estimated >= 1L)
})

test_that("joint_refit utility uses current theta state rather than Phase A summaries", {
  state <- make_linking_refit_state(list(link_refit_mode = "joint_refit"))
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      delta_spoke_mean = 0,
      log_alpha_spoke_mean = NA_real_,
      link_transform_state = "shift_only"
    )
  )
  cand <- tibble::tibble(i = "h1", j = "s21")

  out_joint <- pairwiseLLM:::.adaptive_link_attach_predictive_utility(
    candidates = cand,
    state = state,
    controller = state$controller,
    spoke_id = 2L
  )

  state_shift <- pairwiseLLM:::.adaptive_apply_controller_config(state, list(link_refit_mode = "shift_only"))
  out_shift <- pairwiseLLM:::.adaptive_link_attach_predictive_utility(
    candidates = cand,
    state = state_shift,
    controller = state_shift$controller,
    spoke_id = 2L
  )

  theta_cur <- state$btl_fit$theta_mean
  expected_joint <- stats::plogis(theta_cur[["h1"]] - theta_cur[["s21"]])

  expect_true(is.finite(out_joint$link_p[[1L]]))
  expect_equal(out_joint$link_p[[1L]], expected_joint, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(out_joint$link_p[[1L]], out_shift$link_p[[1L]], tolerance = 1e-12)))
})

test_that("soft-lock joint refit keeps Phase A prior center and uses current theta only for initialization", {
  state <- make_linking_refit_state(list(link_refit_mode = "joint_refit", hub_lock_mode = "soft_lock"))
  state$btl_fit$theta_mean[c("h1", "h2", "h3")] <- c(10, 9, 8)
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)

  captured <- list()
  testthat::with_mocked_bindings(
    .adaptive_link_fit_transform = function(cross_edges, hub_theta, spoke_theta, transform_mode) {
      captured$hub_theta <<- hub_theta
      captured$spoke_theta <<- spoke_theta
      list(
        delta_mean = 0,
        delta_sd = 1,
        log_alpha_mean = NA_real_,
        log_alpha_sd = NA_real_,
        theta_hub_post = hub_theta,
        theta_spoke_post = spoke_theta,
        diagnostics = list(
          divergences = 0L,
          max_rhat = 1.0,
          min_ess_bulk = 500,
          diagnostics_divergences_pass = TRUE,
          diagnostics_rhat_pass = TRUE,
          diagnostics_ess_pass = TRUE
        ),
        fit_contract = list(
          estimation_method = "cmdstan_hmc",
          uncertainty_approximation = "cmdstan_posterior_draws"
        )
      )
    },
    .adaptive_link_ppc_brier_cross = function(...) 0,
    .package = "pairwiseLLM",
    {
      pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
    }
  )

  hub_prior_center <- attr(captured$hub_theta, "theta_prior_center", exact = TRUE)
  hub_init <- attr(captured$hub_theta, "theta_init", exact = TRUE)
  expect_true(length(hub_prior_center) > 0L)
  expect_true(length(hub_init) > 0L)

  phase_a_hub <- state$linking$phase_a$artifacts[["1"]]$items
  phase_center_map <- stats::setNames(as.double(phase_a_hub$theta_raw_mean), c("h1", "h2", "h3"))
  expect_equal(unname(hub_prior_center[c("h1", "h2", "h3")]), unname(phase_center_map[c("h1", "h2", "h3")]))
  expect_equal(unname(hub_init[c("h1", "h2", "h3")]), c(10, 9, 8))
})

test_that("auto escalation stays in shift_only before lag and stop eligibility are available", {
  state <- make_linking_refit_state(
    list(
      link_transform_mode = "auto",
      link_refit_mode = "shift_only",
      link_transform_escalation_window_refits = 3L,
      link_transform_escalation_passes_required = 2L
    )
  )

  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "s22", "h2", 1L, spoke_id = 2L)
  state1 <- testthat::with_mocked_bindings(
    .adaptive_link_cross_edges = function(...) {
      tibble::tibble(
        spoke_item = rep(c("s21", "s22", "s23"), each = 6L),
        hub_item = rep(c("h1", "h2", "h3"), times = 6L),
        y_spoke = rep(c(1L, 0L), length.out = 18L),
        step_id = seq_len(18L),
        spoke_in_A = TRUE,
        run_mode = "link_one_spoke",
        is_probe_step = FALSE
      )
    },
    .adaptive_link_probe_edges_realized = function(...) {
      tibble::tibble(
        hub_item = c("h1", "h2", "h3"),
        spoke_item = c("s21", "s22", "s21"),
        y_spoke = c(1L, 0L, 1L),
        spoke_in_A = c(TRUE, TRUE, TRUE),
        is_probe_step = TRUE
      )
    },
    .adaptive_link_fit_transform_alt_shift_scale = function(...) {
      list(converged = TRUE, delta_mean = 0.2, log_alpha_mean = 0.3, log_alpha_sd = 0.02)
    },
    .adaptive_link_probe_brier_for_fit = function(..., log_alpha_mean = NA_real_) {
      if (is.finite(log_alpha_mean)) 0.10 else 0.12
    },
    .adaptive_link_probe_pred_rmse_lagged_for_fit = function(...) 0.01,
    .adaptive_link_theta_global_rmse_lagged = function(...) 0.02,
    .adaptive_linking_refit_update_state(state, list(last_refit_step = 0L)),
    .package = "pairwiseLLM"
  )
  expect_identical(state1$controller$link_transform_state_by_spoke[["2"]], "shift_only")
  expect_identical(state1$controller$link_refit_stats_by_spoke[["2"]]$escalation_recent_pass_count, 0L)
  expect_false(isTRUE(state1$controller$link_refit_stats_by_spoke[["2"]]$link_stop_eligible))
})

test_that("temporary shift-scale alternative fit returns finite MAP summaries", {
  state <- make_linking_refit_state()
  cross_edges <- tibble::tibble(
    spoke_item = c("s21", "s22", "s21", "s22", "s23", "s23"),
    hub_item = c("h1", "h2", "h3", "h1", "h2", "h3"),
    y_spoke = c(1L, 0L, 1L, 0L, 1L, 0L),
    step_id = seq_len(6L),
    spoke_in_A = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    run_mode = "link_one_spoke",
    is_probe_step = FALSE
  )
  attr(cross_edges, "judge_params") <- list(beta = 0, epsilon = 0)
  fit <- pairwiseLLM:::.adaptive_link_fit_transform_alt_shift_scale(
    cross_edges = cross_edges,
    hub_theta = c(h1 = 1.5, h2 = 0.5, h3 = -0.5),
    spoke_theta = c(s21 = 0.2, s22 = -0.1, s23 = -0.4),
    delta_init = 0
  )

  expect_true(isTRUE(fit$converged))
  expect_true(is.finite(fit$delta_mean))
  expect_true(is.finite(fit$log_alpha_mean))
  expect_true(is.finite(fit$log_alpha_sd))
})

test_that("auto escalation streak resets when eligibility fails", {
  state <- make_linking_refit_state(
    list(
      link_transform_mode = "auto",
      link_refit_mode = "shift_only",
      link_transform_escalation_window_refits = 3L,
      link_transform_escalation_passes_required = 2L
    )
  )
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)

  out <- testthat::with_mocked_bindings(
    .adaptive_link_probe_edges_realized = function(...) tibble::tibble(),
    .package = "pairwiseLLM",
    {
      s1 <- pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
      pairwiseLLM:::.adaptive_linking_refit_update_state(s1, list(last_refit_step = 0L))
    }
  )

  expect_identical(out$controller$link_transform_state_by_spoke[["2"]], "shift_only")
  expect_identical(out$controller$link_refit_stats_by_spoke[["2"]]$escalation_recent_pass_count, 0L)
})

test_that("freeze transition is one-way and refit reuses frozen transform parameters", {
  state <- make_linking_refit_state(
    list(
      link_transform_mode = "shift_only",
      link_refit_mode = "shift_only"
    )
  )
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state$linking$phase_a$ready_spokes <- 2L
  state <- pairwiseLLM:::.adaptive_link_apply_stop_state(
    state,
    tibble::tibble(
      refit_id = 1L,
      spoke_id = 2L,
      link_stop_pass = TRUE,
      link_transform_mode = "shift_only",
      delta_spoke_mean = 0.17,
      log_alpha_spoke_mean = NA_real_
    )
  )
  state$controller$link_transform_frozen_by_spoke <- list(`2` = FALSE)
  state$controller$link_transform_frozen_refit_id_by_spoke <- list(`2` = 9L)

  out <- testthat::with_mocked_bindings(
    .adaptive_link_fit_transform = function(...) {
      rlang::abort("fit should not run for frozen spoke")
    },
    .adaptive_link_ppc_brier_cross = function(...) 0.12,
    .package = "pairwiseLLM",
    {
      pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
    }
  )

  stats <- out$controller$link_refit_stats_by_spoke[["2"]]
  expect_true(isTRUE(out$controller$link_state_frozen_by_spoke[["2"]]))
  expect_identical(out$controller$link_state_frozen_refit_id_by_spoke[["2"]], 1L)
  expect_true(isTRUE(stats$link_state_frozen))
  expect_equal(stats$delta_spoke_mean, 0.17, tolerance = 1e-12)
})

test_that("link stage rows retire frozen spokes with zero budget and zero new work", {
  state <- make_linking_refit_state(
    list(
      link_transform_mode = "shift_only",
      link_refit_mode = "shift_only"
    )
  )
  state$linking$phase_a$ready_spokes <- c(2L, 3L)
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "s31", "h2", 1L, spoke_id = 3L)
  state <- pairwiseLLM:::.adaptive_link_apply_stop_state(
    state,
    tibble::tibble(
      refit_id = 1L,
      spoke_id = 2L,
      link_stop_pass = TRUE,
      link_transform_state = "shift_only",
      delta_spoke_mean = 0.17,
      log_alpha_spoke_mean = NA_real_
    )
  )
  state$controller$link_transform_state_by_spoke <- list(`2` = "shift_only", `3` = "shift_only")
  state$controller$link_transform_frozen_refit_id_by_spoke <- list(`2` = 99L)
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_state = "shift_only",
      link_stop_pass = TRUE,
      stop_recent_pass_count = 2L,
      stop_recent_window_size = 3L,
      stability_window_refits_used = 3L,
      stability_passes_required_used = 2L,
      link_state_frozen = TRUE,
      link_epoch_id = 1L,
      n_probe_pairs_since_last_refit = 7L,
      n_cross_edges_active_since_last_refit = 11L,
      n_cross_edges_probe_since_last_refit = 7L,
      n_cross_edges_total_since_last_refit = 18L
    ),
    `3` = list(
      link_transform_state = "shift_only",
      link_stop_pass = FALSE,
      stop_recent_pass_count = 0L,
      stop_recent_window_size = 0L,
      stability_window_refits_used = 3L,
      stability_passes_required_used = 2L,
      link_state_frozen = FALSE,
      link_epoch_id = 1L
    )
  )
  state$controller$link_budget_refit_id <- 2L
  state$controller$link_budget_map <- list(
    `3` = list(
      B_spoke_refit_budget = 4L,
      B_spoke_refit_budget_source = "concurrent_allocator"
    )
  )

  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 2L,
    refit_context = list(last_refit_step = 1L)
  )
  frozen_row <- rows[rows$spoke_id == 2L, , drop = FALSE]

  expect_identical(nrow(frozen_row), 1L)
  expect_true(isTRUE(frozen_row$link_state_frozen[[1L]]))
  expect_identical(frozen_row$link_state_frozen_refit_id[[1L]], 1L)
  expect_true(isTRUE(frozen_row$link_stop_pass[[1L]]))
  expect_identical(frozen_row$B_spoke_refit_budget[[1L]], 0L)
  expect_identical(frozen_row$n_cross_edges_active_since_last_refit[[1L]], 0L)
  expect_identical(frozen_row$n_probe_pairs_since_last_refit[[1L]], 0L)
  expect_identical(frozen_row$n_cross_edges_total_since_last_refit[[1L]], 0L)
})

test_that("escalation path does not evaluate without realized held-out probes", {
  state <- make_linking_refit_state(
    list(
      link_transform_mode = "auto",
      link_refit_mode = "shift_only",
      link_transform_escalation_window_refits = 1L,
      link_transform_escalation_passes_required = 1L
    )
  )
  ids <- as.character(state$item_ids)
  A <- match("s21", ids)
  B <- match("h1", ids)
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + 1L,
      pair_id = 1L,
      i = A,
      j = B,
      A = A,
      B = B,
      Y = 1L,
      set_i = as.integer(state$set_ids[[A]]),
      set_j = as.integer(state$set_ids[[B]]),
      is_cross_set = TRUE,
      link_spoke_id = 2L,
      run_mode = "link_probe",
      is_probe_step = TRUE
    )
  )

  out <- testthat::with_mocked_bindings(
    .adaptive_link_probe_edges_realized = function(...) tibble::tibble(),
    .package = "pairwiseLLM",
    {
      pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
    }
  )

  expect_identical(out$controller$link_transform_state_by_spoke[["2"]], "shift_only")
  expect_false(isTRUE(out$controller$link_refit_stats_by_spoke[["2"]]$scale_ready))
})

test_that("judge parameter mode controls linking judge scope in fit contract", {
  state <- make_linking_refit_state(
    list(link_transform_mode = "shift_only", link_refit_mode = "shift_only", judge_param_mode = "phase_specific")
  )
  state$btl_fit$beta_mean <- 0.01
  state$btl_fit$epsilon_mean <- 0.02
  state$btl_fit$beta_link_mean <- 0.15
  state$btl_fit$epsilon_link_mean <- 0.25
  state$btl_fit$beta_within_mean <- -0.1
  state$btl_fit$epsilon_within_mean <- 0.05
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "h2", "s22", 0L, spoke_id = 2L)

  state <- pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
  contract <- state$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  expect_identical(contract$judge$mode, "phase_specific")
  expect_identical(contract$judge$scope, "link")
  expect_equal(contract$judge$beta, 0.15, tolerance = 1e-12)
  expect_equal(contract$judge$epsilon, 0.25, tolerance = 1e-12)
})

test_that("phase-specific judge mode allows startup fallback but aborts after startup when link params are missing", {
  state <- make_linking_refit_state(
    list(link_transform_mode = "shift_only", link_refit_mode = "shift_only", judge_param_mode = "phase_specific")
  )
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "h2", "s22", 0L, spoke_id = 2L)
  state$btl_fit$beta_within_mean <- 0.03
  state$btl_fit$epsilon_within_mean <- 0.02
  state$btl_fit$beta_link_mean <- NULL
  state$btl_fit$epsilon_link_mean <- NULL

  out_startup <- expect_no_error(
    pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
  )
  expect_true(isTRUE(
    out_startup$controller$link_refit_stats_by_spoke[["2"]]$fit_contract$judge$cold_start_fallback_used
  ))

  out_startup$btl_fit$beta_link_mean <- NULL
  out_startup$btl_fit$epsilon_link_mean <- NULL
  out_startup <- append_cross_step(out_startup, 3L, "s21", "h3", 1L, spoke_id = 2L)

  expect_error(
    pairwiseLLM:::.adaptive_linking_refit_update_state(out_startup, list(last_refit_step = 2L)),
    "Phase-specific judge mode requires `beta_link_mean`"
  )
})

test_that("linking CmdStan transform refit always sets stable output targets", {
  sample_args_seen <- NULL
  fake_fit <- new.env(parent = emptyenv())
  fake_fit$draws <- function(variables, format) {
    expect_identical(format, "matrix")
    out <- matrix(0.1, nrow = 2, ncol = length(variables))
    colnames(out) <- variables
    out
  }
  fake_fit$diagnostic_summary <- function() {
    tibble::tibble(num_divergent = c(0, 0))
  }
  fake_fit$summary <- function(variables) {
    tibble::tibble(
      variable = variables,
      rhat = rep(1, length(variables)),
      ess_bulk = rep(500, length(variables))
    )
  }
  model_stub <- function(path, cpp_options) {
    expect_true(file.exists(path))
    expect_identical(cpp_options, list(stan_threads = TRUE))
    list(sample = function(...) {
      sample_args_seen <<- list(...)
      fake_fit
    })
  }

  out_dir <- withr::local_tempdir()
  fit <- pairwiseLLM:::.adaptive_link_fit_transform_cmdstan(
    stan_data = list(N = 1L),
    variable_names = c("delta"),
    cmdstan = list(
      chains = 1L,
      parallel_chains = 1L,
      threads_per_chain = 1L,
      iter_warmup = 10L,
      iter_sampling = 10L,
      output_dir = out_dir
    ),
    seed = 123L,
    model_fn = model_stub
  )

  expect_true(is.matrix(fit$draws_matrix))
  expect_identical(sample_args_seen$output_dir, out_dir)
  expect_true(dir.exists(sample_args_seen$output_dir))
  expect_true(is.character(sample_args_seen$output_basename))
  expect_length(sample_args_seen$output_basename, 1L)
  expect_match(sample_args_seen$output_basename, "^link_transform_refit-")
})

test_that("startup-gap helper and edge extractors cover fallback edge paths", {
  state <- make_linking_refit_state()

  expect_true(isTRUE(pairwiseLLM:::.adaptive_link_phase_b_startup_gap_for_spoke(state, 2L)))
  expect_equal(nrow(pairwiseLLM:::.adaptive_link_cross_edges(state, spoke_id = 2L)), 0L)
  expect_equal(nrow(pairwiseLLM:::.adaptive_link_within_edges(state, set_id = 1L)), 0L)

  state$controller$link_refit_stats_by_spoke <- list(`2` = list(delta_spoke_mean = 0))
  expect_false(isTRUE(pairwiseLLM:::.adaptive_link_phase_b_startup_gap_for_spoke(state, 2L)))
})

test_that("joint shift_scale fit rejects unsupported hub lock modes", {
  edges <- tibble::tibble(
    spoke_item = c("s1", "s2"),
    hub_item = c("h1", "h2"),
    y_spoke = c(1L, 0L),
    step_id = c(1L, 2L),
    spoke_in_A = c(TRUE, FALSE)
  )
  attr(edges, "judge_params") <- list(beta = 0.1, epsilon = 0.05, mode = "phase_specific", scope = "link")
  attr(edges, "refit_contract") <- list(
    link_refit_mode = "joint_refit",
    hub_lock_mode = "hard_like",
    hub_lock_kappa = 0.5
  )
  attr(edges, "within_hub_edges") <- tibble::tibble(
    A_item = c("h1", "h2"),
    B_item = c("h2", "h1"),
    y_A = c(1L, 0L),
    step_id = c(3L, 4L)
  )
  attr(edges, "within_spoke_edges") <- tibble::tibble(
    A_item = c("s1", "s2"),
    B_item = c("s2", "s1"),
    y_A = c(1L, 0L),
    step_id = c(5L, 6L)
  )

  hub_theta <- c(h1 = 0.4, h2 = -0.1)
  spoke_theta <- c(s1 = -0.3, s2 = 0.2)
  attr(hub_theta, "theta_sd") <- c(h1 = 0.1, h2 = 0.1)
  attr(spoke_theta, "theta_sd") <- c(s1 = 0.2, s2 = 0.2)

  expect_error(
    pairwiseLLM:::.adaptive_link_fit_transform(
      edges,
      hub_theta = hub_theta,
      spoke_theta = spoke_theta,
      transform_mode = "shift_scale"
    ),
    "Unsupported `hub_lock_mode`"
  )
})

test_that("link likelihood applies signed beta by original presentation side", {
  edges_mixed <- tibble::tibble(
    spoke_item = c("s1", "s1"),
    hub_item = c("h1", "h1"),
    y_spoke = c(1L, 0L),
    step_id = c(1L, 2L),
    spoke_in_A = c(TRUE, FALSE)
  )
  edges_all_a <- edges_mixed
  edges_all_a$spoke_in_A <- c(TRUE, TRUE)
  attr(edges_mixed, "judge_params") <- list(beta = 1, epsilon = 0, mode = "phase_specific", scope = "link")
  attr(edges_all_a, "judge_params") <- list(beta = 1, epsilon = 0, mode = "phase_specific", scope = "link")

  hub_theta <- c(h1 = 0)
  spoke_theta <- c(s1 = 0)

  fit_mixed <- testthat::with_mocked_bindings(
    .adaptive_link_fit_transform_cmdstan = function(stan_data,
                                                    variable_names,
                                                    cmdstan,
                                                    seed,
                                                    model_fn = NULL) {
      delta_draws <- if (sum(stan_data$beta_signed) == 0) {
        c(-0.1, 0, 0.1, 0)
      } else {
        c(-0.6, -0.5, -0.4, -0.5)
      }
      list(
        draws_matrix = cbind(delta = delta_draws),
        diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 1000),
        mcmc_config_used = list(
          chains = 4L,
          parallel_chains = 4L,
          threads_per_chain = 1L,
          cmdstanr_version = "test"
        )
      )
    },
    .package = "pairwiseLLM",
    pairwiseLLM:::.adaptive_link_fit_transform(
      edges_mixed,
      hub_theta,
      spoke_theta,
      transform_mode = "shift_only"
    )
  )
  fit_all_a <- testthat::with_mocked_bindings(
    .adaptive_link_fit_transform_cmdstan = function(stan_data,
                                                    variable_names,
                                                    cmdstan,
                                                    seed,
                                                    model_fn = NULL) {
      delta_draws <- if (sum(stan_data$beta_signed) == 0) {
        c(-0.1, 0, 0.1, 0)
      } else {
        c(-0.6, -0.5, -0.4, -0.5)
      }
      list(
        draws_matrix = cbind(delta = delta_draws),
        diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 1000),
        mcmc_config_used = list(
          chains = 4L,
          parallel_chains = 4L,
          threads_per_chain = 1L,
          cmdstanr_version = "test"
        )
      )
    },
    .package = "pairwiseLLM",
    pairwiseLLM:::.adaptive_link_fit_transform(
      edges_all_a,
      hub_theta,
      spoke_theta,
      transform_mode = "shift_only"
    )
  )

  expect_true(abs(fit_mixed$delta_mean) < 0.5)
  expect_true(fit_all_a$delta_mean < (fit_mixed$delta_mean - 0.05))
})

test_that("shift_only theta treatment records plugin-var default and fixed-eap fallback", {
  fixed <- make_linking_refit_state(
    list(
      link_transform_mode = "shift_only",
      link_refit_mode = "shift_only",
      shift_only_theta_treatment = "fixed_eap"
    )
  )
  plugin <- make_linking_refit_state(
    list(
      link_transform_mode = "shift_only",
      link_refit_mode = "shift_only",
      shift_only_theta_treatment = "fixed_eap_plugin_var"
    )
  )

  fixed <- append_cross_step(fixed, 1L, "s21", "h1", 1L, spoke_id = 2L)
  fixed <- append_cross_step(fixed, 2L, "h2", "s22", 0L, spoke_id = 2L)
  plugin <- append_cross_step(plugin, 1L, "s21", "h1", 1L, spoke_id = 2L)
  plugin <- append_cross_step(plugin, 2L, "h2", "s22", 0L, spoke_id = 2L)

  out_fixed <- pairwiseLLM:::.adaptive_linking_refit_update_state(fixed, list(last_refit_step = 0L))
  out_plugin <- pairwiseLLM:::.adaptive_linking_refit_update_state(plugin, list(last_refit_step = 0L))

  sd_fixed <- out_fixed$controller$link_refit_stats_by_spoke[["2"]]$delta_spoke_sd
  sd_plugin <- out_plugin$controller$link_refit_stats_by_spoke[["2"]]$delta_spoke_sd
  expect_true(is.finite(sd_fixed))
  expect_true(is.finite(sd_plugin))
  c_fixed <- out_fixed$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  c_plugin <- out_plugin$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  expect_identical(c_fixed$theta_treatment, "fixed_eap")
  expect_identical(c_plugin$theta_treatment, "fixed_eap_plugin_var")
  expect_true(sd_plugin >= sd_fixed)
})

test_that("invalid linking mode combinations fail validation", {
  defaults <- pairwiseLLM:::.adaptive_controller_defaults(8L)
  expect_identical(defaults$shift_only_theta_treatment, "fixed_eap_plugin_var")
  expect_true(is.list(defaults$link_transform_state_by_spoke))
  expect_true(is.list(defaults$link_transform_bad_refits_by_spoke))
  expect_true(is.list(defaults$link_refit_stats_by_spoke))

  keys <- pairwiseLLM:::.adaptive_controller_public_keys()
  expect_true("shift_only_theta_treatment" %in% keys)

  ok <- pairwiseLLM:::.adaptive_validate_controller_config(
    list(shift_only_theta_treatment = "fixed_eap_plugin_var"),
    n_items = 5L,
    set_ids = c(1L, 2L)
  )
  expect_identical(ok$shift_only_theta_treatment, "fixed_eap_plugin_var")

  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(shift_only_theta_treatment = "bad"),
      n_items = 5L,
      set_ids = c(1L, 2L)
    ),
    "must be one of"
  )

  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(
        run_mode = "link_multi_spoke",
        multi_spoke_mode = "concurrent",
        link_refit_mode = "joint_refit",
        hub_lock_mode = "free"
      ),
      n_items = 5L,
      set_ids = c(1L, 2L, 3L)
    ),
    "only supported"
  )
})

test_that("linking runtime aborts loudly if an unsupported hub lock mode leaks into refit state", {
  state <- make_linking_refit_state(list(link_refit_mode = "joint_refit", hub_lock_mode = "soft_lock"))
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state$controller$hub_lock_mode <- "bogus"

  expect_error(
    pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L)),
    "Unsupported `hub_lock_mode`"
  )
})

test_that("free hub lock leaves hub unanchored and blocks linking stop", {
  state <- make_stable_epoch_stop_state()
  state$linking$run_mode <- "link_one_spoke"
  state$linking$spoke_ids <- 2L
  state$controller$run_mode <- "link_one_spoke"
  state$controller$multi_spoke_mode <- "independent"
  state$controller$link_refit_mode <- "joint_refit"
  state$controller$hub_lock_mode <- "free"
  state$controller$link_epoch_signature_by_spoke <- list(
    `2` = current_link_epoch_signature(state, spoke_id = 2L)
  )

  out <- run_mocked_stop_window_refit(state)
  stats <- out$controller$link_refit_stats_by_spoke[["2"]]
  row <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    out,
    refit_id = 3L,
    refit_context = list(last_refit_step = 3L)
  )
  row <- row[row$spoke_id == 2L, , drop = FALSE]

  expect_false(isTRUE(stats$hub_anchored))
  expect_false(isTRUE(stats$link_stop_pass))
  expect_match(as.character(stats$stop_blocker_codes), "hub_not_anchored")
  expect_false(isTRUE(row$hub_anchored[[1L]]))
  expect_match(as.character(row$stop_blocker_codes[[1L]]), "hub_not_anchored")
})

test_that("within-set candidate routing remains independent of linking refit fields", {
  state <- adaptive_rank_start(make_test_items(8), seed = 42L)
  state$warm_start_done <- TRUE

  before <- pairwiseLLM:::generate_stage_candidates_from_state(
    state,
    stage_name = "mid_link",
    fallback_name = "base",
    C_max = 5000L,
    seed = 99L
  )

  state$btl_fit$link_draws <- matrix(runif(20), nrow = 4)
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(delta_spoke_mean = 3, delta_spoke_sd = 9, log_alpha_spoke_mean = -2, log_alpha_spoke_sd = 8)
  )
  after <- pairwiseLLM:::generate_stage_candidates_from_state(
    state,
    stage_name = "mid_link",
    fallback_name = "base",
    C_max = 5000L,
    seed = 99L
  )

  expect_identical(before, after)
})

test_that("phase-B candidate routing responds to linking-global transform parameters", {
  state <- make_linking_refit_state()
  state$warm_start_done <- TRUE
  state$controller$current_link_spoke_id <- 2L
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "h2", "s22", 0L, spoke_id = 2L)
  active_ids <- c("h1", "h2", "s21", "s22")

  base <- pairwiseLLM:::.adaptive_link_phase_b_routing_scores(
    state = state,
    controller = state$controller,
    active_ids = active_ids,
    hub_id = 1L
  )

  state$controller$link_refit_stats_by_spoke <- list(`2` = list(delta_spoke_mean = 4))
  shifted_up <- pairwiseLLM:::.adaptive_link_phase_b_routing_scores(
    state = state,
    controller = state$controller,
    active_ids = active_ids,
    hub_id = 1L
  )

  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(delta_spoke_mean = -4, log_alpha_spoke_mean = log(1.4), link_transform_state = "shift_scale")
  )
  state$controller$link_transform_state_by_spoke <- list(`2` = "shift_scale")
  shifted_down_scale <- pairwiseLLM:::.adaptive_link_phase_b_routing_scores(
    state = state,
    controller = state$controller,
    active_ids = active_ids,
    hub_id = 1L
  )

  expect_false(identical(base, shifted_up))
  expect_false(identical(shifted_up, shifted_down_scale))
})

test_that("concurrent allocation uses utility mass and enforces floor", {
  alloc <- pairwiseLLM:::.adaptive_link_concurrent_targets(
    spoke_stats = list(
      `2` = list(utility_mass = 0.8, candidate_count = 99L),
      `3` = list(utility_mass = 0.2, candidate_count = 99L)
    ),
    total_pairs = 16L,
    floor_pairs = 5L
  )
  expect_true(all(alloc >= 5L))
  expect_identical(sum(alloc), 16L)
  expect_true(alloc[["2"]] > alloc[["3"]])

  state <- make_linking_refit_state(
    list(
      multi_spoke_mode = "concurrent",
      min_cross_set_pairs_per_spoke_per_refit = 2L,
      hub_lock_mode = "soft_lock"
    )
  )
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "s22", "h2", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 3L, "s31", "h1", 0L, spoke_id = 3L)

  state <- pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
  stats2 <- state$controller$link_refit_stats_by_spoke[["2"]]
  stats3 <- state$controller$link_refit_stats_by_spoke[["3"]]

  expect_true(isTRUE(stats2$concurrent_floor_met))
  expect_false(isTRUE(stats3$concurrent_floor_met))
  expect_true(stats2$concurrent_target_pairs >= 2L)
  expect_true(stats3$concurrent_target_pairs >= 2L)
  expect_true(is.finite(stats2$concurrent_utility_mass))
  expect_true(is.finite(stats3$concurrent_utility_mass))
})

test_that("independent mode authorizes exactly one spoke per refit window", {
  state <- make_linking_refit_state(list(multi_spoke_mode = "independent"))

  budget_map <- pairwiseLLM:::.adaptive_link_budget_map_for_refit(
    state = state,
    controller = state$controller,
    eligible_spoke_ids = c(2L, 3L)
  )
  budgets <- vapply(
    budget_map,
    function(entry) as.integer(entry$B_spoke_refit_budget %||% NA_integer_),
    integer(1L)
  )
  active_spoke <- as.integer(names(budgets)[budgets > 0L][[1L]])
  inactive_spoke <- setdiff(c(2L, 3L), active_spoke)

  expect_identical(sum(budgets > 0L), 1L)
  expect_identical(
    budget_map[[as.character(active_spoke)]]$B_spoke_refit_budget_source,
    "single_spoke_controller_feasible_capacity"
  )
  expect_identical(budget_map[[as.character(inactive_spoke)]]$B_spoke_refit_budget, 0L)
  expect_identical(
    budget_map[[as.character(inactive_spoke)]]$B_spoke_refit_budget_source,
    "independent_inactive_spoke"
  )

  state$controller$link_budget_refit_id <- pairwiseLLM:::.adaptive_link_refit_window_id(state)
  state$controller$link_budget_map <- budget_map
  if (identical(active_spoke, 2L)) {
    state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  } else {
    state <- append_cross_step(state, 1L, "s31", "h1", 1L, spoke_id = 3L)
  }

  expect_identical(
    pairwiseLLM:::.adaptive_link_active_spoke(state, state$controller, eligible_spoke_ids = c(2L, 3L)),
    active_spoke
  )

  state <- pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row_active <- rows[rows$spoke_id == active_spoke, , drop = FALSE]
  row_inactive <- rows[rows$spoke_id == inactive_spoke, , drop = FALSE]
  expect_identical(nrow(row_active), 1L)
  expect_identical(nrow(row_inactive), 1L)
  expect_true(row_active$B_spoke_refit_budget[[1L]] > 0L)
  expect_identical(row_inactive$B_spoke_refit_budget[[1L]], 0L)
  expect_identical(
    as.character(row_inactive$B_spoke_refit_budget_source[[1L]]),
    "independent_inactive_spoke"
  )
})

test_that("independent mode compacts the active spoke budget to feasible late-phase capacity", {
  state <- make_linking_refit_state(list(multi_spoke_mode = "independent"))
  state$controller$current_link_spoke_id <- 2L

  budget_map <- testthat::with_mocked_bindings(
    .adaptive_round_compute_quotas = function(round_id, n_items, controller) {
      stats::setNames(c(4L, 4L, 4L, 3L), c("anchor_link", "long_link", "mid_link", "local_link"))
    },
    .adaptive_link_adjust_stage_quotas_for_feasibility = function(...) {
      adjusted <- stats::setNames(c(4L, 3L, 2L, 0L), c("anchor_link", "long_link", "mid_link", "local_link"))
      attr(adjusted, "quota_meta") <- list()
      adjusted
    },
    pairwiseLLM:::.adaptive_link_budget_map_for_refit(
      state = state,
      controller = state$controller,
      eligible_spoke_ids = c(2L, 3L)
    ),
    .package = "pairwiseLLM"
  )

  expect_identical(budget_map[["2"]]$B_spoke_refit_budget, 9L)
  expect_identical(
    budget_map[["2"]]$B_spoke_refit_budget_source,
    "single_spoke_controller_feasible_capacity"
  )
  expect_identical(budget_map[["3"]]$B_spoke_refit_budget, 0L)
})

test_that("phase B window exhaustion can trigger an early refit without a starved step", {
  state <- make_linking_refit_state(list(multi_spoke_mode = "independent"))
  state <- append_cross_step(state, 1L, "h1", "s21", 1L, spoke_id = 2L)
  state$step_log$link_stage <- "anchor_link"
  state$step_log$round_stage <- "anchor_link"

  fit_fn <- function(state, config) {
    draws <- matrix(
      c(
        0.2, 0.1, 0.0, -0.1, -0.2, -0.3, -0.4,
        0.1, 0.0, -0.1, -0.2, -0.3, -0.4, -0.5
      ),
      nrow = 2,
      byrow = TRUE
    )
    colnames(draws) <- as.character(state$item_ids)
    make_test_btl_fit(state$item_ids, draws = draws, model_variant = "btl_e_b")
  }

  out <- testthat::with_mocked_bindings(
    .adaptive_refit_scope_counts = function(state) {
      list(
        M_done = 1L,
        last_refit_M_done = 0L,
        last_refit_step = 0L,
        scope_set_id = NA_integer_
      )
    },
    .adaptive_refit_pairs_target = function(state, config) 30L,
    .adaptive_refit_eligibility = function(total_committed, last_refit_committed, refit_pairs_target) {
      list(eligible = FALSE)
    },
    .adaptive_link_phase_b_window_exhausted = function(...) TRUE,
    pairwiseLLM:::maybe_refit_btl(
      state = state,
      config = state$config$btl_config,
      fit_fn = fit_fn
    ),
    .package = "pairwiseLLM"
  )

  expect_true(isTRUE(out$refit_performed))
  expect_identical(out$state$refit_meta$last_refit_M_done, 1L)
  expect_identical(out$state$refit_meta$last_refit_step, 1L)
})

test_that("run_one_step does not rewrite independent Phase B budget map within a refit window", {
  state <- make_linking_refit_state(list(multi_spoke_mode = "independent"))
  budget_map <- pairwiseLLM:::.adaptive_link_budget_map_for_refit(
    state = state,
    controller = state$controller,
    eligible_spoke_ids = c(2L, 3L)
  )
  state$controller$link_budget_refit_id <- pairwiseLLM:::.adaptive_link_refit_window_id(state)
  state$controller$link_budget_map <- budget_map

  active_spoke <- as.integer(names(which(vapply(
    budget_map,
    function(entry) as.integer(entry$B_spoke_refit_budget %||% 0L) > 0L,
    logical(1L)
  )))[[1L]])
  inactive_spoke <- setdiff(c(2L, 3L), active_spoke)
  state$controller$current_link_spoke_id <- inactive_spoke

  judged <- FALSE
  judge <- function(...) {
    judged <<- TRUE
    list(winner = "A")
  }

  out <- testthat::with_mocked_bindings(
    select_next_pair = function(state, step_id = NULL, candidates = NULL) {
      list(
        i = 1L,
        j = 3L,
        A = 1L,
        B = 3L,
        round_id = 1L,
        round_stage = "anchor_link",
        pair_type = "anchor_link",
        run_mode = "link_multi_spoke",
        link_spoke_id_selected = active_spoke,
        is_probe_step = FALSE
      )
    },
    pairwiseLLM:::run_one_step(state, judge = judge),
    .package = "pairwiseLLM"
  )

  expect_true(judged)
  expect_identical(out$controller$link_budget_refit_id, state$controller$link_budget_refit_id)
  expect_identical(out$controller$link_budget_map, budget_map)
})

test_that("link stage rows abort when realized active work exceeds emitted budget", {
  state <- make_linking_refit_state(list(multi_spoke_mode = "independent"))
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "s22", "h2", 1L, spoke_id = 2L)
  state$step_log$link_stage <- c("anchor_link", "anchor_link")
  state$step_log$round_stage <- c("anchor_link", "anchor_link")
  state$controller$link_budget_refit_id <- 1L
  state$controller$link_budget_map <- list(
    `2` = list(
      B_spoke_refit_budget = 1L,
      B_spoke_refit_budget_source = "single_spoke_controller"
    ),
    `3` = list(
      B_spoke_refit_budget = 0L,
      B_spoke_refit_budget_source = "independent_inactive_spoke"
    )
  )

  expect_error(
    pairwiseLLM:::.adaptive_link_stage_refit_rows(
      state,
      refit_id = 1L,
      refit_context = list(last_refit_step = 0L)
    ),
    "realized active counts exceed emitted budget"
  )
})

test_that("link stage rows use cached concurrent budget for the completed refit window", {
  state <- make_linking_refit_state(list(multi_spoke_mode = "concurrent"))
  state$round_log <- pairwiseLLM:::append_round_log(
    state$round_log,
    list(
      refit_id = 1L,
      round_id_at_refit = 1L,
      step_id_at_refit = 15L,
      model_variant = "btl_e_b",
      n_items = nrow(state$items)
    )
  )
  state$step_log <- tibble::tibble(
    step_id = seq_len(15L),
    pair_id = seq_len(15L),
    is_cross_set = rep(TRUE, 15L),
    link_spoke_id = rep(3L, 15L),
    link_stage = rep("anchor_link", 15L),
    round_stage = rep("anchor_link", 15L),
    run_mode = rep("link_multi_spoke", 15L)
  )
  state$controller$link_budget_refit_id <- 1L
  state$controller$link_budget_map <- list(
    `2` = list(
      B_spoke_refit_budget = 0L,
      B_spoke_refit_budget_source = "concurrent_allocator",
      concurrent_target_pairs = 0L,
      concurrent_floor_pairs = 5L
    ),
    `3` = list(
      B_spoke_refit_budget = 15L,
      B_spoke_refit_budget_source = "concurrent_allocator",
      concurrent_target_pairs = 15L,
      concurrent_floor_pairs = 5L
    )
  )

  rows <- testthat::with_mocked_bindings(
    .adaptive_link_budget_map_for_refit = function(...) {
      list(
        `2` = list(B_spoke_refit_budget = 1L, B_spoke_refit_budget_source = "concurrent_allocator"),
        `3` = list(B_spoke_refit_budget = 14L, B_spoke_refit_budget_source = "concurrent_allocator")
      )
    },
    pairwiseLLM:::.adaptive_link_stage_refit_rows(
      state,
      refit_id = 1L,
      refit_context = list(last_refit_step = 0L)
    ),
    .package = "pairwiseLLM"
  )

  row_spoke_3 <- rows[rows$spoke_id == 3L, , drop = FALSE]
  expect_identical(as.integer(row_spoke_3$B_spoke_refit_budget[[1L]]), 15L)
  expect_identical(as.integer(row_spoke_3$stage_realized_anchor_link[[1L]]), 15L)
  expect_identical(as.integer(row_spoke_3$stage_budget_unfilled[[1L]]), 0L)
})

test_that("link_stage_log rows expose feasibility and blocker explanations canonically", {
  state <- make_linking_refit_state(list(multi_spoke_mode = "independent"))
  panel_keys <- paste0("pair_", seq_len(30L))
  state$linking$probe$panels_by_spoke <- list(
    `2` = tibble::tibble(
      probe_panel_id = "panel_eval",
      link_epoch_id = 4L,
      spoke_id = 2L,
      hub_item_id = rep("h1", 30L),
      spoke_item_id = paste0("s21_probe_", seq_len(30L)),
      spoke_bin = rep(1L, 30L),
      hub_bin = rep(1L, 30L),
      planned_rank = seq_len(30L),
      pair_key = panel_keys,
      realized = FALSE,
      realized_step_id = NA_integer_,
      realized_pair_id = NA_integer_,
      realized_run_mode = NA_character_
    )
  )
  state$linking$probe$realized_edges <- tibble::tibble(
    step_id = seq_len(15L),
    pair_id = seq_len(15L),
    run_mode = rep("link_probe_holdout", 15L),
    spoke_id = rep(2L, 15L),
    link_epoch_id = rep(4L, 15L),
    probe_panel_id = rep("panel_eval", 15L),
    hub_item_id = rep("h1", 15L),
    spoke_item_id = paste0("s21_probe_", seq_len(15L)),
    pair_key = panel_keys[seq_len(15L)],
    Y = rep(1L, 15L)
  )
  state$controller$link_budget_refit_id <- 1L
  state$controller$link_budget_map <- list(
    `2` = list(
      B_spoke_refit_budget = 6L,
      B_spoke_refit_budget_source = "single_spoke_controller"
    ),
    `3` = list(
      B_spoke_refit_budget = 0L,
      B_spoke_refit_budget_source = "independent_inactive_spoke"
    )
  )
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_epoch_id = 4L,
      linking_identified = TRUE,
      link_stop_eligible = FALSE,
      probe_panel_shortfall = 15L,
      probe_panel_id = "panel_eval",
      probe_edges_planned = 30L,
      probe_edges_realized = 15L,
      probe_edges_min_for_stop_used = 30L,
      probe_brier = 0.38,
      probe_brier_max_used = 0.19,
      probe_brier_pass = FALSE,
      probe_pred_rmse_lagged = 0.03,
      probe_pred_rmse_max_used = 0.015,
      probe_pred_rmse_pass = FALSE,
      theta_global_rmse_lagged = 0.08,
      theta_global_rmse_max_used = 0.05,
      theta_global_rmse_pass = FALSE,
      delta_spoke_sd = 0.20,
      link_stop_reliability_min_used = 0.90,
      reliability_link_global = NA_real_,
      reliability_stop_pass = FALSE,
      stop_blocker_codes = paste(
        c(
          "diagnostics_failed",
          "lag_not_eligible",
          "min_refits_not_met",
          "probe_edges_min_for_stop",
          "reliability_link_global",
          "probe_brier",
          "probe_pred_rmse_lagged",
          "theta_global_rmse_lagged",
          "hub_not_anchored"
        ),
        collapse = ","
      ),
      lag_domain_reset_reason = "spoke_artifact_replaced"
    )
  )

  rows <- testthat::with_mocked_bindings(
    generate_stage_candidates_from_state = function(state, stage_name, ...) {
      n <- switch(stage_name,
        anchor_link = 4L,
        long_link = 0L,
        mid_link = 4L,
        local_link = 4L
      )
      if (n < 1L) {
        return(tibble::tibble())
      }
      tibble::tibble(i = rep("h1", n), j = paste0(stage_name, "_", seq_len(n)))
    },
    .adaptive_filter_link_backfill_candidates = function(candidates, ...) {
      list(candidates = tibble::as_tibble(candidates), counts = list(), star_caps = list())
    },
    .adaptive_link_attach_predictive_utility = function(candidates, ...) {
      cand <- tibble::as_tibble(candidates)
      cand$link_d_opt_gain <- 1
      cand$link_u <- 1
      cand
    },
    pairwiseLLM:::.adaptive_link_stage_refit_rows(
      state,
      refit_id = 1L,
      refit_context = list(last_refit_step = 0L)
    ),
    .package = "pairwiseLLM"
  )

  row <- rows[rows$spoke_id == 2L, , drop = FALSE]
  expect_identical(row$feasible_stage_capacity_anchor_link[[1L]], 4L)
  expect_identical(row$feasible_stage_capacity_long_link[[1L]], 0L)
  expect_identical(row$feasibility_budget_released[[1L]], 3L)
  expect_true(isTRUE(row$feasibility_reallocation_used[[1L]]))
  expect_identical(
    as.character(row$feasibility_reallocation_rule[[1L]]),
    "pooled_utility_backfill"
  )
  expect_identical(as.integer(row$link_epoch_id[[1L]]), 4L)
  expect_identical(as.character(row$probe_panel_id[[1L]]), "panel_eval")
  expect_identical(as.integer(row$probe_edges_planned[[1L]]), 30L)
  expect_identical(as.integer(row$probe_edges_realized[[1L]]), 15L)
  expect_identical(
    as.character(row$probe_acceleration_mode_used[[1L]]),
    "active_floor_plus_sole_blocker"
  )
  expect_identical(as.integer(row$probe_active_floor_used[[1L]]), 20L)
  expect_false(isTRUE(row$probe_only_blocker_trigger[[1L]]))
  expect_false(isTRUE(row$probe_acceleration_used[[1L]]))
  expect_identical(as.integer(row$probe_effort_base_cap[[1L]]), 2L)
  expect_identical(as.integer(row$probe_effort_effective_cap[[1L]]), 2L)
  expect_identical(as.integer(row$probe_remaining_to_min_start[[1L]]), 15L)
  expect_identical(as.character(row$stop_blocker_codes[[1L]]), paste(
    c(
      "diagnostics_failed",
      "lag_not_eligible",
      "min_refits_not_met",
      "probe_edges_min_for_stop",
      "reliability_link_global",
      "probe_brier",
      "probe_pred_rmse_lagged",
      "theta_global_rmse_lagged",
      "hub_not_anchored"
    ),
    collapse = ","
  ))
  expect_false(isTRUE(row$probe_brier_pass[[1L]]))
  expect_false(isTRUE(row$probe_pred_rmse_pass[[1L]]))
  expect_false(isTRUE(row$theta_global_rmse_pass[[1L]]))
  expect_identical(as.character(row$lag_domain_reset_reason[[1L]]), "spoke_artifact_replaced")
})

test_that("link_stage_log probe sole-blocker audit uses refit-start blocker state", {
  build_probe_audit_state <- function(realized_before_refit,
                                      realized_current_refit,
                                      current_stop_blocker_codes) {
    total_realized <- as.integer(realized_before_refit + realized_current_refit)
    state <- make_linking_refit_state(list(
      multi_spoke_mode = "independent",
      probe_edges_min_for_stop = 30L,
      probe_sole_blocker_min_realized = 20L,
      probe_pairs_per_refit_per_spoke = 2L,
      probe_pairs_per_refit_per_spoke_sole_blocker_max = 10L
    ))
    state$controller$linking_identified_by_spoke <- list(`2` = TRUE)
    state$controller$link_epoch_id_by_spoke <- list(`2` = 4L)
    state$controller$link_budget_refit_id <- 7L
    state$controller$link_budget_map <- list(
      `2` = list(
        B_spoke_refit_budget = 10L,
        B_spoke_refit_budget_source = "single_spoke_controller"
      )
    )
    state$refit_meta$last_refit_step <- as.integer(realized_before_refit)

    panel_keys <- paste0("probe_pair_", seq_len(40L))
    spoke_probe_ids <- rep(c("s21", "s22"), length.out = 40L)
    state$linking$probe$panels_by_spoke <- list(
      `2` = tibble::tibble(
        probe_panel_id = "panel_a",
        link_epoch_id = 4L,
        spoke_id = 2L,
        hub_item_id = rep("h1", 40L),
        spoke_item_id = spoke_probe_ids,
        spoke_bin = rep(1L, 40L),
        hub_bin = rep(1L, 40L),
        planned_rank = seq_len(40L),
        pair_key = panel_keys,
        realized = seq_len(40L) <= total_realized,
        realized_step_id = c(seq_len(total_realized), rep(NA_integer_, 40L - total_realized)),
        realized_pair_id = c(seq_len(total_realized), rep(NA_integer_, 40L - total_realized)),
        realized_run_mode = c(
          rep("link_probe_holdout", total_realized),
          rep(NA_character_, 40L - total_realized)
        )
      )
    )
    state$linking$probe$realized_edges <- tibble::tibble(
      step_id = seq_len(total_realized),
      pair_id = seq_len(total_realized),
      run_mode = rep("link_probe_holdout", total_realized),
      spoke_id = rep(2L, total_realized),
      link_epoch_id = rep(4L, total_realized),
      probe_panel_id = rep("panel_a", total_realized),
      hub_item_id = rep("h1", total_realized),
      spoke_item_id = spoke_probe_ids[seq_len(total_realized)],
      pair_key = panel_keys[seq_len(total_realized)],
      Y = rep(1L, total_realized)
    )
    for (step_id in seq_len(total_realized)) {
      state <- append_probe_step(
        state = state,
        step_id = step_id,
        hub_item_id = "h1",
        spoke_item_id = spoke_probe_ids[[step_id]],
        Y = 1L,
        spoke_id = 2L
      )
    }

    state$controller$link_refit_stats_by_spoke <- list(
      `2` = list(
        link_epoch_id = 4L,
        link_identified = TRUE,
        linking_identified = TRUE,
        link_stop_eligible = identical(current_stop_blocker_codes, "none"),
        probe_panel_shortfall = max(0L, 40L - total_realized),
        probe_panel_id = "panel_a",
        probe_edges_planned = 40L,
        probe_edges_realized = total_realized,
        probe_edges_min_for_stop_used = 30L,
        probe_brier = 0.10,
        probe_brier_max_used = 0.19,
        probe_brier_pass = TRUE,
        probe_pred_rmse_lagged = 0.01,
        probe_pred_rmse_max_used = 0.015,
        probe_pred_rmse_pass = TRUE,
        theta_global_rmse_lagged = 0.02,
        theta_global_rmse_max_used = 0.05,
        theta_global_rmse_pass = TRUE,
        delta_spoke_sd = 0.20,
        link_stop_reliability_min_used = 0.90,
        reliability_link_global = 0.95,
        reliability_stop_pass = TRUE,
        link_diagnostics_pass = TRUE,
        link_lag_eligible = TRUE,
        lag_eligible = TRUE,
        link_min_refit_eligible = TRUE,
        hub_anchored = TRUE,
        stop_blocker_codes = current_stop_blocker_codes
      )
    )

    prior_surface_row <- list(
      refit_id = 6L,
      spoke_id = 2L,
      hub_id = 1L,
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_refit_mode = "shift_only",
      hub_lock_mode = "soft_lock",
      link_epoch_id = 4L,
      probe_panel_id = "panel_a",
      probe_edges_planned = 40L,
      probe_edges_realized_before_refit = max(0L, realized_before_refit - 2L),
      probe_edges_realized_delta_since_last_refit = 2L,
      probe_edges_realized = as.integer(realized_before_refit),
      probe_edges_min_for_stop_used = 30L,
      probe_panel_shortfall = max(0L, 40L - realized_before_refit),
      link_diagnostics_pass = TRUE,
      link_lag_eligible = TRUE,
      link_min_refit_eligible = TRUE,
      link_stop_reliability_min_used = 0.90,
      reliability_link_global = 0.95,
      probe_brier = 0.10,
      probe_brier_max_used = 0.19,
      probe_pred_rmse_lagged = 0.01,
      probe_pred_rmse_max_used = 0.015,
      theta_global_rmse_lagged = 0.02,
      theta_global_rmse_max_used = 0.05,
      hub_anchored = TRUE,
      stop_blocker_codes = "probe_edges_min_for_stop",
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
    state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
      state$link_stage_log,
      prior_surface_row
    )

    state
  }

  rows_before_threshold <- testthat::with_mocked_bindings(
    generate_stage_candidates_from_state = function(...) tibble::tibble(),
    .adaptive_filter_link_backfill_candidates = function(candidates, ...) {
      list(candidates = tibble::as_tibble(candidates), counts = list(), star_caps = list())
    },
    .adaptive_link_attach_predictive_utility = function(candidates, ...) {
      tibble::as_tibble(candidates)
    },
    pairwiseLLM:::.adaptive_link_stage_refit_rows(
      state = build_probe_audit_state(
        realized_before_refit = 18L,
        realized_current_refit = 2L,
        current_stop_blocker_codes = "probe_edges_min_for_stop"
      ),
      refit_id = 7L,
      refit_context = list(last_refit_step = 18L)
    ),
    .package = "pairwiseLLM"
  )
  row_before_threshold <- rows_before_threshold[rows_before_threshold$spoke_id == 2L, , drop = FALSE]
  expect_identical(as.integer(row_before_threshold$probe_edges_realized_before_refit[[1L]]), 18L)
  expect_false(isTRUE(row_before_threshold$probe_only_blocker_trigger[[1L]]))
  expect_false(isTRUE(row_before_threshold$probe_acceleration_used[[1L]]))
  expect_identical(as.integer(row_before_threshold$probe_effort_effective_cap[[1L]]), 2L)
  expect_identical(as.integer(row_before_threshold$probe_remaining_to_min_start[[1L]]), 12L)

  rows_at_threshold <- testthat::with_mocked_bindings(
    generate_stage_candidates_from_state = function(...) tibble::tibble(),
    .adaptive_filter_link_backfill_candidates = function(candidates, ...) {
      list(candidates = tibble::as_tibble(candidates), counts = list(), star_caps = list())
    },
    .adaptive_link_attach_predictive_utility = function(candidates, ...) {
      tibble::as_tibble(candidates)
    },
    pairwiseLLM:::.adaptive_link_stage_refit_rows(
      state = build_probe_audit_state(
        realized_before_refit = 20L,
        realized_current_refit = 10L,
        current_stop_blocker_codes = "none"
      ),
      refit_id = 7L,
      refit_context = list(last_refit_step = 20L)
    ),
    .package = "pairwiseLLM"
  )
  row_at_threshold <- rows_at_threshold[rows_at_threshold$spoke_id == 2L, , drop = FALSE]
  expect_identical(as.character(row_at_threshold$stop_blocker_codes[[1L]]), "none")
  expect_identical(as.integer(row_at_threshold$probe_edges_realized_before_refit[[1L]]), 20L)
  expect_true(isTRUE(row_at_threshold$probe_only_blocker_trigger[[1L]]))
  expect_true(isTRUE(row_at_threshold$probe_acceleration_used[[1L]]))
  expect_identical(as.integer(row_at_threshold$probe_active_floor_used[[1L]]), 10L)
  expect_identical(as.integer(row_at_threshold$probe_effort_effective_cap[[1L]]), 10L)
  expect_identical(as.integer(row_at_threshold$probe_remaining_to_min_start[[1L]]), 10L)
})

test_that("linking stage targets are deterministic from budget, floors, and taper", {
  controller <- pairwiseLLM:::.adaptive_controller_defaults(10L)
  q_base <- pairwiseLLM:::.adaptive_link_compute_stage_targets(
    budget = 10L,
    controller = controller,
    linking_identified = FALSE
  )
  q_taper <- pairwiseLLM:::.adaptive_link_compute_stage_targets(
    budget = 10L,
    controller = controller,
    linking_identified = TRUE
  )

  expect_identical(unname(q_base[c("anchor_link", "long_link", "mid_link", "local_link")]), c(3L, 4L, 2L, 1L))
  expect_identical(unname(q_taper[c("anchor_link", "long_link", "mid_link", "local_link")]), c(4L, 2L, 3L, 1L))

  meta_taper <- attr(q_taper, "quota_meta")
  expect_true(isTRUE(meta_taper$long_link_taper_applied))
  expect_identical(meta_taper$stage_target_long_link_pre_taper, 4L)
  expect_identical(meta_taper$stage_target_long_link_post_taper, 2L)
})

test_that("canonical blocker weights are deterministic and conservative on missing metrics", {
  controller <- pairwiseLLM:::.adaptive_controller_defaults(10L)

  weights_missing <- pairwiseLLM:::.adaptive_link_blocker_weights(
    stats_row = list(),
    controller = controller
  )
  expect_identical(
    unname(weights_missing),
    c(0, 0, 0, 0, 0)
  )

  weights <- pairwiseLLM:::.adaptive_link_blocker_weights(
    stats_row = list(
      probe_panel_shortfall = 15L,
      probe_edges_min_for_stop_used = 30L,
      probe_brier = 0.38,
      probe_pred_rmse_lagged = 0.03,
      theta_global_rmse_lagged = 0.08,
      delta_spoke_sd = 0.20,
      delta_sd_max_used = 0.10
    ),
    controller = controller
  )
  expect_equal(weights[["probe_panel_shortfall"]], 0.5, tolerance = 1e-12)
  expect_equal(weights[["probe_brier"]], 1, tolerance = 1e-12)
  expect_equal(weights[["probe_pred_rmse_lagged"]], 1, tolerance = 1e-12)
  expect_equal(weights[["theta_global_rmse_lagged"]], 0.6, tolerance = 1e-12)
  expect_equal(weights[["delta_spoke_sd"]], 1, tolerance = 1e-12)
})

test_that("late-phase taper redistribution leans toward anchor when probe and delta blockers dominate", {
  controller <- pairwiseLLM:::.adaptive_controller_defaults(10L)
  controller$current_link_spoke_id <- 2L
  controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      probe_panel_shortfall = 30L,
      probe_edges_min_for_stop_used = 30L,
      delta_spoke_sd = 0.30,
      delta_sd_max_used = 0.10
    )
  )

  q_taper <- pairwiseLLM:::.adaptive_link_compute_stage_targets(
    budget = 10L,
    controller = controller,
    linking_identified = TRUE
  )

  expect_identical(unname(q_taper[c("anchor_link", "long_link", "mid_link", "local_link")]), c(5L, 2L, 2L, 1L))
})

test_that("identified Phase B feasibility adjustment prefers local backfill when theta RMSE dominates", {
  state <- make_linking_refit_state(list(link_transform_mode = "shift_only"))
  state$round$round_id <- 1L
  state$refit_meta$last_refit_step <- 0L

  base_quotas <- c(anchor_link = 3L, long_link = 2L, mid_link = 1L, local_link = 0L)
  attr(base_quotas, "quota_meta") <- list(linking_identified = TRUE)
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      theta_global_rmse_lagged = 0.20,
      theta_global_rmse_max_used = 0.05
    )
  )

  adjusted <- testthat::with_mocked_bindings(
    generate_stage_candidates_from_state = function(state, stage_name, ...) {
      n <- switch(stage_name,
        anchor_link = 3L,
        long_link = 0L,
        mid_link = 4L,
        local_link = 4L
      )
      if (n < 1L) {
        return(tibble::tibble())
      }
      tibble::tibble(
        i = rep("h1", n),
        j = paste0(stage_name, "_", seq_len(n))
      )
    },
    .adaptive_filter_link_backfill_candidates = function(candidates, ...) {
      list(candidates = tibble::as_tibble(candidates), counts = list(), star_caps = list())
    },
    .adaptive_link_attach_predictive_utility = function(candidates, ...) {
      cand <- tibble::as_tibble(candidates)
      cand$link_d_opt_gain <- 1
      cand$link_u <- 1
      cand
    },
    pairwiseLLM:::.adaptive_link_adjust_stage_quotas_for_feasibility(
      state = state,
      controller = utils::modifyList(state$controller, list(current_link_spoke_id = 2L)),
      spoke_id = 2L,
      stage_quotas = base_quotas,
      stage_order = pairwiseLLM:::.adaptive_stage_order(),
      refit_id = 1L
    ),
    .package = "pairwiseLLM"
  )

  expect_identical(adjusted[["long_link"]], 0L)
  expect_identical(adjusted[["anchor_link"]], 3L)
  expect_true(adjusted[["local_link"]] >= 1L)
  expect_true((adjusted[["mid_link"]] + adjusted[["local_link"]]) >= 3L)
})

test_that("phase B feasibility adjustment reduces impossible stage targets before starvation", {
  state <- make_linking_refit_state(list(link_transform_mode = "shift_only"))
  state$round$round_id <- 1L
  state$refit_meta$last_refit_step <- 0L

  base_quotas <- c(anchor_link = 2L, long_link = 2L, mid_link = 1L, local_link = 1L)
  attr(base_quotas, "quota_meta") <- list(linking_identified = FALSE)

  adjusted <- testthat::with_mocked_bindings(
    generate_stage_candidates_from_state = function(state, stage_name, ...) {
      n <- switch(stage_name,
        anchor_link = 2L,
        long_link = 0L,
        mid_link = 3L,
        local_link = 2L
      )
      if (n < 1L) {
        return(tibble::tibble())
      }
      tibble::tibble(
        i = rep("h1", n),
        j = paste0("s", seq_len(n))
      )
    },
    .adaptive_filter_link_backfill_candidates = function(candidates, ...) {
      list(candidates = tibble::as_tibble(candidates), counts = list(), star_caps = list())
    },
    .adaptive_link_attach_predictive_utility = function(candidates, ...) {
      cand <- tibble::as_tibble(candidates)
      cand$link_d_opt_gain <- seq(from = nrow(cand), to = 1, by = -1)
      cand$link_u <- cand$link_d_opt_gain
      cand
    },
    pairwiseLLM:::.adaptive_link_adjust_stage_quotas_for_feasibility(
      state = state,
      controller = state$controller,
      spoke_id = 2L,
      stage_quotas = base_quotas,
      stage_order = pairwiseLLM:::.adaptive_stage_order(),
      refit_id = 1L
    ),
    .package = "pairwiseLLM"
  )

  expect_identical(adjusted[["anchor_link"]], 2L)
  expect_identical(adjusted[["long_link"]], 0L)
  meta <- attr(adjusted, "quota_meta")
  expect_true(isTRUE(meta$feasibility_reallocation_used))
  expect_identical(as.character(meta$feasibility_reallocation_rule), "pooled_utility_backfill")
  expect_identical(meta$feasible_stage_capacity_long_link, 0L)
  expect_true(adjusted[["mid_link"]] >= 1L)
  expect_true(adjusted[["local_link"]] >= 1L)
  expect_identical(sum(adjusted, na.rm = TRUE), sum(base_quotas))
})

test_that("identified Phase B feasibility adjustment remains within feasible stage capacities", {
  state <- make_linking_refit_state(list(link_transform_mode = "shift_only"))
  state$round$round_id <- 1L
  state$refit_meta$last_refit_step <- 0L

  base_quotas <- c(anchor_link = 3L, long_link = 2L, mid_link = 1L, local_link = 0L)
  attr(base_quotas, "quota_meta") <- list(linking_identified = TRUE)
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      hub_anchored = TRUE,
      probe_brier = 0.25,
      probe_pred_rmse_lagged = 0.03,
      theta_global_rmse_lagged = 0.06
    )
  )

  adjusted <- testthat::with_mocked_bindings(
    generate_stage_candidates_from_state = function(state, stage_name, ...) {
      n <- switch(stage_name,
        anchor_link = 3L,
        long_link = 0L,
        mid_link = 4L,
        local_link = 4L
      )
      if (n < 1L) {
        return(tibble::tibble())
      }
      tibble::tibble(
        i = rep("h1", n),
        j = paste0(stage_name, "_", seq_len(n))
      )
    },
    .adaptive_filter_link_backfill_candidates = function(candidates, ...) {
      list(candidates = tibble::as_tibble(candidates), counts = list(), star_caps = list())
    },
    .adaptive_link_attach_predictive_utility = function(candidates, ...) {
      cand <- tibble::as_tibble(candidates)
      stage_name <- ifelse(grepl("^local_link_", cand$j[[1L]]), "local_link", "mid_link")
      cand$link_d_opt_gain <- if (identical(stage_name, "local_link")) 10 else 4
      cand$link_u <- cand$link_d_opt_gain
      cand
    },
    pairwiseLLM:::.adaptive_link_adjust_stage_quotas_for_feasibility(
      state = state,
      controller = utils::modifyList(state$controller, list(current_link_spoke_id = 2L)),
      spoke_id = 2L,
      stage_quotas = base_quotas,
      stage_order = pairwiseLLM:::.adaptive_stage_order(),
      refit_id = 1L
    ),
    .package = "pairwiseLLM"
  )

  meta <- attr(adjusted, "quota_meta")
  expect_identical(adjusted[["long_link"]], 0L)
  expect_true(isTRUE(meta$feasibility_reallocation_used))
  expect_lte(adjusted[["mid_link"]], meta$feasible_stage_capacity_mid_link)
  expect_lte(adjusted[["local_link"]], meta$feasible_stage_capacity_local_link)
  expect_lte(sum(adjusted, na.rm = TRUE), sum(base_quotas))
})

test_that("phase B feasibility adjustment aborts on candidate-generation errors", {
  state <- make_linking_refit_state(list(link_transform_mode = "shift_only"))
  state$round$round_id <- 1L
  state$refit_meta$last_refit_step <- 0L

  base_quotas <- c(anchor_link = 2L, long_link = 2L, mid_link = 1L, local_link = 1L)
  attr(base_quotas, "quota_meta") <- list(linking_identified = FALSE)

  err <- tryCatch(
    testthat::with_mocked_bindings(
      generate_stage_candidates_from_state = function(state, stage_name, ...) {
        if (identical(stage_name, "long_link")) {
          rlang::abort("synthetic generation failure")
        }
        tibble::tibble(i = "h1", j = "s1")
      },
      .adaptive_filter_link_backfill_candidates = function(candidates, ...) {
        list(candidates = tibble::as_tibble(candidates), counts = list(), star_caps = list())
      },
      .adaptive_link_attach_predictive_utility = function(candidates, ...) {
        cand <- tibble::as_tibble(candidates)
        cand$link_d_opt_gain <- 1
        cand$link_u <- 1
        cand
      },
      pairwiseLLM:::.adaptive_link_adjust_stage_quotas_for_feasibility(
        state = state,
        controller = state$controller,
        spoke_id = 2L,
        stage_quotas = base_quotas,
        stage_order = pairwiseLLM:::.adaptive_stage_order(),
        refit_id = 1L
      ),
      .package = "pairwiseLLM"
    ),
    error = identity
  )
  expect_s3_class(err, "rlang_error")
  expect_true(grepl(
    "Phase B feasibility computation failed before quota reduction",
    conditionMessage(err),
    fixed = TRUE
  ))
  expect_true(grepl("refit_id=1, spoke_id=2, stage_name=`long_link`", conditionMessage(err), fixed = TRUE))
  expect_true(grepl(
    "helper=`generate_stage_candidates_from_state`",
    conditionMessage(err),
    fixed = TRUE
  ))
  expect_true(grepl("synthetic generation failure", conditionMessage(err), fixed = TRUE))
})

test_that("phase B feasibility adjustment aborts on utility-attachment errors", {
  state <- make_linking_refit_state(list(link_transform_mode = "shift_only"))
  state$round$round_id <- 1L
  state$refit_meta$last_refit_step <- 0L

  base_quotas <- c(anchor_link = 2L, long_link = 2L, mid_link = 1L, local_link = 1L)
  attr(base_quotas, "quota_meta") <- list(linking_identified = FALSE)

  err <- tryCatch(
    testthat::with_mocked_bindings(
      generate_stage_candidates_from_state = function(state, stage_name, ...) {
        tibble::tibble(i = "h1", j = paste0(stage_name, "_candidate"))
      },
      .adaptive_filter_link_backfill_candidates = function(candidates, ...) {
        list(candidates = tibble::as_tibble(candidates), counts = list(), star_caps = list())
      },
      .adaptive_link_attach_predictive_utility = function(candidates, ...) {
        rlang::abort("synthetic utility failure")
      },
      pairwiseLLM:::.adaptive_link_adjust_stage_quotas_for_feasibility(
        state = state,
        controller = state$controller,
        spoke_id = 2L,
        stage_quotas = base_quotas,
        stage_order = pairwiseLLM:::.adaptive_stage_order(),
        refit_id = 1L
      ),
      .package = "pairwiseLLM"
    ),
    error = identity
  )
  expect_s3_class(err, "rlang_error")
  expect_true(grepl(
    "Phase B feasibility computation failed before quota reduction",
    conditionMessage(err),
    fixed = TRUE
  ))
  expect_true(grepl("refit_id=1, spoke_id=2, stage_name=`anchor_link`", conditionMessage(err), fixed = TRUE))
  expect_true(grepl(
    "helper=`.adaptive_link_attach_predictive_utility`",
    conditionMessage(err),
    fixed = TRUE
  ))
  expect_true(grepl("synthetic utility failure", conditionMessage(err), fixed = TRUE))
})

test_that("phase B feasibility adjustment keeps genuine empty stages non-fatal", {
  state <- make_linking_refit_state(list(link_transform_mode = "shift_only"))
  state$round$round_id <- 1L
  state$refit_meta$last_refit_step <- 0L

  base_quotas <- c(anchor_link = 2L, long_link = 2L, mid_link = 1L, local_link = 1L)
  attr(base_quotas, "quota_meta") <- list(linking_identified = FALSE)

  adjusted <- testthat::with_mocked_bindings(
    generate_stage_candidates_from_state = function(state, stage_name, ...) {
      if (identical(stage_name, "long_link")) {
        return(tibble::tibble())
      }
      tibble::tibble(i = "h1", j = paste0(stage_name, "_candidate"))
    },
    .adaptive_filter_link_backfill_candidates = function(candidates, ...) {
      list(candidates = tibble::as_tibble(candidates), counts = list(), star_caps = list())
    },
    .adaptive_link_attach_predictive_utility = function(candidates, ...) {
      cand <- tibble::as_tibble(candidates)
      cand$link_d_opt_gain <- 1
      cand$link_u <- 1
      cand
    },
    pairwiseLLM:::.adaptive_link_adjust_stage_quotas_for_feasibility(
      state = state,
      controller = state$controller,
      spoke_id = 2L,
      stage_quotas = base_quotas,
      stage_order = pairwiseLLM:::.adaptive_stage_order(),
      refit_id = 1L
    ),
    .package = "pairwiseLLM"
  )

  expect_identical(adjusted[["long_link"]], 0L)
  meta <- attr(adjusted, "quota_meta")
  expect_identical(meta$feasible_stage_capacity_long_link, 0L)
  expect_true(isTRUE(meta$feasibility_reallocation_used))
})

test_that("concurrent spoke routing enforces floor before budget targets", {
  state <- make_linking_refit_state(
    list(
      multi_spoke_mode = "concurrent",
      min_cross_set_pairs_per_spoke_per_refit = 2L
    )
  )
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state$refit_meta$last_refit_step <- 0L
  # Spoke 3 is below floor while spoke 2 already has one edge.
  pick <- testthat::with_mocked_bindings(
    .adaptive_link_budget_map_for_refit = function(state, controller = NULL, eligible_spoke_ids = NULL, seed = 1L) {
      list(
        `2` = list(B_spoke_refit_budget = 2L, concurrent_floor_pairs = 2L, concurrent_utility_mass = 1),
        `3` = list(B_spoke_refit_budget = 2L, concurrent_floor_pairs = 2L, concurrent_utility_mass = 1)
      )
    },
    pairwiseLLM:::.adaptive_link_active_spoke(state, state$controller),
    .package = "pairwiseLLM"
  )
  expect_identical(pick, 3L)

  state <- append_cross_step(state, 2L, "s31", "h1", 1L, spoke_id = 3L)
  state <- append_cross_step(state, 3L, "s32", "h2", 1L, spoke_id = 3L)
  # Both spokes satisfy floor now; routing follows the explicit budget map.
  pick2 <- testthat::with_mocked_bindings(
    .adaptive_link_budget_map_for_refit = function(state, controller = NULL, eligible_spoke_ids = NULL, seed = 1L) {
      list(
        `2` = list(B_spoke_refit_budget = 4L, concurrent_floor_pairs = 2L, concurrent_utility_mass = 9),
        `3` = list(B_spoke_refit_budget = 2L, concurrent_floor_pairs = 2L, concurrent_utility_mass = 1)
      )
    },
    pairwiseLLM:::.adaptive_link_active_spoke(state, state$controller),
    .package = "pairwiseLLM"
  )
  expect_identical(pick2, 2L)
})

test_that("concurrent routing uses budget deficit, not least-used balancing", {
  state <- make_linking_refit_state(
    list(
      multi_spoke_mode = "concurrent",
      min_cross_set_pairs_per_spoke_per_refit = 1L
    )
  )
  state$refit_meta$last_refit_step <- 0L
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "s31", "h1", 1L, spoke_id = 3L)
  state <- append_cross_step(state, 3L, "s32", "h2", 1L, spoke_id = 3L)
  # Least-used balancing would pick spoke 2 (1 vs 2), but budget-deficit routing picks spoke 3.
  pick <- testthat::with_mocked_bindings(
    .adaptive_link_budget_map_for_refit = function(state, controller = NULL, eligible_spoke_ids = NULL, seed = 1L) {
      list(
        `2` = list(B_spoke_refit_budget = 1L, concurrent_floor_pairs = 1L, concurrent_utility_mass = 1),
        `3` = list(B_spoke_refit_budget = 4L, concurrent_floor_pairs = 1L, concurrent_utility_mass = 10)
      )
    },
    pairwiseLLM:::.adaptive_link_active_spoke(state, state$controller),
    .package = "pairwiseLLM"
  )
  expect_identical(pick, 3L)
})

test_that("concurrent routing stops active spoke selection once refit budgets are met", {
  state <- make_linking_refit_state(
    list(
      multi_spoke_mode = "concurrent",
      min_cross_set_pairs_per_spoke_per_refit = 1L
    )
  )
  state$refit_meta$last_refit_step <- 0L
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "s31", "h1", 1L, spoke_id = 3L)

  pick <- testthat::with_mocked_bindings(
    .adaptive_link_budget_map_for_refit = function(state, controller = NULL, eligible_spoke_ids = NULL, seed = 1L) {
      list(
        `2` = list(B_spoke_refit_budget = 1L, concurrent_floor_pairs = 1L, concurrent_utility_mass = 5),
        `3` = list(B_spoke_refit_budget = 1L, concurrent_floor_pairs = 1L, concurrent_utility_mass = 5)
      )
    },
    pairwiseLLM:::.adaptive_link_active_spoke(state, state$controller),
    .package = "pairwiseLLM"
  )

  expect_true(is.na(pick))
})

test_that("concurrent floor is enforced as a routing floor when feasible", {
  state <- make_linking_refit_state(
    list(
      multi_spoke_mode = "concurrent",
      min_cross_set_pairs_per_spoke_per_refit = 2L
    )
  )
  state$refit_meta$last_refit_step <- 0L
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(uncertainty = 1),
    `3` = list(uncertainty = 1)
  )

  for (step_id in 1:4) {
    spoke <- pairwiseLLM:::.adaptive_link_active_spoke(state, state$controller)
    if (identical(spoke, 2L)) {
      state <- append_cross_step(state, step_id, "s21", "h1", 1L, spoke_id = 2L)
    } else {
      state <- append_cross_step(state, step_id, "s31", "h1", 1L, spoke_id = 3L)
    }
  }

  step_subset <- state$step_log[
    !is.na(state$step_log$pair_id) &
      state$step_log$is_cross_set %in% TRUE &
      as.integer(state$step_log$step_id) > 0L,
    ,
    drop = FALSE
  ]
  counts <- table(factor(as.integer(step_subset$link_spoke_id), levels = c(2L, 3L)))
  expect_true(all(as.integer(counts) >= 2L))
})

test_that("concurrent sparse-domain fallback is deterministic when floors cannot be met", {
  state <- make_linking_refit_state(
    list(
      multi_spoke_mode = "concurrent",
      min_cross_set_pairs_per_spoke_per_refit = 5L
    )
  )
  state$refit_meta$last_refit_step <- 0L
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(uncertainty = 0),
    `3` = list(uncertainty = 0)
  )
  # One observed pair per spoke leaves equal floor deficits that cannot be satisfied quickly;
  # tie-breaks must be deterministic.
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "s31", "h1", 1L, spoke_id = 3L)

  picks <- integer()
  for (idx in 1:4) {
    spoke <- pairwiseLLM:::.adaptive_link_active_spoke(state, state$controller)
    picks <- c(picks, as.integer(spoke))
    if (identical(spoke, 2L)) {
      state <- append_cross_step(state, idx + 2L, "s21", "h1", 1L, spoke_id = 2L)
    } else {
      state <- append_cross_step(state, idx + 2L, "s31", "h1", 1L, spoke_id = 3L)
    }
  }

  expect_identical(picks, c(2L, 3L, 2L, 3L))
})

test_that("active linking transformed reliability uses the active hub-spoke domain and decomposition guards", {
  state <- make_linking_refit_state()
  state$round$anchor_ids <- character()
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "s22", "h2", 0L, spoke_id = 2L)

  active <- pairwiseLLM:::.adaptive_link_active_item_ids(state, spoke_id = 2L, hub_id = 1L)
  rel_active <- pairwiseLLM:::.adaptive_link_reliability_transformed_active(
    state = state,
    active_ids = active$active_all,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_scale",
    delta_mean = 0.25,
    log_alpha_mean = log(1.3)
  )
  rel_manual <- pairwiseLLM:::.adaptive_link_global_score_stats_active(
    state = state,
    active_ids = active$active_all,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_scale",
    delta_mean = 0.25,
    log_alpha_mean = log(1.3)
  )$reliability
  rel_all <- pairwiseLLM:::.adaptive_link_reliability_active(state, active$active_all)

  expect_equal(rel_active, rel_manual, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(rel_active, rel_all, tolerance = 1e-12)))
})

test_that("scale_ready uses current-epoch active edges only", {
  make_state <- function(epoch_start_step) {
    state <- make_linking_refit_state(
      list(
        spoke_quantile_coverage_bins = 1L,
        shift_scale_min_cross_set_edges = 2L,
        shift_scale_min_distinct_spoke_items_per_bin = 1L
      )
    )
    state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
    state <- append_cross_step(state, 2L, "h2", "s22", 0L, spoke_id = 2L)
    state$controller$link_epoch_id_by_spoke <- list(`2` = 2L)
    state$controller$link_epoch_start_step_by_spoke <- list(`2` = as.integer(epoch_start_step))
    state$controller$link_epoch_signature_by_spoke <- list(
      `2` = current_link_epoch_signature(state, spoke_id = 2L)
    )
    state$controller$link_stage_coverage_bins_used <- list(`2` = 1L)
    state
  }

  run_case <- function(state) {
    testthat::with_mocked_bindings(
      .adaptive_link_fit_transform = function(cross_edges, hub_theta, spoke_theta, transform_mode) {
        list(
          delta_mean = 0,
          delta_sd = 0.1,
          log_alpha_mean = NA_real_,
          log_alpha_sd = NA_real_,
          theta_hub_post = hub_theta,
          theta_spoke_post = spoke_theta,
          posterior_draws = list(),
          diagnostics = list(
            divergences = 0L,
            max_rhat = 1,
            min_ess_bulk = 1000,
            diagnostics_divergences_pass = TRUE,
            diagnostics_rhat_pass = TRUE,
            diagnostics_ess_pass = TRUE
          ),
          fit_contract = list()
        )
      },
      .adaptive_link_reliability_transformed_active = function(...) 0.95,
      .adaptive_link_ts_btl_rank_spearman_active = function(...) 0.95,
      .adaptive_link_rank_stability_lagged = function(...) {
        list(lag_eligible = FALSE, rho_rank_lagged = NA_real_, rho_rank_lagged_pass = FALSE)
      },
      .adaptive_link_probe_edges_realized = function(...) tibble::tibble(),
      .adaptive_link_probe_brier_for_fit = function(...) NA_real_,
      .adaptive_link_probe_pred_rmse_lagged_for_fit = function(...) NA_real_,
      .adaptive_linking_refit_update_state(state, list(last_refit_step = 0L)),
      .package = "pairwiseLLM"
    )
  }

  stats_all_edges <- run_case(make_state(epoch_start_step = 1L))$controller$link_refit_stats_by_spoke[["2"]]
  stats_epoch_edges <- run_case(make_state(epoch_start_step = 3L))$controller$link_refit_stats_by_spoke[["2"]]

  expect_true(isTRUE(stats_all_edges$scale_ready))
  expect_false(isTRUE(stats_epoch_edges$scale_ready))
})

test_that("scale_ready tolerates missing legacy coverage bin state on resume", {
  state <- make_linking_refit_state(
    list(
      spoke_quantile_coverage_bins = 1L,
      shift_scale_min_cross_set_edges = 2L,
      shift_scale_min_distinct_spoke_items_per_bin = 1L
    )
  )
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "h2", "s22", 0L, spoke_id = 2L)
  state$controller$link_epoch_id_by_spoke <- list(`2` = 2L)
  state$controller$link_epoch_start_step_by_spoke <- list(`2` = 1L)
  state$controller$link_epoch_signature_by_spoke <- list(
    `2` = current_link_epoch_signature(state, spoke_id = 2L)
  )
  state$controller$link_stage_coverage_bins_used <- list(`2` = NA_integer_)

  out <- testthat::with_mocked_bindings(
    .adaptive_link_fit_transform = function(cross_edges, hub_theta, spoke_theta, transform_mode) {
      list(
        delta_mean = 0,
        delta_sd = 0.1,
        log_alpha_mean = NA_real_,
        log_alpha_sd = NA_real_,
        theta_hub_post = hub_theta,
        theta_spoke_post = spoke_theta,
        posterior_draws = list(),
        diagnostics = list(
          divergences = 0L,
          max_rhat = 1,
          min_ess_bulk = 1000,
          diagnostics_divergences_pass = TRUE,
          diagnostics_rhat_pass = TRUE,
          diagnostics_ess_pass = TRUE
        ),
        fit_contract = list()
      )
    },
    .adaptive_link_reliability_transformed_active = function(...) 0.95,
    .adaptive_link_ts_btl_rank_spearman_active = function(...) 0.95,
    .adaptive_link_rank_stability_lagged = function(...) {
      list(lag_eligible = FALSE, rho_rank_lagged = NA_real_, rho_rank_lagged_pass = FALSE)
    },
    .adaptive_link_probe_edges_realized = function(...) tibble::tibble(),
    .adaptive_link_probe_brier_for_fit = function(...) NA_real_,
    .adaptive_link_probe_pred_rmse_lagged_for_fit = function(...) NA_real_,
    .adaptive_linking_refit_update_state(state, list(last_refit_step = 0L)),
    .package = "pairwiseLLM"
  )

  stats <- out$controller$link_refit_stats_by_spoke[["2"]]
  expect_true(isTRUE(stats$scale_ready))
})

test_that("epoch resets require regime changes, not ordinary probe-panel churn", {
  make_reset_state <- function(with_prior_panel_row = FALSE) {
    state <- make_linking_refit_state()
    state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
    state <- append_cross_step(state, 2L, "h2", "s22", 0L, spoke_id = 2L)
    state$controller$link_epoch_id_by_spoke <- list(`2` = 4L)
    state$controller$link_epoch_start_step_by_spoke <- list(`2` = 1L)
    state$controller$link_epoch_signature_by_spoke <- list(
      `2` = current_link_epoch_signature(state, spoke_id = 2L)
    )
    state$controller$link_stop_recent_pass_window_by_spoke <- list(`2` = c(TRUE, TRUE))
    state$controller$link_escalation_recent_pass_window_by_spoke <- list(`2` = c(TRUE))
    if (isTRUE(with_prior_panel_row)) {
      state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
        state$link_stage_log,
        list(
          refit_id = 1L,
          spoke_id = 2L,
          hub_id = 1L,
          link_epoch_id = 4L,
          probe_panel_id = "prior_panel",
          probe_edges_planned = 1L,
          probe_edges_realized = 0L,
          probe_panel_shortfall = 1L
        )
      )
    }
    state
  }

  run_reset <- function(state) {
    testthat::with_mocked_bindings(
      .adaptive_link_fit_transform = function(cross_edges, hub_theta, spoke_theta, transform_mode) {
        list(
          delta_mean = 0,
          delta_sd = 0.1,
          log_alpha_mean = NA_real_,
          log_alpha_sd = NA_real_,
          theta_hub_post = hub_theta,
          theta_spoke_post = spoke_theta,
          posterior_draws = list(),
          diagnostics = list(
            divergences = 0L,
            max_rhat = 1,
            min_ess_bulk = 1000,
            diagnostics_divergences_pass = TRUE,
            diagnostics_rhat_pass = TRUE,
            diagnostics_ess_pass = TRUE
          ),
          fit_contract = list()
        )
      },
      .adaptive_link_reliability_transformed_active = function(...) 0.95,
      .adaptive_link_ts_btl_rank_spearman_active = function(...) 0.95,
      .adaptive_link_rank_stability_lagged = function(...) {
        list(lag_eligible = FALSE, rho_rank_lagged = NA_real_, rho_rank_lagged_pass = FALSE)
      },
      .adaptive_link_probe_edges_realized = function(...) tibble::tibble(),
      .adaptive_link_probe_brier_for_fit = function(...) NA_real_,
      .adaptive_link_probe_pred_rmse_lagged_for_fit = function(...) NA_real_,
      .adaptive_linking_refit_update_state(state, list(last_refit_step = 2L)),
      .package = "pairwiseLLM"
    )
  }

  artifact_state <- make_reset_state()
  artifact_state$linking$phase_a$artifacts[["2"]]$refit_id <- 9L
  artifact_reset <- run_reset(artifact_state)

  artifact_state <- make_reset_state()
  artifact_state$linking$phase_a$artifacts[["2"]]$refit_id <- 9L
  artifact_reset <- run_reset(artifact_state)

  expect_identical(artifact_reset$controller$link_epoch_id_by_spoke[["2"]], 5L)
  expect_identical(artifact_reset$controller$link_epoch_start_step_by_spoke[["2"]], 3L)
  expect_identical(artifact_reset$controller$link_stop_recent_pass_window_by_spoke[["2"]], logical())
  expect_identical(artifact_reset$controller$link_escalation_recent_pass_window_by_spoke[["2"]], logical())
  artifact_stats <- artifact_reset$controller$link_refit_stats_by_spoke[["2"]]
  expect_true(isTRUE(artifact_stats$lag_domain_reset))
  expect_identical(as.character(artifact_stats$lag_domain_reset_reason), "spoke_artifact_replaced")
  expect_false(isTRUE(artifact_stats$lag_eligible))

  probe_state <- make_reset_state()
  panel <- tibble::tibble(
    probe_panel_id = "new_panel",
    link_epoch_id = 4L,
    spoke_id = 2L,
    hub_item_id = "h1",
    spoke_item_id = "s21",
    spoke_bin = 1L,
    hub_bin = 1L,
    planned_rank = 1L,
    pair_key = pairwiseLLM:::make_unordered_key("h1", "s21"),
    realized = FALSE,
    realized_step_id = NA_integer_,
    realized_pair_id = NA_integer_,
    realized_run_mode = NA_character_
  )
  probe_state$linking$probe$panels_by_spoke <- list(`2` = panel)
  probe_reset <- run_reset(probe_state)

  expect_identical(probe_reset$controller$link_epoch_id_by_spoke[["2"]], 4L)
  expect_identical(probe_reset$controller$link_epoch_start_step_by_spoke[["2"]], 1L)
  probe_stats <- probe_reset$controller$link_refit_stats_by_spoke[["2"]]
  expect_false(isTRUE(probe_stats$lag_domain_reset))
  expect_true(is.na(probe_stats$lag_domain_reset_reason))

  probe_state_changed <- make_reset_state(with_prior_panel_row = TRUE)
  probe_state_changed$linking$probe$panels_by_spoke <- list(`2` = panel)
  probe_reset_changed <- run_reset(probe_state_changed)

  expect_identical(probe_reset_changed$controller$link_epoch_id_by_spoke[["2"]], 5L)
  expect_identical(probe_reset_changed$controller$link_epoch_start_step_by_spoke[["2"]], 3L)
  expect_identical(probe_reset_changed$controller$link_stop_recent_pass_window_by_spoke[["2"]], logical())
  expect_identical(probe_reset_changed$controller$link_escalation_recent_pass_window_by_spoke[["2"]], logical())
  probe_stats_changed <- probe_reset_changed$controller$link_refit_stats_by_spoke[["2"]]
  expect_true(isTRUE(probe_stats_changed$lag_domain_reset))
  expect_identical(as.character(probe_stats_changed$lag_domain_reset_reason), "probe_panel_rebuild")
})

test_that("refit-local memo invalidates on refit and epoch boundaries for stage-row construction", {
  state <- make_linking_refit_state()
  state$warm_start_done <- TRUE
  state$controller$current_link_spoke_id <- 2L
  state$linking$phase_a$ready_spokes <- 2L
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "h2", "s22", 0L, spoke_id = 2L)
  state$controller$link_epoch_id_by_spoke <- list(`2` = 1L)
  state$linking$probe$panels_by_spoke <- list(
    `2` = tibble::tibble(
      probe_panel_id = "panel_a",
      link_epoch_id = 1L,
      spoke_id = 2L,
      hub_item_id = "h1",
      spoke_item_id = "s21",
      spoke_bin = 1L,
      hub_bin = 1L,
      planned_rank = 1L,
      pair_key = pairwiseLLM:::make_unordered_key("h1", "s21"),
      realized = FALSE,
      realized_step_id = NA_integer_,
      realized_pair_id = NA_integer_,
      realized_run_mode = NA_character_
    )
  )

  orig_routing <- pairwiseLLM:::.adaptive_link_phase_b_routing_scores
  orig_coverage <- pairwiseLLM:::.adaptive_link_spoke_coverage
  calls <- new.env(parent = emptyenv())
  calls$routing <- 0L
  calls$coverage <- 0L

  rows <- testthat::with_mocked_bindings(
    .adaptive_link_phase_b_routing_scores = function(...) {
      calls$routing <- as.integer(calls$routing) + 1L
      orig_routing(...)
    },
    .adaptive_link_spoke_coverage = function(...) {
      calls$coverage <- as.integer(calls$coverage) + 1L
      orig_coverage(...)
    },
    {
      same_refit_1 <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
        state = state,
        refit_id = 1L,
        refit_context = list(last_refit_step = 0L)
      )
      same_refit_2 <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
        state = state,
        refit_id = 1L,
        refit_context = list(last_refit_step = 0L)
      )

      state_refit <- state
      state_refit$round_log <- pairwiseLLM:::append_round_log(
        state_refit$round_log,
        list(refit_id = 1L, diagnostics_pass = TRUE)
      )
      next_refit <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
        state = state_refit,
        refit_id = 2L,
        refit_context = list(last_refit_step = 2L)
      )

      state_epoch <- state_refit
      state_epoch$controller$link_epoch_id_by_spoke <- list(`2` = 2L)
      state_epoch$linking$probe$panels_by_spoke <- list(
        `2` = tibble::tibble(
          probe_panel_id = "panel_b",
          link_epoch_id = 2L,
          spoke_id = 2L,
          hub_item_id = "h1",
          spoke_item_id = "s21",
          spoke_bin = 1L,
          hub_bin = 1L,
          planned_rank = 1L,
          pair_key = pairwiseLLM:::make_unordered_key("h1", "s21"),
          realized = FALSE,
          realized_step_id = NA_integer_,
          realized_pair_id = NA_integer_,
          realized_run_mode = NA_character_
        )
      )
      next_epoch <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
        state = state_epoch,
        refit_id = 2L,
        refit_context = list(last_refit_step = 2L)
      )

      list(
        same_refit_1 = same_refit_1,
        same_refit_2 = same_refit_2,
        next_refit = next_refit,
        next_epoch = next_epoch
      )
    },
    .package = "pairwiseLLM"
  )

  expect_identical(as.integer(calls$routing), 3L)
  expect_identical(as.integer(calls$coverage), 3L)
  expect_identical(
    as.integer(rows$same_refit_1$coverage_bins_used),
    as.integer(rows$same_refit_2$coverage_bins_used)
  )
  expect_identical(
    as.character(rows$same_refit_1$coverage_source),
    as.character(rows$same_refit_2$coverage_source)
  )
  expect_true(nrow(rows$next_refit) >= 1L)
  expect_true(nrow(rows$next_epoch) >= 1L)
})

test_that("probe realized bookkeeping is derived from canonical realized-edge log", {
  state <- make_linking_refit_state()
  state$controller$link_epoch_id_by_spoke <- list(`2` = 4L)
  pair_key <- pairwiseLLM:::make_unordered_key("h1", "s21")
  state$linking$probe$panels_by_spoke <- list(
    `2` = tibble::tibble(
      probe_panel_id = "panel_a",
      link_epoch_id = 4L,
      spoke_id = 2L,
      hub_item_id = "h1",
      spoke_item_id = "s21",
      spoke_bin = 1L,
      hub_bin = 1L,
      planned_rank = 1L,
      pair_key = pair_key,
      realized = FALSE,
      realized_step_id = NA_integer_,
      realized_pair_id = NA_integer_,
      realized_run_mode = NA_character_
    )
  )
  state$linking$probe$realized_edges <- tibble::tibble(
    step_id = 10L,
    pair_id = 10L,
    run_mode = "link_probe_holdout",
    spoke_id = 2L,
    link_epoch_id = 4L,
    probe_panel_id = "panel_a",
    hub_item_id = "h1",
    spoke_item_id = "s21",
    pair_key = pair_key,
    Y = 1L
  )
  ids <- as.character(state$item_ids)
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 10L,
      timestamp = as.POSIXct("2026-01-01 00:00:10", tz = "UTC"),
      pair_id = 10L,
      i = match("h1", ids),
      j = match("s21", ids),
      A = match("h1", ids),
      B = match("s21", ids),
      Y = 0L,
      set_i = 1L,
      set_j = 2L,
      is_cross_set = TRUE,
      link_spoke_id = 2L,
      run_mode = "link_probe_holdout",
      is_probe_step = TRUE
    )
  )

  panel <- pairwiseLLM:::.adaptive_link_probe_panel_for_spoke(state, spoke_id = 2L, epoch_id = 4L)
  expect_true(isTRUE(panel$realized[[1L]]))
  expect_identical(as.integer(panel$realized_step_id[[1L]]), 10L)
  expect_identical(pairwiseLLM:::.adaptive_link_probe_realized_count(state, spoke_id = 2L, epoch_id = 4L), 1L)
  realized_edges <- pairwiseLLM:::.adaptive_link_probe_edges_realized(state, spoke_id = 2L, epoch_id = 4L)
  expect_identical(nrow(realized_edges), 1L)
  expect_identical(as.integer(realized_edges$step_id[[1L]]), 10L)
})

test_that("probe realization index keeps latest realized rows and exposes current-window counts", {
  state <- make_linking_refit_state()
  state$controller$link_epoch_id_by_spoke <- list(`2` = 4L)
  pair_a <- pairwiseLLM:::make_unordered_key("h1", "s21")
  pair_b <- pairwiseLLM:::make_unordered_key("h2", "s22")
  panel <- tibble::tibble(
    probe_panel_id = "panel_a",
    link_epoch_id = 4L,
    spoke_id = 2L,
    hub_item_id = c("h1", "h2"),
    spoke_item_id = c("s21", "s22"),
    spoke_bin = c(1L, 2L),
    hub_bin = c(1L, 1L),
    planned_rank = c(1L, 2L),
    pair_key = c(pair_a, pair_b),
    realized = c(FALSE, FALSE),
    realized_step_id = c(NA_integer_, NA_integer_),
    realized_pair_id = c(NA_integer_, NA_integer_),
    realized_run_mode = c(NA_character_, NA_character_)
  )
  state$linking$probe$panels_by_spoke <- list(`2` = panel)
  state$linking$probe$realized_edges <- tibble::tibble(
    step_id = c(10L, 12L, 14L),
    pair_id = c(10L, 12L, 14L),
    run_mode = rep("link_probe_holdout", 3L),
    spoke_id = rep(2L, 3L),
    link_epoch_id = rep(4L, 3L),
    probe_panel_id = rep("panel_a", 3L),
    hub_item_id = c("h1", "h2", "h1"),
    spoke_item_id = c("s21", "s22", "s21"),
    pair_key = c(pair_a, pair_b, pair_a),
    Y = c(1L, 0L, 0L)
  )

  entry <- pairwiseLLM:::.adaptive_link_probe_realized_index_entry_get(
    state = state,
    spoke_id = 2L,
    epoch_id = 4L,
    probe_panel_id = "panel_a"
  )
  expect_identical(as.integer(entry$realized_count), 2L)
  expect_identical(as.integer(entry$last_realized_step_id), 14L)
  expect_identical(as.integer(entry$row_ids), c(2L, 3L))

  realized_log <- pairwiseLLM:::.adaptive_link_probe_realized_log_for_panel(
    state = state,
    spoke_id = 2L,
    epoch_id = 4L,
    panel = panel
  )
  expect_identical(as.integer(realized_log$step_id), c(12L, 14L))
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_realized_count_since_step(
      state = state,
      spoke_id = 2L,
      epoch_id = 4L,
      last_step_id = 11L,
      panel = panel
    ),
    2L
  )
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_realized_count_since_step(
      state = state,
      spoke_id = 2L,
      epoch_id = 4L,
      last_step_id = 12L,
      panel = panel
    ),
    1L
  )
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_realized_last_step_id(
      state = state,
      spoke_id = 2L,
      epoch_id = 4L,
      panel = panel
    ),
    14L
  )
})

test_that("stale panel realized flags do not become canonical realized probe evidence", {
  state <- make_linking_refit_state()
  state$controller$link_epoch_id_by_spoke <- list(`2` = 4L)
  pair_key <- pairwiseLLM:::make_unordered_key("h1", "s21")
  panel <- tibble::tibble(
    probe_panel_id = "panel_a",
    link_epoch_id = 4L,
    spoke_id = 2L,
    hub_item_id = "h1",
    spoke_item_id = "s21",
    spoke_bin = 1L,
    hub_bin = 1L,
    planned_rank = 1L,
    pair_key = pair_key,
    realized = TRUE,
    realized_step_id = 10L,
    realized_pair_id = 10L,
    realized_run_mode = "link_probe_holdout"
  )
  state$linking$probe$panels_by_spoke <- list(`2` = panel)
  state$linking$probe$realized_edges <- pairwiseLLM:::.adaptive_link_probe_empty_realized_log()

  realized_log <- pairwiseLLM:::.adaptive_link_probe_realized_log_for_panel(
    state = state,
    spoke_id = 2L,
    epoch_id = 4L,
    panel = panel
  )
  expect_identical(nrow(realized_log), 0L)
  expect_identical(pairwiseLLM:::.adaptive_link_probe_realized_count(state, spoke_id = 2L, epoch_id = 4L), 0L)
})

test_that("same-epoch realized-edge panel mismatch fails before refit rows are emitted", {
  state <- make_linking_refit_state()
  state$controller$link_epoch_id_by_spoke <- list(`2` = 4L)
  pair_key <- pairwiseLLM:::make_unordered_key("h1", "s21")
  state$linking$probe$panels_by_spoke <- list(
    `2` = tibble::tibble(
      probe_panel_id = "panel_b",
      link_epoch_id = 4L,
      spoke_id = 2L,
      hub_item_id = "h1",
      spoke_item_id = "s21",
      spoke_bin = 1L,
      hub_bin = 1L,
      planned_rank = 1L,
      pair_key = pair_key,
      realized = FALSE,
      realized_step_id = NA_integer_,
      realized_pair_id = NA_integer_,
      realized_run_mode = NA_character_
    )
  )
  state$linking$probe$realized_edges <- tibble::tibble(
    step_id = 10L,
    pair_id = 10L,
    run_mode = "link_probe_holdout",
    spoke_id = 2L,
    link_epoch_id = 4L,
    probe_panel_id = "panel_a",
    hub_item_id = "h1",
    spoke_item_id = "s21",
    pair_key = pair_key,
    Y = 1L
  )

  expect_error(
    pairwiseLLM:::.adaptive_link_probe_panel_for_spoke(state, spoke_id = 2L, epoch_id = 4L),
    "realized_edges\\$probe_panel_id"
  )
})

test_that("link stage refit rows use canonical realized probe counts and enforce monotonicity", {
  state <- make_linking_refit_state()
  state$controller$link_epoch_id_by_spoke <- list(`2` = 4L)
  state$controller$current_link_spoke_id <- 2L
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_epoch_id = 4L,
      link_transform_policy = "auto",
      link_transform_state = "shift_only"
    )
  )
  pair_key <- pairwiseLLM:::make_unordered_key("h1", "s21")
  state$linking$probe$panels_by_spoke <- list(
    `2` = tibble::tibble(
      probe_panel_id = "panel_a",
      link_epoch_id = 4L,
      spoke_id = 2L,
      hub_item_id = "h1",
      spoke_item_id = "s21",
      spoke_bin = 1L,
      hub_bin = 1L,
      planned_rank = 1L,
      pair_key = pair_key,
      realized = FALSE,
      realized_step_id = NA_integer_,
      realized_pair_id = NA_integer_,
      realized_run_mode = NA_character_
    )
  )
  ids <- as.character(state$item_ids)
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 10L,
      timestamp = as.POSIXct("2026-01-01 00:00:10", tz = "UTC"),
      pair_id = 10L,
      i = match("h1", ids),
      j = match("s21", ids),
      A = match("h1", ids),
      B = match("s21", ids),
      Y = 1L,
      set_i = 1L,
      set_j = 2L,
      is_cross_set = TRUE,
      link_spoke_id = 2L,
      run_mode = "link_probe_holdout",
      is_probe_step = TRUE,
      link_stage = "probe_panel",
      round_stage = "probe_panel"
    )
  )
  state$linking$probe$realized_edges <- tibble::tibble(
    step_id = 10L,
    pair_id = 10L,
    run_mode = "link_probe_holdout",
    spoke_id = 2L,
    link_epoch_id = 4L,
    probe_panel_id = "panel_a",
    hub_item_id = "h1",
    spoke_item_id = "s21",
    pair_key = pair_key,
    Y = 1L
  )

  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 2L,
    refit_context = list(last_refit_step = 0L)
  )
  row <- rows[rows$spoke_id == 2L, , drop = FALSE]
  expect_identical(as.integer(row$probe_edges_realized[[1L]]), 1L)
  expect_identical(as.integer(row$probe_panel_shortfall[[1L]]), 0L)
  expect_identical(as.integer(row$n_probe_pairs_since_last_refit[[1L]]), 1L)
  expect_identical(as.integer(row$n_cross_edges_probe_since_last_refit[[1L]]), 1L)

  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    state$link_stage_log,
    list(
      refit_id = 1L,
      spoke_id = 2L,
      hub_id = 1L,
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_refit_mode = "shift_only",
      hub_lock_mode = "soft_lock",
      link_epoch_id = 4L,
      probe_panel_id = "panel_a",
      probe_edges_planned = 1L,
      probe_edges_realized = 2L,
      probe_panel_shortfall = 0L,
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )

  expect_error(
    pairwiseLLM:::.adaptive_link_stage_refit_rows(
      state = state,
      refit_id = 2L,
      refit_context = list(last_refit_step = 0L)
    ),
    "probe monotonicity invariant failed"
  )
})

test_that("link stop gating enforces diagnostics and lag eligibility", {
  state <- make_linking_refit_state()
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_mode = "shift_only",
      delta_spoke_mean = 0.1,
      delta_spoke_sd = 0.02,
      delta_change_lagged = 0.01,
      reliability_link_global = 0.95,
      link_stop_reliability_min_used = 0.90,
      link_reliability_stop_pass = TRUE,
      ts_btl_rank_spearman_active = 0.94,
      lag_eligible = FALSE,
      rank_stability_lagged = NA_real_,
      link_identified = TRUE,
      active_item_count_hub = 1L,
      active_item_count_spoke = 2L
    )
  )
  state$controller$linking_identified_by_spoke <- list(`2` = TRUE)
  state$round_log <- pairwiseLLM:::append_round_log(
    state$round_log,
    list(refit_id = 1L, diagnostics_pass = TRUE)
  )

  rows_lag <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row_lag <- rows_lag[rows_lag$spoke_id == 2L, , drop = FALSE]
  expect_false(isTRUE(row_lag$link_stop_eligible[[1L]]))
  expect_false(isTRUE(row_lag$link_stop_pass[[1L]]))

  state$controller$link_refit_stats_by_spoke[["2"]]$lag_eligible <- TRUE
  state$controller$link_refit_stats_by_spoke[["2"]]$rank_stability_lagged <- 0.99
  state$round_log$diagnostics_pass[[nrow(state$round_log)]] <- FALSE
  rows_diag <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row_diag <- rows_diag[rows_diag$spoke_id == 2L, , drop = FALSE]
  expect_false(isTRUE(row_diag$link_stop_eligible[[1L]]))
  expect_false(isTRUE(row_diag$link_stop_pass[[1L]]))

  state$controller$link_refit_stats_by_spoke[["2"]]$link_stop_gate_open <- FALSE
  state$round_log$diagnostics_pass[[nrow(state$round_log)]] <- TRUE
  rows_missing <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row_missing <- rows_missing[rows_missing$spoke_id == 2L, , drop = FALSE]
  expect_false(isTRUE(row_missing$link_stop_eligible[[1L]]))
  expect_false(isTRUE(row_missing$link_stop_pass[[1L]]))
})

test_that("runtime linking_identified uses active TS-BTL rank threshold and not lagged rank stability", {
  base_state <- make_linking_refit_state(
    list(
      link_identified_reliability_min = 0.80,
      link_rank_corr_min = 0.90,
      delta_sd_max = 100
    )
  )
  base_state <- append_cross_step(base_state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  base_state <- append_cross_step(base_state, 2L, "h2", "s22", 0L, spoke_id = 2L)

  out_pass <- testthat::with_mocked_bindings(
    .adaptive_link_rank_stability_lagged = function(...) {
      list(lag_eligible = FALSE, rho_rank_lagged = NA_real_, rho_rank_lagged_pass = FALSE)
    },
    .adaptive_link_ts_btl_rank_spearman_active = function(...) 0.94,
    .adaptive_link_reliability_transformed_active = function(...) 0.95,
    .adaptive_linking_refit_update_state(base_state, list(last_refit_step = 0L)),
    .package = "pairwiseLLM"
  )
  stats_pass <- out_pass$controller$link_refit_stats_by_spoke[["2"]]
  expect_false(isTRUE(stats_pass$lag_eligible))
  expect_true(isTRUE(stats_pass$link_rank_corr_pass))
  expect_true(isTRUE(stats_pass$link_identified))

  out_fail <- testthat::with_mocked_bindings(
    .adaptive_link_rank_stability_lagged = function(...) {
      list(lag_eligible = FALSE, rho_rank_lagged = NA_real_, rho_rank_lagged_pass = FALSE)
    },
    .adaptive_link_ts_btl_rank_spearman_active = function(...) 0.40,
    .adaptive_link_reliability_transformed_active = function(...) 0.95,
    .adaptive_linking_refit_update_state(base_state, list(last_refit_step = 0L)),
    .package = "pairwiseLLM"
  )
  stats_fail <- out_fail$controller$link_refit_stats_by_spoke[["2"]]
  expect_false(isTRUE(stats_fail$link_rank_corr_pass))
  expect_false(isTRUE(stats_fail$link_identified))
})

test_that("link stop decision is reproducible from supplement-defined link_stage_log fields", {
  state <- make_linking_refit_state()
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_mode = "shift_scale",
      delta_spoke_mean = 0.1,
      delta_spoke_sd = 0.01,
      log_alpha_spoke_mean = 0.02,
      log_alpha_spoke_sd = 0.03,
      delta_change_lagged = 0.01,
      log_alpha_change_lagged = 0.01,
      reliability_link_global = 0.95,
      link_stop_reliability_min_used = 0.90,
      link_reliability_stop_pass = TRUE,
      ts_btl_rank_spearman_active = 0.95,
      lag_eligible = TRUE,
      link_lag_eligible = TRUE,
      link_min_refit_eligible = TRUE,
      link_stop_gate_open = TRUE,
      link_stop_eligible = TRUE,
      stop_recent_pass_count = 2L,
      stop_recent_window_size = 3L,
      stability_window_refits_used = 3L,
      stability_passes_required_used = 2L,
      link_stop_pass = TRUE,
      rank_stability_lagged = 0.99,
      link_identified = TRUE,
      hub_anchored = TRUE,
      probe_brier = 0.10,
      probe_brier_max_used = 0.19,
      probe_brier_pass = TRUE,
      probe_pred_rmse_lagged = 0.01,
      probe_pred_rmse_max_used = 0.015,
      probe_pred_rmse_pass = TRUE,
      theta_global_rmse_lagged = 0.02,
      theta_global_rmse_max_used = 0.05,
      theta_global_rmse_pass = TRUE,
      active_item_count_hub = 1L,
      active_item_count_spoke = 2L
    )
  )
  state$controller$linking_identified_by_spoke <- list(`2` = TRUE)
  state$round_log <- pairwiseLLM:::append_round_log(
    state$round_log,
    list(refit_id = 1L, diagnostics_pass = TRUE)
  )

  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row <- rows[rows$spoke_id == 2L, , drop = FALSE]
  hub_ids <- as.character(state$items$item_id[state$items$set_id == 1L])
  hub_theta <- as.double(state$btl_fit$theta_mean[hub_ids])
  reconstructed <- pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = row,
    diagnostics_pass = TRUE,
    hub_theta_sd = stats::sd(hub_theta),
    controller = state$controller
  )
  expect_identical(row$link_stop_pass[[1L]], reconstructed)
})

test_that("link stage log stores active TS-BTL correlation separately from lagged rank stability", {
  state <- make_linking_refit_state()
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_mode = "shift_only",
      delta_spoke_mean = 0.1,
      delta_spoke_sd = 0.02,
      delta_change_lagged = 0.01,
      reliability_link_global = 0.91,
      link_stop_reliability_min_used = 0.90,
      link_reliability_stop_pass = TRUE,
      ts_btl_rank_spearman_active = 0.93,
      lag_eligible = TRUE,
      rank_stability_lagged = 0.99
    )
  )
  state$round_log <- pairwiseLLM:::append_round_log(
    state$round_log,
    list(refit_id = 1L, diagnostics_pass = TRUE)
  )
  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row <- rows[rows$spoke_id == 2L, , drop = FALSE]
  expect_equal(row$ts_btl_rank_spearman[[1L]], 0.93, tolerance = 1e-12)
  expect_equal(row$rank_stability_lagged[[1L]], 0.99, tolerance = 1e-12)
})

test_that("link stop reconstruction rejects legacy pass-only rows without normative probe fields", {
  row <- tibble::tibble(
    link_stop_eligible = TRUE,
    reliability_stop_pass = TRUE
  )
  out <- pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = row,
    diagnostics_pass = TRUE,
    hub_theta_sd = NA_real_,
    controller = list()
  )
  expect_false(isTRUE(out))

  row$reliability_stop_pass <- FALSE
  out2 <- pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = row,
    diagnostics_pass = TRUE,
    hub_theta_sd = NA_real_,
    controller = list()
  )
  expect_false(isTRUE(out2))
})

test_that("link stop reconstruction fallback path honors numeric gates", {
  controller <- list(
    link_stop_reliability_min = 0.90,
    probe_brier_max = 0.19,
    probe_pred_rmse_max = 0.015,
    theta_global_rmse_max = 0.05
  )
  row_shift <- tibble::tibble(
    link_stop_eligible = TRUE,
    reliability_stop_pass = TRUE,
    hub_anchored = TRUE,
    probe_brier = 0.10,
    probe_pred_rmse_lagged = 0.01,
    theta_global_rmse_lagged = 0.02
  )
  pass_shift <- pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = row_shift,
    diagnostics_pass = TRUE,
    hub_theta_sd = 0.5,
    controller = controller
  )
  expect_true(isTRUE(pass_shift))

  row_scale <- tibble::tibble(
    link_stop_eligible = TRUE,
    reliability_stop_pass = TRUE,
    hub_anchored = TRUE,
    probe_brier = 0.10,
    probe_pred_rmse_lagged = 0.01,
    theta_global_rmse_lagged = 0.02
  )
  pass_scale <- pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = row_scale,
    diagnostics_pass = TRUE,
    hub_theta_sd = 0.5,
    controller = controller
  )
  expect_true(isTRUE(pass_scale))

  row_scale$theta_global_rmse_lagged <- 0.045
  expect_true(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = row_scale,
    diagnostics_pass = TRUE,
    hub_theta_sd = 0.5,
    controller = controller
  )))

  row_scale$theta_global_rmse_lagged <- 0.055
  fail_scale <- pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = row_scale,
    diagnostics_pass = TRUE,
    hub_theta_sd = 0.5,
    controller = controller
  )
  expect_false(isTRUE(fail_scale))

  row_rank <- row_shift
  row_rank$hub_anchored <- FALSE
  fail_rank <- pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = row_rank,
    diagnostics_pass = TRUE,
    hub_theta_sd = 0.5,
    controller = controller
  )
  expect_false(isTRUE(fail_rank))
})

test_that("auto escalation requires diagnostics to pass before any decision opens", {
  state <- make_linking_refit_state(
    list(
      link_transform_policy = "auto",
      link_refit_mode = "shift_only",
      link_transform_escalation_window_refits = 1L,
      link_transform_escalation_passes_required = 1L
    )
  )
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "s22", "h2", 1L, spoke_id = 2L)

  out <- testthat::with_mocked_bindings(
    .adaptive_link_cross_edges = function(...) {
      tibble::tibble(
        spoke_item = rep(c("s21", "s22", "s21"), each = 6L),
        hub_item = rep(c("h1", "h2", "h3"), times = 6L),
        y_spoke = rep(c(1L, 0L), length.out = 18L),
        step_id = seq_len(18L),
        spoke_in_A = TRUE,
        run_mode = "link_one_spoke",
        is_probe_step = FALSE
      )
    },
    .adaptive_link_probe_edges_realized = function(...) {
      tibble::tibble(
        hub_item = rep(c("h1", "h2", "h3"), length.out = 30L),
        spoke_item = rep(c("s21", "s22", "s21"), length.out = 30L),
        y_spoke = rep(c(1L, 0L), length.out = 30L),
        spoke_in_A = TRUE,
        is_probe_step = TRUE
      )
    },
    .adaptive_link_fit_transform_alt_shift_scale = function(...) {
      list(converged = TRUE, delta_mean = 0.2, log_alpha_mean = 0.3, log_alpha_sd = 0.02)
    },
    .adaptive_link_fit_transform = function(cross_edges, hub_theta, spoke_theta, transform_mode) {
      list(
        delta_mean = 0.1,
        delta_sd = 0.1,
        log_alpha_mean = NA_real_,
        log_alpha_sd = NA_real_,
        theta_hub_post = hub_theta,
        theta_spoke_post = spoke_theta,
        posterior_draws = list(delta = c(0.1, 0.1)),
        diagnostics = list(
          divergences = 0L,
          max_rhat = 1.20,
          min_ess_bulk = 50,
          diagnostics_divergences_pass = TRUE,
          diagnostics_rhat_pass = FALSE,
          diagnostics_ess_pass = FALSE
        ),
        fit_contract = list(
          estimation_method = "cmdstan_hmc",
          uncertainty_approximation = "cmdstan_posterior_draws"
        )
      )
    },
    .adaptive_link_probe_brier_for_fit = function(..., log_alpha_mean = NA_real_) {
      if (is.finite(log_alpha_mean)) 0.10 else 0.12
    },
    .adaptive_link_probe_pred_rmse_lagged_for_fit = function(...) 0.01,
    .adaptive_link_theta_global_rmse_lagged = function(...) 0.02,
    .package = "pairwiseLLM",
    {
      pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
    }
  )

  stats <- out$controller$link_refit_stats_by_spoke[["2"]]
  expect_false(isTRUE(stats$link_stop_gate_open))
  expect_false(isTRUE(stats$link_stop_eligible))
  expect_false(isTRUE(stats$escalated_this_refit))
  expect_identical(out$controller$link_transform_state_by_spoke[["2"]], "shift_only")
  expect_identical(stats$link_fit_method, "cmdstan_hmc")
  expect_identical(stats$link_uncertainty_approximation, "cmdstan_posterior_draws")
  expect_identical(stats$alternative_fit_method, "map_laplace_hessian")
  expect_identical(stats$alternative_uncertainty_approximation, "laplace_hessian")
})

test_that("stable Phase B epochs expose finite lagged stop metrics and clear unavailable blockers", {
  state <- make_stable_epoch_stop_state()

  out <- testthat::with_mocked_bindings(
    .adaptive_link_cross_edges = function(...) {
      tibble::tibble(
        spoke_item = c("s21", "s22", "s21"),
        hub_item = c("h1", "h2", "h3"),
        y_spoke = c(1L, 0L, 1L),
        step_id = c(1L, 2L, 3L),
        spoke_in_A = c(TRUE, TRUE, TRUE),
        run_mode = c("link_multi_spoke", "link_multi_spoke", "link_multi_spoke"),
        is_probe_step = c(FALSE, FALSE, FALSE)
      )
    },
    .adaptive_link_fit_transform = function(cross_edges, hub_theta, spoke_theta, transform_mode) {
      list(
        delta_mean = 0.14,
        delta_sd = 0.01,
        log_alpha_mean = NA_real_,
        log_alpha_sd = NA_real_,
        theta_hub_post = hub_theta,
        theta_spoke_post = spoke_theta,
        posterior_draws = list(delta = c(0.14, 0.14)),
        diagnostics = list(
          divergences = 0L,
          max_rhat = 1.0,
          min_ess_bulk = 500,
          diagnostics_divergences_pass = TRUE,
          diagnostics_rhat_pass = TRUE,
          diagnostics_ess_pass = TRUE
        ),
        fit_contract = list(
          estimation_method = "cmdstan_hmc",
          uncertainty_approximation = "cmdstan_posterior_draws"
        )
      )
    },
    .adaptive_link_global_score_stats_active = function(...) list(reliability = 0.96),
    .adaptive_link_reliability_transformed_active = function(...) 0.96,
    .adaptive_link_ts_btl_rank_spearman_active = function(...) 0.95,
    .adaptive_link_rank_stability_lagged = function(...) {
      list(rho_rank_lagged = 0.98, rho_rank_lagged_pass = TRUE)
    },
    .adaptive_link_probe_edges_realized = function(...) {
      tibble::tibble(
        hub_item = c("h1", "h2"),
        spoke_item = c("s21", "s22"),
        y_spoke = c(1L, 0L),
        step_id = c(11L, 12L),
        spoke_in_A = c(TRUE, TRUE),
        run_mode = c("link_probe_holdout", "link_probe_holdout"),
        is_probe_step = c(TRUE, TRUE),
        pair_key = c(
          pairwiseLLM:::make_unordered_key("h1", "s21"),
          pairwiseLLM:::make_unordered_key("h2", "s22")
        )
      )
    },
    .adaptive_link_probe_brier_for_fit = function(...) 0.10,
    .adaptive_link_probe_pred_rmse_lagged_for_fit = function(...) 0.01,
    .adaptive_link_theta_global_rmse_lagged = function(...) 0.02,
    .package = "pairwiseLLM",
    {
      pairwiseLLM:::.adaptive_linking_refit_update_state(
        state,
        refit_context = list(last_refit_step = 0L)
      )
    }
  )

  stats <- out$controller$link_refit_stats_by_spoke[["2"]]
  row <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    out,
    refit_id = 3L,
    refit_context = list(last_refit_step = 0L)
  )
  row <- row[row$spoke_id == 2L, , drop = FALSE]

  expect_true(isTRUE(stats$link_lag_eligible))
  expect_true(is.finite(stats$probe_pred_rmse_lagged))
  expect_true(is.finite(stats$theta_global_rmse_lagged))
  expect_true(isTRUE(stats$probe_pred_rmse_pass))
  expect_true(isTRUE(stats$theta_global_rmse_pass))
  expect_false(grepl("probe_pred_rmse_lagged", as.character(stats$stop_blocker_codes), fixed = TRUE))
  expect_false(grepl("theta_global_rmse_lagged", as.character(stats$stop_blocker_codes), fixed = TRUE))
  expect_true(isTRUE(row$link_lag_eligible[[1L]]))
  expect_true(is.finite(row$probe_pred_rmse_lagged[[1L]]))
  expect_true(is.finite(row$theta_global_rmse_lagged[[1L]]))
})

test_that("anchored-joint lag helpers are finite and log normalization disables escalation fields", {
  current_theta <- c(h1 = 0.80, h2 = 0.40, h3 = 0.10, s21 = -0.08, s22 = -0.38)
  lag_theta <- c(h1 = 0.80, h2 = 0.40, h3 = 0.10, s21 = -0.10, s22 = -0.40)
  edges <- tibble::tibble(
    hub_item = c("h1", "h2"),
    spoke_item = c("s21", "s22"),
    spoke_in_A = c(TRUE, TRUE)
  )

  theta_rmse <- pairwiseLLM:::.adaptive_link_theta_global_rmse_from_maps(
    current_theta = current_theta,
    lag_theta = lag_theta,
    scope_ids = c("s21", "s22")
  )
  probe_rmse <- pairwiseLLM:::.adaptive_link_probe_pred_rmse_lagged_anchored_joint(
    edges = edges,
    current_theta = current_theta,
    lag_theta = lag_theta,
    judge_params = list(beta = 0, epsilon = 0)
  )

  expect_true(is.finite(theta_rmse))
  expect_true(theta_rmse > 0)
  expect_true(is.finite(probe_rmse))
  expect_true(probe_rmse > 0)

  raw_row <- tibble::tibble(
    refit_id = 3L,
    spoke_id = 2L,
    hub_id = 1L,
    link_epoch_id = 4L,
    link_estimation_mode = "anchored_joint",
    link_transform_policy = "auto",
    link_transform_state = "shift_only",
    link_refit_mode = "shift_only",
    hub_lock_mode = "hard_lock",
    hub_lock_kappa = 0.75,
    scale_ready = TRUE,
    alternative_fit_method = "map_laplace_hessian",
    alternative_uncertainty_approximation = "laplace_hessian",
    alt_eval_active_edges = 3L,
    alt_eval_converged = TRUE,
    probe_brier_delta_min_used = 0.005,
    logalpha_sd_guardrail_used = 0.10,
    escalation_recent_pass_count = 1L,
    escalation_recent_window_size = 2L,
    escalated_this_refit = TRUE
  )
  normalized <- pairwiseLLM:::.adaptive_log_normalize_mode_fields(
    row = raw_row,
    schema = pairwiseLLM:::schema_link_stage_log,
    log_name = "link_stage_log"
  )

  expect_true(is.na(normalized$link_transform_policy[[1L]]))
  expect_true(is.na(normalized$link_transform_state[[1L]]))
  expect_true(is.na(normalized$link_refit_mode[[1L]]))
  expect_true(is.na(normalized$hub_lock_kappa[[1L]]))
  expect_false(normalized$scale_ready[[1L]])
  expect_false(normalized$alt_eval_converged[[1L]])
  expect_false(normalized$escalated_this_refit[[1L]])
  expect_true(is.na(normalized$alternative_fit_method[[1L]]))
  expect_true(is.na(normalized$alternative_uncertainty_approximation[[1L]]))
  expect_true(is.na(normalized$alt_eval_active_edges[[1L]]))
  expect_true(is.na(normalized$probe_brier_delta_min_used[[1L]]))
  expect_true(is.na(normalized$logalpha_sd_guardrail_used[[1L]]))
  expect_true(is.na(normalized$escalation_recent_pass_count[[1L]]))
  expect_true(is.na(normalized$escalation_recent_window_size[[1L]]))
})

test_that("stable Phase B epochs can open the stop gate and become stop-eligible", {
  state <- make_stable_epoch_stop_state()

  out <- testthat::with_mocked_bindings(
    .adaptive_link_cross_edges = function(...) {
      tibble::tibble(
        spoke_item = c("s21", "s22", "s21"),
        hub_item = c("h1", "h2", "h3"),
        y_spoke = c(1L, 0L, 1L),
        step_id = c(1L, 2L, 3L),
        spoke_in_A = c(TRUE, TRUE, TRUE),
        run_mode = c("link_multi_spoke", "link_multi_spoke", "link_multi_spoke"),
        is_probe_step = c(FALSE, FALSE, FALSE)
      )
    },
    .adaptive_link_fit_transform = function(cross_edges, hub_theta, spoke_theta, transform_mode) {
      list(
        delta_mean = 0.14,
        delta_sd = 0.01,
        log_alpha_mean = NA_real_,
        log_alpha_sd = NA_real_,
        theta_hub_post = hub_theta,
        theta_spoke_post = spoke_theta,
        posterior_draws = list(delta = c(0.14, 0.14)),
        diagnostics = list(
          divergences = 0L,
          max_rhat = 1.0,
          min_ess_bulk = 500,
          diagnostics_divergences_pass = TRUE,
          diagnostics_rhat_pass = TRUE,
          diagnostics_ess_pass = TRUE
        ),
        fit_contract = list(
          estimation_method = "cmdstan_hmc",
          uncertainty_approximation = "cmdstan_posterior_draws"
        )
      )
    },
    .adaptive_link_global_score_stats_active = function(...) list(reliability = 0.96),
    .adaptive_link_reliability_transformed_active = function(...) 0.96,
    .adaptive_link_ts_btl_rank_spearman_active = function(...) 0.95,
    .adaptive_link_rank_stability_lagged = function(...) {
      list(rho_rank_lagged = 0.98, rho_rank_lagged_pass = TRUE)
    },
    .adaptive_link_probe_edges_realized = function(...) {
      tibble::tibble(
        hub_item = c("h1", "h2"),
        spoke_item = c("s21", "s22"),
        y_spoke = c(1L, 0L),
        step_id = c(11L, 12L),
        spoke_in_A = c(TRUE, TRUE),
        run_mode = c("link_probe_holdout", "link_probe_holdout"),
        is_probe_step = c(TRUE, TRUE),
        pair_key = c(
          pairwiseLLM:::make_unordered_key("h1", "s21"),
          pairwiseLLM:::make_unordered_key("h2", "s22")
        )
      )
    },
    .adaptive_link_probe_brier_for_fit = function(...) 0.10,
    .adaptive_link_probe_pred_rmse_lagged_for_fit = function(...) 0.01,
    .adaptive_link_theta_global_rmse_lagged = function(...) 0.02,
    .package = "pairwiseLLM",
    {
      pairwiseLLM:::.adaptive_linking_refit_update_state(
        state,
        refit_context = list(last_refit_step = 0L)
      )
    }
  )

  stats <- out$controller$link_refit_stats_by_spoke[["2"]]
  row <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    out,
    refit_id = 3L,
    refit_context = list(last_refit_step = 0L)
  )
  row <- row[row$spoke_id == 2L, , drop = FALSE]

  expect_true(isTRUE(stats$link_stop_gate_open))
  expect_true(isTRUE(stats$link_stop_eligible))
  expect_true(isTRUE(row$link_stop_gate_open[[1L]]))
  expect_true(isTRUE(row$link_stop_eligible[[1L]]))
  expect_true(isTRUE(stats$link_diagnostics_pass))
  expect_true(isTRUE(row$link_diagnostics_pass[[1L]]))
  expect_identical(as.integer(row$link_epoch_id[[1L]]), 4L)
  expect_identical(as.character(row$probe_panel_id[[1L]]), "panel_a")
  expect_identical(as.integer(row$probe_edges_realized[[1L]]), 2L)
})

test_that("anchored-joint deterministic diagnostics open stop gates and freeze the spoke", {
  state <- make_stable_epoch_stop_state(
    probe_edges_min_for_stop = 2L,
    min_refits_in_phase_b = 3L,
    stability_lag = 2L
  )
  state$controller$link_estimation_mode <- "anchored_joint"
  state$controller$hub_lock_mode <- "hard_lock"
  state$controller$run_mode <- "link_one_spoke"
  state$controller$current_link_spoke_id <- 2L
  state$controller$stability_window_refits <- 1L
  state$controller$stability_passes_required <- 1L
  state$config$btl_config$stability_lag <- 2L
  state$linking$phase_a$ready_spokes <- 2L
  state$linking$phase_a$active_spokes <- 2L
  state$controller$link_epoch_signature_by_spoke <- list(
    `2` = pairwiseLLM:::.adaptive_link_epoch_signature_string(
      pairwiseLLM:::.adaptive_link_epoch_signature_components(
        transform_state = NA_character_,
        refit_mode = NA_character_,
        lock_mode = "hard_lock",
        hub_art = state$linking$phase_a$artifacts[["1"]],
        spoke_art = state$linking$phase_a$artifacts[["2"]],
        link_estimation_mode = "anchored_joint"
      )
    )
  )
  anchored_hist <- as.integer(state$link_stage_log$spoke_id) == 2L
  state$link_stage_log$link_estimation_mode[anchored_hist] <- "anchored_joint"
  state$link_stage_log$link_transform_policy[anchored_hist] <- NA_character_
  state$link_stage_log$link_transform_state[anchored_hist] <- NA_character_
  state$link_stage_log$link_refit_mode[anchored_hist] <- NA_character_
  state$link_stage_log$hub_lock_mode[anchored_hist] <- "hard_lock"
  state$linking$phase_a$artifacts[["1"]]$phase_a_within_set_evidence <- tibble::tibble(
    pair_id = 1L,
    step_id = 1L,
    A_item = "h1",
    B_item = "h2",
    y_A = 1L
  )
  state$linking$phase_a$artifacts[["2"]]$phase_a_within_set_evidence <- tibble::tibble(
    pair_id = 2L,
    step_id = 2L,
    A_item = "s21",
    B_item = "s22",
    y_A = 1L
  )
  state$linking$phase_a$artifacts[["1"]]$phase_a_within_set_evidence_hash <-
    pairwiseLLM:::.adaptive_phase_a_hash_object(
      state$linking$phase_a$artifacts[["1"]]$phase_a_within_set_evidence
    )
  state$linking$phase_a$artifacts[["2"]]$phase_a_within_set_evidence_hash <-
    pairwiseLLM:::.adaptive_phase_a_hash_object(
      state$linking$phase_a$artifacts[["2"]]$phase_a_within_set_evidence
    )

  accepted <- pairwiseLLM:::.adaptive_link_anchored_joint_resolve_state(
    state = state,
    spoke_id = 2L,
    controller = state$controller
  )
  state$linking$anchored_joint$accepted_state_by_spoke[["2"]] <- accepted
  state$linking$anchored_joint$fisher_t0_by_spoke[["2"]] <- list(
    fisher = diag(length(accepted$theta_spoke_global_mean)),
    item_ids = names(accepted$theta_spoke_global_mean),
    anchored_joint_init_state_method = accepted$anchored_joint_init_state_method
  )

  out <- testthat::with_mocked_bindings(
    .adaptive_link_fit_anchored_joint = function(...) {
      list(
        delta_mean = 0,
        delta_sd = NA_real_,
        log_alpha_mean = NA_real_,
        log_alpha_sd = NA_real_,
        theta_hub_post = accepted$theta_hub_fixed,
        theta_spoke_post = accepted$theta_spoke_global_mean,
        theta_spoke_sd_post = stats::setNames(
          c(0.08, 0.07),
          names(accepted$theta_spoke_global_mean)
        ),
        posterior_draws = list(),
        diagnostics = list(
          converged = TRUE,
          hessian_posdef = TRUE
        ),
        fit_contract = list(
          contract_type = "link_refit",
          estimation_method = "map_laplace",
          uncertainty_approximation = "laplace_hessian",
          anchored_joint = list(free_block_dim = 2L),
          priors = list(
            anchored_joint_spoke_prior_scale = 1.0,
            anchored_joint_sd_floor = 0.02,
            anchored_joint_spoke_prior_fallback_sd = 1.0,
            prior_sd_fallback_used = FALSE,
            prior_sd_fallback_items = character()
          )
        )
      )
    },
    .adaptive_link_global_score_stats_active = function(...) {
      list(reliability = 0.96, V_mu = 1.2, V_post = 0.04)
    },
    .adaptive_link_reliability_transformed_active = function(...) 0.96,
    .adaptive_link_ts_btl_rank_spearman_active = function(...) 0.95,
    .adaptive_link_rank_stability_lagged = function(...) {
      list(lag_eligible = TRUE, rho_rank_lagged = 0.99, rho_rank_lagged_pass = TRUE)
    },
    .adaptive_link_probe_brier_for_fit = function(...) 0.10,
    .adaptive_link_probe_edges_realized = function(...) {
      tibble::tibble(
        hub_item = c("h1", "h2"),
        spoke_item = c("s21", "s22"),
        y_spoke = c(1L, 0L),
        step_id = c(11L, 12L),
        spoke_in_A = c(TRUE, TRUE),
        run_mode = c("link_probe_holdout", "link_probe_holdout"),
        is_probe_step = c(TRUE, TRUE),
        pair_key = c(
          pairwiseLLM:::make_unordered_key("h1", "s21"),
          pairwiseLLM:::make_unordered_key("h2", "s22")
        )
      )
    },
    .adaptive_link_probe_pred_rmse_lagged_anchored_joint = function(...) 0.01,
    .adaptive_link_theta_global_rmse_from_maps = function(...) 0.02,
    .package = "pairwiseLLM",
    {
      pairwiseLLM:::.adaptive_linking_refit_update_state(
        state,
        refit_context = list(last_refit_step = 3L)
      )
    }
  )

  stats <- out$controller$link_refit_stats_by_spoke[["2"]]
  expect_true(isTRUE(stats$link_diagnostics_pass))
  expect_true(isTRUE(stats$link_stop_gate_open))
  expect_true(isTRUE(stats$link_stop_eligible))
  expect_true(isTRUE(stats$link_stop_pass))

  out <- pairwiseLLM:::.adaptive_link_apply_stop_state(
    out,
    pairwiseLLM:::.adaptive_link_stage_refit_rows(
      out,
      refit_id = 3L,
      refit_context = list(last_refit_step = 3L)
    )
  )

  expect_true(isTRUE(out$controller$link_state_frozen_by_spoke[["2"]]))
  expect_identical(out$controller$link_state_frozen_refit_id_by_spoke[["2"]], 3L)
  expect_identical(pairwiseLLM:::.adaptive_link_effective_active_spokes(out), integer())
  expect_true(isTRUE(pairwiseLLM:::.adaptive_link_all_spokes_stopped(out)))
  expect_true(is.na(pairwiseLLM:::.adaptive_link_probe_next_holdout_spoke(
    out,
    controller = out$controller
  )))
})

test_that("linking identified state is reconstructable from canonical link-stage fields", {
  row <- tibble::tibble(
    link_transform_mode = "shift_scale",
    reliability_link_global = 0.92,
    ts_btl_rank_spearman = 0.93
  )
  identified <- pairwiseLLM:::.adaptive_link_reconstruct_identified_from_logs(
    link_row = row,
    controller = list(
      link_identified_reliability_min = 0.80,
      link_rank_corr_min = 0.90
    )
  )
  expect_true(isTRUE(identified))

  row$ts_btl_rank_spearman <- 0.85
  identified_fail <- pairwiseLLM:::.adaptive_link_reconstruct_identified_from_logs(
    link_row = row,
    controller = list(
      link_identified_reliability_min = 0.80,
      link_rank_corr_min = 0.90
    )
  )
  expect_false(isTRUE(identified_fail))
})

test_that("linking refit stats carry latest coverage metadata for link-stage log rows", {
  state <- make_linking_refit_state()
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state$controller$link_stage_coverage_bins_used <- list(`2` = 3L)
  state$controller$link_stage_coverage_source <- list(`2` = "linking_global_score")

  state <- pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row <- rows[rows$spoke_id == 2L, , drop = FALSE]
  expect_identical(row$coverage_bins_used[[1L]], 3L)
  expect_identical(row$coverage_source[[1L]], "linking_global_score")
})

test_that("taper decisions are reconstructable from canonical link-stage quota fields", {
  row_raw <- tibble::tibble(
    quota_taper_applied = NA,
    quota_long_link_raw = 8L,
    quota_long_link_effective = 4L
  )
  row_notaper <- tibble::tibble(
    quota_taper_applied = NA,
    quota_long_link_raw = 8L,
    quota_long_link_effective = 8L
  )
  row_explicit <- tibble::tibble(
    quota_taper_applied = TRUE,
    quota_long_link_raw = 8L,
    quota_long_link_effective = 8L
  )

  expect_true(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_taper_from_logs(row_raw)))
  expect_false(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_taper_from_logs(row_notaper)))
  expect_true(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_taper_from_logs(row_explicit)))
})

test_that("item log keeps raw summaries separate from transformed global summaries in linking mode", {
  state <- make_linking_refit_state(
    list(link_transform_mode = "shift_scale", multi_spoke_mode = "independent")
  )
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_state = "shift_scale",
      delta_spoke_mean = 0.3,
      log_alpha_spoke_mean = log(1.2)
    ),
    `3` = list(
      link_transform_state = "shift_only",
      delta_spoke_mean = -0.2,
      log_alpha_spoke_mean = NA_real_
    )
  )

  item_log <- pairwiseLLM:::.adaptive_build_item_log_refit(state, refit_id = 1L)
  row_s2 <- item_log[item_log$item_id == "s21", , drop = FALSE]
  row_s3 <- item_log[item_log$item_id == "s31", , drop = FALSE]
  row_h <- item_log[item_log$item_id == "h1", , drop = FALSE]

  expect_equal(
    row_s2$theta_link_eap[[1L]],
    0.3 + 1.2 * row_s2$theta_raw_eap[[1L]],
    tolerance = 1e-12
  )
  expect_equal(
    row_s3$theta_link_eap[[1L]],
    -0.2 + row_s3$theta_raw_eap[[1L]],
    tolerance = 1e-12
  )
  expect_equal(row_h$theta_link_eap[[1L]], row_h$theta_raw_eap[[1L]], tolerance = 1e-12)
})

test_that("item log uses typed NA global summaries when spoke transform parameters are unavailable", {
  state <- make_linking_refit_state(
    list(link_transform_mode = "shift_scale", multi_spoke_mode = "independent")
  )
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_mode = "shift_scale",
      delta_spoke_mean = 0.25,
      log_alpha_spoke_mean = NA_real_
    ),
    `3` = list(
      link_transform_mode = "shift_only",
      delta_spoke_mean = NA_real_
    )
  )

  item_log <- pairwiseLLM:::.adaptive_build_item_log_refit(state, refit_id = 1L)
  row_s2 <- item_log[item_log$item_id == "s21", , drop = FALSE]
  row_s3 <- item_log[item_log$item_id == "s31", , drop = FALSE]
  row_h <- item_log[item_log$item_id == "h1", , drop = FALSE]

  expect_true(is.na(row_s2$theta_link_eap[[1L]]))
  expect_true(is.na(row_s2$theta_link_sd[[1L]]))
  expect_true(is.na(row_s3$theta_link_eap[[1L]]))
  expect_true(is.na(row_s3$theta_link_sd[[1L]]))
  expect_true(is.finite(row_h$theta_link_eap[[1L]]))
  expect_true(is.finite(row_h$theta_link_sd[[1L]]))
})

test_that("non-linking item log keeps current raw/global behavior under seeded setup", {
  state <- adaptive_rank_start(make_test_items(5), seed = 15L)
  ids <- as.character(state$item_ids)
  draws <- matrix(
    c(
      0.9, 0.5, 0.2, -0.2, -0.6,
      0.8, 0.6, 0.1, -0.1, -0.7,
      1.0, 0.4, 0.3, -0.3, -0.5,
      0.7, 0.3, 0.0, -0.4, -0.8
    ),
    nrow = 4,
    byrow = TRUE
  )
  colnames(draws) <- ids
  state$btl_fit <- make_test_btl_fit(ids, draws = draws, model_variant = "btl_e_b")

  item_log <- pairwiseLLM:::.adaptive_build_item_log_refit(state, refit_id = 1L)
  expect_equal(item_log$theta_raw_eap, item_log$theta_link_eap, tolerance = 1e-12)
  expect_equal(item_log$theta_link_sd, item_log$theta_raw_sd, tolerance = 1e-12)
  expect_identical(
    item_log$rank_link,
    as.integer(rank(-as.double(item_log$theta_link_eap), ties.method = "first"))
  )
})

test_that("item log exposes phase scope and keeps link summaries NA during linking Phase A", {
  state <- make_linking_refit_state(
    list(link_transform_mode = "shift_scale", multi_spoke_mode = "independent")
  )
  state$linking$phase_a$ready_for_phase_b <- FALSE
  state$linking$phase_a$phase <- "phase_a"
  state$linking$phase_a$set_status <- tibble::tibble(
    set_id = c(1L, 2L, 3L),
    source = c("run", "run", "run"),
    status = c("ready", "pending_finalization", "ready"),
    validation_message = c("ok", "pending_finalization: within-set stop criteria not yet met", "ok"),
    artifact_path = c(NA_character_, NA_character_, NA_character_)
  )

  item_log <- pairwiseLLM:::.adaptive_build_item_log_refit(state, refit_id = 1L)
  in_scope <- item_log[item_log$in_phase_scope %in% TRUE, , drop = FALSE]
  out_scope <- item_log[!item_log$in_phase_scope %in% TRUE, , drop = FALSE]

  expect_true(all(item_log$phase_scope == "phase_a_set"))
  expect_true(all(item_log$phase_scope_set_id == 2L))
  expect_true(all(is.na(item_log$theta_link_eap)))
  expect_true(all(is.na(item_log$theta_link_sd)))

  expect_true(nrow(in_scope) > 0L)
  expect_true(all(in_scope$set_id == 2L))
  expect_true(all(is.finite(in_scope$theta_raw_eap)))
  expect_true(all(is.finite(in_scope$rank_raw)))

  expect_true(nrow(out_scope) > 0L)
  expect_true(all(is.finite(out_scope$theta_raw_eap)))
  expect_true(all(is.finite(out_scope$rank_raw)))
})

test_that("lagged rank stability gate uses Spearman threshold of at least 0.98", {
  state <- make_linking_refit_state()
  ids <- c("h1", "h2", "s21", "s22")
  base <- c(h1 = 4, h2 = 3, s21 = 2, s22 = 1)
  same <- c(h1 = 3.9, h2 = 2.9, s21 = 2.1, s22 = 1.1)
  swapped <- c(h1 = 1, h2 = 4, s21 = 2, s22 = 3)
  state$refit_meta$theta_mean_history <- list(base, same, swapped)

  pass <- pairwiseLLM:::.adaptive_link_rank_stability_lagged(
    state = state,
    active_ids = ids,
    stability_lag = 1L,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = 0,
    log_alpha_mean = NA_real_,
    lag_row = tibble::tibble(
      link_transform_mode = "shift_only",
      delta_spoke_mean = 0,
      log_alpha_spoke_mean = NA_real_
    )
  )
  expect_true(isTRUE(pass$lag_eligible))
  expect_false(is.na(pass$rho_rank_lagged))
  expect_false(isTRUE(pass$rho_rank_lagged_pass))

  state$refit_meta$theta_mean_history <- list(base, same)
  pass2 <- pairwiseLLM:::.adaptive_link_rank_stability_lagged(
    state = state,
    active_ids = ids,
    stability_lag = 1L,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = 0,
    log_alpha_mean = NA_real_,
    lag_row = tibble::tibble(
      link_transform_mode = "shift_only",
      delta_spoke_mean = 0,
      log_alpha_spoke_mean = NA_real_
    )
  )
  expect_true(isTRUE(pass2$lag_eligible))
  expect_true(isTRUE(pass2$rho_rank_lagged_pass))
  expect_true(pass2$rho_rank_lagged >= 0.98)
})

test_that("single-set mode does not emit linking stage rows", {
  state <- adaptive_rank_start(make_test_items(4), seed = 21L)
  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  expect_equal(nrow(rows), 0L)
})

test_that("linking active-domain helper guard branches return typed NA outputs", {
  state <- make_linking_refit_state()

  mode <- pairwiseLLM:::.adaptive_link_transform_state_for_spoke(
    controller = list(link_transform_policy = "auto", link_transform_state_by_spoke = list(`2` = "bad")),
    spoke_id = 2L
  )
  expect_identical(mode, "shift_only")

  bad_draws <- state
  bad_draws$btl_fit$btl_posterior_draws <- NULL
  expect_true(is.na(pairwiseLLM:::.adaptive_link_reliability_active(bad_draws, c("h1", "h2"))))
  expect_true(is.na(pairwiseLLM:::.adaptive_link_reliability_active(state, c("missing_a", "missing_b"))))
  expect_true(is.na(pairwiseLLM:::.adaptive_link_reliability_active(state, c("h1"))))

  no_ts <- state
  no_ts$trueskill_state <- NULL
  expect_true(is.na(pairwiseLLM:::.adaptive_link_ts_btl_rank_spearman_active(no_ts, c("h1", "h2"))))
  expect_true(is.na(pairwiseLLM:::.adaptive_link_ts_btl_rank_spearman_active(state, c("missing_a", "missing_b"))))
  expect_true(is.na(
    pairwiseLLM:::.adaptive_link_ts_btl_rank_spearman_active(state, c("h1", "h2"), theta_mean = c(1, 2))
  ))
  theta_nonfinite <- setNames(c(NA_real_, 1), c("h1", "h2"))
  expect_true(is.na(
    pairwiseLLM:::.adaptive_link_ts_btl_rank_spearman_active(state, c("h1", "h2"), theta_mean = theta_nonfinite)
  ))
  theta_tied <- setNames(c(1, 1), c("h1", "h2"))
  state$trueskill_state$items$mu[state$trueskill_state$items$item_id %in% c("h1", "h2")] <- 1
  expect_true(is.na(
    pairwiseLLM:::.adaptive_link_ts_btl_rank_spearman_active(state, c("h1", "h2"), theta_mean = theta_tied)
  ))

  no_hist <- state
  no_hist$refit_meta$theta_mean_history <- list()
  lag_none <- pairwiseLLM:::.adaptive_link_rank_stability_lagged(
    no_hist,
    c("h1", "h2"),
    stability_lag = 2L,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = 0,
    log_alpha_mean = NA_real_,
    lag_row = tibble::tibble()
  )
  expect_false(isTRUE(lag_none$lag_eligible))
  expect_true(is.na(lag_none$rho_rank_lagged))

  bad_hist <- state
  bad_hist$refit_meta$theta_mean_history <- list(c(h1 = 1, h2 = 2), c(h1 = NA_real_, h2 = 3), c(h1 = 2, h2 = 1))
  lag_bad <- pairwiseLLM:::.adaptive_link_rank_stability_lagged(
    bad_hist,
    c("h1", "h2"),
    stability_lag = 1L,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = 0,
    log_alpha_mean = NA_real_,
    lag_row = tibble::tibble(
      link_transform_mode = "shift_only",
      delta_spoke_mean = 0,
      log_alpha_spoke_mean = NA_real_
    )
  )
  expect_true(isTRUE(lag_bad$lag_eligible))
  expect_false(isTRUE(lag_bad$rho_rank_lagged_pass))
  bad_hist2 <- state
  bad_hist2$refit_meta$theta_mean_history <- list(c(h1 = 1, h2 = 2), "bad", c(h1 = 2, h2 = 1))
  lag_bad2 <- pairwiseLLM:::.adaptive_link_rank_stability_lagged(
    bad_hist2,
    c("h1", "h2"),
    stability_lag = 1L,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = 0,
    log_alpha_mean = NA_real_,
    lag_row = tibble::tibble(
      link_transform_mode = "shift_only",
      delta_spoke_mean = 0,
      log_alpha_spoke_mean = NA_real_
    )
  )
  expect_true(isTRUE(lag_bad2$lag_eligible))
  expect_false(isTRUE(lag_bad2$rho_rank_lagged_pass))
  bad_hist3 <- state
  bad_hist3$refit_meta$theta_mean_history <- list(c(h1 = 1, h2 = 2), c(h1 = 2, h2 = 1))
  lag_bad3 <- pairwiseLLM:::.adaptive_link_rank_stability_lagged(
    bad_hist3,
    c("h1", "missing"),
    stability_lag = 1L,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = 0,
    log_alpha_mean = NA_real_,
    lag_row = tibble::tibble(
      link_transform_mode = "shift_only",
      delta_spoke_mean = 0,
      log_alpha_spoke_mean = NA_real_
    )
  )
  expect_true(isTRUE(lag_bad3$lag_eligible))
  expect_false(isTRUE(lag_bad3$rho_rank_lagged_pass))

  short_hub <- state
  short_hub$btl_fit$theta_mean <- c(h1 = 1)
  expect_true(is.na(pairwiseLLM:::.adaptive_link_delta_sd_max_derived(short_hub, hub_id = 1L, delta_sd_mult = 0.1)))
  unnamed_hub <- state
  unnamed_hub$btl_fit$theta_mean <- c(1, 2)
  expect_true(is.na(pairwiseLLM:::.adaptive_link_delta_sd_max_derived(unnamed_hub, hub_id = 1L, delta_sd_mult = 0.1)))
})

test_that("active-domain TS-BTL correlation is computed on active items only", {
  state <- make_linking_refit_state()
  ts <- state$trueskill_state$items
  ts$mu[match(c("h1", "h2", "s21", "s22", "s31", "s32"), ts$item_id)] <- c(6, 5, 4, 3, 2, 1)
  state$trueskill_state$items <- ts

  theta <- c(h1 = 0.1, h2 = 0.2, s21 = 0.9, s22 = 1.0, s31 = 0.8, s32 = 0.7)
  active_ids <- c("h1", "h2", "s21", "s22")
  rho_active <- pairwiseLLM:::.adaptive_link_ts_btl_rank_spearman_active(
    state = state,
    active_ids = active_ids,
    theta_mean = theta
  )
  rho_full <- pairwiseLLM:::.adaptive_link_ts_btl_rank_spearman_active(
    state = state,
    active_ids = names(theta),
    theta_mean = theta
  )
  expect_equal(rho_active, -1, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(rho_active, rho_full, tolerance = 1e-12)))
})

test_that("transformed-domain helper and reconstruction guard branches are covered", {
  state <- make_linking_refit_state()

  empty_theta <- pairwiseLLM:::.adaptive_link_transform_theta_mean_for_spoke(
    state = state,
    theta_mean = c(1, 2),
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = 0.2
  )
  expect_identical(length(empty_theta), 0L)

  theta <- c(h1 = 1, s21 = 2, s31 = 3)
  transformed_bad_mode <- pairwiseLLM:::.adaptive_link_transform_theta_mean_for_spoke(
    state = state,
    theta_mean = theta,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "bad_mode",
    delta_mean = 0.2
  )
  expect_equal(transformed_bad_mode[["h1"]], 1, tolerance = 1e-12)
  expect_equal(transformed_bad_mode[["s21"]], 2.2, tolerance = 1e-12)
  expect_false("s31" %in% names(transformed_bad_mode))

  transformed_bad_alpha <- pairwiseLLM:::.adaptive_link_transform_theta_mean_for_spoke(
    state = state,
    theta_mean = theta,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_scale",
    delta_mean = 0.2,
    log_alpha_mean = NA_real_
  )
  expect_true(all(is.na(transformed_bad_alpha)))

  active <- pairwiseLLM:::.adaptive_link_active_item_ids(state, spoke_id = 2L, hub_id = 1L)
  rel_bad_mode <- pairwiseLLM:::.adaptive_link_reliability_transformed_active(
    state = state,
    active_ids = active$active_all,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "bad_mode",
    delta_mean = 0.1
  )
  expect_true(is.finite(rel_bad_mode))
  rel_bad_delta <- pairwiseLLM:::.adaptive_link_reliability_transformed_active(
    state = state,
    active_ids = active$active_all,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = NA_real_
  )
  expect_true(is.na(rel_bad_delta))
  rel_bad_alpha <- pairwiseLLM:::.adaptive_link_reliability_transformed_active(
    state = state,
    active_ids = active$active_all,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_scale",
    delta_mean = 0.1,
    log_alpha_mean = NA_real_
  )
  expect_true(is.na(rel_bad_alpha))

  lag_state <- state
  lag_state$refit_meta$theta_mean_history <- list(c(h1 = 1, h2 = 2), "bad")
  lag <- pairwiseLLM:::.adaptive_link_rank_stability_lagged(
    state = lag_state,
    active_ids = c("h1", "h2"),
    stability_lag = 1L,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = 0.1,
    lag_row = tibble::tibble(
      link_transform_mode = "shift_only",
      delta_spoke_mean = 0.1,
      log_alpha_spoke_mean = NA_real_
    )
  )
  expect_true(isTRUE(lag$lag_eligible))
  expect_false(isTRUE(lag$rho_rank_lagged_pass))

  expect_error(
    pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
      link_row = tibble::tibble(link_stop_eligible = c(TRUE, TRUE)),
      diagnostics_pass = TRUE,
      hub_theta_sd = 1,
      controller = list()
    ),
    "exactly one row"
  )
  expect_false(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = tibble::tibble(link_stop_eligible = FALSE),
    diagnostics_pass = TRUE,
    hub_theta_sd = 1,
    controller = list()
  )))
  expect_false(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = tibble::tibble(link_stop_eligible = TRUE),
    diagnostics_pass = FALSE,
    hub_theta_sd = 1,
    controller = list()
  )))

  expect_error(
    pairwiseLLM:::.adaptive_link_reconstruct_identified_from_logs(
      link_row = tibble::tibble(link_transform_state = c("shift_only", "shift_only")),
      controller = list()
    ),
    "exactly one row"
  )
  expect_true(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_identified_from_logs(
    link_row = tibble::tibble(
      link_transform_state = "shift_scale",
      reliability_link_global = 0.95,
      ts_btl_rank_spearman = 0.95
    ),
    controller = list(link_identified_reliability_min = 0.80, link_rank_corr_min = 0.90)
  )))
  expect_true(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_identified_from_logs(
    link_row = tibble::tibble(
      link_transform_state = "shift_scale",
      reliability_link_global = 0.95,
      ts_btl_rank_spearman = 0.95
    ),
    controller = list(link_identified_reliability_min = 0.80, link_rank_corr_min = 0.90)
  )))
  expect_true(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_identified_from_logs(
    link_row = tibble::tibble(
      link_transform_state = "shift_only",
      reliability_link_global = 0.95,
      ts_btl_rank_spearman = 0.95
    ),
    controller = list(link_identified_reliability_min = 0.80, link_rank_corr_min = 0.90)
  )))
})

test_that("phase-specific judge scope uses explicit phase boundary metadata before first cross-set row", {
  state <- make_linking_refit_state(
    list(run_mode = "link_multi_spoke", hub_id = 1L, judge_param_mode = "phase_specific")
  )
  state$linking$phase_a$phase <- "phase_b"
  state$linking$phase_a$phase_b_started_at_step <- 1L

  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      pair_id = 1L,
      i = 1L,
      j = 2L,
      A = 1L,
      B = 2L,
      Y = 1L,
      set_i = 1L,
      set_j = 1L,
      is_cross_set = FALSE,
      link_spoke_id = NA_integer_,
      run_mode = "link_one_spoke",
      link_stage = "local_link"
    )
  )

  results <- pairwiseLLM:::.adaptive_results_from_step_log(state)
  expect_true(nrow(results) == 1L)
  expect_identical(results$phase[[1L]], "phase3")
  expect_identical(results$judge_scope[[1L]], "link")
})

test_that("adaptive_state validation branches for linking controls are covered", {
  expect_error(
    pairwiseLLM:::.adaptive_state_normalize_items(tibble::tibble(item_id = "a", set_id = "x")),
    "integer-like"
  )
  expect_error(
    pairwiseLLM:::.adaptive_state_normalize_items(tibble::tibble(item_id = "a", set_id = 0L)),
    "must be >= 1"
  )
  expect_error(
    pairwiseLLM:::.adaptive_state_normalize_items(
      tibble::tibble(item_id = "a", global_item_id = NA_character_)
    ),
    "global_item_id"
  )
  expect_error(
    pairwiseLLM:::.adaptive_state_normalize_items(
      tibble::tibble(item_id = c("a", "b"), global_item_id = c("g", "g"))
    ),
    "must be unique"
  )

  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      structure(list(1L), names = ""),
      n_items = 5L
    ),
    "named list with non-empty names"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(hub_lock_kappa = "x"),
      n_items = 5L
    ),
    "single numeric value"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(link_transform_escalation_is_one_way = "x"),
      n_items = 5L
    ),
    "must be TRUE or FALSE"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(run_mode = ""),
      n_items = 5L
    ),
    "single string value"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(phase_a_compatible_config_hashes = c("ok", NA_character_)),
      n_items = 5L
    ),
    "character vector"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(phase_a_artifacts = 1L),
      n_items = 5L
    ),
    "named list"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(phase_a_set_source = c("run")),
      n_items = 5L
    ),
    "named character vector"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(phase_a_set_source = c(`1` = "bad")),
      n_items = 5L
    ),
    "values must be `run` or `import`"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(run_mode = "link_multi_spoke", hub_id = 99L),
      n_items = 5L,
      set_ids = c(1L, 2L, 3L)
    ),
    "must match one observed"
  )
  probe_defaults <- pairwiseLLM:::.adaptive_controller_resolve(5L)
  expect_identical(
    probe_defaults$probe_acceleration_mode,
    "active_floor_plus_sole_blocker"
  )
  expect_true(isTRUE(probe_defaults$probe_active_floor_enabled))
  expect_true(isTRUE(probe_defaults$probe_sole_blocker_acceleration_enabled))
  expect_identical(probe_defaults$probe_pairs_per_refit_per_spoke_bootstrap_max, 6L)
  expect_identical(probe_defaults$probe_pairs_per_refit_per_spoke_sole_blocker_max, 12L)
  expect_identical(probe_defaults$probe_accel_bootstrap_target, 12L)
  expect_identical(probe_defaults$probe_active_floor_frac, 0.5)
  expect_identical(probe_defaults$probe_active_floor_min, 20L)
  expect_true(isTRUE(probe_defaults$probe_active_floor_requires_anchor_progress))
  expect_identical(probe_defaults$probe_sole_blocker_min_realized, 20L)
  expect_identical(probe_defaults$probe_sole_blocker_active_floor_min, 10L)
  probe_ok <- pairwiseLLM:::.adaptive_validate_controller_config(
    list(
      probe_acceleration_mode = "active_floor_plus_sole_blocker",
      probe_pairs_per_refit_per_spoke = 2L,
      probe_pairs_per_refit_per_spoke_bootstrap_max = 6L,
      probe_pairs_per_refit_per_spoke_sole_blocker_max = 12L,
      probe_accel_bootstrap_target = 12L,
      probe_active_floor_frac = 0.5,
      probe_active_floor_min = 20L,
      probe_active_floor_requires_anchor_progress = TRUE,
      probe_sole_blocker_min_realized = 20L,
      probe_sole_blocker_active_floor_min = 10L
    ),
    n_items = 5L
  )
  expect_identical(
    probe_ok$probe_acceleration_mode,
    "active_floor_plus_sole_blocker"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(
        probe_pairs_per_refit_per_spoke = 3L,
        probe_pairs_per_refit_per_spoke_bootstrap_max = 2L
      ),
      n_items = 5L
    ),
    "bootstrap_max"
  )
  q <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = 1L,
    n_items = 10L,
    controller = list(
      run_mode = "link_multi_spoke",
      current_link_spoke_id = 2L,
      linking_identified_by_spoke = list(`2` = TRUE),
      linking_identified = FALSE
    )
  )
  expect_true(q[["long_link"]] <= 8L)
})

test_that("linking CmdStan diagnostics validator enforces canonical HMC fields", {
  diag_ok <- pairwiseLLM:::.adaptive_link_cmdstan_validate_diagnostics(
    diagnostics = list(divergences = 0L, max_rhat = 1.005, min_ess_bulk = 900),
    thresholds = list(divergences_max = 0L, max_rhat = 1.01, min_ess_bulk = 400)
  )
  expect_identical(diag_ok$divergences, 0L)
  expect_true(isTRUE(diag_ok$diagnostics_divergences_pass))
  expect_true(isTRUE(diag_ok$diagnostics_rhat_pass))
  expect_true(isTRUE(diag_ok$diagnostics_ess_pass))

  expect_error(
    pairwiseLLM:::.adaptive_link_cmdstan_validate_diagnostics(
      diagnostics = list(divergences = NA_integer_, max_rhat = 1.01, min_ess_bulk = 500),
      thresholds = list(divergences_max = 0L, max_rhat = 1.01, min_ess_bulk = 400)
    ),
    "missing or malformed"
  )
})

test_that("linking CmdStan schedule and refit seed are stable under fixed inputs", {
  sched1 <- pairwiseLLM:::.adaptive_link_cmdstan_schedule(1L, n_param = 1L, joint_used = FALSE)
  sched2 <- pairwiseLLM:::.adaptive_link_cmdstan_schedule(2L, n_param = 3L, joint_used = TRUE)
  expect_true(sched2$iter_sampling > sched1$iter_sampling)
  expect_true(sched2$iter_warmup > sched1$iter_warmup)
  edges <- tibble::tibble(step_id = c(NA_integer_, 2L), y_spoke = c(2L, 1L))
  seed1 <- pairwiseLLM:::.adaptive_link_refit_seed(edges, "shift_only", "shift_only")
  seed2 <- pairwiseLLM:::.adaptive_link_refit_seed(edges, "shift_only", "shift_only")
  expect_true(seed1 >= 1L)
  expect_identical(seed1, seed2)

  edges_large <- tibble::tibble(
    step_id = c(1e12, 1e15, 3e15 + 9),
    y_spoke = c(0L, 1L, 1L)
  )
  seed_large_a <- pairwiseLLM:::.adaptive_link_refit_seed(edges_large, "shift_scale", "joint_refit")
  seed_large_b <- pairwiseLLM:::.adaptive_link_refit_seed(edges_large, "shift_scale", "joint_refit")
  expect_true(is.finite(seed_large_a))
  expect_false(is.na(seed_large_a))
  expect_true(seed_large_a >= 1L)
  expect_identical(seed_large_a, seed_large_b)
})

test_that("linking refit retries CmdStan effort until diagnostics pass", {
  state <- make_linking_refit_state(list(link_refit_mode = "shift_only"))
  state$config$btl_config$cmdstan_fit_fn <- NULL
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "h2", "s22", 0L, spoke_id = 2L)

  sampled <- list()
  fit_calls <- 0L
  out <- testthat::with_mocked_bindings(
    .adaptive_link_fit_transform_cmdstan = function(stan_data,
                                                    variable_names,
                                                    cmdstan,
                                                    seed,
                                                    model_fn = NULL) {
      fit_calls <<- fit_calls + 1L
      sampled[[length(sampled) + 1L]] <<- list(
        chains = as.integer(cmdstan$chains),
        iter_warmup = as.integer(cmdstan$iter_warmup),
        iter_sampling = as.integer(cmdstan$iter_sampling)
      )
      diagnostics <- if (fit_calls < 3L) {
        list(divergences = 0L, max_rhat = 1.02, min_ess_bulk = 80)
      } else {
        list(divergences = 0L, max_rhat = 1.004, min_ess_bulk = 480)
      }
      list(
        draws_matrix = cbind(delta = c(0, 0, 0, 0)),
        diagnostics = diagnostics,
        mcmc_config_used = list(
          chains = as.integer(cmdstan$chains),
          parallel_chains = as.integer(cmdstan$chains),
          threads_per_chain = 1L,
          cmdstanr_version = "test"
        )
      )
    },
    .package = "pairwiseLLM",
    {
      pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
    }
  )

  stats <- out$controller$link_refit_stats_by_spoke[["2"]]
  expect_length(sampled, 3L)
  expect_true(sampled[[2L]]$iter_sampling > sampled[[1L]]$iter_sampling)
  expect_true(sampled[[3L]]$iter_sampling > sampled[[2L]]$iter_sampling)
  expect_identical(stats$link_diagnostics_divergences, 0L)
  expect_true(isTRUE(stats$link_diagnostics_divergences_pass))
  expect_true(isTRUE(stats$link_diagnostics_rhat_pass))
  expect_true(isTRUE(stats$link_diagnostics_ess_pass))
  expect_identical(stats$fit_contract$mcmc$repair_attempts, 3L)
  expect_identical(stats$link_fit_method, "cmdstan_hmc")
  expect_identical(stats$link_uncertainty_approximation, "cmdstan_posterior_draws")
})

test_that("committed result orientation remains Y=1 => A wins in refit inputs", {
  state <- make_linking_refit_state()
  state <- append_cross_step(state, 1L, "h1", "s21", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "h2", "s22", 0L, spoke_id = 2L)

  rows <- pairwiseLLM:::.adaptive_results_from_step_log(state)
  expect_identical(as.character(rows$better_id[[1L]]), as.character(rows$A_id[[1L]]))
  expect_identical(as.integer(rows$winner_pos[[1L]]), 1L)
  expect_identical(as.character(rows$better_id[[2L]]), as.character(rows$B_id[[2L]]))
  expect_identical(as.integer(rows$winner_pos[[2L]]), 2L)
})

test_that("linking lag domain metadata resets once per spoke domain and persists in stage rows", {
  state <- make_linking_refit_state()
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)

  s1 <- pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
  stats1 <- s1$controller$link_refit_stats_by_spoke[["2"]]
  expect_false(isTRUE(stats1$lag_domain_reset))
  expect_true(is.character(stats1$lag_domain_key))
  expect_true(nchar(stats1$lag_domain_key) > 0L)
  expect_false(isTRUE(stats1$lag_eligible))

  rows1 <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = s1,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row1 <- rows1[rows1$spoke_id == 2L, , drop = FALSE]
  expect_false(isTRUE(row1$lag_domain_reset[[1L]]))
  expect_identical(as.character(row1$lag_domain_key[[1L]]), stats1$lag_domain_key)

  s1$link_stage_log <- pairwiseLLM:::append_link_stage_log(s1$link_stage_log, rows1)
  s2 <- pairwiseLLM:::.adaptive_linking_refit_update_state(s1, list(last_refit_step = 1L))
  stats2 <- s2$controller$link_refit_stats_by_spoke[["2"]]
  expect_false(isTRUE(stats2$lag_domain_reset))
  expect_identical(stats2$lag_domain_key, stats1$lag_domain_key)
})

test_that("current refit summary cache reconciles exactly to canonical step-log counts", {
  state <- make_linking_refit_state()
  state <- append_cross_step(state, 1L, "h1", "s21", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "h2", "s22", 0L, spoke_id = 2L)
  state <- append_probe_step(state, 3L, "h3", "s21", 1L, spoke_id = 2L)
  state$step_log$round_stage[1:2] <- c("anchor_link", "mid_link")
  state$step_log$link_stage[1:2] <- c("anchor_link", "mid_link")
  state$refit_meta$last_refit_step <- 1L

  state <- pairwiseLLM:::.adaptive_link_refit_summary_rebuild_current(state)
  summary <- pairwiseLLM:::.adaptive_link_refit_summary_current(
    state = state,
    refit_id = pairwiseLLM:::.adaptive_link_refit_window_id(state),
    spoke_id = 2L,
    refit_context = list(last_refit_step = 1L),
    reconcile = TRUE
  )

  expect_identical(summary$n_pairs_cross_set_done, 3L)
  expect_identical(summary$n_pairs_cross_set_active_done, 2L)
  expect_identical(summary$n_pairs_cross_set_probe_done, 1L)
  expect_identical(summary$n_unique_cross_pairs_seen, 3L)
  expect_identical(summary$n_cross_edges_active_since_last_refit, 1L)
  expect_identical(summary$n_cross_edges_probe_since_last_refit, 1L)
  expect_identical(summary$n_cross_edges_total_since_last_refit, 2L)
  expect_false(isTRUE(summary$probe_panel_acceleration_used_since_last_refit))
  expect_identical(summary$stage_realized[["anchor_link"]], 0L)
  expect_identical(summary$stage_realized[["mid_link"]], 1L)
})

test_that("D-opt information state accumulates by refit window and logs audit fields", {
  state <- make_linking_refit_state()
  id_map <- stats::setNames(seq_along(state$item_ids), as.character(state$item_ids))
  step_row <- list(
    i = as.integer(id_map[["h1"]]),
    j = as.integer(id_map[["s21"]]),
    is_cross_set = TRUE,
    run_mode = "link_one_spoke",
    utility_mode = "linking_d_optimal_transform",
    link_spoke_id = 2L,
    is_probe_step = NA,
    delta_spoke_estimate_pre = 0,
    log_alpha_spoke_estimate_pre = NA_real_,
    link_transform_mode = "shift_only"
  )

  s1 <- pairwiseLLM:::.adaptive_link_d_opt_update_after_commit(
    state_before = state,
    state_after = state,
    step_row = step_row
  )
  d_opt_1 <- s1$controller$link_d_opt_it_by_spoke
  expect_true(length(d_opt_1) >= 1L)
  expect_true(any(grepl("^1::2$", names(d_opt_1))))
  entry1 <- d_opt_1[["1::2"]]
  expect_true(is.matrix(entry1$it))
  expect_identical(as.integer(entry1$it_n_pairs_accumulated), 1L)

  s1$round_log <- pairwiseLLM:::append_round_log(s1$round_log, list(refit_id = 1L, diagnostics_pass = TRUE))
  s2 <- pairwiseLLM:::.adaptive_link_d_opt_update_after_commit(
    state_before = s1,
    state_after = s1,
    step_row = step_row
  )
  d_opt_2 <- s2$controller$link_d_opt_it_by_spoke
  expect_true(any(grepl("^2::2$", names(d_opt_2))))
  expect_false(any(grepl("^1::", names(d_opt_2))))
  entry2 <- d_opt_2[["2::2"]]
  expect_identical(as.integer(entry2$it_n_pairs_accumulated), 1L)

  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = s1,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row <- rows[rows$spoke_id == 2L, , drop = FALSE]
  expect_true(nrow(row) == 1L)
  expect_true(is.finite(row$it_logdet_start[[1L]]))
  expect_true(is.finite(row$it_logdet_end[[1L]]))
  expect_true(is.finite(row$it_trace_end[[1L]]))
  expect_identical(as.integer(row$it_n_pairs_accumulated[[1L]]), 1L)
})

test_that("D-opt updater guard branches return state unchanged when prerequisites fail", {
  state <- make_linking_refit_state()
  id_map <- stats::setNames(seq_along(state$item_ids), as.character(state$item_ids))

  base_row <- list(
    i = as.integer(id_map[["h1"]]),
    j = as.integer(id_map[["s21"]]),
    is_cross_set = TRUE,
    run_mode = "link_one_spoke",
    utility_mode = "linking_d_optimal_transform",
    link_spoke_id = 2L,
    is_probe_step = NA,
    delta_spoke_estimate_pre = 0,
    log_alpha_spoke_estimate_pre = NA_real_,
    link_transform_mode = "shift_only"
  )

  bad_rows <- list(
    tibble::tibble(),
    utils::modifyList(base_row, list(is_cross_set = FALSE)),
    utils::modifyList(base_row, list(run_mode = "within_set")),
    utils::modifyList(base_row, list(is_probe_step = TRUE)),
    utils::modifyList(base_row, list(link_spoke_id = NA_integer_)),
    utils::modifyList(base_row, list(i = NA_integer_)),
    utils::modifyList(base_row, list(link_spoke_id = 3L))
  )
  for (row in bad_rows) {
    out <- pairwiseLLM:::.adaptive_link_d_opt_update_after_commit(
      state_before = state,
      state_after = state,
      step_row = row
    )
    expect_identical(out$controller$link_d_opt_it_by_spoke, state$controller$link_d_opt_it_by_spoke)
  }
})

test_that("anchored-joint utility and Fisher updates use the accepted state", {
  state <- make_linking_refit_state(
    list(
      run_mode = "link_multi_spoke",
      link_estimation_mode = "anchored_joint",
      hub_lock_mode = "hard_lock"
    )
  )
  state$linking$phase_a$artifacts[["1"]]$phase_a_within_set_evidence <- tibble::tibble(
    pair_id = 1L,
    step_id = 1L,
    A_item = "h1",
    B_item = "h2",
    y_A = 1L
  )
  state$linking$phase_a$artifacts[["2"]]$phase_a_within_set_evidence <- tibble::tibble(
    pair_id = 2L,
    step_id = 2L,
    A_item = "s21",
    B_item = "s22",
    y_A = 1L
  )
  state$linking$phase_a$artifacts[["1"]]$phase_a_within_set_evidence_hash <-
    pairwiseLLM:::.adaptive_phase_a_hash_object(state$linking$phase_a$artifacts[["1"]]$phase_a_within_set_evidence)
  state$linking$phase_a$artifacts[["2"]]$phase_a_within_set_evidence_hash <-
    pairwiseLLM:::.adaptive_phase_a_hash_object(state$linking$phase_a$artifacts[["2"]]$phase_a_within_set_evidence)
  controller <- pairwiseLLM:::.adaptive_controller_resolve(state)
  accepted <- pairwiseLLM:::.adaptive_link_anchored_joint_resolve_state(
    state = state,
    spoke_id = 2L,
    controller = controller
  )
  state$linking$anchored_joint$accepted_state_by_spoke[["2"]] <- accepted
  state$linking$anchored_joint$fisher_t0_by_spoke[["2"]] <- list(
    free_block_dim = length(accepted$theta_spoke_global_mean),
    I_s_t0_zero = TRUE,
    n_link_active_pairs = 0L,
    anchored_joint_init_state_method = accepted$anchored_joint_init_state_method
  )

  state$btl_fit$beta_mean <- 1.4
  state$btl_fit$epsilon_mean <- 0.35
  cand <- pairwiseLLM:::.adaptive_link_attach_predictive_utility(
    candidates = tibble::tibble(i = "h1", j = "s21"),
    state = state,
    controller = controller,
    spoke_id = 2L
  )

  expected_p <- pairwiseLLM:::.adaptive_link_model_d_prob(
    theta_a = accepted$theta_hub_fixed[["h1"]],
    theta_b = accepted$theta_spoke_global_mean[["s21"]],
    beta = accepted$judge_params$beta,
    epsilon = accepted$judge_params$epsilon
  )
  expect_equal(cand$link_p[[1L]], expected_p, tolerance = 1e-8)
  expect_true(is.finite(cand$link_d_opt_gain[[1L]]))

  id_map <- stats::setNames(seq_along(state$item_ids), as.character(state$item_ids))
  updated <- pairwiseLLM:::.adaptive_link_d_opt_update_after_commit(
    state_before = state,
    state_after = state,
    step_row = list(
      i = as.integer(id_map[["h1"]]),
      j = as.integer(id_map[["s21"]]),
      A = as.integer(id_map[["h1"]]),
      B = as.integer(id_map[["s21"]]),
      is_cross_set = TRUE,
      run_mode = "link_one_spoke",
      utility_mode = "linking_d_optimal_anchored_joint",
      link_spoke_id = 2L,
      is_probe_step = FALSE
    )
  )
  d_opt_entry <- updated$controller$link_d_opt_it_by_spoke[[paste0("1::2")]]
  expect_identical(dim(d_opt_entry$it), c(2L, 2L))
  expect_identical(d_opt_entry$it_n_pairs_accumulated, 1L)
})

test_that("anchored-joint fit keeps the hub fixed and records prior-SD fallback", {
  state <- make_linking_refit_state(
    list(
      run_mode = "link_multi_spoke",
      link_estimation_mode = "anchored_joint",
      hub_lock_mode = "hard_lock"
    )
  )
  state$linking$phase_a$artifacts[["1"]]$phase_a_within_set_evidence <- tibble::tibble(
    pair_id = 1L,
    step_id = 1L,
    A_item = "h1",
    B_item = "h2",
    y_A = 1L
  )
  state$linking$phase_a$artifacts[["2"]]$phase_a_within_set_evidence <- tibble::tibble(
    pair_id = 2L,
    step_id = 2L,
    A_item = "s21",
    B_item = "s22",
    y_A = 1L
  )
  state$linking$phase_a$artifacts[["1"]]$phase_a_within_set_evidence_hash <-
    pairwiseLLM:::.adaptive_phase_a_hash_object(state$linking$phase_a$artifacts[["1"]]$phase_a_within_set_evidence)
  state$linking$phase_a$artifacts[["2"]]$phase_a_within_set_evidence_hash <-
    pairwiseLLM:::.adaptive_phase_a_hash_object(state$linking$phase_a$artifacts[["2"]]$phase_a_within_set_evidence)
  state$linking$phase_a$artifacts[["2"]]$items$theta_raw_sd[[1L]] <- NA_real_

  controller <- pairwiseLLM:::.adaptive_controller_resolve(state)
  accepted <- pairwiseLLM:::.adaptive_link_anchored_joint_resolve_state(
    state = state,
    spoke_id = 2L,
    controller = controller
  )
  fit <- NULL
  expect_warning(
    fit <- pairwiseLLM:::.adaptive_link_fit_anchored_joint(
      state = state,
      spoke_id = 2L,
      controller = controller,
      cross_edges = tibble::tibble(
        hub_item = "h1",
        spoke_item = "s21",
        y_spoke = 1L,
        step_id = 3L,
        spoke_in_A = TRUE,
        run_mode = "link_one_spoke",
        is_probe_step = FALSE
      ),
      judge_params = accepted$judge_params,
      accepted_state = accepted
    ),
    "Anchored-joint spoke prior SD fallback applied"
  )

  expect_equal(fit$theta_hub_post, accepted$theta_hub_fixed, tolerance = 1e-8)
  expect_true(all(is.finite(fit$theta_spoke_post)))
  expect_identical(fit$fit_contract$estimation_method, "map_laplace")
  expect_identical(fit$fit_contract$uncertainty_approximation, "laplace_hessian")
  expect_true(isTRUE(fit$fit_contract$priors$prior_sd_fallback_used))
  expect_true("s21" %in% fit$fit_contract$priors$prior_sd_fallback_items)
})
