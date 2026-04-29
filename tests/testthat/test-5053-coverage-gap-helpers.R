make_link_probe_state <- function() {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs31", "gs32")
  )
  state <- pairwiseLLM::adaptive_rank_start(
    items,
    seed = 101L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      link_estimation_mode = "transform",
      hub_lock_mode = "soft_lock"
    )
  )
  state$controller$probe_pairs_per_refit_per_spoke <- 0L

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
  colnames(draws) <- state$item_ids
  state$btl_fit <- make_test_btl_fit(state$item_ids, draws = draws, model_variant = "btl_e_b")

  make_artifact <- function(set_id, ids, theta) {
    list(
      set_id = as.integer(set_id),
      diagnostics = list(
        diagnostics_pass = TRUE,
        reliability_EAP_within = 0.95
      ),
      n_pairs_committed = 4L,
      quality_gate_accepted = TRUE,
      items = tibble::tibble(
        global_item_id = ids,
        theta_raw_mean = as.double(theta),
        theta_raw_sd = rep(0.15, length(ids)),
        rank_mu_raw = seq_along(ids)
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
      `1` = make_artifact(1L, c("gh1", "gh2", "gh3"), c(0.80, 0.40, 0.10)),
      `2` = make_artifact(2L, c("gs21", "gs22"), c(-0.30, -0.60)),
      `3` = make_artifact(3L, c("gs31", "gs32"), c(0.20, -0.10))
    ),
    ready_for_phase_b = TRUE,
    phase = "phase_b"
  )
  state$controller$current_link_spoke_id <- 2L
  state$controller$link_epoch_id_by_spoke <- list(`2` = 3L, `3` = 1L)
  state$controller$link_transform_state_by_spoke <- list(`2` = "shift_only")
  state$controller$link_transform_state_by_spoke <- list(`2` = "shift_only")
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      delta_spoke_mean = 0.25,
      log_alpha_spoke_mean = NA_real_,
      link_epoch_id = 3L
    )
  )
  state
}

append_cross_probe_step <- function(state,
                                    step_id,
                                    A_id,
                                    B_id,
                                    Y,
                                    spoke_id,
                                    is_probe_step = TRUE,
                                    run_mode = "link_probe_holdout") {
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
      run_mode = as.character(run_mode),
      is_probe_step = is_probe_step
    )
  )
  state
}

make_legacy_resume_state <- function() {
  ids <- c("A", "B")
  z <- stats::setNames(c(0L, 0L), ids)
  structure(
    list(
      schema_version = 1L,
      ids = ids,
      N = 2L,
      texts = stats::setNames(c("a", "b"), ids),
      fit = NULL,
      deg = z,
      pos1 = z,
      pos2 = z,
      imb = z,
      pos_count = z,
      unordered_count = stats::setNames(integer(), character()),
      pair_count = stats::setNames(integer(), character()),
      pair_ordered_count = stats::setNames(integer(), character()),
      ordered_seen = stats::setNames(logical(), character()),
      history_pairs = pairwiseLLM:::.adaptive_empty_pairs_tbl(),
      history_results = pairwiseLLM:::.adaptive_empty_results_tbl(),
      failed_attempts = pairwiseLLM:::.adaptive_empty_failed_attempts_tbl(),
      results_seen = stats::setNames(logical(), character()),
      comparisons_scheduled = 0L,
      comparisons_observed = 0L,
      phase = "phase2",
      iter = 0L,
      budget_max = 10L,
      M1_target = 2L,
      last_check_at = 0L,
      new_since_refit = 0L,
      last_refit_at = 0L,
      posterior = list(U_dup_threshold = 0.2),
      mode = "warm_start",
      repair_attempts = 0L,
      stop_reason = NULL
    ),
    class = "adaptive_state"
  )
}

make_local_fit_contract <- function() {
  theta <- matrix(c(0.1, 0.2, 0.4, 0.5), nrow = 2L)
  colnames(theta) <- c("A", "B")
  pairwiseLLM:::build_btl_fit_contract(
    theta_draws = theta,
    epsilon_draws = c(0.1, 0.2),
    beta_draws = c(-0.1, 0.1),
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 1000),
    model_variant = "btl_e_b",
    diagnostics_pass = TRUE,
    mcmc_config_used = list(chains = 2L)
  )
}

test_that("build_btl_results_data and fit-contract helpers cover empty and normalized branches", {
  empty <- pairwiseLLM::build_btl_results_data(
    tibble::tibble(ID1 = character(), ID2 = character(), better_id = character()),
    phase = "phase1",
    backend = "offline",
    model = "fixture"
  )
  expect_identical(nrow(empty), 0L)
  expect_s3_class(empty$received_at, "POSIXct")

  theta <- matrix(c(0.1, 0.2, 0.4, 0.5), nrow = 2L)
  colnames(theta) <- c("A", "B")
  fit <- pairwiseLLM:::build_btl_fit_contract(
    theta_draws = theta,
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 500),
    diagnostics_pass = TRUE,
    inference_contract = list(
      judge_param_mode = "phase_specific",
      phase_levels = c("phase2", "phase3", ""),
      judge_scope_levels = c("within", "link", "within"),
      phase_boundary_detected = FALSE
    ),
    mcmc_config_used = list(chains = 2L)
  )

  expect_identical(fit$inference_contract$judge_param_mode, "phase_specific")
  expect_identical(fit$inference_contract$phase_levels, c("phase2", "phase3"))
  expect_identical(fit$inference_contract$judge_scope_levels, c("within", "link"))
  expect_false(fit$inference_contract$phase_boundary_detected)
  expect_true(is.na(pairwiseLLM:::.btl_contract_diagnostics_pass(NULL)))
  expect_error(
    pairwiseLLM:::.btl_contract_inference_contract(list(judge_scope_levels = "bad")),
    "judge_scope_levels"
  )

  inferred <- pairwiseLLM:::.btl_mcmc_inference_contract_from_results(
    tibble::tibble(
      phase = c("phase2", "phase3"),
      judge_scope = c("within", "bad")
    )
  )
  expect_identical(inferred$judge_param_mode, "phase_specific")
  expect_identical(inferred$judge_scope_levels, "within")

  expect_error(
    pairwiseLLM::build_btl_results_data(
      tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
      backend = "",
      model = "fixture"
    ),
    "backend"
  )
})

test_that("current runtime probe helpers continue to emit holdout rather than legacy link_probe", {
  state <- make_link_probe_state()
  state$linking$probe$panels_by_spoke <- list(
    `2` = pairwiseLLM:::.adaptive_link_probe_construct_panel(
      state,
      state$controller,
      spoke_id = 2L
    )
  )

  holdout <- pairwiseLLM:::.adaptive_link_probe_select_holdout(
    state,
    step_id = 11L,
    spoke_id = 2L
  )

  expect_identical(holdout$run_mode, "link_probe_holdout")
  expect_false(identical(holdout$run_mode, "link_probe"))
})

test_that("btl contract and config helpers cover current defaults and round-log fallbacks", {
  cfg <- pairwiseLLM:::btl_mcmc_config(
    4L,
    list(cmdstan = list(output_dir = tempdir()), target_mean_degree = 2)
  )
  expect_identical(cfg$cmdstan$output_dir, tempdir())

  state <- make_legacy_resume_state()
  state$posterior$stop_metrics <- list(
    scheduled_pairs = 0L,
    completed_pairs = 0L,
    proposed_pairs = 0L
  )
  state$posterior$mcmc_config_used <- list(chains = 4L, parallel_chains = 2L)
  state$config <- list(
    mcmc = cfg,
    round_log = tibble::tibble(iter_at_refit = integer())
  )
  state$batch_log <- tibble::tibble()

  row <- pairwiseLLM:::build_round_log_row(
    state = state,
    fit = NULL,
    metrics = list(scheduled_pairs = 0L, completed_pairs = 0L, proposed_pairs = 0L),
    stop_out = list(stop_decision = FALSE, stop_reason = "none"),
    config = cfg,
    new_pairs = 0L
  )

  expect_identical(row$round_id[[1L]], 1L)
  expect_true(is.na(row$reliability_EAP[[1L]]))

  item_log <- pairwiseLLM:::build_item_log(state, fit = NULL)
  expect_identical(nrow(item_log), 0L)
})

test_that("adaptive schema validators cover fit-backed legacy state and canonical guards", {
  legacy <- make_legacy_resume_state()
  legacy$fit <- make_local_fit_contract()
  expect_no_error(pairwiseLLM:::validate_btl_mcmc_state(legacy))

  state <- pairwiseLLM::adaptive_rank_start(make_test_items(3), seed = 11L)
  expect_no_error(pairwiseLLM:::validate_state(state))

  bad_link_stage <- state
  bad_link_stage$link_stage_log <- 1L
  expect_error(pairwiseLLM:::validate_state(bad_link_stage), "missing required columns")

  bad_items <- state
  bad_items$items$item_id <- 1:3
  expect_error(pairwiseLLM:::validate_state(bad_items), "items\\$item_id")

  bad_set_id <- state
  bad_set_id$items$set_id <- c("1", "1", "1")
  expect_error(pairwiseLLM:::validate_state(bad_set_id), "items\\$set_id")

  bad_global <- state
  bad_global$items$global_item_id[[1L]] <- ""
  expect_error(pairwiseLLM:::validate_state(bad_global), "global_item_id")

  one_spoke <- pairwiseLLM::adaptive_rank_start(
    tibble::tibble(
      item_id = c("h1", "h2", "s1", "s2", "t1", "t2"),
      text = c("h1", "h2", "s1", "s2", "t1", "t2"),
      set_id = c(1L, 1L, 2L, 2L, 3L, 3L),
      global_item_id = paste0("g", 1:6)
    ),
    seed = 1L
  )
  one_spoke$linking$run_mode <- "link_one_spoke"
  one_spoke$linking$hub_id <- 1L
  expect_error(pairwiseLLM:::validate_state(one_spoke), "exactly one spoke")
})

test_that("resume schema alignment migrates legacy transform columns and typed missing fields", {
  step_schema <- pairwiseLLM:::schema_step_log
  step_tbl <- tibble::tibble(
    posterior_win_prob_pre = 0.8,
    link_transform_mode = "shift_scale"
  )
  aligned_step <- pairwiseLLM:::.adaptive_align_log_schema_for_resume(
    step_tbl,
    step_schema,
    "step_log"
  )

  expect_true("posterior_win_prob_ij_pre" %in% names(aligned_step))
  expect_identical(aligned_step$link_transform_policy[[1L]], "fixed_shift_scale")
  expect_identical(aligned_step$link_transform_state[[1L]], "shift_scale")
  expect_false("link_transform_mode" %in% names(aligned_step))

  stage_tbl <- tibble::tibble(link_transform_mode = c("shift_only", NA_character_))
  aligned_stage <- pairwiseLLM:::.adaptive_align_log_schema_for_resume(
    stage_tbl,
    pairwiseLLM:::schema_link_stage_log,
    "link_stage_log",
    fill_missing = FALSE
  )
  expect_identical(aligned_stage$link_transform_policy[[1L]], "fixed_shift_only")
  expect_true(is.na(aligned_stage$link_transform_state[[2L]]))

  expect_error(
    pairwiseLLM:::.adaptive_align_log_schema_for_resume(1L, step_schema, "step_log"),
    "must be a data frame"
  )
})

test_that("persistence helpers cover current and legacy item-log resume schemas", {
  current_schema <- pairwiseLLM:::.adaptive_item_log_current_schema()
  current <- tibble::as_tibble(
    lapply(current_schema, function(type) {
      pairwiseLLM:::.adaptive_schema_typed_na(type)
    })
  )[0, ]
  names(current) <- names(current_schema)
  expect_no_error(
    pairwiseLLM:::.adaptive_validate_item_log_resume_schema(current, "item_log[[1]]")
  )

  legacy <- tibble::tibble(
    refit_id = 1L,
    item_id = "A",
    theta_mean = 0,
    `theta_p2.5` = 0,
    `theta_p5` = 0,
    `theta_p50` = 0,
    `theta_p95` = 0,
    `theta_p97.5` = 0,
    theta_sd = 0.1,
    rank_mean = 1,
    degree = 0L,
    pos_count_A = 0L,
    pos_count_B = 0L
  )
  expect_no_error(
    pairwiseLLM:::.adaptive_validate_item_log_resume_schema(legacy, "item_log[[2]]")
  )

  bad <- legacy
  bad$degree <- 0
  expect_error(
    pairwiseLLM:::.adaptive_validate_item_log_resume_schema(bad, "item_log[[3]]"),
    "supported item log schema"
  )
})

test_that("phase A helpers cover stop-pass sources, pending runs, and config surface variants", {
  state <- make_link_probe_state()

  expect_true(pairwiseLLM:::.adaptive_phase_a_set_stop_passed(
    artifact = list(
      diagnostics = list(diagnostics_pass = TRUE, reliability_EAP_within = 0.95),
      n_pairs_committed = 2L
    ),
    source = "run",
    controller = state$controller
  ))
  expect_true(pairwiseLLM:::.adaptive_phase_a_set_stop_passed(
    artifact = list(quality_gate_accepted = TRUE),
    source = "import",
    controller = state$controller
  ))
  expect_false(pairwiseLLM:::.adaptive_phase_a_set_stop_passed(NULL, "import", state$controller))

  state$linking$phase_a$set_status$status[[2L]] <- "pending"
  expect_identical(pairwiseLLM:::.adaptive_phase_a_pending_run_sets(state), 2L)

  state$linking$phase_a$set_status$status[[2L]] <- "ready"
  state$linking$phase_a$phase <- "phase_b"
  state$linking$phase_a$ready_spokes <- integer()
  ctx <- pairwiseLLM:::.adaptive_link_phase_context(state, controller = state$controller)
  expect_identical(ctx$phase, "phase_b")
  expect_true(all(c(2L, 3L) %in% ctx$ready_spokes))

  hard_lock <- pairwiseLLM:::.adaptive_apply_controller_config(
    state,
    adaptive_config = list(hub_lock_mode = "hard_lock", hub_lock_kappa = 0.4)
  )
  surface <- pairwiseLLM:::.adaptive_phase_a_required_config_surface(hard_lock, set_id = 2L)
  expect_identical(names(surface), c("judge_param_mode", "model_variant"))
  expect_false("hub_lock_kappa" %in% names(surface))

  expect_null(pairwiseLLM:::.adaptive_phase_a_latest_refit_row(state, set_id = 99L))
})

test_that("probe helpers cover selection, commit registration, caching, and stop budget branches", {
  state <- make_link_probe_state()
  panel <- tibble::tibble(
    probe_panel_id = c("panel", "panel"),
    link_epoch_id = c(3L, 3L),
    spoke_id = c(2L, 2L),
    hub_item_id = c("h2", "h1"),
    spoke_item_id = c("s22", "s21"),
    spoke_bin = c(2L, 1L),
    hub_bin = c(1L, 1L),
    planned_rank = c(2L, 1L),
    pair_key = c("h2:s22", "h1:s21"),
    realized = c(TRUE, FALSE),
    realized_step_id = c(10L, NA_integer_),
    realized_pair_id = c(10L, NA_integer_),
    realized_run_mode = c("link_probe_holdout", NA_character_)
  )
  state$linking$probe <- list(
    panels_by_spoke = list(`2` = panel),
    prediction_cache = pairwiseLLM:::.adaptive_link_probe_empty_cache(),
    realized_edges = tibble::tibble(
      step_id = 10L,
      pair_id = 10L,
      run_mode = "link_probe_holdout",
      spoke_id = 2L,
      link_epoch_id = 3L,
      probe_panel_id = "panel",
      hub_item_id = "h2",
      spoke_item_id = "s22",
      pair_key = "h2:s22",
      Y = 0L
    ),
    collect_holdout_now_by_spoke = list(`2` = TRUE)
  )
  state <- append_cross_probe_step(
    state,
    step_id = 10L,
    A_id = "h2",
    B_id = "s22",
    Y = 0L,
    spoke_id = 2L
  )

  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_panel_id(panel),
    pairwiseLLM:::.adaptive_link_probe_panel_id(panel)
  )
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_panel_id(panel),
    pairwiseLLM:::.adaptive_link_probe_panel_id(panel[c(2L, 1L), , drop = FALSE])
  )
  expect_identical(pairwiseLLM:::.adaptive_link_probe_realized_count(state, 2L, epoch_id = 3L), 1L)
  next_pair <- pairwiseLLM:::.adaptive_link_probe_next_pair(state, 2L, epoch_id = 3L)
  expect_identical(next_pair$hub_item_id[[1L]], "h1")

  holdout <- pairwiseLLM:::.adaptive_link_probe_select_holdout(state, step_id = 11L, spoke_id = 2L)
  expect_identical(holdout$run_mode, "link_probe_holdout")
  expect_identical(holdout$link_epoch_id_selected, 3L)

  committed <- pairwiseLLM:::.adaptive_link_probe_register_commit(
    state,
    tibble::tibble(
      step_id = 11L,
      pair_id = 11L,
      A = match("h1", state$item_ids),
      B = match("s21", state$item_ids),
      Y = 1L,
      run_mode = "link_probe_holdout",
      link_spoke_id = 2L,
      is_probe_step = TRUE
    )
  )
  updated_panel <- committed$linking$probe$panels_by_spoke[["2"]]
  expect_true(updated_panel$realized[[2L]])

  cached <- pairwiseLLM:::.adaptive_link_probe_cache_predictions(committed, refit_id = 2L, spoke_id = 2L)
  expect_true(nrow(cached$linking$probe$prediction_cache) >= 1L)

  boot <- pairwiseLLM:::.adaptive_stop_boundary_bootstrap(
    utils::modifyList(
      committed,
      list(meta = list(stop_boundary_step_id = 9L, pairs_committed_after_stop = -1L))
    )
  )
  expect_identical(boot$meta$pairs_committed_after_stop, 1L)

  status <- pairwiseLLM:::.adaptive_stop_boundary_budget_status(
    boot,
    controller = list(max_pairs_after_stop = -2L)
  )
  expect_identical(status$max_pairs_after_stop, 0L)
})

test_that("candidate helpers cover probe panels, selection metadata, and backfill ordering", {
  state <- make_link_probe_state()

  empty_panel <- pairwiseLLM:::.adaptive_link_probe_construct_panel(
    state = state,
    controller = utils::modifyList(state$controller, list(hub_id = 99L)),
    spoke_id = 2L
  )
  expect_identical(nrow(empty_panel), 0L)

  state$linking$probe <- list(
    panels_by_spoke = list(),
    prediction_cache = pairwiseLLM:::.adaptive_link_probe_empty_cache(),
    realized_edges = pairwiseLLM:::.adaptive_link_probe_empty_realized_log(),
    collect_holdout_now_by_spoke = list()
  )
  ensured <- pairwiseLLM:::.adaptive_link_probe_ensure_panels(state, controller = state$controller, spoke_ids = 2L)
  expect_true(is.data.frame(ensured$linking$probe$panels_by_spoke[["2"]]))
  expect_true(nrow(ensured$linking$probe$panels_by_spoke[["2"]]) >= 1L)
  ensured$refit_meta$refit_pairs_target_current <- 3L
  ensured$controller$refit_pairs_target <- 3L
  ensured$controller$probe_pairs_per_refit_per_spoke <- 1L

  next_spoke <- pairwiseLLM:::.adaptive_link_probe_next_holdout_spoke(
    ensured,
    controller = ensured$controller,
    eligible_spoke_ids = 2L
  )
  expect_identical(next_spoke, 2L)

  ensured$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    pairwiseLLM:::new_link_stage_log(),
    list(
      refit_id = 1L,
      spoke_id = 2L,
      hub_id = 1L,
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )
  next_spoke <- pairwiseLLM:::.adaptive_link_probe_next_holdout_spoke(
    ensured,
    controller = ensured$controller,
    eligible_spoke_ids = 2L
  )
  expect_identical(next_spoke, 2L)

  expect_error(
    pairwiseLLM:::.adaptive_link_probe_ensure_panels(
      state,
      controller = utils::modifyList(state$controller, list(hub_id = 99L)),
      spoke_ids = 2L
    ),
    "no held-out panel could be constructed"
  )

  meta_empty <- pairwiseLLM:::.adaptive_selected_coverage_meta(tibble::tibble())
  expect_true(is.na(meta_empty$coverage_bins_used))

  meta_filled <- pairwiseLLM:::.adaptive_selected_coverage_meta(tibble::tibble(
    coverage_bins_used = 4L,
    coverage_source = "linking_global_score",
    link_spoke_id = 2L
  ))
  expect_identical(meta_filled$coverage_source, "linking_global_score")

  cand <- tibble::tibble(
    i = c("h2", "h1"),
    j = c("s22", "s21"),
    link_stage = c("mid_link", "anchor_link"),
    link_d_opt_gain = c(0.1, 0.1)
  )
  set_map <- stats::setNames(state$items$set_id, state$items$item_id)
  ord <- pairwiseLLM:::.adaptive_link_backfill_order(cand, hub_id = 1L, set_map = set_map)
  expect_identical(ord, c(2L, 1L))

  empty_pool <- pairwiseLLM:::.adaptive_link_candidate_pool(
    state = state,
    controller = state$controller,
    spoke_id = NA_integer_
  )
  expect_identical(nrow(empty_pool), 0L)
})

test_that("probe effort plan accelerates deterministically for identified probe-blocked spokes", {
  state <- make_link_probe_state()
  state$refit_meta$last_refit_step <- 10L
  state$refit_meta$refit_pairs_target_current <- 6L
  state$controller$refit_pairs_target <- 6L
  state$controller$probe_pairs_per_refit_per_spoke <- 1L
  state$controller$probe_edges_min_for_stop <- 3L
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_identified = TRUE,
      link_stop_eligible = FALSE,
      link_epoch_id = 3L
    )
  )
  state$linking$probe <- list(
    panels_by_spoke = list(
      `2` = tibble::tibble(
        probe_panel_id = "panel-2",
        link_epoch_id = 3L,
        spoke_id = 2L,
        hub_item_id = c("h1", "h2", "h3"),
        spoke_item_id = c("s21", "s21", "s22"),
        spoke_bin = c(1L, 1L, 2L),
        hub_bin = c(1L, 2L, 3L),
        planned_rank = c(1L, 2L, 3L),
        pair_key = make_unordered_key(c("h1", "h2", "h3"), c("s21", "s21", "s22")),
        realized = c(FALSE, FALSE, FALSE),
        realized_step_id = c(NA_integer_, NA_integer_, NA_integer_),
        realized_pair_id = c(NA_integer_, NA_integer_, NA_integer_),
        realized_run_mode = c(NA_character_, NA_character_, NA_character_)
      )
    ),
    prediction_cache = pairwiseLLM:::.adaptive_link_probe_empty_cache(),
    realized_edges = pairwiseLLM:::.adaptive_link_probe_empty_realized_log(),
    collect_holdout_now_by_spoke = list()
  )
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    pairwiseLLM:::new_link_stage_log(),
    list(
      refit_id = 1L,
      spoke_id = 2L,
      hub_id = 1L,
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      linking_identified = TRUE,
      link_stop_eligible = FALSE,
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )

  plan0 <- pairwiseLLM:::.adaptive_link_probe_effort_plan(
    state = state,
    controller = state$controller,
    spoke_id = 2L
  )
  expect_identical(plan0$base_cap, 1L)
  expect_identical(plan0$effective_cap, 1L)
  expect_false(isTRUE(plan0$acceleration_used))

  step1 <- append_cross_probe_step(state, 11L, "h1", "s21", 1L, 2L)
  step1 <- pairwiseLLM:::.adaptive_link_probe_register_commit(
    step1,
    tibble::tibble(
      step_id = 11L,
      pair_id = 11L,
      A = match("h1", step1$item_ids),
      B = match("s21", step1$item_ids),
      Y = 1L,
      run_mode = "link_probe_holdout",
      link_spoke_id = 2L,
      is_probe_step = TRUE
    )
  )
  plan1 <- pairwiseLLM:::.adaptive_link_probe_effort_plan(
    state = step1,
    controller = step1$controller,
    spoke_id = 2L
  )
  expect_identical(plan1$effective_cap, 1L)
  expect_identical(plan1$realized_refit, 1L)
  expect_true(is.na(pairwiseLLM:::.adaptive_link_probe_next_holdout_spoke(
    step1,
    controller = step1$controller,
    eligible_spoke_ids = 2L
  )))
})

test_that("probe effort plan opens active-floor routing only after floor and anchor progress", {
  append_active_step <- function(state, step_id, A_id, B_id, spoke_id, stage_name) {
    out <- append_cross_probe_step(
      state = state,
      step_id = step_id,
      A_id = A_id,
      B_id = B_id,
      Y = 1L,
      spoke_id = spoke_id,
      is_probe_step = FALSE,
      run_mode = "link_multi_spoke"
    )
    idx <- nrow(out$step_log)
    out$step_log$round_stage[[idx]] <- as.character(stage_name)
    out$step_log$link_stage[[idx]] <- as.character(stage_name)
    out
  }

  state <- make_link_probe_state()
  state$controller$probe_pairs_per_refit_per_spoke <- 2L
  state$controller$probe_pairs_per_refit_per_spoke_bootstrap_max <- 6L
  state$controller$probe_accel_bootstrap_target <- 12L
  state$controller$probe_active_floor_frac <- 0.5
  state$controller$probe_active_floor_min <- 2L
  state$controller$probe_active_floor_requires_anchor_progress <- TRUE
  state$controller$link_budget_refit_id <- pairwiseLLM:::.adaptive_link_refit_window_id(state)
  state$controller$link_budget_map <- list(
    `2` = list(
      B_spoke_refit_budget = 4L,
      B_spoke_refit_budget_source = "single_spoke_controller"
    )
  )

  plan0 <- pairwiseLLM:::.adaptive_link_probe_effort_plan(
    state = state,
    controller = state$controller,
    spoke_id = 2L
  )
  expect_identical(plan0$acceleration_mode_used, "active_floor_plus_sole_blocker")
  expect_identical(plan0$active_floor_used, 2L)
  expect_false(isTRUE(plan0$allow_when_active))
  expect_identical(plan0$effective_cap, 2L)

  state_no_anchor <- append_active_step(state, 11L, "h1", "s21", 2L, "long_link")
  state_no_anchor <- append_active_step(state_no_anchor, 12L, "h2", "s22", 2L, "long_link")
  plan1 <- pairwiseLLM:::.adaptive_link_probe_effort_plan(
    state = state_no_anchor,
    controller = state_no_anchor$controller,
    spoke_id = 2L
  )
  expect_true(isTRUE(plan1$active_floor_met))
  expect_false(isTRUE(plan1$anchor_progress_met))
  expect_false(isTRUE(plan1$allow_when_active))
  expect_false(isTRUE(plan1$acceleration_used))
  expect_identical(plan1$effective_cap, 2L)

  state_with_anchor <- append_active_step(state, 21L, "h1", "s21", 2L, "anchor_link")
  state_with_anchor <- append_active_step(state_with_anchor, 22L, "h2", "s22", 2L, "long_link")
  plan2 <- pairwiseLLM:::.adaptive_link_probe_effort_plan(
    state = state_with_anchor,
    controller = state_with_anchor$controller,
    spoke_id = 2L
  )
  expect_true(isTRUE(plan2$anchor_progress_met))
  expect_true(isTRUE(plan2$allow_when_active))
  expect_true(isTRUE(plan2$acceleration_used))
  expect_identical(plan2$effective_cap, 6L)
  expect_identical(pairwiseLLM:::.adaptive_link_probe_released_cap_when_active(plan2), 1L)

  state_with_anchor_more <- append_active_step(state_with_anchor, 23L, "h3", "s21", 2L, "mid_link")
  plan3 <- pairwiseLLM:::.adaptive_link_probe_effort_plan(
    state = state_with_anchor_more,
    controller = state_with_anchor_more$controller,
    spoke_id = 2L
  )
  expect_identical(pairwiseLLM:::.adaptive_link_probe_released_cap_when_active(plan3), 2L)
})

test_that("probe effort plan treats canonical anchor-stage exhaustion as anchor progress", {
  append_active_step <- function(state, step_id, A_id, B_id, spoke_id, stage_name) {
    out <- append_cross_probe_step(
      state = state,
      step_id = step_id,
      A_id = A_id,
      B_id = B_id,
      Y = 1L,
      spoke_id = spoke_id,
      is_probe_step = FALSE,
      run_mode = "link_multi_spoke"
    )
    idx <- nrow(out$step_log)
    out$step_log$round_stage[[idx]] <- as.character(stage_name)
    out$step_log$link_stage[[idx]] <- as.character(stage_name)
    out
  }

  state <- make_link_probe_state()
  state$controller$probe_pairs_per_refit_per_spoke <- 2L
  state$controller$probe_pairs_per_refit_per_spoke_bootstrap_max <- 6L
  state$controller$probe_accel_bootstrap_target <- 12L
  state$controller$probe_active_floor_frac <- 0.5
  state$controller$probe_active_floor_min <- 1L
  state$controller$probe_active_floor_requires_anchor_progress <- TRUE
  state$controller$link_budget_refit_id <- pairwiseLLM:::.adaptive_link_refit_window_id(state)
  state$controller$link_budget_map <- list(
    `2` = list(
      B_spoke_refit_budget = 2L,
      B_spoke_refit_budget_source = "single_spoke_controller"
    )
  )
  state$refit_meta$link_stage_exhausted_by_refit_spoke <- list(
    `1::2` = list(anchor_link = TRUE)
  )
  state <- append_active_step(state, 31L, "h1", "s21", 2L, "long_link")

  plan <- pairwiseLLM:::.adaptive_link_probe_effort_plan(
    state = state,
    controller = state$controller,
    spoke_id = 2L
  )
  expect_true(isTRUE(plan$active_floor_met))
  expect_true(isTRUE(plan$anchor_progress_met))
  expect_true(isTRUE(plan$allow_when_active))
  expect_true(isTRUE(plan$acceleration_used))
  expect_identical(plan$effective_cap, 6L)
})

test_that("Phase B refit target scales for concurrent probe-active floors", {
  items <- tibble::tibble(
    item_id = c(
      paste0("h", seq_len(10L)),
      paste0("s2", seq_len(5L)),
      paste0("s3", seq_len(5L))
    ),
    set_id = c(rep(1L, 10L), rep(2L, 5L), rep(3L, 5L)),
    global_item_id = item_id
  )
  state <- pairwiseLLM::adaptive_rank_start(
    items,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      multi_spoke_mode = "concurrent",
      hub_lock_mode = "soft_lock"
    )
  )
  state$linking$phase_a <- list(
    phase = "phase_b",
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    ready_spokes = c(2L, 3L),
    required_sets = c(1L, 2L, 3L),
    set_stop_pass_by_set = list(`1` = TRUE, `2` = TRUE, `3` = TRUE),
    phase_b_started_at_step = 1L
  )
  state$controller <- pairwiseLLM:::.adaptive_controller_with_phase_scope(
    state,
    controller = pairwiseLLM:::.adaptive_controller_resolve(state)
  )

  expect_identical(
    pairwiseLLM:::.adaptive_refit_pairs_target(state, list(refit_pairs_target = 30L)),
    44L
  )

  state$controller$multi_spoke_mode <- "independent"
  expect_identical(
    pairwiseLLM:::.adaptive_refit_pairs_target(state, list(refit_pairs_target = 30L)),
    30L
  )
})

make_probe_blocker_surface <- function(stop_blocker_codes,
                                       probe_edges_min_for_stop_used = 6L,
                                       hub_anchored = TRUE,
                                       reliability_link_global = 0.95,
                                       probe_brier = 0.10,
                                       probe_pred_rmse_lagged = 0.01,
                                       theta_global_rmse_lagged = 0.04) {
  tibble::tibble(
    link_lag_eligible = TRUE,
    link_min_refit_eligible = TRUE,
    link_diagnostics_pass = TRUE,
    reliability_link_global = reliability_link_global,
    link_stop_reliability_min_used = 0.90,
    probe_brier = probe_brier,
    probe_brier_max_used = 0.19,
    probe_pred_rmse_lagged = probe_pred_rmse_lagged,
    probe_pred_rmse_max_used = 0.015,
    theta_global_rmse_lagged = theta_global_rmse_lagged,
    theta_global_rmse_max_used = 0.05,
    hub_anchored = hub_anchored,
    probe_edges_min_for_stop_used = as.integer(probe_edges_min_for_stop_used),
    stop_blocker_codes = as.character(stop_blocker_codes)
  )
}

test_that("probe effort plan applies sole-blocker acceleration only when probe count is the only blocker", {
  append_active_step <- function(state, step_id, A_id, B_id, spoke_id, stage_name) {
    out <- append_cross_probe_step(
      state = state,
      step_id = step_id,
      A_id = A_id,
      B_id = B_id,
      Y = 1L,
      spoke_id = spoke_id,
      is_probe_step = FALSE,
      run_mode = "link_multi_spoke"
    )
    idx <- nrow(out$step_log)
    out$step_log$round_stage[[idx]] <- as.character(stage_name)
    out$step_log$link_stage[[idx]] <- as.character(stage_name)
    out
  }

  state <- make_link_probe_state()
  state$controller$probe_pairs_per_refit_per_spoke <- 1L
  state$controller$probe_pairs_per_refit_per_spoke_bootstrap_max <- 2L
  state$controller$probe_pairs_per_refit_per_spoke_sole_blocker_max <- 4L
  state$controller$probe_edges_min_for_stop <- 6L
  state$controller$probe_sole_blocker_min_realized <- 3L
  state$controller$probe_active_floor_frac <- 0.5
  state$controller$probe_active_floor_min <- 4L
  state$controller$probe_sole_blocker_active_floor_min <- 2L
  state$controller$link_budget_refit_id <- pairwiseLLM:::.adaptive_link_refit_window_id(state)
  state$controller$link_budget_map <- list(
    `2` = list(
      B_spoke_refit_budget = 8L,
      B_spoke_refit_budget_source = "single_spoke_controller"
    )
  )

  panel <- tibble::tibble(
    probe_panel_id = rep("panel-sole", 6L),
    link_epoch_id = rep(3L, 6L),
    spoke_id = rep(2L, 6L),
    hub_item_id = c("h1", "h2", "h3", "h1", "h2", "h3"),
    spoke_item_id = c("s21", "s21", "s21", "s22", "s22", "s22"),
    spoke_bin = c(1L, 1L, 1L, 2L, 2L, 2L),
    hub_bin = c(1L, 2L, 3L, 1L, 2L, 3L),
    probe_edges_planned = rep(6L, 6L),
    probe_panel_reallocation_used = rep(FALSE, 6L),
    planned_rank = seq_len(6L),
    pair_key = make_unordered_key(
      c("h1", "h2", "h3", "h1", "h2", "h3"),
      c("s21", "s21", "s21", "s22", "s22", "s22")
    ),
    realized = rep(FALSE, 6L),
    realized_step_id = rep(NA_integer_, 6L),
    realized_pair_id = rep(NA_integer_, 6L),
    realized_run_mode = rep(NA_character_, 6L)
  )
  state$linking$probe <- pairwiseLLM:::.adaptive_link_probe_empty_state()
  state$linking$probe$panels_by_spoke[["2"]] <- panel

  for (idx in seq_len(3L)) {
    state <- append_cross_probe_step(
      state = state,
      step_id = idx,
      A_id = panel$hub_item_id[[idx]],
      B_id = panel$spoke_item_id[[idx]],
      Y = 1L,
      spoke_id = 2L
    )
    state <- pairwiseLLM:::.adaptive_link_probe_register_commit(
      state,
      tibble::tibble(
        step_id = idx,
        pair_id = idx,
        A = match(panel$hub_item_id[[idx]], state$item_ids),
        B = match(panel$spoke_item_id[[idx]], state$item_ids),
        Y = 1L,
        run_mode = "link_probe_holdout",
        link_spoke_id = 2L,
        is_probe_step = TRUE
      )
    )
  }

  state$refit_meta$last_refit_step <- 3L
  state <- append_active_step(state, 4L, "h1", "s21", 2L, "anchor_link")
  state <- append_active_step(state, 5L, "h2", "s22", 2L, "long_link")

  surface_ok <- make_probe_blocker_surface(
    stop_blocker_codes = "probe_edges_min_for_stop"
  )
  plan_ok <- pairwiseLLM:::.adaptive_link_probe_effort_plan(
    state = state,
    controller = state$controller,
    spoke_id = 2L,
    surface_row = surface_ok,
    surface_source = "test_surface"
  )
  expect_true(isTRUE(plan_ok$probe_only_blocker_trigger))
  expect_identical(plan_ok$active_floor_used, 2L)
  expect_true(isTRUE(plan_ok$allow_when_active))
  expect_true(isTRUE(plan_ok$acceleration_used))
  expect_identical(plan_ok$effective_cap, 3L)

  surface_blocked <- make_probe_blocker_surface(
    reliability_link_global = 0.80,
    stop_blocker_codes = "probe_edges_min_for_stop,reliability_link_global"
  )
  plan_blocked <- pairwiseLLM:::.adaptive_link_probe_effort_plan(
    state = state,
    controller = state$controller,
    spoke_id = 2L,
    surface_row = surface_blocked,
    surface_source = "test_surface"
  )
  expect_false(isTRUE(plan_blocked$probe_only_blocker_trigger))
  expect_identical(plan_blocked$active_floor_used, 4L)
  expect_false(isTRUE(plan_blocked$allow_when_active))
  expect_false(isTRUE(plan_blocked$acceleration_used))
  expect_identical(plan_blocked$effective_cap, 1L)
})

test_that("probe effort plan blocks sole-blocker acceleration when hub anchoring is still a blocker", {
  state <- make_link_probe_state()
  state$controller$probe_pairs_per_refit_per_spoke <- 1L
  state$controller$probe_edges_min_for_stop <- 6L
  state$controller$probe_sole_blocker_min_realized <- 3L
  state$controller$probe_active_floor_frac <- 0.5
  state$controller$probe_active_floor_min <- 4L
  state$controller$link_budget_refit_id <- pairwiseLLM:::.adaptive_link_refit_window_id(state)
  state$controller$link_budget_map <- list(
    `2` = list(
      B_spoke_refit_budget = 8L,
      B_spoke_refit_budget_source = "single_spoke_controller"
    )
  )
  state$linking$probe <- pairwiseLLM:::.adaptive_link_probe_empty_state()
  state$linking$probe$panels_by_spoke[["2"]] <- tibble::tibble(
    probe_panel_id = rep("panel-sole", 6L),
    link_epoch_id = rep(3L, 6L),
    spoke_id = rep(2L, 6L),
    hub_item_id = c("h1", "h2", "h3", "h1", "h2", "h3"),
    spoke_item_id = c("s21", "s21", "s21", "s22", "s22", "s22"),
    spoke_bin = c(1L, 1L, 1L, 2L, 2L, 2L),
    hub_bin = c(1L, 2L, 3L, 1L, 2L, 3L),
    probe_edges_planned = rep(6L, 6L),
    probe_panel_reallocation_used = rep(FALSE, 6L),
    planned_rank = seq_len(6L),
    pair_key = make_unordered_key(
      c("h1", "h2", "h3", "h1", "h2", "h3"),
      c("s21", "s21", "s21", "s22", "s22", "s22")
    ),
    realized = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    realized_step_id = c(1L, 2L, 3L, NA, NA, NA),
    realized_pair_id = c(1L, 2L, 3L, NA, NA, NA),
    realized_run_mode = c("link_probe_holdout", "link_probe_holdout", "link_probe_holdout", NA, NA, NA)
  )
  state$linking$probe$realized_edges <- tibble::tibble(
    step_id = 1:3,
    pair_id = 1:3,
    run_mode = rep("link_probe_holdout", 3L),
    spoke_id = rep(2L, 3L),
    link_epoch_id = rep(3L, 3L),
    probe_panel_id = rep("panel-sole", 3L),
    hub_item_id = c("h1", "h2", "h3"),
    spoke_item_id = c("s21", "s21", "s21"),
    pair_key = make_unordered_key(c("h1", "h2", "h3"), c("s21", "s21", "s21")),
    Y = rep(1L, 3L)
  )
  state$refit_meta$last_refit_step <- 3L

  plan <- pairwiseLLM:::.adaptive_link_probe_effort_plan(
    state = state,
    controller = state$controller,
    spoke_id = 2L,
    surface_row = make_probe_blocker_surface(
      hub_anchored = FALSE,
      stop_blocker_codes = "probe_edges_min_for_stop,hub_not_anchored"
    ),
    surface_source = "test_surface"
  )

  expect_false(isTRUE(plan$probe_only_blocker_trigger))
  expect_identical(plan$active_floor_used, 4L)
  expect_false(isTRUE(plan$allow_when_active))
  expect_false(isTRUE(plan$acceleration_used))
})

test_that("probe effort plan aborts when sole-blocker evaluation lacks canonical blocker state", {
  state <- make_link_probe_state()
  state$controller$probe_pairs_per_refit_per_spoke <- 1L
  state$controller$probe_edges_min_for_stop <- 6L
  state$controller$probe_sole_blocker_min_realized <- 3L
  state$controller$link_budget_refit_id <- pairwiseLLM:::.adaptive_link_refit_window_id(state)
  state$controller$link_budget_map <- list(
    `2` = list(
      B_spoke_refit_budget = 8L,
      B_spoke_refit_budget_source = "single_spoke_controller"
    )
  )
  state$linking$probe <- pairwiseLLM:::.adaptive_link_probe_empty_state()
  state$linking$probe$panels_by_spoke[["2"]] <- tibble::tibble(
    probe_panel_id = rep("panel-sole", 6L),
    link_epoch_id = rep(3L, 6L),
    spoke_id = rep(2L, 6L),
    hub_item_id = c("h1", "h2", "h3", "h1", "h2", "h3"),
    spoke_item_id = c("s21", "s21", "s21", "s22", "s22", "s22"),
    spoke_bin = c(1L, 1L, 1L, 2L, 2L, 2L),
    hub_bin = c(1L, 2L, 3L, 1L, 2L, 3L),
    probe_edges_planned = rep(6L, 6L),
    probe_panel_reallocation_used = rep(FALSE, 6L),
    planned_rank = seq_len(6L),
    pair_key = make_unordered_key(
      c("h1", "h2", "h3", "h1", "h2", "h3"),
      c("s21", "s21", "s21", "s22", "s22", "s22")
    ),
    realized = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    realized_step_id = c(1L, 2L, 3L, NA, NA, NA),
    realized_pair_id = c(1L, 2L, 3L, NA, NA, NA),
    realized_run_mode = c("link_probe_holdout", "link_probe_holdout", "link_probe_holdout", NA, NA, NA)
  )
  state$linking$probe$realized_edges <- tibble::tibble(
    step_id = 1:3,
    pair_id = 1:3,
    run_mode = rep("link_probe_holdout", 3L),
    spoke_id = rep(2L, 3L),
    link_epoch_id = rep(3L, 3L),
    probe_panel_id = rep("panel-sole", 3L),
    hub_item_id = c("h1", "h2", "h3"),
    spoke_item_id = c("s21", "s21", "s21"),
    pair_key = make_unordered_key(c("h1", "h2", "h3"), c("s21", "s21", "s21")),
    Y = rep(1L, 3L)
  )
  state$refit_meta$last_refit_step <- 3L

  surface_missing <- make_probe_blocker_surface(
    stop_blocker_codes = NA_character_
  )

  expect_error(
    pairwiseLLM:::.adaptive_link_probe_effort_plan(
      state = state,
      controller = state$controller,
      spoke_id = 2L,
      surface_row = surface_missing,
      surface_source = "test_surface"
    ),
    "canonical stop blockers are unavailable"
  )
})

test_that("probe effort plan aborts when canonical blocker codes omit hub anchoring blockers", {
  state <- make_link_probe_state()
  state$controller$probe_pairs_per_refit_per_spoke <- 1L
  state$controller$probe_edges_min_for_stop <- 6L
  state$controller$probe_sole_blocker_min_realized <- 3L
  state$controller$link_budget_refit_id <- pairwiseLLM:::.adaptive_link_refit_window_id(state)
  state$controller$link_budget_map <- list(
    `2` = list(
      B_spoke_refit_budget = 8L,
      B_spoke_refit_budget_source = "single_spoke_controller"
    )
  )
  state$linking$probe <- pairwiseLLM:::.adaptive_link_probe_empty_state()
  state$linking$probe$panels_by_spoke[["2"]] <- tibble::tibble(
    probe_panel_id = rep("panel-sole", 6L),
    link_epoch_id = rep(3L, 6L),
    spoke_id = rep(2L, 6L),
    hub_item_id = c("h1", "h2", "h3", "h1", "h2", "h3"),
    spoke_item_id = c("s21", "s21", "s21", "s22", "s22", "s22"),
    spoke_bin = c(1L, 1L, 1L, 2L, 2L, 2L),
    hub_bin = c(1L, 2L, 3L, 1L, 2L, 3L),
    probe_edges_planned = rep(6L, 6L),
    probe_panel_reallocation_used = rep(FALSE, 6L),
    planned_rank = seq_len(6L),
    pair_key = make_unordered_key(
      c("h1", "h2", "h3", "h1", "h2", "h3"),
      c("s21", "s21", "s21", "s22", "s22", "s22")
    ),
    realized = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    realized_step_id = c(1L, 2L, 3L, NA, NA, NA),
    realized_pair_id = c(1L, 2L, 3L, NA, NA, NA),
    realized_run_mode = c("link_probe_holdout", "link_probe_holdout", "link_probe_holdout", NA, NA, NA)
  )
  state$linking$probe$realized_edges <- tibble::tibble(
    step_id = 1:3,
    pair_id = 1:3,
    run_mode = rep("link_probe_holdout", 3L),
    spoke_id = rep(2L, 3L),
    link_epoch_id = rep(3L, 3L),
    probe_panel_id = rep("panel-sole", 3L),
    hub_item_id = c("h1", "h2", "h3"),
    spoke_item_id = c("s21", "s21", "s21"),
    pair_key = make_unordered_key(c("h1", "h2", "h3"), c("s21", "s21", "s21")),
    Y = rep(1L, 3L)
  )
  state$refit_meta$last_refit_step <- 3L

  expect_error(
    pairwiseLLM:::.adaptive_link_probe_effort_plan(
      state = state,
      controller = state$controller,
      spoke_id = 2L,
      surface_row = make_probe_blocker_surface(
        hub_anchored = FALSE,
        stop_blocker_codes = "probe_edges_min_for_stop"
      ),
      surface_source = "test_surface"
    ),
    "hub_not_anchored"
  )
})

test_that("independent multi-spoke holdout routing ignores inactive spokes", {
  state <- make_link_probe_state()
  state$controller$multi_spoke_mode <- "independent"
  state$controller$current_link_spoke_id <- 2L
  state$refit_meta$refit_pairs_target_current <- 6L
  state$controller$refit_pairs_target <- 6L
  state$controller$link_budget_refit_id <- pairwiseLLM:::.adaptive_link_refit_window_id(state)
  state$controller$link_budget_map <- list(
    `2` = list(
      B_spoke_refit_budget = 4L,
      B_spoke_refit_budget_source = "single_spoke_controller"
    ),
    `3` = list(
      B_spoke_refit_budget = 0L,
      B_spoke_refit_budget_source = "independent_inactive_spoke"
    )
  )
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    pairwiseLLM:::new_link_stage_log(),
    list(
      refit_id = 1L,
      spoke_id = 2L,
      hub_id = 1L,
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    state$link_stage_log,
    list(
      refit_id = 1L,
      spoke_id = 3L,
      hub_id = 1L,
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )

  next_spoke <- testthat::with_mocked_bindings(
    .adaptive_link_probe_effort_plan = function(state, controller, spoke_id) {
      if (identical(as.integer(spoke_id), 2L)) {
        list(
          realized_total = 0L,
          realized_refit = 0L,
          effective_cap = 2L,
          remaining_to_min_start = 30L,
          acceleration_used = FALSE
        )
      } else {
        list(
          realized_total = 29L,
          realized_refit = 0L,
          effective_cap = 2L,
          remaining_to_min_start = 1L,
          acceleration_used = FALSE
        )
      }
    },
    .adaptive_link_probe_panel_for_spoke = function(state, spoke_id, epoch_id = NULL) {
      tibble::tibble(
        probe_panel_id = paste0("panel-", as.integer(spoke_id)),
        link_epoch_id = 1L,
        pair_key = paste0("pair-", as.integer(spoke_id))
      )
    },
    pairwiseLLM:::.adaptive_link_probe_next_holdout_spoke(
      state,
      controller = state$controller,
      eligible_spoke_ids = c(2L, 3L)
    ),
    .package = "pairwiseLLM"
  )

  expect_identical(next_spoke, 2L)
})

test_that("concurrent probe progress guard does not impose a cross-spoke startup gate", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23", "s31", "s32", "s33"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
    global_item_id = paste0("g", seq_len(9L))
  )
  state <- adaptive_rank_start(
    items,
    seed = 808L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      multi_spoke_mode = "concurrent",
      min_cross_set_pairs_per_spoke_per_refit = 1L
    )
  )
  state$warm_start_done <- TRUE
  state$linking$phase_a$phase <- "phase_b"
  state$linking$phase_a$ready_for_phase_b <- TRUE
  state$linking$phase_a$strict_ready_for_phase_b <- TRUE
  state$linking$phase_a$ready_spokes <- c(2L, 3L)
  state$refit_meta$last_refit_step <- 10L
  state$refit_meta$refit_pairs_target_current <- 6L
  state$controller$refit_pairs_target <- 6L
  state$controller$probe_pairs_per_refit_per_spoke <- 1L
  state$controller$probe_edges_min_for_stop <- 3L
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(link_identified = TRUE, link_stop_eligible = FALSE, link_epoch_id = 3L),
    `3` = list(link_identified = TRUE, link_stop_eligible = FALSE, link_epoch_id = 3L)
  )
  panel_tbl <- function(spoke_id) {
    tibble::tibble(
      probe_panel_id = paste0("panel-", spoke_id),
      link_epoch_id = 3L,
      spoke_id = as.integer(spoke_id),
      hub_item_id = c("h1", "h2", "h3"),
      spoke_item_id = paste0("s", spoke_id, c("1", "2", "3")),
      spoke_bin = c(1L, 2L, 3L),
      hub_bin = c(1L, 2L, 3L),
      planned_rank = c(1L, 2L, 3L),
      pair_key = make_unordered_key(c("h1", "h2", "h3"), paste0("s", spoke_id, c("1", "2", "3"))),
      realized = c(FALSE, FALSE, FALSE),
      realized_step_id = c(NA_integer_, NA_integer_, NA_integer_),
      realized_pair_id = c(NA_integer_, NA_integer_, NA_integer_),
      realized_run_mode = c(NA_character_, NA_character_, NA_character_)
    )
  }
  state$linking$probe <- list(
    panels_by_spoke = list(`2` = panel_tbl(2L), `3` = panel_tbl(3L)),
    prediction_cache = pairwiseLLM:::.adaptive_link_probe_empty_cache(),
    realized_edges = pairwiseLLM:::.adaptive_link_probe_empty_realized_log(),
    collect_holdout_now_by_spoke = list()
  )
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    pairwiseLLM:::append_link_stage_log(
      pairwiseLLM:::new_link_stage_log(),
      list(
        refit_id = 1L,
        spoke_id = 2L,
        hub_id = 1L,
        link_transform_policy = "auto",
        link_transform_state = "shift_only",
        linking_identified = TRUE,
        link_stop_eligible = FALSE,
        link_stop_pass = FALSE,
        link_state_frozen = FALSE
      )
    ),
    list(
      refit_id = 1L,
      spoke_id = 3L,
      hub_id = 1L,
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      linking_identified = TRUE,
      link_stop_eligible = FALSE,
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )

  state$controller$link_budget_refit_id <- 1L
  state$controller$link_budget_map <- list(
    `2` = list(B_spoke_refit_budget = 2L, B_spoke_refit_budget_source = "concurrent_allocator"),
    `3` = list(B_spoke_refit_budget = 2L, B_spoke_refit_budget_source = "concurrent_allocator")
  )

  guard0 <- testthat::with_mocked_bindings(
    .adaptive_link_effective_active_spokes = function(...) c(2L, 3L),
    pairwiseLLM:::.adaptive_link_probe_active_progress_guard(
      state,
      controller = state$controller,
      eligible_spoke_ids = c(2L, 3L)
    ),
    .package = "pairwiseLLM"
  )
  expect_false(isTRUE(guard0$block_probes))
  expect_identical(guard0$pending_spokes, integer())
  expect_identical(guard0$budgeted_spokes, c(2L, 3L))

  state <- append_cross_probe_step(state, 11L, "h1", "s21", 1L, 2L, run_mode = "link_multi_spoke")
  state$step_log$is_probe_step[nrow(state$step_log)] <- FALSE

  guard1 <- testthat::with_mocked_bindings(
    .adaptive_link_effective_active_spokes = function(...) c(2L, 3L),
    pairwiseLLM:::.adaptive_link_probe_active_progress_guard(
      state,
      controller = state$controller,
      eligible_spoke_ids = c(2L, 3L)
    ),
    .package = "pairwiseLLM"
  )
  expect_false(isTRUE(guard1$block_probes))
  expect_identical(guard1$pending_spokes, integer())
  expect_identical(guard1$budgeted_spokes, c(2L, 3L))

  state <- append_cross_probe_step(state, 12L, "h1", "s31", 1L, 3L, run_mode = "link_multi_spoke")
  state$step_log$is_probe_step[nrow(state$step_log)] <- FALSE

  guard2 <- testthat::with_mocked_bindings(
    .adaptive_link_effective_active_spokes = function(...) c(2L, 3L),
    pairwiseLLM:::.adaptive_link_probe_active_progress_guard(
      state,
      controller = state$controller,
      eligible_spoke_ids = c(2L, 3L)
    ),
    .package = "pairwiseLLM"
  )
  expect_false(isTRUE(guard2$block_probes))
  expect_identical(guard2$pending_spokes, integer())
  expect_identical(guard2$budgeted_spokes, c(2L, 3L))

  single_spoke_guard <- pairwiseLLM:::.adaptive_link_probe_active_progress_guard(
    state,
    controller = utils::modifyList(state$controller, list(run_mode = "link_one_spoke")),
    eligible_spoke_ids = 2L
  )
  expect_false(isTRUE(single_spoke_guard$block_probes))
  expect_identical(single_spoke_guard$pending_spokes, integer())
  expect_identical(single_spoke_guard$budgeted_spokes, integer())
})

test_that("refit helpers cover probe metrics, stop reconstruction, and concurrent allocation edges", {
  state <- make_link_probe_state()
  state$linking$probe <- list(
    panels_by_spoke = list(
      `2` = tibble::tibble(
        probe_panel_id = c("p", "p"),
        link_epoch_id = c(3L, 3L),
        spoke_id = c(2L, 2L),
        hub_item_id = c("h1", "h2"),
        spoke_item_id = c("s21", "s22"),
        spoke_bin = c(1L, 2L),
        hub_bin = c(1L, 1L),
        planned_rank = c(1L, 2L),
        pair_key = c("h1:s21", "h2:s22"),
        realized = c(TRUE, TRUE),
        realized_step_id = c(1L, 2L),
        realized_pair_id = c(1L, 2L),
        realized_run_mode = c("link_probe_holdout", "link_probe_holdout")
      )
    ),
    prediction_cache = tibble::tibble(
      refit_id = c(2L, 1L),
      spoke_id = c(2L, 2L),
      link_epoch_id = c(3L, 3L),
      probe_panel_id = c("p", "p"),
      hub_item_id = c("h1", "h1"),
      spoke_item_id = c("s21", "s21"),
      pair_key = c("h1:s21", "h1:s21"),
      pred_prob = c(0.8, 0.6)
    ),
    realized_edges = tibble::tibble(
      step_id = c(1L, 2L),
      pair_id = c(1L, 2L),
      run_mode = c("link_probe_holdout", "link_probe_holdout"),
      spoke_id = c(2L, 2L),
      link_epoch_id = c(3L, 3L),
      probe_panel_id = c("p", "p"),
      hub_item_id = c("h1", "h2"),
      spoke_item_id = c("s21", "s22"),
      pair_key = c("h1:s21", "h2:s22"),
      Y = c(1L, 0L)
    ),
    collect_holdout_now_by_spoke = list()
  )
  state <- append_cross_probe_step(state, 1L, "h1", "s21", 1L, 2L)
  state <- append_cross_probe_step(state, 2L, "h2", "s22", 0L, 2L)
  state <- append_cross_probe_step(
    state,
    3L,
    "h1",
    "s21",
    1L,
    2L,
    is_probe_step = FALSE,
    run_mode = "link_one_spoke"
  )
  state <- append_cross_probe_step(
    state,
    4L,
    "h2",
    "s22",
    0L,
    2L,
    is_probe_step = FALSE,
    run_mode = "link_one_spoke"
  )
  state$refit_meta$theta_mean_history <- list(
    c(h1 = 0.8, h2 = 0.4, h3 = 0.1, s21 = -0.1, s22 = -0.3, s31 = 0.2, s32 = -0.2),
    c(h1 = 0.9, h2 = 0.5, h3 = 0.2, s21 = -0.2, s22 = -0.4, s31 = 0.3, s32 = -0.1)
  )

  expect_equal(
    pairwiseLLM:::.adaptive_link_probe_pred_rmse_lagged(
      state,
      refit_id = 2L,
      spoke_id = 2L,
      lag_refit_id = 1L,
      epoch_id = 3L
    ),
    0.2,
    tolerance = 1e-12
  )

  state$controller$probe_pairs_per_refit_per_spoke <- 1L
  state$controller$hub_anchor_required_phase_b <- FALSE
  state$controller$spoke_quantile_coverage_bins <- 1L
  state$linking$probe$panels_by_spoke <- list(
    `2` = pairwiseLLM:::.adaptive_link_probe_construct_panel(state, state$controller, spoke_id = 2L)
  )
  panel <- state$linking$probe$panels_by_spoke$`2`
  panel_id <- panel$probe_panel_id[[1L]]
  state$linking$probe$realized_edges$probe_panel_id[] <- panel_id
  state$linking$probe$realized_edges$hub_item_id <- as.character(panel$hub_item_id[1:2])
  state$linking$probe$realized_edges$spoke_item_id <- as.character(panel$spoke_item_id[1:2])
  state$linking$probe$realized_edges$pair_key <- as.character(panel$pair_key[1:2])
  probe_idx <- which(as.character(state$step_log$run_mode) == "link_probe_holdout")
  state$step_log$A[probe_idx] <- match(as.character(panel$hub_item_id[1:2]), state$item_ids)
  state$step_log$B[probe_idx] <- match(as.character(panel$spoke_item_id[1:2]), state$item_ids)
  state$step_log$i[probe_idx] <- state$step_log$A[probe_idx]
  state$step_log$j[probe_idx] <- state$step_log$B[probe_idx]
  stage_rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 2L,
    refit_context = list(last_refit_step = 0L)
  )
  row_stage <- stage_rows[stage_rows$spoke_id == 2L, , drop = FALSE]
  expect_false(isTRUE(row_stage$probe_panel_reallocation_used[[1L]]))
  expect_false(isTRUE(row_stage$probe_edges_count_toward_active_constraints_used[[1L]]))
  expect_identical(row_stage$n_probe_pairs_since_last_refit[[1L]], 2L)

  state_drift <- state
  state_drift <- append_cross_probe_step(
    state_drift,
    step_id = 11L,
    A_id = "h2",
    B_id = "s22",
    Y = 1L,
    spoke_id = 2L,
    run_mode = "link_probe"
  )
  stage_rows_drift <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state_drift,
    refit_id = 2L,
    refit_context = list(last_refit_step = 0L)
  )
  row_stage_drift <- stage_rows_drift[stage_rows_drift$spoke_id == 2L, , drop = FALSE]
  expect_identical(row_stage_drift$n_probe_pairs_since_last_refit[[1L]], 2L)

  state_legacy_holdout <- state
  state_legacy_holdout$step_log$is_holdout_probe_step <- FALSE
  stage_rows_legacy_holdout <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state_legacy_holdout,
    refit_id = 2L,
    refit_context = list(last_refit_step = 0L)
  )
  row_stage_legacy_holdout <- stage_rows_legacy_holdout[
    stage_rows_legacy_holdout$spoke_id == 2L,
    ,
    drop = FALSE
  ]
  expect_identical(row_stage_legacy_holdout$n_probe_pairs_since_last_refit[[1L]], 2L)

  state_legacy_drift <- state_drift
  state_legacy_drift$step_log$is_holdout_probe_step <- TRUE
  state_legacy_drift$step_log$is_drift_probe_step <- FALSE
  state_legacy_drift$step_log$is_probe_step <- FALSE
  stage_rows_legacy_drift <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state_legacy_drift,
    refit_id = 2L,
    refit_context = list(last_refit_step = 0L)
  )
  row_stage_legacy_drift <- stage_rows_legacy_drift[
    stage_rows_legacy_drift$spoke_id == 2L,
    ,
    drop = FALSE
  ]
  expect_identical(row_stage_legacy_drift$n_probe_pairs_since_last_refit[[1L]], 2L)

  normalized_step_log <- pairwiseLLM:::.adaptive_align_log_schema_for_resume(
    state_legacy_drift$step_log,
    pairwiseLLM:::schema_step_log,
    name = "step_log",
    fill_missing = TRUE
  )
  expect_false(any(
    as.character(normalized_step_log$run_mode) == "link_probe" &
      normalized_step_log$is_holdout_probe_step %in% TRUE
  ))
  expect_true(all(
    normalized_step_log$is_probe_step[
      as.character(normalized_step_log$run_mode) == "link_probe"
    ] %in% TRUE
  ))

  ids_k <- pairwiseLLM:::.adaptive_link_theta_global_scope_ids(
    state,
    spoke_id = 2L,
    scope = "min_cross_set_edges_k"
  )
  expect_identical(ids_k, c("s21", "s22"))

  lag_row <- tibble::tibble(
    link_transform_state = "shift_only",
    delta_spoke_mean = 0.1,
    log_alpha_spoke_mean = NA_real_
  )
  state$refit_meta$theta_mean_history <- list(
    c(h1 = 1, s21 = 0),
    c(h1 = 1, s21 = 1),
    c(h1 = 1, s21 = 2)
  )
  rmse <- pairwiseLLM:::.adaptive_link_theta_global_rmse_lagged(
    state,
    spoke_id = 2L,
    hub_id = 1L,
    scope_ids = c("h1", "s21"),
    transform_mode = "shift_only",
    delta_mean = 0.2,
    log_alpha_mean = NA_real_,
    lag_row = lag_row,
    lag = 2L
  )
  expect_equal(rmse, sqrt((2.1^2) / 2), tolerance = 1e-12)

  old_row <- tibble::tibble(
    link_stop_eligible = TRUE,
    reliability_stop_pass = TRUE
  )
  expect_false(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    old_row,
    diagnostics_pass = TRUE,
    hub_theta_sd = c(h1 = 0.1),
    controller = list()
  )))

  edges <- pairwiseLLM:::.adaptive_link_probe_edges_realized(state, spoke_id = 2L, epoch_id = 3L)
  expect_identical(nrow(edges), 2L)

  probs <- pairwiseLLM:::.adaptive_link_cross_probabilities(
    edges = tibble::tibble(
      hub_item = c("h1", "missing"),
      spoke_item = c("s21", "s22"),
      spoke_in_A = c(TRUE, FALSE)
    ),
    hub_theta = c(h1 = 0.8),
    spoke_theta = c(s21 = -0.2, s22 = -0.4),
    delta_mean = 0.1,
    log_alpha_mean = NA_real_,
    judge_params = list(beta = 0.2, epsilon = 2)
  )
  expect_true(is.na(probs[[2L]]))

  hub_theta_fit <- stats::setNames(
    seq(0.8, by = -0.2, length.out = length(unique(edges$hub_item))),
    unique(edges$hub_item)
  )
  spoke_theta_fit <- stats::setNames(
    seq(-0.2, by = -0.2, length.out = length(unique(edges$spoke_item))),
    unique(edges$spoke_item)
  )
  expect_true(is.finite(pairwiseLLM:::.adaptive_link_probe_brier_for_fit(
    edges = edges,
    hub_theta = hub_theta_fit,
    spoke_theta = spoke_theta_fit,
    delta_mean = 0.1
  )))
  expect_true(is.finite(pairwiseLLM:::.adaptive_link_probe_pred_rmse_lagged_for_fit(
    edges = edges,
    hub_theta = hub_theta_fit,
    spoke_theta = spoke_theta_fit,
    delta_mean = 0.1,
    log_alpha_mean = NA_real_,
    lag_delta_mean = 0.3,
    lag_log_alpha_mean = 0
  )))

  alt <- pairwiseLLM:::.adaptive_link_fit_transform_alt_shift_scale(
    cross_edges = edges,
    hub_theta = c(h1 = 0.8, h2 = 0.4),
    spoke_theta = c(s21 = -0.2, s22 = -0.4)
  )
  expect_true(is.list(alt))

  targets <- pairwiseLLM:::.adaptive_link_concurrent_targets(
    spoke_stats = list(
      `2` = list(candidate_count = 1L, utility_mass = 0.9),
      `3` = list(candidate_count = 3L, utility_mass = 0.1)
    ),
    total_pairs = 4L,
    floor_pairs = 2L
  )
  expect_identical(sum(targets), 4L)
  expect_true(targets[["3"]] >= 2L)

  rows <- tibble::tibble(
    B_spoke_refit_budget = 3L,
    stage_target_anchor_link = 1L,
    stage_target_long_link = 1L,
    stage_target_mid_link = 1L,
    stage_target_local_link = 0L,
    stage_realized_anchor_link = 1L,
    stage_realized_long_link = 1L,
    stage_realized_mid_link = 1L,
    stage_realized_local_link = 0L,
    stage_shortfall_anchor_link = 0L,
    stage_shortfall_long_link = 0L,
    stage_shortfall_mid_link = 0L,
    stage_shortfall_local_link = 0L,
    stage_budget_unfilled = 0L,
    stage_reallocation_used = FALSE,
    stage_reallocation_rule_used = NA_character_
  )
  expect_no_error(pairwiseLLM:::.adaptive_assert_link_stage_budget_invariants(rows))
})

test_that("candidate ranking and refit-stop helpers cover remaining routing and stop branches", {
  state <- make_link_probe_state()
  state$controller$multi_spoke_mode <- "concurrent"
  state$controller$link_state_frozen_by_spoke <- list(`2` = TRUE)
  state$controller$link_transform_frozen_by_spoke <- list(`2` = FALSE)
  state$controller$probe_pairs_per_refit_per_spoke <- 1L
  state$refit_meta$last_refit_step <- 0L
  state <- append_cross_probe_step(state, 1L, "h1", "s21", 1L, 2L)

  ranked <- pairwiseLLM:::.adaptive_link_ranked_spokes(
    state = state,
    controller = state$controller,
    eligible_spoke_ids = c(2L, 3L)
  )
  expect_identical(ranked[[1L]], 3L)

  expect_error(
    pairwiseLLM:::generate_stage_candidates_from_state(
      state = state,
      stage_name = "long_link",
      fallback_name = "base",
      C_max = 20L,
      seed = 1L,
      link_spoke_id = 99L
    ),
    "not eligible in phase_b"
  )

  ord <- pairwiseLLM:::.adaptive_linking_selection_order(
    tibble::tibble(
      i = c("b", "a"),
      j = c("z", "y"),
      link_d_opt_gain = c(1, 1)
    )
  )
  expect_identical(ord, c(2L, 1L))

  phase_a_state <- pairwiseLLM::adaptive_rank_start(
    tibble::tibble(
      item_id = c("a1", "a2", "b1", "b2"),
      text = c("a1", "a2", "b1", "b2"),
      set_id = c(1L, 1L, 2L, 2L),
      global_item_id = c("ga1", "ga2", "gb1", "gb2")
    ),
    seed = 55L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  phase_a_state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("ready", "pending"),
      validation_message = c("ok", "pending"),
      artifact_path = c(NA_character_, NA_character_)
    )
  )
  expect_identical(pairwiseLLM:::.adaptive_refit_phase_a_scope(phase_a_state)$set_id, 2L)
  scope <- pairwiseLLM:::.adaptive_link_phase_a_scope(phase_a_state)
  expect_identical(scope$active_set_id, 2L)

  invalid_y <- make_link_probe_state()
  invalid_y$step_log <- pairwiseLLM:::append_step_log(
    invalid_y$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      pair_id = 1L,
      A = match("h1", invalid_y$item_ids),
      B = match("s21", invalid_y$item_ids),
      Y = 2L
    )
  )
  expect_error(
    pairwiseLLM:::.adaptive_results_from_step_log(invalid_y),
    "must encode Y in \\{0,1\\}"
  )

  metrics_state <- pairwiseLLM::adaptive_rank_start(make_test_items(4), seed = 9L)
  draws <- matrix(
    c(
      1.0, 0.7, 0.4, 0.1,
      1.1, 0.8, 0.5, 0.2,
      1.2, 0.9, 0.6, 0.3
    ),
    nrow = 3,
    byrow = TRUE
  )
  colnames(draws) <- metrics_state$item_ids
  metrics_state$btl_fit <- make_test_btl_fit(
    metrics_state$item_ids,
    draws = draws,
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 1000)
  )
  metrics_state$refit_meta$theta_mean_history <- list(
    c(item1 = 1.0, item2 = 0.7, item3 = 0.4, item4 = 0.1),
    c(item1 = 1.1, item2 = 0.8, item3 = 0.5, item4 = 0.2),
    c(item1 = 1.2, item2 = 0.9, item3 = 0.6, item4 = 0.3)
  )
  cfg <- list(
    stability_lag = 1L,
    eap_reliability_min = 0.1,
    theta_corr_min = 0.5,
    theta_sd_rel_change_max = 1,
    rank_spearman_min = 0.5,
    ess_bulk_min = 100,
    ess_bulk_min_near_stop = 200,
    max_rhat = 1.05,
    divergences_max = 0L
  )
  metrics <- pairwiseLLM:::compute_stop_metrics(metrics_state, cfg)
  expect_true(isTRUE(metrics$diagnostics_pass))
  expect_true(isTRUE(metrics$lag_eligible))
  expect_true(pairwiseLLM:::should_stop(metrics, cfg))
})

test_that("remaining contract and routing validators cover missing edge branches", {
  expect_error(pairwiseLLM:::.btl_contract_inference_contract(1L), "list or NULL")
  expect_error(
    pairwiseLLM:::.btl_mcmc_inference_contract_from_results(
      tibble::tibble(),
      inference_contract = list(judge_param_mode = "bad")
    ),
    "judge_param_mode"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_inference_contract_from_results(
      tibble::tibble(),
      inference_contract = list(phase_boundary_detected = NA)
    ),
    "phase_boundary_detected"
  )

  fit <- make_local_fit_contract()
  expect_error(pairwiseLLM:::validate_btl_fit_contract(fit, ids = character()), "non-empty character vector")
  bad_fit <- fit
  colnames(bad_fit$theta_draws) <- NULL
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_fit, ids = c("A", "B")), "column names")
  bad_fit2 <- fit
  bad_fit2$theta_mean <- unname(bad_fit2$theta_mean)
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_fit2, ids = c("A", "B")), "theta_mean")
  bad_fit3 <- fit
  bad_fit3$theta_sd <- unname(bad_fit3$theta_sd)
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_fit3, ids = c("A", "B")), "theta_sd")
  bad_fit4 <- fit
  bad_fit4$n_draws <- 99L
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_fit4, ids = c("A", "B")), "n_draws")
  bad_fit5 <- fit
  bad_fit5$mcmc_config_used <- NULL
  expect_error(pairwiseLLM:::validate_btl_fit_contract(bad_fit5, ids = c("A", "B")), "mcmc_config_used")

  state <- make_link_probe_state()
  expect_length(pairwiseLLM:::.adaptive_link_spoke_bins(character(), numeric(), bins = 3L), 0L)
  expect_length(
    pairwiseLLM:::.adaptive_link_probe_quantile_bins(character(), numeric(), bins = 3L),
    0L
  )

  panel_fallback <- pairwiseLLM:::.adaptive_link_probe_construct_panel(
    state = state,
    controller = utils::modifyList(state$controller, list(hub_anchor_required_phase_b = FALSE)),
    spoke_id = 2L
  )
  expect_true(is.data.frame(panel_fallback))
  expect_true(nrow(panel_fallback) > 0L)

  no_phase_b <- state
  no_phase_b$linking$phase_a$phase <- "phase_a"
  ensured <- pairwiseLLM:::.adaptive_link_probe_ensure_panels(
    no_phase_b,
    controller = no_phase_b$controller,
    spoke_ids = 2L
  )
  expect_true(is.list(ensured$linking$probe))

  expect_identical(pairwiseLLM:::.adaptive_linking_selection_order(tibble::tibble()), integer())
})

test_that("link-stage validators and transform helpers cover uncovered error branches", {
  rows_missing <- tibble::tibble(B_spoke_refit_budget = 1L)
  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_budget_invariants(rows_missing),
    "missing columns"
  )

  bad_budget <- tibble::tibble(
    B_spoke_refit_budget = 1L,
    stage_target_anchor_link = 1L,
    stage_target_long_link = 1L,
    stage_target_mid_link = 0L,
    stage_target_local_link = 0L,
    stage_realized_anchor_link = 1L,
    stage_realized_long_link = 1L,
    stage_realized_mid_link = 0L,
    stage_realized_local_link = 0L,
    stage_shortfall_anchor_link = 0L,
    stage_shortfall_long_link = 0L,
    stage_shortfall_mid_link = 0L,
    stage_shortfall_local_link = 0L,
    stage_budget_unfilled = 0L,
    stage_reallocation_used = FALSE,
    stage_reallocation_rule_used = NA_character_
  )
  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_budget_invariants(bad_budget),
    "must sum to the per-spoke budget"
  )

  incomplete <- tibble::tibble(spoke_id = 2L)
  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_rows_completeness(incomplete),
    "missing required columns"
  )

  bad_realized <- tibble::tibble(
    refit_id = 1L,
    spoke_id = 2L,
    hub_id = 1L,
    link_epoch_id = 1L,
    link_estimation_mode = "transform",
    link_transform_policy = "auto",
    link_transform_state = "shift_only",
    link_refit_mode = "shift_only",
    hub_lock_mode = "hard_lock",
    reliability_link_global = 0.9,
    linking_identified = TRUE,
    link_stop_eligible = TRUE,
    link_stop_pass = FALSE,
    link_state_frozen = FALSE,
    n_pairs_cross_set_done = 1L,
    n_unique_cross_pairs_seen = 1L,
    n_cross_edges_active_since_last_refit = 1L,
    n_cross_edges_probe_since_last_refit = 0L,
    n_cross_edges_total_since_last_refit = 1L,
    coverage_bins_used = 2L,
    B_spoke_refit_budget = 2L,
    B_spoke_refit_budget_source = "test",
    stage_target_anchor_link = 1L,
    stage_target_long_link = 1L,
    stage_target_mid_link = 0L,
    stage_target_local_link = 0L,
    stage_realized_anchor_link = 1L,
    stage_realized_long_link = 0L,
    stage_realized_mid_link = 0L,
    stage_realized_local_link = 0L,
    stage_shortfall_anchor_link = 0L,
    stage_shortfall_long_link = 1L,
    stage_shortfall_mid_link = 0L,
    stage_shortfall_local_link = 0L,
    stage_reallocation_used = FALSE,
    stage_reallocation_rule_used = "none",
    stage_budget_unfilled = 0L,
    probe_brier = 0.10,
    probe_brier_max_used = 0.19,
    probe_brier_pass = TRUE,
    probe_pred_rmse_lagged = 0.01,
    probe_pred_rmse_max_used = 0.015,
    probe_pred_rmse_pass = TRUE,
    phase_a_within_edges_hub_used = NA_integer_,
    phase_a_within_edges_spoke_used = NA_integer_,
    phase_b_active_edges_used = NA_integer_,
    anchored_joint_hub_items_fixed_count = NA_integer_,
    theta_global_rmse_lagged = 0.02,
    theta_global_rmse_max_used = 0.05,
    theta_global_rmse_pass = TRUE,
    anchored_joint_init_state_method = NA_character_,
    anchored_joint_spoke_prior_scale_used = NA_real_,
    anchored_joint_sd_floor_used = NA_real_,
    anchored_joint_spoke_prior_fallback_used = NA,
    anchored_joint_spoke_prior_fallback_sd_used = NA_real_,
    judge_params_fixed_for_anchored_joint = NA,
    anchored_joint_free_block_dim = NA_integer_
  )
  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_budget_invariants(bad_realized),
    "realized counts plus unfilled budget"
  )

  bad_nonneg <- bad_realized
  bad_nonneg$stage_realized_long_link <- 2L
  bad_nonneg$stage_budget_unfilled <- -1L
  bad_nonneg$stage_reallocation_used <- TRUE
  bad_nonneg$stage_reallocation_rule_used <- "pooled_utility_backfill"
  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_budget_invariants(bad_nonneg),
    "must be non-negative"
  )

  bad_no_backfill <- bad_realized
  bad_no_backfill$B_spoke_refit_budget <- 3L
  bad_no_backfill$stage_target_mid_link <- 1L
  bad_no_backfill$stage_realized_long_link <- 2L
  bad_no_backfill$stage_budget_unfilled <- 0L
  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_budget_invariants(bad_no_backfill),
    "cannot exceed stage targets"
  )

  bad_rule <- bad_realized
  bad_rule$stage_budget_unfilled <- 1L
  bad_rule$stage_reallocation_rule_used <- "pooled_utility_backfill"
  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_budget_invariants(bad_rule),
    "must use reallocation rule `none`"
  )

  bad_backfill <- bad_realized
  bad_backfill$stage_reallocation_used <- TRUE
  bad_backfill$stage_budget_unfilled <- 1L
  bad_backfill$stage_reallocation_rule_used <- "none"
  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_budget_invariants(bad_backfill),
    "must use reallocation rule `pooled_utility_backfill`"
  )

  key_na <- bad_realized
  key_na$refit_id <- NA_integer_
  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_rows_completeness(key_na),
    "key fields refit_id/spoke_id/hub_id"
  )

  mode_na <- bad_realized
  mode_na$link_transform_state <- NA_character_
  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_rows_completeness(mode_na),
    "mode fields must be populated"
  )

  logical_na <- bad_realized
  logical_na$link_stop_pass <- NA
  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_rows_completeness(logical_na),
    "linking_identified/link_stop_eligible/link_stop_pass"
  )

  frozen_na <- bad_realized
  frozen_na$link_state_frozen <- NA
  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_rows_completeness(frozen_na),
    "`link_state_frozen` must be populated"
  )

  legacy_mode <- bad_realized |> dplyr::select(-link_transform_policy, -link_transform_state)
  legacy_mode$link_transform_mode <- "shift_only"
  legacy_mode$stage_budget_unfilled <- 1L
  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_rows_completeness(legacy_mode),
    "missing required columns: link_transform_policy, link_transform_state"
  )

  empty_fit <- pairwiseLLM:::.adaptive_link_fit_transform_alt_shift_scale(
    cross_edges = tibble::tibble(),
    hub_theta = c(),
    spoke_theta = c()
  )
  expect_false(empty_fit$converged)

  empty_cross <- pairwiseLLM:::.adaptive_link_cross_edges(make_link_probe_state(), spoke_id = 2L)
  expect_identical(nrow(empty_cross), 0L)
})

test_that("probe panel size uses the normative clamp target", {
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_panel_size(n_spoke_items = 3L),
    40L
  )
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_panel_size(n_spoke_items = 200L),
    50L
  )
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_panel_size(n_spoke_items = 1000L),
    160L
  )
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_panel_size(
      n_spoke_items = 1000L
    ),
    160L
  )
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_panel_size(
      n_spoke_items = 1000L,
      probe_panel_edges = 12L
    ),
    12L
  )
})

test_that("probe panel construction respects anchor-only HubEligible and legal hub-spoke capacity", {
  state <- make_link_probe_state()
  routing_scores <- pairwiseLLM:::.adaptive_link_phase_b_routing_scores(
    state = state,
    controller = state$controller,
    active_ids = c("h1", "h2", "h3", "s21", "s22"),
    hub_id = 1L
  )
  hub_anchors <- pairwiseLLM:::.adaptive_link_phase_b_hub_anchors(
    state = state,
    hub_ids = c("h1", "h2", "h3"),
    hub_scores = routing_scores,
    defaults = pairwiseLLM:::adaptive_defaults(5L)
  )

  panel_anchor_only <- pairwiseLLM:::.adaptive_link_probe_construct_panel(
    state,
    state$controller,
    spoke_id = 2L
  )
  expect_setequal(unique(as.character(panel_anchor_only$hub_item_id)), as.character(hub_anchors))
  expect_identical(nrow(panel_anchor_only), 4L)

  state_full_hub <- make_link_probe_state()
  state_full_hub$controller$hub_anchor_required_phase_b <- FALSE
  panel_full_hub <- pairwiseLLM:::.adaptive_link_probe_construct_panel(
    state_full_hub,
    state_full_hub$controller,
    spoke_id = 2L
  )
  expect_setequal(unique(as.character(panel_full_hub$hub_item_id)), c("h1", "h2", "h3"))
  expect_identical(nrow(panel_full_hub), 6L)
})

test_that("probe panel construction keeps the normative target auditable when feasibility caps apply", {
  state <- make_link_probe_state()
  panel <- pairwiseLLM:::.adaptive_link_probe_construct_panel(state, state$controller, spoke_id = 2L)

  expect_identical(pairwiseLLM:::.adaptive_link_probe_planned_edges(panel), 40L)
  expect_identical(unique(as.integer(panel$probe_edges_planned)), 40L)
  expect_identical(nrow(panel), 4L)
})

test_that("large probe cell sampler avoids full-grid construction and respects exclusions", {
  hub_ids <- paste0("h", seq_len(80L))
  spoke_ids <- paste0("s", seq_len(80L))
  excluded <- pairwiseLLM:::make_unordered_key(
    c("h1", "h1", "h2"),
    c("s1", "s2", "s1")
  )

  first_rows <- pairwiseLLM:::.adaptive_link_probe_sample_cell_pairs(
    hub_ids = hub_ids,
    spoke_ids = spoke_ids,
    excluded_keys = excluded,
    take = 5L,
    random = FALSE,
    materialize_limit = 1L
  )
  expect_identical(nrow(first_rows), 5L)
  expect_false(any(first_rows$pair_key %in% excluded))
  expect_identical(anyDuplicated(first_rows$pair_key), 0L)
  expect_identical(as.character(first_rows$hub_item_id[[1L]]), "h1")
  expect_identical(as.character(first_rows$spoke_item_id[[1L]]), "s10")

  sampled_rows <- pairwiseLLM:::.adaptive_link_probe_sample_cell_pairs(
    hub_ids = hub_ids,
    spoke_ids = spoke_ids,
    excluded_keys = c(excluded, first_rows$pair_key),
    take = 12L,
    seed = 101L,
    random = TRUE,
    materialize_limit = 1L
  )
  expect_identical(nrow(sampled_rows), 12L)
  expect_false(any(sampled_rows$pair_key %in% c(excluded, first_rows$pair_key)))
  expect_identical(anyDuplicated(sampled_rows$pair_key), 0L)
})

test_that("remaining candidate-generation and budget helpers cover edge branches", {
  state <- make_link_probe_state()

  round_robin <- pairwiseLLM:::.adaptive_link_concurrent_targets(
    spoke_stats = list(
      `2` = list(candidate_count = 3L, utility_mass = 0),
      `3` = list(candidate_count = 3L, utility_mass = 0)
    ),
    total_pairs = 3L,
    floor_pairs = 2L
  )
  expect_identical(sum(round_robin), 3L)
  expect_true(all(round_robin >= 1L))

  cached_state <- state
  cached_state$controller$multi_spoke_mode <- "concurrent"
  cached_state$controller$link_budget_refit_id <- 1L
  cached_state$controller$link_budget_map <- list(
    `2` = list(
      B_spoke_refit_budget = 0L,
      B_spoke_refit_budget_source = "concurrent_allocator",
      concurrent_target_pairs = 0L,
      concurrent_floor_pairs = 1L
    )
  )
  cached_state$step_log <- tibble::tibble(
    pair_id = 1L,
    step_id = 5L,
    is_cross_set = TRUE,
    link_spoke_id = 2L
  )
  cached_state$refit_meta$last_refit_step <- 0L
  budget_map <- pairwiseLLM:::.adaptive_link_budget_map_for_refit(
    state = cached_state,
    controller = cached_state$controller,
    eligible_spoke_ids = 2L,
    seed = 1L
  )
  expect_identical(budget_map[["2"]]$B_spoke_refit_budget, 0L)
  expect_identical(budget_map[["2"]]$B_spoke_refit_budget_source, "concurrent_allocator")
  expect_true(isTRUE(budget_map[["2"]]$concurrent_floor_met))
  expect_true(isTRUE(budget_map[["2"]]$concurrent_target_met))

  one_spoke_state <- cached_state
  one_spoke_state$controller$run_mode <- "link_one_spoke"
  one_spoke_state$controller$link_budget_refit_id <- NA_integer_
  one_spoke_state$controller$link_budget_map <- list()
  one_spoke_budget <- pairwiseLLM:::.adaptive_link_budget_map_for_refit(
    state = one_spoke_state,
    controller = one_spoke_state$controller,
    eligible_spoke_ids = 2L,
    seed = 1L
  )
  expect_true(one_spoke_budget[["2"]]$B_spoke_refit_budget > 0L)
  expect_identical(
    one_spoke_budget[["2"]]$B_spoke_refit_budget_source,
    "single_spoke_controller_feasible_capacity"
  )

  expect_identical(
    pairwiseLLM:::.adaptive_select_rolling_anchors(c(a = 1), adaptive_defaults(2L)),
    "a"
  )

  one_band <- pairwiseLLM:::.adaptive_assign_strata(c(a = 1), adaptive_defaults(2L))
  expect_identical(unname(one_band$stratum_id), 1L)

  bad_phase_b <- state
  bad_phase_b$items <- bad_phase_b$items[bad_phase_b$items$set_id != 1L, , drop = FALSE]
  bad_phase_b$item_ids <- bad_phase_b$items$item_id
  bad_phase_b$set_ids <- bad_phase_b$items$set_id
  expect_error(
    pairwiseLLM:::generate_stage_candidates_from_state(
      state = bad_phase_b,
      stage_name = "anchor_link",
      fallback_name = "base",
      C_max = 10L,
      seed = 1L,
      link_spoke_id = 2L
    ),
    "no hub items found"
  )

  no_spoke <- state
  no_spoke$items <- no_spoke$items[no_spoke$items$set_id != 2L, , drop = FALSE]
  no_spoke$item_ids <- no_spoke$items$item_id
  no_spoke$set_ids <- no_spoke$items$set_id
  no_spoke$linking$phase_a$ready_spokes <- c(2L, 3L)
  expect_error(
    pairwiseLLM:::generate_stage_candidates_from_state(
      state = no_spoke,
      stage_name = "anchor_link",
      fallback_name = "base",
      C_max = 10L,
      seed = 1L,
      link_spoke_id = 2L
    ),
    "no spoke items found"
  )

  probe_state <- state
  probe_state$linking$probe <- list(
    panels_by_spoke = list(
      `2` = tibble::tibble(
        probe_panel_id = "panel",
        link_epoch_id = 3L,
        spoke_id = 2L,
        hub_item_id = "h1",
        spoke_item_id = "s21",
        spoke_bin = 1L,
        hub_bin = 1L,
        planned_rank = 1L,
        pair_key = make_unordered_key("h1", "s21"),
        realized = FALSE,
        realized_step_id = NA_integer_,
        realized_pair_id = NA_integer_,
        realized_run_mode = NA_character_
      )
    ),
    prediction_cache = pairwiseLLM:::.adaptive_link_probe_empty_cache(),
    realized_edges = pairwiseLLM:::.adaptive_link_probe_empty_realized_log(),
    collect_holdout_now_by_spoke = list(`2` = FALSE)
  )
  probe_state$controller$link_state_frozen_by_spoke <- list(`2` = FALSE)
  probe_state$controller$link_transform_frozen_by_spoke <- list(`2` = TRUE)
  pruned <- pairwiseLLM:::generate_stage_candidates_from_state(
    state = probe_state,
    stage_name = "anchor_link",
    fallback_name = "base",
    C_max = 50L,
    seed = 1L,
    link_spoke_id = 2L
  )
  expect_false(make_unordered_key("h1", "s21") %in% make_unordered_key(pruned$i, pruned$j))
})

test_that("remaining BTL builders cover input-contract edge branches", {
  expect_error(pairwiseLLM::build_btl_results_data(1L), "data frame or tibble")
  expect_error(
    pairwiseLLM::build_btl_results_data(
      tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
      phase = ""
    ),
    "`phase`"
  )
  expect_error(
    pairwiseLLM::build_btl_results_data(
      tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
      model = ""
    ),
    "`model`"
  )
  expect_error(
    pairwiseLLM::build_btl_results_data(
      tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
      iter_start = NA_real_
    ),
    "`iter_start`"
  )
  expect_error(
    pairwiseLLM::build_btl_results_data(
      tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
      received_at_start = Sys.time()[NA_integer_]
    ),
    "`received_at_start`"
  )
  expect_error(
    pairwiseLLM::build_btl_results_data(
      tibble::tibble(ID1 = "", ID2 = "B", better_id = "B")
    ),
    "ID1"
  )
  expect_error(
    pairwiseLLM::build_btl_results_data(
      tibble::tibble(ID1 = "A", ID2 = "", better_id = "A")
    ),
    "ID2"
  )
  expect_error(
    pairwiseLLM::build_btl_results_data(
      tibble::tibble(ID1 = "A", ID2 = "A", better_id = "A")
    ),
    "self-pairs"
  )
  expect_error(
    pairwiseLLM::build_btl_results_data(
      tibble::tibble(ID1 = "A", ID2 = "B", better_id = "")
    ),
    "better_id"
  )
  expect_error(
    pairwiseLLM::build_btl_results_data(
      tibble::tibble(ID1 = "A", ID2 = "B", better_id = "C")
    ),
    "must match `ID1` or `ID2`"
  )

  shared <- pairwiseLLM:::.btl_mcmc_inference_contract_from_results(
    tibble::tibble(),
    inference_contract = list(judge_scope_levels = "shared")
  )
  expect_identical(shared$judge_param_mode, "global_shared")
})

test_that("cached budget and ranked-spoke helpers match canonical refit reconstruction", {
  append_active_link_step <- function(state, step_id, A_id, B_id, spoke_id, stage_name) {
    out <- append_cross_probe_step(
      state = state,
      step_id = step_id,
      A_id = A_id,
      B_id = B_id,
      Y = 1L,
      spoke_id = spoke_id,
      is_probe_step = FALSE,
      run_mode = "link_multi_spoke"
    )
    idx <- nrow(out$step_log)
    out$step_log$round_stage[[idx]] <- as.character(stage_name)
    out$step_log$link_stage[[idx]] <- as.character(stage_name)
    out
  }

  state <- make_link_probe_state()
  state$linking$phase_a$ready_spokes <- c(2L, 3L)
  state$linking$phase_a$active_spokes <- c(2L, 3L)
  state$controller$multi_spoke_mode <- "concurrent"
  state$controller$min_cross_set_pairs_per_spoke_per_refit <- 2L

  state <- append_active_link_step(state, 1L, "h1", "s21", 2L, "anchor_link")
  state <- append_cross_probe_step(
    state = state,
    step_id = 2L,
    A_id = "h2",
    B_id = "s22",
    Y = 0L,
    spoke_id = 2L,
    is_probe_step = TRUE,
    run_mode = "link_probe_holdout"
  )
  state <- append_active_link_step(state, 3L, "h1", "s31", 3L, "anchor_link")
  state <- append_active_link_step(state, 4L, "h2", "s32", 3L, "long_link")
  state$refit_meta$last_refit_step <- 0L
  state <- pairwiseLLM:::.adaptive_link_refit_summary_rebuild_current(
    state,
    current_refit_id = 1L,
    spoke_ids = c(2L, 3L)
  )

  raw2 <- pairwiseLLM:::.adaptive_link_refit_summary_from_step_log(
    state = state,
    refit_id = 1L,
    spoke_id = 2L,
    refit_context = list(last_refit_step = 0L)
  )
  raw3 <- pairwiseLLM:::.adaptive_link_refit_summary_from_step_log(
    state = state,
    refit_id = 1L,
    spoke_id = 3L,
    refit_context = list(last_refit_step = 0L)
  )

  cached_only <- state
  cached_only$step_log$step_id[] <- 0L
  budget_map <- pairwiseLLM:::.adaptive_link_budget_map_for_refit(
    state = cached_only,
    controller = cached_only$controller,
    eligible_spoke_ids = c(2L, 3L)
  )
  ranked_spokes <- pairwiseLLM:::.adaptive_link_ranked_spokes(
    state = cached_only,
    controller = cached_only$controller,
    eligible_spoke_ids = c(2L, 3L)
  )

  expect_identical(
    budget_map[["2"]]$concurrent_floor_met,
    raw2$n_cross_edges_total_since_last_refit >= budget_map[["2"]]$concurrent_floor_pairs
  )
  expect_identical(
    budget_map[["2"]]$concurrent_target_met,
    raw2$n_cross_edges_total_since_last_refit >= budget_map[["2"]]$concurrent_target_pairs
  )
  expect_identical(
    budget_map[["3"]]$concurrent_floor_met,
    raw3$n_cross_edges_total_since_last_refit >= budget_map[["3"]]$concurrent_floor_pairs
  )
  expect_identical(
    budget_map[["3"]]$concurrent_target_met,
    raw3$n_cross_edges_total_since_last_refit >= budget_map[["3"]]$concurrent_target_pairs
  )
  expect_identical(as.integer(ranked_spokes[[1L]]), 2L)
})

test_that("feasibility snapshot and holdout ordering match history-state rebuilds", {
  state <- make_link_probe_state()
  state$linking$phase_a$ready_spokes <- c(2L, 3L)
  state$linking$phase_a$active_spokes <- c(2L, 3L)
  state$history_pairs <- tibble::tibble(
    A_id = c("h1", "s21"),
    B_id = c("s21", "h1")
  )
  state$history_state <- pairwiseLLM:::.adaptive_history_state_rebuild(
    state$history_pairs,
    state$item_ids
  )
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      pair_id = 1L,
      i = match("h1", state$item_ids),
      j = match("s21", state$item_ids),
      A = match("h1", state$item_ids),
      B = match("s21", state$item_ids),
      Y = 1L,
      set_i = 1L,
      set_j = 2L,
      is_cross_set = TRUE,
      link_spoke_id = 2L,
      run_mode = "link_multi_spoke",
      round_stage = "anchor_link",
      link_stage = "anchor_link",
      is_probe_step = FALSE
    )
  )

  snapshot_cached <- pairwiseLLM:::.adaptive_link_stage_feasibility_snapshot(
    state = state,
    controller = state$controller,
    spoke_id = 2L,
    stage_order = pairwiseLLM:::.adaptive_stage_order()
  )
  rebuilt_state <- state
  rebuilt_state$history_state <- NULL
  snapshot_rebuilt <- pairwiseLLM:::.adaptive_link_stage_feasibility_snapshot(
    state = rebuilt_state,
    controller = rebuilt_state$controller,
    spoke_id = 2L,
    stage_order = pairwiseLLM:::.adaptive_stage_order()
  )
  mass_cached <- pairwiseLLM:::.adaptive_link_spoke_utility_mass(
    state = state,
    controller = state$controller,
    spoke_id = 2L,
    top_k = 3L,
    seed = 11L
  )
  mass_rebuilt <- pairwiseLLM:::.adaptive_link_spoke_utility_mass(
    state = rebuilt_state,
    controller = rebuilt_state$controller,
    spoke_id = 2L,
    top_k = 3L,
    seed = 11L
  )

  expect_identical(snapshot_cached$feasible_counts, snapshot_rebuilt$feasible_counts)
  expect_equal(snapshot_cached$feasible_utility_mass, snapshot_rebuilt$feasible_utility_mass)
  expect_identical(snapshot_cached$candidate_count, snapshot_rebuilt$candidate_count)
  expect_equal(
    unlist(snapshot_cached$utility_values_by_stage, use.names = FALSE),
    unlist(snapshot_rebuilt$utility_values_by_stage, use.names = FALSE)
  )
  expect_equal(mass_cached$utility_mass, mass_rebuilt$utility_mass)
  expect_identical(mass_cached$top_k_used, mass_rebuilt$top_k_used)
  expect_identical(mass_cached$candidate_count, mass_rebuilt$candidate_count)

  state$linking$probe$panels_by_spoke <- list(
    `2` = tibble::tibble(
      probe_panel_id = "panel_holdout",
      link_epoch_id = 3L,
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

  holdout_cached <- pairwiseLLM:::.adaptive_link_probe_select_holdout(
    state,
    step_id = 2L,
    spoke_id = 2L
  )
  rebuilt_holdout_state <- state
  rebuilt_holdout_state$history_state <- NULL
  holdout_rebuilt <- pairwiseLLM:::.adaptive_link_probe_select_holdout(
    rebuilt_holdout_state,
    step_id = 2L,
    spoke_id = 2L
  )

  expect_identical(holdout_cached$A, holdout_rebuilt$A)
  expect_identical(holdout_cached$B, holdout_rebuilt$B)
  expect_identical(holdout_cached$deg_i, holdout_rebuilt$deg_i)
  expect_identical(holdout_cached$deg_j, holdout_rebuilt$deg_j)
  expect_identical(holdout_cached$recent_deg_i, holdout_rebuilt$recent_deg_i)
  expect_identical(holdout_cached$recent_deg_j, holdout_rebuilt$recent_deg_j)
})

test_that("feasibility-capacity summary is memoized and reused by utility-mass ranking", {
  state <- make_link_probe_state()
  state$linking$phase_a$ready_spokes <- c(2L, 3L)
  state$linking$phase_a$active_spokes <- c(2L, 3L)
  state$history_pairs <- tibble::tibble(A_id = character(), B_id = character())
  state$history_state <- pairwiseLLM:::.adaptive_history_state_rebuild(
    state$history_pairs,
    state$item_ids
  )

  calls <- new.env(parent = emptyenv())
  calls$generate <- 0L
  calls$attach <- 0L

  summary_and_mass <- testthat::with_mocked_bindings(
    generate_stage_candidates_from_state = function(state, stage_name, fallback_name, C_max, seed,
                                                    link_spoke_id = NA_integer_) {
      calls$generate <- as.integer(calls$generate) + 1L
      n <- switch(stage_name,
        anchor_link = 1L,
        long_link = 2L,
        mid_link = 3L,
        local_link = 4L
      )
      tibble::tibble(
        i = rep("h1", n),
        j = paste0("s", stage_name, seq_len(n))
      )
    },
    .adaptive_link_attach_predictive_utility = function(candidates, state, controller, spoke_id) {
      calls$attach <- as.integer(calls$attach) + 1L
      cand <- tibble::as_tibble(candidates)
      base <- seq(from = nrow(cand), to = 1, by = -1)
      cand$link_d_opt_gain <- as.double(base)
      cand$link_u <- as.double(base)
      cand
    },
    {
      first <- pairwiseLLM:::.adaptive_link_stage_feasibility_snapshot(
        state = state,
        controller = state$controller,
        spoke_id = 2L,
        stage_order = pairwiseLLM:::.adaptive_stage_order(),
        seed_base = 11L,
        seed_stride = 1L
      )
      second <- pairwiseLLM:::.adaptive_link_stage_feasibility_snapshot(
        state = state,
        controller = state$controller,
        spoke_id = 2L,
        stage_order = pairwiseLLM:::.adaptive_stage_order(),
        seed_base = 11L,
        seed_stride = 1L
      )
      mass <- pairwiseLLM:::.adaptive_link_spoke_utility_mass(
        state = state,
        controller = state$controller,
        spoke_id = 2L,
        top_k = 3L,
        seed = 11L
      )
      list(first = first, second = second, mass = mass)
    },
    .package = "pairwiseLLM"
  )

  expect_identical(as.integer(calls$generate), 4L)
  expect_identical(as.integer(calls$attach), 4L)
  expect_identical(summary_and_mass$first$feasible_counts, summary_and_mass$second$feasible_counts)
  expect_identical(summary_and_mass$mass$candidate_count, 10L)
  expect_identical(summary_and_mass$mass$top_k_used, 3L)
  expect_equal(summary_and_mass$mass$utility_mass, 10)
})

test_that("round log row cache-backed summaries match canonical reconstruction", {
  append_active_link_step <- function(state, step_id, A_id, B_id, spoke_id, stage_name) {
    out <- append_cross_probe_step(
      state = state,
      step_id = step_id,
      A_id = A_id,
      B_id = B_id,
      Y = 1L,
      spoke_id = spoke_id,
      is_probe_step = FALSE,
      run_mode = "link_multi_spoke"
    )
    idx <- nrow(out$step_log)
    out$step_log$round_stage[[idx]] <- as.character(stage_name)
    out$step_log$link_stage[[idx]] <- as.character(stage_name)
    out
  }

  state <- make_link_probe_state()
  state$linking$phase_a$ready_spokes <- c(2L, 3L)
  state$linking$phase_a$active_spokes <- c(2L, 3L)
  state$history_pairs <- tibble::tibble(
    A_id = c("h1", "h1", "h2"),
    B_id = c("s21", "s31", "s32")
  )
  state$history_state <- pairwiseLLM:::.adaptive_history_state_rebuild(
    state$history_pairs,
    state$item_ids
  )
  state <- append_active_link_step(state, 1L, "h1", "s21", 2L, "anchor_link")
  state <- append_cross_probe_step(
    state = state,
    step_id = 2L,
    A_id = "h2",
    B_id = "s22",
    Y = 0L,
    spoke_id = 2L,
    is_probe_step = TRUE,
    run_mode = "link_probe_holdout"
  )
  state <- append_active_link_step(state, 3L, "h1", "s31", 3L, "anchor_link")
  state <- append_active_link_step(state, 4L, "h2", "s32", 3L, "long_link")
  state$refit_meta$last_refit_step <- 0L
  state <- pairwiseLLM:::.adaptive_link_refit_summary_rebuild_current(
    state,
    current_refit_id = 1L,
    spoke_ids = c(2L, 3L)
  )

  refit_context <- list(
    step_id_at_refit = 4L,
    timestamp = as.POSIXct("2026-01-01 00:04:00", tz = "UTC"),
    last_refit_M_done = 0L,
    last_refit_step = 0L
  )
  row_cached <- suppressWarnings(
    pairwiseLLM:::.adaptive_round_log_row(
      state = state,
      metrics = list(diagnostics_pass = TRUE),
      stop_decision = FALSE,
      stop_reason = NA_character_,
      refit_context = refit_context,
      config = state$config$btl_config
    )
  )

  uncached_state <- state
  uncached_state$history_state <- NULL
  uncached_state$refit_meta$link_refit_summary_cache_by_refit_spoke <- list()
  uncached_state$refit_meta$link_unique_cross_pair_keys_by_spoke <- list()
  row_rebuilt <- suppressWarnings(
    pairwiseLLM:::.adaptive_round_log_row(
      state = uncached_state,
      metrics = list(diagnostics_pass = TRUE),
      stop_decision = FALSE,
      stop_reason = NA_character_,
      refit_context = refit_context,
      config = uncached_state$config$btl_config
    )
  )

  compare_cols <- c(
    "mean_degree",
    "min_degree",
    "mean_degree_scope",
    "min_degree_scope",
    "pos_balance_sd",
    "n_unique_pairs_seen",
    "new_pairs_since_last_refit",
    "new_active_pairs_since_last_refit",
    "new_probe_pairs_since_last_refit",
    "new_total_cross_pairs_since_last_refit",
    "recent_deg_median_since_last_refit",
    "recent_deg_max_since_last_refit"
  )
  expect_identical(row_cached[compare_cols], row_rebuilt[compare_cols])
})

test_that("routing, probe-panel, and candidate helper guards cover remaining branches", {
  state <- make_link_probe_state()

  controller_aj <- utils::modifyList(
    state$controller,
    list(link_estimation_mode = "anchored_joint")
  )
  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_anchored_joint_artifact_copy_init = function(...) {
        list(theta_spoke_global_mean = c(s21 = NA_real_, s22 = -0.2))
      },
      pairwiseLLM:::.adaptive_link_phase_b_routing_scores(
        state = state,
        controller = controller_aj,
        active_ids = c("s21", "s22"),
        hub_id = 1L
      ),
      .package = "pairwiseLLM"
    ),
    "accepted spoke scores missing/non-finite"
  )

  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_link_phase_a_theta_map = function(state, set_id, field) {
        c(h1 = 0.2, h2 = 0.1, h3 = 0)
      },
      pairwiseLLM:::.adaptive_link_phase_b_routing_scores(
        state = state,
        controller = state$controller,
        active_ids = c("h1", "missing_item"),
        hub_id = 1L
      ),
      .package = "pairwiseLLM"
    ),
    "Phase A theta_raw_mean missing/non-finite for set_id=1"
  )

  empty_hub_anchors <- testthat::with_mocked_bindings(
    .adaptive_select_rolling_anchors = function(scores, defaults) character(),
    pairwiseLLM:::.adaptive_link_phase_b_hub_anchors(
      state = state,
      hub_ids = c("h1", "h2"),
      hub_scores = c(h1 = 2, h2 = 1),
      defaults = pairwiseLLM:::adaptive_defaults(4L)
    ),
    .package = "pairwiseLLM"
  )
  expect_identical(empty_hub_anchors, character())

  state$round$per_round_item_uses <- c(h1 = 1L, h2 = 1L)
  saturated_hub_anchors <- testthat::with_mocked_bindings(
    .adaptive_select_rolling_anchors = function(scores, defaults) "h1",
    .adaptive_rank_index_from_scores = function(scores) c(h1 = 1L, h2 = 2L),
    pairwiseLLM:::.adaptive_link_phase_b_hub_anchors(
      state = state,
      hub_ids = c("h1", "h2"),
      hub_scores = c(h1 = 2, h2 = 1),
      defaults = pairwiseLLM:::adaptive_defaults(4L)
    ),
    .package = "pairwiseLLM"
  )
  expect_identical(saturated_hub_anchors, "h1")

  expect_error(
    pairwiseLLM:::.adaptive_link_spoke_coverage(
      state = list(step_log = tibble::tibble(), refit_meta = list()),
      controller = list(
        spoke_quantile_coverage_bins = 2L,
        spoke_quantile_coverage_min_per_bin_per_refit = 1L
      ),
      spoke_id = 2L,
      spoke_ids = c("s1", "s2"),
      routing_scores = c(s1 = 0.2, s2 = NA_real_)
    ),
    "routing scores must be finite"
  )

  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_link_phase_b_hub_anchors = function(...) character(),
      pairwiseLLM:::.adaptive_link_probe_construct_panel(
        state = state,
        controller = state$controller,
        spoke_id = 2L
      ),
      .package = "pairwiseLLM"
    ),
    "HubEligible` anchor pool is empty"
  )

  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_link_probe_panel_size = function(...) 0L,
      pairwiseLLM:::.adaptive_link_probe_construct_panel(
        state = state,
        controller = state$controller,
        spoke_id = 2L
      ),
      .package = "pairwiseLLM"
    ),
    "legal held-out probe candidates exist"
  )

  state_no_legal <- make_link_probe_state()
  state_no_legal$controller$hub_anchor_required_phase_b <- FALSE
  all_pairs <- expand.grid(
    hub = c("h1", "h2", "h3"),
    spoke = c("s21", "s22"),
    stringsAsFactors = FALSE
  )
  for (idx in seq_len(nrow(all_pairs))) {
    state_no_legal <- append_cross_probe_step(
      state = state_no_legal,
      step_id = idx,
      A_id = all_pairs$hub[[idx]],
      B_id = all_pairs$spoke[[idx]],
      Y = 1L,
      spoke_id = 2L,
      is_probe_step = FALSE,
      run_mode = "link_multi_spoke"
    )
  }
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_construct_panel(
      state = state_no_legal,
      controller = state_no_legal$controller,
      spoke_id = 2L
    ),
    pairwiseLLM:::.adaptive_link_probe_empty_panel()
  )

  state_not_link <- make_link_probe_state()
  state_not_link$controller$run_mode <- "within_set"
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_ensure_panels(
      state_not_link,
      controller = state_not_link$controller,
      spoke_ids = 2L
    ),
    state_not_link
  )

  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_ensure_panels(
      state,
      controller = state$controller,
      spoke_ids = integer()
    ),
    state
  )

  make_resume_panel <- function(panel_id = "panel-built",
                                planned_edges = 1L,
                                pair_key = "pair-built") {
    tibble::tibble(
      probe_panel_id = panel_id,
      link_epoch_id = 3L,
      spoke_id = 2L,
      hub_item_id = "h1",
      spoke_item_id = "s21",
      spoke_bin = 1L,
      hub_bin = 1L,
      probe_edges_planned = as.integer(planned_edges),
      probe_panel_reallocation_used = FALSE,
      planned_rank = 1L,
      pair_key = pair_key,
      realized = FALSE,
      realized_step_id = NA_integer_,
      realized_pair_id = NA_integer_,
      realized_run_mode = NA_character_
    )
  }

  make_resume_state <- function(stage_panel_id = "panel-built",
                                stage_planned_edges = 1L,
                                realized_panel_id = NULL,
                                realized_pair_key = "pair-built") {
    out <- make_link_probe_state()
    out$meta$resumed_from_session <- TRUE
    out$linking$probe <- pairwiseLLM:::.adaptive_link_probe_empty_state()
    out$link_stage_log <- pairwiseLLM:::append_link_stage_log(
      pairwiseLLM:::new_link_stage_log(),
      list(
        refit_id = 1L,
        spoke_id = 2L,
        hub_id = 1L,
        link_transform_policy = "auto",
        link_transform_state = "shift_only",
        link_stop_pass = FALSE,
        link_state_frozen = FALSE,
        link_epoch_id = 3L,
        probe_panel_id = as.character(stage_panel_id),
        probe_edges_planned = as.integer(stage_planned_edges),
        probe_edges_realized = if (is.null(realized_panel_id)) 0L else 1L
      )
    )
    if (!is.null(realized_panel_id)) {
      out$linking$probe$realized_edges <- dplyr::bind_rows(
        pairwiseLLM:::.adaptive_link_probe_empty_realized_log(),
        tibble::tibble(
          step_id = 1L,
          pair_id = 1L,
          run_mode = "link_probe_holdout",
          spoke_id = 2L,
          link_epoch_id = 3L,
          probe_panel_id = as.character(realized_panel_id),
          hub_item_id = "h1",
          spoke_item_id = "s21",
          pair_key = as.character(realized_pair_key),
          Y = 1L
        )
      )
      out$linking$probe$realized_index_by_panel <-
        pairwiseLLM:::.adaptive_link_probe_realized_index_build(
          out$linking$probe$realized_edges
        )
    }
    out
  }

  built_panel <- make_resume_panel()

  empty_resume <- make_resume_state(stage_panel_id = "panel-built", stage_planned_edges = 1L)
  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_link_probe_construct_panel = function(...) tibble::tibble(),
      pairwiseLLM:::.adaptive_link_probe_ensure_panels(
        empty_resume,
        controller = empty_resume$controller,
        spoke_ids = 2L
      ),
      .package = "pairwiseLLM"
    ),
    "deterministic reconstruction also failed"
  )

  realized_id_mismatch <- make_resume_state(
    stage_panel_id = "panel-built",
    stage_planned_edges = 2L,
    realized_panel_id = "legacy-panel",
    realized_pair_key = "pair-built"
  )
  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_link_probe_construct_panel = function(...) built_panel,
      pairwiseLLM:::.adaptive_link_probe_ensure_panels(
        realized_id_mismatch,
        controller = realized_id_mismatch$controller,
        spoke_ids = 2L
      ),
      .package = "pairwiseLLM"
    ),
    "realized_edges\\$probe_panel_id"
  )

  realized_pair_mismatch <- make_resume_state(
    stage_panel_id = "panel-built",
    stage_planned_edges = 1L,
    realized_panel_id = "panel-built",
    realized_pair_key = "pair-other"
  )
  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_link_probe_construct_panel = function(...) built_panel,
      pairwiseLLM:::.adaptive_link_probe_ensure_panels(
        realized_pair_mismatch,
        controller = realized_pair_mismatch$controller,
        spoke_ids = 2L
      ),
      .package = "pairwiseLLM"
    ),
    "does not contain all canonical realized probe edges"
  )

  planned_size_mismatch <- make_resume_state(
    stage_panel_id = "panel-built",
    stage_planned_edges = 2L,
    realized_panel_id = "panel-built",
    realized_pair_key = "pair-built"
  )
  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_link_probe_construct_panel = function(...) built_panel,
      pairwiseLLM:::.adaptive_link_probe_ensure_panels(
        planned_size_mismatch,
        controller = planned_size_mismatch$controller,
        spoke_ids = 2L
      ),
      .package = "pairwiseLLM"
    ),
    "probe_edges_planned"
  )

  ranked <- testthat::with_mocked_bindings(
    .adaptive_link_effective_active_spokes = function(...) c(2L, 3L),
    .adaptive_link_refit_summary_current = function(state, refit_id, spoke_id, refit_context) {
      list(
        n_cross_edges_active_since_last_refit =
          if (as.integer(spoke_id) == 2L) 4L else 1L
      )
    },
    pairwiseLLM:::.adaptive_link_ranked_spokes(
      state = make_link_probe_state(),
      controller = make_link_probe_state()$controller,
      eligible_spoke_ids = c(2L, 3L)
    ),
    .package = "pairwiseLLM"
  )
  expect_identical(ranked, c(3L, 2L))

  phase_b_abort <- adaptive_rank_start(
    tibble::tibble(
      item_id = c("h1", "h2", "s1", "s2"),
      set_id = c(1L, 1L, 2L, 2L),
      global_item_id = c("gh1", "gh2", "gs1", "gs2")
    ),
    seed = 801L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_link_phase_context = function(state, controller = NULL) {
        list(phase = "phase_b", active_spokes = integer(), hub_id = 1L)
      },
      pairwiseLLM:::generate_stage_candidates_from_state(
        state = phase_b_abort,
        stage_name = "anchor_link",
        fallback_name = "base",
        C_max = 10L,
        seed = 1L
      ),
      .package = "pairwiseLLM"
    ),
    "no ready spokes are eligible"
  )

  phase_b_empty <- testthat::with_mocked_bindings(
    .adaptive_link_phase_context = function(state, controller = NULL) {
      list(phase = "phase_b", active_spokes = 2L, hub_id = 1L)
    },
    .adaptive_link_active_spoke = function(...) NA_integer_,
    pairwiseLLM:::generate_stage_candidates_from_state(
      state = phase_b_abort,
      stage_name = "anchor_link",
      fallback_name = "base",
      C_max = 10L,
      seed = 1L
    ),
    .package = "pairwiseLLM"
  )
  expect_identical(phase_b_empty, tibble::tibble(i = character(), j = character()))

  phase_a_missing <- testthat::with_mocked_bindings(
    .adaptive_link_phase_context = function(state, controller = NULL) {
      list(phase = "phase_a", active_phase_a_set = NA_integer_, hub_id = 1L)
    },
    pairwiseLLM:::generate_stage_candidates_from_state(
      state = phase_b_abort,
      stage_name = "local_link",
      fallback_name = "base",
      C_max = 10L,
      seed = 1L
    ),
    .package = "pairwiseLLM"
  )
  expect_identical(phase_a_missing, tibble::tibble(i = character(), j = character()))

  phase_a_small <- adaptive_rank_start(
    tibble::tibble(
      item_id = c("h1", "h2", "s1"),
      set_id = c(1L, 1L, 2L),
      global_item_id = c("gh1", "gh2", "gs1")
    ),
    seed = 802L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  phase_a_empty <- testthat::with_mocked_bindings(
    .adaptive_link_phase_context = function(state, controller = NULL) {
      list(phase = "phase_a", active_phase_a_set = 2L, hub_id = 1L)
    },
    pairwiseLLM:::generate_stage_candidates_from_state(
      state = phase_a_small,
      stage_name = "local_link",
      fallback_name = "base",
      C_max = 10L,
      seed = 1L
    ),
    .package = "pairwiseLLM"
  )
  expect_identical(phase_a_empty, tibble::tibble(i = character(), j = character()))

  within_state <- adaptive_rank_start(
    tibble::tibble(
      item_id = c("a", "b", "c"),
      set_id = c(1L, 1L, 1L),
      global_item_id = c("ga", "gb", "gc")
    ),
    seed = 803L
  )
  within_state$round$anchor_ids <- character()
  tracker <- new.env(parent = emptyenv())
  tracker$calls <- 0L
  fallback_candidates <- testthat::with_mocked_bindings(
    .adaptive_select_rolling_anchors = function(scores, defaults) {
      tracker$calls <- tracker$calls + 1L
      "a"
    },
    pairwiseLLM:::generate_stage_candidates_from_state(
      state = within_state,
      stage_name = "anchor_link",
      fallback_name = "base",
      C_max = 10L,
      seed = 1L
    ),
    .package = "pairwiseLLM"
  )
  expect_identical(tracker$calls, 1L)
  expect_true(nrow(fallback_candidates) > 0L)
})
