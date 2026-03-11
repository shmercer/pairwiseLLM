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
      hub_id = 1L
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
  expect_true(is.na(surface$hub_lock_kappa))

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

  next_spoke <- pairwiseLLM:::.adaptive_link_probe_next_holdout_spoke(
    ensured,
    controller = ensured$controller,
    eligible_spoke_ids = 2L
  )
  expect_true(is.na(next_spoke))

  ensured$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    pairwiseLLM:::new_link_stage_log(),
    list(
      refit_id = 1L,
      spoke_id = 2L,
      hub_id = 1L,
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_stop_pass = FALSE,
      transform_frozen = FALSE
    )
  )
  ensured$refit_meta$refit_pairs_target_current <- 3L
  ensured$controller$refit_pairs_target <- 3L
  ensured$controller$probe_pairs_per_refit_per_spoke <- 1L
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
    link_u = c(0.1, 0.1)
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
      transform_frozen = FALSE
    )
  )

  plan0 <- pairwiseLLM:::.adaptive_link_probe_effort_plan(
    state = state,
    controller = state$controller,
    spoke_id = 2L
  )
  expect_identical(plan0$base_cap, 1L)
  expect_identical(plan0$effective_cap, 3L)
  expect_true(isTRUE(plan0$acceleration_used))

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
  expect_identical(plan1$effective_cap, 3L)
  expect_identical(plan1$realized_refit, 1L)
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_next_holdout_spoke(
      step1,
      controller = step1$controller,
      eligible_spoke_ids = 2L
    ),
    2L
  )

  step2 <- append_cross_probe_step(step1, 12L, "h2", "s21", 0L, 2L)
  step2 <- pairwiseLLM:::.adaptive_link_probe_register_commit(
    step2,
    tibble::tibble(
      step_id = 12L,
      pair_id = 12L,
      A = match("h2", step2$item_ids),
      B = match("s21", step2$item_ids),
      Y = 0L,
      run_mode = "link_probe_holdout",
      link_spoke_id = 2L,
      is_probe_step = TRUE
    )
  )
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_next_holdout_spoke(
      step2,
      controller = step2$controller,
      eligible_spoke_ids = 2L
    ),
    2L
  )

  step3 <- append_cross_probe_step(step2, 13L, "h3", "s22", 1L, 2L)
  step3 <- pairwiseLLM:::.adaptive_link_probe_register_commit(
    step3,
    tibble::tibble(
      step_id = 13L,
      pair_id = 13L,
      A = match("h3", step3$item_ids),
      B = match("s22", step3$item_ids),
      Y = 1L,
      run_mode = "link_probe_holdout",
      link_spoke_id = 2L,
      is_probe_step = TRUE
    )
  )
  expect_true(is.na(pairwiseLLM:::.adaptive_link_probe_next_holdout_spoke(
    step3,
    controller = step3$controller,
    eligible_spoke_ids = 2L
  )))
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
  stage_rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 2L,
    refit_context = list(last_refit_step = 0L)
  )
  row_stage <- stage_rows[stage_rows$spoke_id == 2L, , drop = FALSE]
  expect_true(isTRUE(row_stage$probe_panel_reallocation_used[[1L]]))
  expect_identical(row_stage$n_probe_pairs_since_last_refit[[1L]], 2L)

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
  rmse <- pairwiseLLM:::.adaptive_link_theta_global_rmse_lagged(
    state,
    spoke_id = 2L,
    hub_id = 1L,
    scope_ids = c("h1", "s21"),
    transform_mode = "shift_only",
    delta_mean = 0.2,
    log_alpha_mean = NA_real_,
    lag_row = lag_row
  )
  expect_true(is.finite(rmse))

  old_row <- tibble::tibble(
    link_stop_eligible = TRUE,
    reliability_stop_pass = TRUE,
    delta_sd_pass = TRUE,
    log_alpha_sd_pass = NA,
    delta_change_pass = TRUE,
    log_alpha_change_pass = NA,
    rank_stability_pass = TRUE
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

  expect_true(is.finite(pairwiseLLM:::.adaptive_link_probe_brier_for_fit(
    edges = edges,
    hub_theta = c(h1 = 0.8, h2 = 0.4),
    spoke_theta = c(s21 = -0.2, s22 = -0.4),
    delta_mean = 0.1
  )))
  expect_true(is.finite(pairwiseLLM:::.adaptive_link_probe_pred_rmse_lagged_for_fit(
    edges = edges,
    hub_theta = c(h1 = 0.8, h2 = 0.4),
    spoke_theta = c(s21 = -0.2, s22 = -0.4),
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
  state$controller$link_transform_frozen_by_spoke <- list(`2` = TRUE)
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
    tibble::tibble(i = c("b", "a"), j = c("z", "y"), u0 = c(NA_real_, NA_real_))
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
    link_transform_policy = "auto",
    link_transform_state = "shift_only",
    link_refit_mode = "shift_only",
    hub_lock_mode = "hard_lock",
    reliability_EAP_link = 0.9,
    linking_identified = TRUE,
    link_stop_eligible = TRUE,
    link_stop_pass = FALSE,
    transform_frozen = FALSE,
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
    stage_budget_unfilled = 0L
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
  frozen_na$transform_frozen <- NA
  expect_error(
    pairwiseLLM:::.adaptive_assert_link_stage_rows_completeness(frozen_na),
    "`transform_frozen` must be populated"
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
  probe_state$controller$link_transform_frozen_by_spoke <- list(`2` = FALSE)
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
