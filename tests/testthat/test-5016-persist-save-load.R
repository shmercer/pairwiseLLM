make_probe_resume_state <- function() {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22"),
    set_id = c(1L, 1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22")
  )
  state <- adaptive_rank_start(
    items,
    seed = 61L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("ready", "ready"),
      validation_message = c("ok", "ok"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(
      `1` = list(items = tibble::tibble(
        global_item_id = c("gh1", "gh2", "gh3"),
        theta_raw_mean = c(0.5, 0, -0.5),
        theta_raw_sd = c(0.1, 0.1, 0.1),
        rank_mu_raw = c(1, 2, 3)
      )),
      `2` = list(items = tibble::tibble(
        global_item_id = c("gs21", "gs22"),
        theta_raw_mean = c(0.2, -0.2),
        theta_raw_sd = c(0.1, 0.1),
        rank_mu_raw = c(1, 2)
      ))
    ),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L),
    set_stop_pass_by_set = list(`1` = TRUE, `2` = TRUE),
    phase = "phase_b",
    ready_spokes = 2L,
    active_phase_a_set = NA_integer_,
    phase_b_started_at_step = 1L
  )
  state$refit_meta$refit_pairs_target_current <- 3L
  state$controller$refit_pairs_target <- 3L
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    pairwiseLLM:::new_link_stage_log(),
    list(
      refit_id = 1L,
      spoke_id = 2L,
      hub_id = 1L,
      link_estimation_mode = "transform",
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_refit_mode = "shift_only",
      hub_lock_mode = "soft_lock",
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )
  state
}

make_anchored_joint_resume_state <- function() {
  items <- tibble::tibble(
    item_id = c("a1", "a2", "b1", "b2"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("g_a1", "g_a2", "g_b1", "g_b2")
  )
  state <- adaptive_rank_start(
    items,
    seed = 71L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      link_estimation_mode = "anchored_joint",
      hub_lock_mode = "hard_lock"
    )
  )
  draws <- matrix(
    c(
      1.0, 0.8, -0.5, -0.7,
      1.1, 0.9, -0.4, -0.6,
      1.2, 1.0, -0.3, -0.5,
      0.9, 0.7, -0.6, -0.8
    ),
    nrow = 4,
    byrow = TRUE
  )
  colnames(draws) <- as.character(state$item_ids)
  state$btl_fit <- make_test_btl_fit(state$item_ids, draws = draws, model_variant = "btl_e_b")
  t0 <- as.POSIXct("2026-01-02 00:00:00", tz = "UTC")
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = t0,
      pair_id = 1L,
      A = 1L,
      B = 2L,
      Y = 1L,
      set_i = 1L,
      set_j = 1L,
      is_cross_set = FALSE,
      run_mode = "within_set"
    )
  )
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 2L,
      timestamp = t0 + 1,
      pair_id = 2L,
      A = 3L,
      B = 4L,
      Y = 0L,
      set_i = 2L,
      set_j = 2L,
      is_cross_set = FALSE,
      run_mode = "within_set"
    )
  )
  state$history_pairs <- tibble::tibble(
    A_id = c("a1", "b1"),
    B_id = c("a2", "b2")
  )
  state$round_log <- pairwiseLLM:::append_round_log(
    state$round_log,
    list(
      refit_id = 1L,
      round_id_at_refit = 1L,
      step_id_at_refit = 1L,
      diagnostics_pass = TRUE,
      phase_scope = "phase_a_set",
      phase_scope_set_id = 1L
    )
  )
  state$round_log <- pairwiseLLM:::append_round_log(
    state$round_log,
    list(
      refit_id = 2L,
      round_id_at_refit = 2L,
      step_id_at_refit = 2L,
      diagnostics_pass = TRUE,
      phase_scope = "phase_a_set",
      phase_scope_set_id = 2L
    )
  )
  art1 <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, set_id = 1L)
  art2 <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, set_id = 2L)
  art1$quality_gate_accepted <- TRUE
  art2$quality_gate_accepted <- TRUE
  state <- pairwiseLLM:::.adaptive_apply_controller_config(
    state,
    adaptive_config = list(phase_a_artifacts = list(`1` = art1, `2` = art2))
  )
  pairwiseLLM:::.adaptive_phase_a_prepare(state)
}

test_that("save_adaptive_session and load_adaptive_session round-trip adaptive artifacts", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  judge <- make_deterministic_judge("i_wins")

  withr::local_seed(1)
  state <- adaptive_rank_run_live(state, judge, n_steps = 2L, progress = "none")

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  expect_true(file.exists(file.path(session_dir, "state.rds")))
  expect_true(file.exists(file.path(session_dir, "step_log.rds")))
  expect_true(file.exists(file.path(session_dir, "round_log.rds")))
  expect_true(file.exists(file.path(session_dir, "metadata.rds")))

  reloaded <- load_adaptive_session(session_dir)
  expect_equal(reloaded$item_ids, state$item_ids)
  expect_equal(nrow(reloaded$step_log), nrow(state$step_log))
  expect_equal(reloaded$meta$schema_version, "adaptive-session")

  file.remove(file.path(session_dir, "state.rds"))
  expect_error(
    load_adaptive_session(session_dir),
    "missing required artifacts"
  )
})

test_that("load_adaptive_session rejects malformed schema metadata", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  meta_path <- file.path(session_dir, "metadata.rds")
  metadata <- readRDS(meta_path)
  metadata$schema_version <- ""
  saveRDS(metadata, meta_path)

  expect_error(
    load_adaptive_session(session_dir),
    "schema_version"
  )
})

test_that("save_adaptive_session overwrite removes stale optional artifacts", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  state$btl_fit <- list(theta = rep(0, 4))
  state$config$persist_item_log <- TRUE
  state$item_log <- list(tibble::tibble(step_id = 1L))

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  expect_true(file.exists(file.path(session_dir, "btl_fit.rds")))
  expect_true(dir.exists(file.path(session_dir, "item_log")))

  state_overwrite <- state
  state_overwrite$btl_fit <- NULL
  state_overwrite$config$persist_item_log <- FALSE

  save_adaptive_session(state_overwrite, session_dir, overwrite = TRUE)

  expect_false(file.exists(file.path(session_dir, "btl_fit.rds")))
  expect_false(dir.exists(file.path(session_dir, "item_log")))

  reloaded <- load_adaptive_session(session_dir)
  expect_null(reloaded$btl_fit)
  expect_false(isTRUE(reloaded$config$persist_item_log))
})

test_that("load_adaptive_session rejects step rows with partial item indices", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  judge <- make_deterministic_judge("i_wins")

  withr::local_seed(1)
  state <- adaptive_rank_run_live(state, judge, n_steps = 1L, progress = "none")

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  step_path <- file.path(session_dir, "step_log.rds")
  step_log <- readRDS(step_path)
  step_log$A[[1]] <- NA_integer_
  saveRDS(step_log, step_path)

  expect_error(
    load_adaptive_session(session_dir),
    "incomplete item indices"
  )
})

test_that("validate_session_dir rejects step_log schema drift (missing/extra/order)", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)

  session_missing <- withr::local_tempdir()
  save_adaptive_session(state, session_missing)
  step_path <- file.path(session_missing, "step_log.rds")
  step_log <- readRDS(step_path)
  step_log$explore_rate_used <- NULL
  saveRDS(step_log, step_path)
  expect_error(
    validate_session_dir(session_missing),
    "missing required columns"
  )

  session_extra <- withr::local_tempdir()
  save_adaptive_session(state, session_extra)
  step_path <- file.path(session_extra, "step_log.rds")
  step_log <- readRDS(step_path)
  step_log$unexpected_col <- 1L
  saveRDS(step_log, step_path)
  expect_error(
    validate_session_dir(session_extra),
    "unexpected columns"
  )

  session_order <- withr::local_tempdir()
  save_adaptive_session(state, session_order)
  step_path <- file.path(session_order, "step_log.rds")
  step_log <- readRDS(step_path)
  reordered <- c(names(step_log)[2L], names(step_log)[1L], names(step_log)[-c(1L, 2L)])
  step_log <- step_log[, reordered, drop = FALSE]
  saveRDS(step_log, step_path)
  expect_error(
    validate_session_dir(session_order),
    "column order does not match canonical schema"
  )
})

test_that("validate_session_dir rejects round_log schema drift for quota fields", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)
  round_path <- file.path(session_dir, "round_log.rds")
  round_log <- readRDS(round_path)
  round_log$long_quota_raw <- NULL
  saveRDS(round_log, round_path)

  expect_error(
    validate_session_dir(session_dir),
    "missing required columns"
  )
})

test_that("load_adaptive_session backfills legacy round_log post-stop columns", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  judge <- make_deterministic_judge("i_wins")

  withr::local_seed(1)
  state <- adaptive_rank_run_live(state, judge, n_steps = 2L, progress = "none")

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  round_path <- file.path(session_dir, "round_log.rds")
  round_log <- readRDS(round_path)
  round_log$max_pairs_after_stop <- NULL
  round_log$pairs_committed_after_stop <- NULL
  saveRDS(round_log, round_path)

  restored <- load_adaptive_session(session_dir)
  expect_true(all(c("max_pairs_after_stop", "pairs_committed_after_stop") %in% names(restored$round_log)))
  expect_true(is.integer(restored$round_log$max_pairs_after_stop))
  expect_true(is.integer(restored$round_log$pairs_committed_after_stop))
  expect_true(all(restored$round_log$max_pairs_after_stop == 0L))
  expect_true(all(restored$round_log$pairs_committed_after_stop == 0L))
})

test_that("load_adaptive_session reconciles refit boundaries and committed history from canonical logs", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items, seed = 31L)
  judge <- make_deterministic_judge("i_wins")
  fit_stub <- make_deterministic_fit_fn(state$item_ids)

  withr::local_seed(1)
  state <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 4L,
    fit_fn = fit_stub$fit_fn,
    btl_config = list(refit_pairs_target = 2L, stability_lag = 1L),
    progress = "none"
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  state_path <- file.path(session_dir, "state.rds")
  round_path <- file.path(session_dir, "round_log.rds")
  step_path <- file.path(session_dir, "step_log.rds")

  stale_state <- readRDS(state_path)
  stale_state$history_pairs <- tibble::tibble(A_id = character(), B_id = character())
  stale_state$refit_meta$last_refit_M_done <- 1L
  stale_state$refit_meta$last_refit_step <- 1L
  stale_state$refit_meta$last_refit_round_id <- 1L
  saveRDS(stale_state, state_path)

  round_log <- readRDS(round_path)
  step_log <- readRDS(step_path)
  committed_at_last_refit <- sum(
    !is.na(step_log$pair_id) &
      step_log$step_id <= round_log$step_id_at_refit[[nrow(round_log)]]
  )

  restored <- load_adaptive_session(session_dir)
  expect_identical(
    restored$refit_meta$last_refit_step,
    as.integer(round_log$step_id_at_refit[[nrow(round_log)]])
  )
  expect_identical(
    restored$refit_meta$last_refit_round_id,
    as.integer(round_log$refit_id[[nrow(round_log)]])
  )
  expect_identical(restored$refit_meta$last_refit_M_done, as.integer(committed_at_last_refit))
  expect_identical(
    nrow(restored$history_pairs),
    as.integer(sum(!is.na(step_log$pair_id)))
  )
  expect_history_state_matches_history(restored)
})

test_that("load_adaptive_session aborts when canonical round totals do not reconcile to committed steps", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items, seed = 32L)
  judge <- make_deterministic_judge("i_wins")
  fit_stub <- make_deterministic_fit_fn(state$item_ids)

  withr::local_seed(1)
  state <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 4L,
    fit_fn = fit_stub$fit_fn,
    btl_config = list(refit_pairs_target = 2L, stability_lag = 1L),
    progress = "none"
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  round_path <- file.path(session_dir, "round_log.rds")
  round_log <- readRDS(round_path)
  round_log$total_pairs_done[[nrow(round_log)]] <- 999L
  saveRDS(round_log, round_path)

  expect_error(
    load_adaptive_session(session_dir),
    "does not reconcile to committed `step_log` rows"
  )
})

test_that("load_adaptive_session preserves canonical round boundaries for artifact-only sessions", {
  state <- adaptive_rank_start(make_test_items(4), seed = 33L)
  state$round_log <- pairwiseLLM:::append_round_log(
    state$round_log,
    list(
      refit_id = 1L,
      round_id_at_refit = 1L,
      step_id_at_refit = 20L,
      total_pairs_done = 0L,
      diagnostics_pass = TRUE
    )
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  restored <- load_adaptive_session(session_dir)
  expect_identical(restored$refit_meta$last_refit_step, 20L)
  expect_identical(restored$refit_meta$last_refit_M_done, 0L)
  expect_identical(restored$refit_meta$last_refit_round_id, 1L)
  expect_identical(nrow(restored$history_pairs), 0L)
})

test_that("load_adaptive_session accepts canonical round totals with held-out probes", {
  state <- make_probe_resume_state()
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      pair_id = 1L,
      status = "ok",
      A = 1L,
      B = 4L,
      Y = 1L,
      is_cross_set = TRUE,
      link_spoke_id = 2L,
      run_mode = "link_one_spoke",
      is_probe_step = FALSE,
      is_holdout_probe_step = FALSE
    )
  )
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 2L,
      timestamp = as.POSIXct("2026-01-01 00:01:00", tz = "UTC"),
      pair_id = 2L,
      status = "ok",
      A = 2L,
      B = 5L,
      Y = 0L,
      is_cross_set = TRUE,
      link_spoke_id = 2L,
      run_mode = "link_probe_holdout",
      is_probe_step = TRUE,
      is_holdout_probe_step = TRUE
    )
  )
  state$round_log <- pairwiseLLM:::append_round_log(
    state$round_log,
    list(
      refit_id = 1L,
      round_id_at_refit = 1L,
      step_id_at_refit = 2L,
      total_pairs_done = 2L,
      new_pairs_since_last_refit = 2L,
      diagnostics_pass = TRUE
    )
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  restored <- load_adaptive_session(session_dir)
  expect_identical(restored$refit_meta$last_refit_M_done, 1L)
  expect_identical(nrow(restored$history_pairs), 1L)
  expect_identical(as.character(restored$history_pairs$A_id[[1L]]), "h1")
  expect_identical(as.character(restored$history_pairs$B_id[[1L]]), "s21")
  expect_history_state_matches_history(restored)
})

test_that("load_adaptive_session aborts on persisted history-state divergence", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items, seed = 34L)
  judge <- make_deterministic_judge("i_wins")

  withr::local_seed(1)
  state <- adaptive_rank_run_live(state, judge, n_steps = 2L, progress = "none")

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  state_path <- file.path(session_dir, "state.rds")
  persisted_state <- readRDS(state_path)
  persisted_state$history_state$deg[[1L]] <- persisted_state$history_state$deg[[1L]] + 1L
  saveRDS(persisted_state, state_path)

  expect_error(
    load_adaptive_session(session_dir),
    "history-state invariant failed during resume"
  )
})

test_that("load_adaptive_session upgrades legacy persisted history-state recent-degree fields", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items, seed = 35L)
  judge <- make_deterministic_judge("i_wins")

  withr::local_seed(1)
  state <- adaptive_rank_run_live(state, judge, n_steps = 2L, progress = "none")

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  state_path <- file.path(session_dir, "state.rds")
  persisted_state <- readRDS(state_path)
  persisted_state$history_state$recent_window_n <- NULL
  persisted_state$history_state$recent_deg <- NULL
  saveRDS(persisted_state, state_path)

  restored <- load_adaptive_session(session_dir)
  expect_history_state_matches_history(restored)
})

test_that("load_adaptive_session rebuilds current refit summary cache from canonical logs", {
  state <- make_probe_resume_state()
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      pair_id = 1L,
      status = "ok",
      A = 1L,
      B = 4L,
      Y = 1L,
      set_i = 1L,
      set_j = 2L,
      is_cross_set = TRUE,
      link_spoke_id = 2L,
      run_mode = "link_one_spoke",
      round_stage = "anchor_link",
      link_stage = "anchor_link",
      is_probe_step = FALSE,
      is_holdout_probe_step = FALSE
    )
  )
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 2L,
      timestamp = as.POSIXct("2026-01-01 00:01:00", tz = "UTC"),
      pair_id = 2L,
      status = "ok",
      A = 2L,
      B = 5L,
      Y = 0L,
      set_i = 1L,
      set_j = 2L,
      is_cross_set = TRUE,
      link_spoke_id = 2L,
      run_mode = "link_probe_holdout",
      round_stage = "probe_panel",
      link_stage = "probe_panel",
      fallback_used = "probe_panel_acceleration",
      is_probe_step = TRUE,
      is_holdout_probe_step = TRUE
    )
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  restored <- load_adaptive_session(session_dir)
  summary <- pairwiseLLM:::.adaptive_link_refit_summary_current(
    state = restored,
    refit_id = 1L,
    spoke_id = 2L,
    refit_context = list(last_refit_step = 0L),
    reconcile = TRUE
  )

  expect_identical(summary$n_pairs_cross_set_done, 2L)
  expect_identical(summary$n_pairs_cross_set_active_done, 1L)
  expect_identical(summary$n_pairs_cross_set_probe_done, 1L)
  expect_identical(summary$n_unique_cross_pairs_seen, 2L)
  expect_identical(summary$n_cross_edges_active_since_last_refit, 1L)
  expect_identical(summary$n_cross_edges_probe_since_last_refit, 1L)
  expect_identical(summary$n_cross_edges_total_since_last_refit, 2L)
  expect_true(isTRUE(summary$probe_panel_acceleration_used_since_last_refit))
  expect_identical(summary$stage_realized[["anchor_link"]], 1L)
})

test_that("load_adaptive_session aborts on refit summary cache drift from canonical logs", {
  state <- make_probe_resume_state()
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      pair_id = 1L,
      status = "ok",
      A = 1L,
      B = 4L,
      Y = 1L,
      set_i = 1L,
      set_j = 2L,
      is_cross_set = TRUE,
      link_spoke_id = 2L,
      run_mode = "link_one_spoke",
      round_stage = "anchor_link",
      link_stage = "anchor_link",
      is_probe_step = FALSE,
      is_holdout_probe_step = FALSE
    )
  )
  state$refit_meta$link_refit_summary_cache_by_refit_spoke <- list(
    `1::2` = list(
      refit_id = 1L,
      spoke_id = 2L,
      n_pairs_cross_set_done = 999L,
      n_pairs_cross_set_active_done = 999L,
      n_pairs_cross_set_probe_done = 0L,
      n_unique_cross_pairs_seen = 1L,
      n_cross_edges_active_since_last_refit = 999L,
      n_cross_edges_probe_since_last_refit = 0L,
      n_cross_edges_total_since_last_refit = 999L,
      probe_panel_acceleration_used_since_last_refit = FALSE,
      stage_realized = c(anchor_link = 999L, long_link = 0L, mid_link = 0L, local_link = 0L)
    )
  )
  state$refit_meta$link_unique_cross_pair_keys_by_spoke <- list(`2` = c("h1::s21"))

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  expect_error(
    load_adaptive_session(session_dir),
    "refit summary cache invariant failed"
  )
})

test_that("load_adaptive_session accepts legacy round totals that exclude held-out probes", {
  state <- make_probe_resume_state()
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      pair_id = 1L,
      status = "ok",
      A = 1L,
      B = 4L,
      Y = 1L,
      is_cross_set = TRUE,
      link_spoke_id = 2L,
      run_mode = "link_one_spoke",
      is_probe_step = FALSE,
      is_holdout_probe_step = FALSE
    )
  )
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 2L,
      timestamp = as.POSIXct("2026-01-01 00:01:00", tz = "UTC"),
      pair_id = 2L,
      status = "ok",
      A = 2L,
      B = 5L,
      Y = 0L,
      is_cross_set = TRUE,
      link_spoke_id = 2L,
      run_mode = "link_probe_holdout",
      is_probe_step = TRUE,
      is_holdout_probe_step = TRUE
    )
  )
  state$round_log <- pairwiseLLM:::append_round_log(
    state$round_log,
    list(
      refit_id = 1L,
      round_id_at_refit = 1L,
      step_id_at_refit = 2L,
      total_pairs_done = 1L,
      new_pairs_since_last_refit = 1L,
      diagnostics_pass = TRUE
    )
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  restored <- load_adaptive_session(session_dir)
  expect_identical(restored$refit_meta$last_refit_M_done, 1L)
  expect_identical(nrow(restored$history_pairs), 1L)
  expect_identical(as.character(restored$history_pairs$A_id[[1L]]), "h1")
  expect_identical(as.character(restored$history_pairs$B_id[[1L]]), "s21")
})

test_that("load_adaptive_session accepts persisted item logs with current schema", {
  items <- make_test_items(6)
  state <- adaptive_rank_start(items, persist_item_log = TRUE)
  judge <- make_deterministic_judge("i_wins")
  stub <- make_deterministic_fit_fn(state$item_ids)

  withr::local_seed(1)
  state <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 4L,
    fit_fn = stub$fit_fn,
    btl_config = list(refit_pairs_target = 2L),
    progress = "none"
  )
  expect_gte(length(state$item_log), 1L)

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  restored <- load_adaptive_session(session_dir)
  expect_true(isTRUE(restored$config$persist_item_log))
  expect_gte(length(restored$item_log), 1L)
  expect_equal(
    names(adaptive_item_log(restored, refit_id = 1L)),
    pairwiseLLM:::.adaptive_item_log_columns()
  )
})

test_that("validate_session_dir accepts legacy item log schema for resume", {
  items <- make_test_items(6)
  state <- adaptive_rank_start(items, persist_item_log = TRUE)
  judge <- make_deterministic_judge("i_wins")
  stub <- make_deterministic_fit_fn(state$item_ids)

  withr::local_seed(1)
  state <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 4L,
    fit_fn = stub$fit_fn,
    btl_config = list(refit_pairs_target = 2L),
    progress = "none"
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  legacy_cols <- c(
    "refit_id",
    "item_id",
    "theta_mean",
    "theta_p2.5",
    "theta_p5",
    "theta_p50",
    "theta_p95",
    "theta_p97.5",
    "theta_sd",
    "rank_mean",
    "degree",
    "pos_count_A",
    "pos_count_B"
  )
  item_path <- file.path(session_dir, "item_log", "refit_0001.rds")
  item_new <- readRDS(item_path)
  legacy_item <- tibble::tibble(
    refit_id = as.integer(item_new$refit_id),
    item_id = as.character(item_new$item_id),
    theta_mean = as.double(item_new$theta_raw_eap),
    `theta_p2.5` = as.double(item_new$`theta_raw_p2.5`),
    `theta_p5` = as.double(item_new$`theta_raw_p5`),
    `theta_p50` = as.double(item_new$`theta_raw_p50`),
    `theta_p95` = as.double(item_new$`theta_raw_p95`),
    `theta_p97.5` = as.double(item_new$`theta_raw_p97.5`),
    theta_sd = as.double(item_new$theta_raw_sd),
    rank_mean = as.double(item_new$rank_raw),
    degree = as.integer(item_new$degree),
    pos_count_A = as.integer(item_new$pos_count_A),
    pos_count_B = as.integer(item_new$pos_count_B)
  )
  legacy_item <- legacy_item[, legacy_cols, drop = FALSE]
  saveRDS(legacy_item, item_path)

  expect_silent(validate_session_dir(session_dir))
})

test_that("load_adaptive_session preserves cleaned linking controller state across save/load", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs23")
  )
  state <- adaptive_rank_start(
    items,
    seed = 17L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      link_transform_policy = "auto"
    )
  )
  state$controller$link_transform_state_by_spoke <- list(`2` = "shift_scale")
  state$controller$link_state_frozen_by_spoke <- list(`2` = TRUE)
  state$controller$link_state_frozen_refit_id_by_spoke <- list(`2` = 3L)
  state$controller$link_epoch_id_by_spoke <- list(`2` = 4L)
  state$controller$link_epoch_start_step_by_spoke <- list(`2` = 8L)
  state$controller$link_escalation_recent_pass_window_by_spoke <- list(`2` = c(TRUE))
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_policy = "auto",
      link_transform_state = "shift_scale",
      link_epoch_id = 4L,
      link_state_frozen = TRUE,
      link_stop_gate_open = FALSE,
      link_stop_eligible = FALSE,
      link_stop_pass = TRUE,
      escalated_this_refit = FALSE
    )
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)
  restored <- load_adaptive_session(session_dir)

  expect_identical(restored$controller$link_transform_state_by_spoke[["2"]], "shift_scale")
  expect_true(isTRUE(restored$controller$link_state_frozen_by_spoke[["2"]]))
  expect_identical(restored$controller$link_state_frozen_refit_id_by_spoke[["2"]], 3L)
  expect_identical(restored$controller$link_epoch_id_by_spoke[["2"]], 4L)
  expect_identical(restored$controller$link_epoch_start_step_by_spoke[["2"]], 8L)
  expect_identical(restored$controller$link_escalation_recent_pass_window_by_spoke[["2"]], c(TRUE))
})

test_that("load_adaptive_session normalizes legacy controller freeze fields into canonical state", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs23")
  )
  state <- adaptive_rank_start(
    items,
    seed = 23L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      link_transform_policy = "auto"
    )
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  persisted_state <- readRDS(file.path(session_dir, "state.rds"))
  persisted_state$controller$link_state_frozen_by_spoke <- NULL
  persisted_state$controller$link_state_frozen_refit_id_by_spoke <- NULL
  persisted_state$controller$link_transform_frozen_by_spoke <- list(`2` = TRUE)
  persisted_state$controller$link_transform_frozen_refit_id_by_spoke <- list(`2` = 5L)
  saveRDS(persisted_state, file.path(session_dir, "state.rds"))

  restored <- load_adaptive_session(session_dir)

  expect_true(isTRUE(restored$controller$link_state_frozen_by_spoke[["2"]]))
  expect_identical(restored$controller$link_state_frozen_refit_id_by_spoke[["2"]], 5L)
})

test_that("load_adaptive_session normalizes legacy link_stage_log transform columns on resume", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs23")
  )
  state <- adaptive_rank_start(items, seed = 19L)
  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  link_path <- file.path(session_dir, "link_stage_log.rds")
  legacy <- pairwiseLLM:::new_link_stage_log()
  legacy$link_transform_policy <- NULL
  legacy$link_transform_state <- NULL
  legacy <- tibble::add_column(legacy, link_transform_mode = character(), .after = "hub_id")
  legacy <- legacy[, c(
    "refit_id", "spoke_id", "hub_id", "link_transform_mode",
    setdiff(names(legacy), c("refit_id", "spoke_id", "hub_id", "link_transform_mode"))
  )]
  legacy <- tibble::add_row(
    legacy,
    refit_id = 1L,
    spoke_id = 2L,
    hub_id = 1L,
    link_transform_mode = "shift_only",
    link_refit_mode = "shift_only",
    hub_lock_mode = "soft_lock",
    reliability_link_global = 0.9,
    linking_identified = TRUE,
    link_stop_eligible = FALSE,
    link_stop_pass = FALSE,
    link_state_frozen = FALSE,
    n_pairs_cross_set_done = 1L,
    n_unique_cross_pairs_seen = 1L,
    n_cross_edges_active_since_last_refit = 1L,
    n_cross_edges_probe_since_last_refit = 0L,
    n_cross_edges_total_since_last_refit = 1L,
    coverage_bins_used = 3L,
    B_spoke_refit_budget = 1L,
    B_spoke_refit_budget_source = "fixed_override",
    stage_target_anchor_link = 1L,
    stage_target_long_link = 0L,
    stage_target_mid_link = 0L,
    stage_target_local_link = 0L,
    stage_realized_anchor_link = 1L,
    stage_realized_long_link = 0L,
    stage_realized_mid_link = 0L,
    stage_realized_local_link = 0L,
    stage_shortfall_anchor_link = 0L,
    stage_shortfall_long_link = 0L,
    stage_shortfall_mid_link = 0L,
    stage_shortfall_local_link = 0L,
    stage_reallocation_used = FALSE,
    stage_reallocation_rule_used = "none",
    stage_budget_unfilled = 0L
  )
  saveRDS(legacy, link_path)

  restored <- load_adaptive_session(session_dir)
  expect_false("link_transform_mode" %in% names(restored$link_stage_log))
  expect_identical(as.character(restored$link_stage_log$link_estimation_mode[[1L]]), "transform")
  expect_identical(as.character(restored$link_stage_log$link_transform_policy[[1L]]), "fixed_shift_only")
  expect_identical(as.character(restored$link_stage_log$link_transform_state[[1L]]), "shift_only")
})

test_that("save/load preserves free hub lock across controller and link_stage_log", {
  state <- make_probe_resume_state()
  state$controller$link_refit_mode <- "joint_refit"
  state$controller$hub_lock_mode <- "free"
  state$link_stage_log$link_refit_mode[] <- "joint_refit"
  state$link_stage_log$hub_lock_mode[] <- "free"

  session_dir <- tempfile("adaptive-session-free-lock-")
  save_adaptive_session(state, session_dir)
  restored <- load_adaptive_session(session_dir)

  expect_identical(restored$controller$link_refit_mode, "joint_refit")
  expect_identical(restored$controller$hub_lock_mode, "free")
  expect_identical(as.character(restored$link_stage_log$link_refit_mode[[1L]]), "joint_refit")
  expect_identical(as.character(restored$link_stage_log$hub_lock_mode[[1L]]), "free")
})

test_that("save/load preserves feasibility and canonical stop-threshold fields in link_stage_log", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22")
  )
  state <- adaptive_rank_start(
    items,
    seed = 41L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    state$link_stage_log,
    list(
      refit_id = 1L,
      spoke_id = 2L,
      hub_id = 1L,
      link_estimation_mode = "transform",
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_refit_mode = "shift_only",
      hub_lock_mode = "soft_lock",
      reliability_link_global = 0.9,
      link_stop_reliability_min_used = 0.9,
      reliability_stop_pass = TRUE,
      linking_identified = TRUE,
      link_stop_eligible = FALSE,
      link_stop_pass = FALSE,
      link_state_frozen = FALSE,
      n_pairs_cross_set_done = 2L,
      n_unique_cross_pairs_seen = 2L,
      n_probe_pairs_since_last_refit = 0L,
      n_cross_edges_active_since_last_refit = 2L,
      n_cross_edges_probe_since_last_refit = 0L,
      n_cross_edges_total_since_last_refit = 2L,
      coverage_bins_used = 3L,
      B_spoke_refit_budget = 4L,
      B_spoke_refit_budget_source = "single_spoke_controller",
      stage_target_anchor_link = 1L,
      stage_target_long_link = 1L,
      stage_target_mid_link = 1L,
      stage_target_local_link = 1L,
      feasible_stage_capacity_anchor_link = 2L,
      feasible_stage_capacity_long_link = 0L,
      feasible_stage_capacity_mid_link = 2L,
      feasible_stage_capacity_local_link = 2L,
      feasibility_budget_released = 1L,
      feasibility_reallocation_used = TRUE,
      feasibility_reallocation_rule = "pooled_utility_backfill",
      stage_realized_anchor_link = 1L,
      stage_realized_long_link = 0L,
      stage_realized_mid_link = 1L,
      stage_realized_local_link = 1L,
      stage_shortfall_anchor_link = 0L,
      stage_shortfall_long_link = 1L,
      stage_shortfall_mid_link = 0L,
      stage_shortfall_local_link = 0L,
      stage_reallocation_used = TRUE,
      stage_reallocation_rule_used = "pooled_utility_backfill",
      stage_budget_unfilled = 1L,
      probe_brier = 0.12,
      probe_brier_max_used = 0.19,
      probe_brier_pass = TRUE,
      probe_pred_rmse_lagged = 0.01,
      probe_pred_rmse_max_used = 0.015,
      probe_pred_rmse_pass = TRUE,
      theta_global_rmse_lagged = 0.03,
      theta_global_rmse_max_used = 0.05,
      theta_global_rmse_pass = TRUE
    )
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)
  restored <- load_adaptive_session(session_dir)
  row <- restored$link_stage_log[1L, , drop = FALSE]

  expect_identical(row$feasible_stage_capacity_long_link[[1L]], 0L)
  expect_identical(row$feasibility_budget_released[[1L]], 1L)
  expect_true(isTRUE(row$feasibility_reallocation_used[[1L]]))
  expect_identical(
    as.character(row$feasibility_reallocation_rule[[1L]]),
    "pooled_utility_backfill"
  )
  expect_equal(row$probe_brier_max_used[[1L]], 0.19, tolerance = 1e-12)
  expect_true(isTRUE(row$probe_brier_pass[[1L]]))
  expect_equal(row$theta_global_rmse_max_used[[1L]], 0.05, tolerance = 1e-12)
  expect_true(isTRUE(row$theta_global_rmse_pass[[1L]]))
})

test_that("save/load preserves planned probe panels and realized probe bookkeeping", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22")
  )
  state <- adaptive_rank_start(
    items,
    seed = 52L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("ready", "ready"),
      validation_message = c("ok", "ok"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(
      `1` = list(items = tibble::tibble(
        global_item_id = c("gh1", "gh2"),
        theta_raw_mean = c(0.2, -0.2),
        theta_raw_sd = c(0.1, 0.1),
        rank_mu_raw = c(1, 2)
      )),
      `2` = list(items = tibble::tibble(
        global_item_id = c("gs21", "gs22"),
        theta_raw_mean = c(0.1, -0.1),
        theta_raw_sd = c(0.1, 0.1),
        rank_mu_raw = c(1, 2)
      ))
    ),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L),
    set_stop_pass_by_set = list(`1` = TRUE, `2` = TRUE),
    phase = "phase_b",
    ready_spokes = 2L,
    active_phase_a_set = NA_integer_,
    phase_b_started_at_step = 1L
  )
  state <- pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins"))
  panel_before <- state$linking$probe$panels_by_spoke[["2"]]
  pair_key <- as.character(panel_before$pair_key[[1L]])
  panel_before$realized[[1L]] <- FALSE
  panel_before$realized_step_id[[1L]] <- NA_integer_
  panel_before$realized_pair_id[[1L]] <- NA_integer_
  panel_before$realized_run_mode[[1L]] <- NA_character_
  state$linking$probe$panels_by_spoke[["2"]] <- panel_before
  state$linking$probe$realized_edges <- tibble::tibble(
    step_id = 99L,
    pair_id = 99L,
    run_mode = "link_probe_holdout",
    spoke_id = 2L,
    link_epoch_id = 1L,
    probe_panel_id = as.character(panel_before$probe_panel_id[[1L]]),
    hub_item_id = as.character(panel_before$hub_item_id[[1L]]),
    spoke_item_id = as.character(panel_before$spoke_item_id[[1L]]),
    pair_key = pair_key,
    Y = 1L
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)
  restored <- load_adaptive_session(session_dir)

  expect_equal(
    restored$linking$probe$panels_by_spoke[["2"]]$pair_key,
    state$linking$probe$panels_by_spoke[["2"]]$pair_key
  )
  expect_equal(
    restored$linking$probe$realized_edges$pair_key,
    state$linking$probe$realized_edges$pair_key
  )
  restored_panel <- pairwiseLLM:::.adaptive_link_probe_panel_for_spoke(
    restored,
    spoke_id = 2L,
    epoch_id = 1L
  )
  expect_true(isTRUE(restored_panel$realized[[1L]]))
  expect_identical(as.integer(restored_panel$realized_step_id[[1L]]), 99L)
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_realized_count(restored, spoke_id = 2L, epoch_id = 1L),
    1L
  )
  realized_key <- pairwiseLLM:::.adaptive_link_probe_realized_index_key(
    spoke_id = 2L,
    epoch_id = 1L,
    probe_panel_id = as.character(panel_before$probe_panel_id[[1L]])
  )
  restored_entry <- restored$linking$probe$realized_index_by_panel[[realized_key]]
  expect_false(is.null(restored_entry))
  expect_identical(as.integer(restored_entry$realized_count), 1L)
  expect_identical(as.integer(restored_entry$last_realized_step_id), 99L)
})

test_that("save/load preserves probe acceleration controller fields and canonical log columns", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22")
  )
  state <- adaptive_rank_start(
    items,
    seed = 61L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      probe_acceleration_mode = "active_floor_plus_sole_blocker",
      probe_active_floor_enabled = TRUE,
      probe_sole_blocker_acceleration_enabled = TRUE,
      probe_pairs_per_refit_per_spoke_bootstrap_max = 6L,
      probe_pairs_per_refit_per_spoke_sole_blocker_max = 12L,
      probe_accel_bootstrap_target = 12L,
      probe_active_floor_frac = 0.5,
      probe_active_floor_min = 20L,
      probe_active_floor_requires_anchor_progress = TRUE,
      probe_sole_blocker_min_realized = 20L,
      probe_sole_blocker_active_floor_min = 10L
    )
  )
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_epoch_id = 4L,
      link_lag_eligible = TRUE,
      link_min_refit_eligible = TRUE,
      link_diagnostics_pass = TRUE,
      reliability_stop_pass = TRUE,
      probe_brier_pass = TRUE,
      probe_pred_rmse_pass = TRUE,
      theta_global_rmse_pass = TRUE,
      stop_blocker_codes = "probe_edges_min_for_stop"
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
      link_state_frozen = FALSE,
      probe_acceleration_mode_used = "active_floor_plus_sole_blocker",
      probe_active_floor_used = 10L,
      probe_only_blocker_trigger = TRUE,
      probe_acceleration_used = TRUE,
      probe_effort_base_cap = 2L,
      probe_effort_effective_cap = 7L,
      probe_remaining_to_min_start = 9L
    )
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)
  restored <- load_adaptive_session(session_dir)

  expect_identical(
    restored$controller$probe_acceleration_mode,
    "active_floor_plus_sole_blocker"
  )
  expect_true(isTRUE(restored$controller$probe_active_floor_enabled))
  expect_true(isTRUE(restored$controller$probe_sole_blocker_acceleration_enabled))
  expect_identical(restored$controller$probe_pairs_per_refit_per_spoke_bootstrap_max, 6L)
  expect_identical(restored$controller$probe_pairs_per_refit_per_spoke_sole_blocker_max, 12L)
  expect_identical(restored$controller$probe_accel_bootstrap_target, 12L)
  expect_identical(restored$controller$probe_active_floor_frac, 0.5)
  expect_identical(restored$controller$probe_active_floor_min, 20L)
  expect_true(isTRUE(restored$controller$probe_active_floor_requires_anchor_progress))
  expect_identical(restored$controller$probe_sole_blocker_min_realized, 20L)
  expect_identical(restored$controller$probe_sole_blocker_active_floor_min, 10L)
  expect_true(all(c(
    "probe_acceleration_mode_used",
    "probe_active_floor_used",
    "probe_only_blocker_trigger",
    "probe_acceleration_used",
    "probe_effort_base_cap",
    "probe_effort_effective_cap",
    "probe_remaining_to_min_start"
  ) %in% names(restored$link_stage_log)))
  expect_identical(
    as.character(restored$link_stage_log$probe_acceleration_mode_used[[1L]]),
    "active_floor_plus_sole_blocker"
  )
  expect_identical(as.integer(restored$link_stage_log$probe_active_floor_used[[1L]]), 10L)
  expect_true(isTRUE(restored$link_stage_log$probe_only_blocker_trigger[[1L]]))
  expect_true(isTRUE(restored$link_stage_log$probe_acceleration_used[[1L]]))
  expect_identical(as.integer(restored$link_stage_log$probe_effort_base_cap[[1L]]), 2L)
  expect_identical(as.integer(restored$link_stage_log$probe_effort_effective_cap[[1L]]), 7L)
  expect_identical(as.integer(restored$link_stage_log$probe_remaining_to_min_start[[1L]]), 9L)
  expect_identical(
    as.character(restored$controller$link_refit_stats_by_spoke$`2`$stop_blocker_codes),
    "probe_edges_min_for_stop"
  )
})

test_that("save/load and resume preserve genuinely accelerated probe runtime state", {
  state <- make_positive_probe_acceleration_runtime_state()
  accelerated_before <- state$link_stage_log[
    state$link_stage_log$probe_acceleration_used %in% TRUE,
    ,
    drop = FALSE
  ]
  expect_gte(nrow(accelerated_before), 1L)

  spoke_id <- as.integer(accelerated_before$spoke_id[[1L]])
  epoch_id <- as.integer(
    state$controller$link_epoch_id_by_spoke[[as.character(spoke_id)]] %||% NA_integer_
  )
  expect_true(is.finite(epoch_id))

  panel_before <- pairwiseLLM:::.adaptive_link_probe_panel_for_spoke(
    state,
    spoke_id = spoke_id,
    epoch_id = epoch_id
  )
  realized_before <- pairwiseLLM:::.adaptive_link_probe_realized_count(
    state,
    spoke_id = spoke_id,
    epoch_id = epoch_id
  )
  expect_gte(realized_before, 1L)

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)
  restored <- load_adaptive_session(session_dir)

  accelerated_cols <- c(
    "probe_acceleration_mode_used",
    "probe_active_floor_used",
    "probe_only_blocker_trigger",
    "probe_acceleration_used",
    "probe_effort_base_cap",
    "probe_effort_effective_cap",
    "probe_remaining_to_min_start",
    "n_cross_edges_probe_since_last_refit"
  )
  restored_accelerated <- restored$link_stage_log[
    restored$link_stage_log$probe_acceleration_used %in% TRUE,
    accelerated_cols,
    drop = FALSE
  ]
  expect_equal(
    restored_accelerated,
    accelerated_before[, accelerated_cols, drop = FALSE]
  )

  restored_panel <- pairwiseLLM:::.adaptive_link_probe_panel_for_spoke(
    restored,
    spoke_id = spoke_id,
    epoch_id = epoch_id
  )
  expect_identical(
    unique(as.character(restored_panel$probe_panel_id)),
    unique(as.character(panel_before$probe_panel_id))
  )
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_realized_count(
      restored,
      spoke_id = spoke_id,
      epoch_id = epoch_id
    ),
    realized_before
  )

  resumed <- pairwiseLLM:::.adaptive_link_sync_warm_start(restored)
  resumed <- pairwiseLLM:::.adaptive_round_activate_if_ready(resumed)
  resumed_judge <- make_linking_score_judge_fixture(c(
    h1 = -0.6, h2 = 0.0, h3 = 0.6,
    s21 = -0.3, s22 = 0.2, s23 = 1.0,
    s31 = -0.4, s32 = 0.1, s33 = 0.9
  ))
  for (idx in seq_len(4L)) {
    resumed <- pairwiseLLM:::run_one_step(resumed, resumed_judge)
    step_row <- tibble::as_tibble(resumed$step_log)[nrow(resumed$step_log), , drop = FALSE]
    if (isTRUE(step_row$status[[1L]] == "ok")) {
      resumed <- pairwiseLLM:::.adaptive_round_commit(resumed, step_row)
    }
  }

  resumed_panel <- pairwiseLLM:::.adaptive_link_probe_panel_for_spoke(
    resumed,
    spoke_id = spoke_id,
    epoch_id = epoch_id
  )
  expect_identical(
    unique(as.character(resumed_panel$probe_panel_id)),
    unique(as.character(panel_before$probe_panel_id))
  )
  expect_gte(
    pairwiseLLM:::.adaptive_link_probe_realized_count(
      resumed,
      spoke_id = spoke_id,
      epoch_id = epoch_id
    ),
    realized_before
  )
})

test_that("resume preserves probe panel identity, epoch, and realized counts across a chunk boundary", {
  state <- make_probe_resume_state()
  state <- pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins"))

  panel_before <- pairwiseLLM:::.adaptive_link_probe_panel_for_spoke(state, spoke_id = 2L, epoch_id = 1L)
  planned_before <- pairwiseLLM:::.adaptive_link_probe_planned_edges(panel_before)
  realized_before <- pairwiseLLM:::.adaptive_link_probe_realized_count(state, spoke_id = 2L, epoch_id = 1L)
  expect_gte(nrow(panel_before), 1L)
  expect_gte(realized_before, 1L)

  state$controller$link_epoch_id_by_spoke <- list(`2` = 1L)
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    state$link_stage_log,
    list(
      refit_id = 2L,
      spoke_id = 2L,
      hub_id = 1L,
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_refit_mode = "shift_only",
      hub_lock_mode = "soft_lock",
      link_stop_pass = FALSE,
      link_state_frozen = FALSE,
      link_epoch_id = 1L,
      probe_panel_id = as.character(panel_before$probe_panel_id[[1L]]),
      probe_edges_planned = as.integer(planned_before),
      probe_edges_realized = as.integer(realized_before),
      probe_panel_shortfall = as.integer(planned_before - realized_before),
      probe_panel_reallocation_used = pairwiseLLM:::.adaptive_link_probe_panel_reallocation_used(panel_before)
    )
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)
  restored <- load_adaptive_session(session_dir)

  panel_after <- pairwiseLLM:::.adaptive_link_probe_panel_for_spoke(restored, spoke_id = 2L, epoch_id = 1L)
  realized_after <- pairwiseLLM:::.adaptive_link_probe_realized_count(restored, spoke_id = 2L, epoch_id = 1L)
  expect_identical(as.character(panel_after$probe_panel_id[[1L]]), as.character(panel_before$probe_panel_id[[1L]]))
  expect_identical(as.integer(panel_after$link_epoch_id[[1L]]), 1L)
  expect_identical(as.integer(restored$controller$link_epoch_id_by_spoke[["2"]]), 1L)
  expect_identical(as.integer(realized_after), as.integer(realized_before))
  restored_key <- pairwiseLLM:::.adaptive_link_probe_realized_index_key(
    spoke_id = 2L,
    epoch_id = 1L,
    probe_panel_id = as.character(panel_before$probe_panel_id[[1L]])
  )
  expect_identical(
    as.integer(restored$linking$probe$realized_index_by_panel[[restored_key]]$realized_count),
    as.integer(realized_after)
  )

  synced <- pairwiseLLM:::.adaptive_apply_controller_config(restored, adaptive_config = NULL)
  expect_true("probe" %in% names(synced$linking))
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_realized_count(synced, spoke_id = 2L, epoch_id = 1L),
    as.integer(realized_after)
  )

  resumed <- adaptive_rank_run_live(
    restored,
    make_deterministic_judge("i_wins"),
    n_steps = 1L,
    progress = "none"
  )
  panel_resumed <- pairwiseLLM:::.adaptive_link_probe_panel_for_spoke(resumed, spoke_id = 2L, epoch_id = 1L)
  realized_resumed <- pairwiseLLM:::.adaptive_link_probe_realized_count(resumed, spoke_id = 2L, epoch_id = 1L)
  expect_identical(as.character(panel_resumed$probe_panel_id[[1L]]), as.character(panel_before$probe_panel_id[[1L]]))
  expect_identical(as.integer(panel_resumed$link_epoch_id[[1L]]), 1L)
  expect_gte(as.integer(realized_resumed), as.integer(realized_after))
})

test_that("resume accepts current-window realized probes beyond the latest link-stage row", {
  state <- make_probe_resume_state()
  state <- pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins"))

  panel <- pairwiseLLM:::.adaptive_link_probe_panel_for_spoke(state, spoke_id = 2L, epoch_id = 1L)
  planned_edges <- pairwiseLLM:::.adaptive_link_probe_planned_edges(panel)
  realized <- pairwiseLLM:::.adaptive_link_probe_realized_count(state, spoke_id = 2L, epoch_id = 1L)
  expect_gte(realized, 1L)

  state$controller$link_epoch_id_by_spoke <- list(`2` = 1L)
  state$link_stage_log <- state$link_stage_log[0, , drop = FALSE]
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
      link_stop_pass = FALSE,
      link_state_frozen = FALSE,
      link_epoch_id = 1L,
      probe_panel_id = as.character(panel$probe_panel_id[[1L]]),
      probe_edges_planned = as.integer(planned_edges),
      probe_edges_realized = 0L,
      probe_panel_shortfall = as.integer(planned_edges),
      probe_panel_reallocation_used = pairwiseLLM:::.adaptive_link_probe_panel_reallocation_used(panel)
    )
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)
  restored <- load_adaptive_session(session_dir)

  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_realized_count(restored, spoke_id = 2L, epoch_id = 1L),
    as.integer(realized)
  )
  last_row <- tibble::as_tibble(restored$link_stage_log) |>
    dplyr::filter(.data$spoke_id == 2L) |>
    dplyr::slice_tail(n = 1L)
  expect_identical(as.integer(last_row$probe_edges_realized[[1L]]), 0L)
})

test_that("resume aborts when current-window holdout steps do not reconcile to canonical realized_edges", {
  state <- make_probe_resume_state()
  state <- pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins"))

  panel <- pairwiseLLM:::.adaptive_link_probe_panel_for_spoke(state, spoke_id = 2L, epoch_id = 1L)
  planned_edges <- pairwiseLLM:::.adaptive_link_probe_planned_edges(panel)
  expect_gte(
    pairwiseLLM:::.adaptive_link_probe_realized_count(state, spoke_id = 2L, epoch_id = 1L),
    1L
  )

  state$controller$link_epoch_id_by_spoke <- list(`2` = 1L)
  state$link_stage_log <- state$link_stage_log[0, , drop = FALSE]
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
      link_stop_pass = FALSE,
      link_state_frozen = FALSE,
      link_epoch_id = 1L,
      probe_panel_id = as.character(panel$probe_panel_id[[1L]]),
      probe_edges_planned = as.integer(planned_edges),
      probe_edges_realized = 0L,
      probe_panel_shortfall = as.integer(planned_edges),
      probe_panel_reallocation_used = pairwiseLLM:::.adaptive_link_probe_panel_reallocation_used(panel)
    )
  )
  state$linking$probe$realized_edges <- pairwiseLLM:::.adaptive_link_probe_empty_realized_log()

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  expect_error(
    load_adaptive_session(session_dir),
    "committed holdout probe steps after the last refit do not reconcile"
  )
})

test_that("resume aborts when persisted probe state disagrees with canonical logs or controller epoch", {
  state <- make_probe_resume_state()
  state <- pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins"))
  panel <- pairwiseLLM:::.adaptive_link_probe_panel_for_spoke(state, spoke_id = 2L, epoch_id = 1L)
  planned_edges <- pairwiseLLM:::.adaptive_link_probe_planned_edges(panel)
  realized <- pairwiseLLM:::.adaptive_link_probe_realized_count(state, spoke_id = 2L, epoch_id = 1L)

  state$controller$link_epoch_id_by_spoke <- list(`2` = 1L)
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    state$link_stage_log,
    list(
      refit_id = 2L,
      spoke_id = 2L,
      hub_id = 1L,
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_refit_mode = "shift_only",
      hub_lock_mode = "soft_lock",
      link_stop_pass = FALSE,
      link_state_frozen = FALSE,
      link_epoch_id = 1L,
      probe_panel_id = as.character(panel$probe_panel_id[[1L]]),
      probe_edges_planned = as.integer(planned_edges),
      probe_edges_realized = as.integer(realized),
      probe_panel_shortfall = as.integer(planned_edges - realized),
      probe_panel_reallocation_used = pairwiseLLM:::.adaptive_link_probe_panel_reallocation_used(panel)
    )
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  persisted_state <- readRDS(file.path(session_dir, "state.rds"))
  persisted_state$controller$link_epoch_id_by_spoke <- list(`2` = 2L)
  saveRDS(persisted_state, file.path(session_dir, "state.rds"))

  expect_error(
    load_adaptive_session(session_dir),
    "probe-state invariant failed.*link_epoch_id_by_spoke"
  )
})

test_that("save/load preserves Phase B global audit metrics built from Phase A artifacts", {
  state <- make_probe_resume_state()
  state$controller$phase_a_mode <- "import"
  state$linking$phase_a$set_status$source <- c("import", "import")
  phase_a_draws <- cbind(
    h1 = c(0.60, 0.55, 0.50, 0.58),
    h2 = c(0.10, 0.05, 0.00, 0.08),
    h3 = c(-0.40, -0.35, -0.30, -0.38),
    s21 = c(0.25, 0.20, 0.15, 0.22),
    s22 = c(-0.15, -0.10, -0.05, -0.12)
  )
  state$btl_fit <- make_test_btl_fit(
    state$item_ids,
    draws = phase_a_draws,
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 500)
  )
  artifact_1 <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, set_id = 1L)
  artifact_2 <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, set_id = 2L)
  artifact_1$quality_gate_accepted <- TRUE
  artifact_2$quality_gate_accepted <- TRUE
  state$linking$phase_a$artifacts <- list(`1` = artifact_1, `2` = artifact_2)
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_state = "shift_only",
      delta_spoke_mean = 0.65,
      log_alpha_spoke_mean = NA_real_
    )
  )
  state$controller$link_transform_state_by_spoke <- list(`2` = "shift_only")
  state$btl_fit <- make_test_btl_fit(
    state$item_ids,
    draws = cbind(
      h1 = c(0.05, -0.05, 0.10, -0.10),
      h2 = c(-0.05, 0.05, -0.10, 0.10),
      h3 = c(0.00, 0.10, -0.05, -0.05),
      s21 = c(0.02, -0.08, 0.04, 0.01),
      s22 = c(-0.03, 0.07, -0.02, -0.01)
    ),
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 500)
  )
  state <- pairwiseLLM:::.adaptive_phase_b_global_metric_history_update(state, refit_id = 1L)

  cfg <- list(
    ess_bulk_min = 100,
    ess_bulk_min_near_stop = 100,
    max_rhat = 1.01,
    divergences_max = 0L,
    eap_reliability_min = 0.10,
    stability_lag = 1L,
    theta_corr_min = 0.90,
    theta_sd_rel_change_max = 0.50,
    rank_spearman_min = 0.90
  )
  metrics_before <- pairwiseLLM:::compute_stop_metrics(state, config = cfg)

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir, overwrite = TRUE)
  restored <- load_adaptive_session(session_dir)
  metrics_after <- pairwiseLLM:::compute_stop_metrics(restored, config = cfg)

  expect_equal(metrics_after$reliability_EAP, metrics_before$reliability_EAP)
  expect_equal(metrics_after$rho_theta, metrics_before$rho_theta)
  expect_equal(metrics_after$delta_sd_theta, metrics_before$delta_sd_theta)
  expect_equal(metrics_after$rho_rank, metrics_before$rho_rank)
  expect_equal(
    restored$refit_meta$phase_b_global_theta_mean_history,
    state$refit_meta$phase_b_global_theta_mean_history
  )
})

test_that("load_adaptive_session reconstructs anchored-joint accepted-state scaffolding", {
  state <- make_anchored_joint_resume_state()
  state$linking$anchored_joint$accepted_state_by_spoke <- list()
  state$linking$anchored_joint$fisher_t0_by_spoke <- list()

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir, overwrite = TRUE)
  restored <- load_adaptive_session(session_dir)

  accepted <- restored$linking$anchored_joint$accepted_state_by_spoke[["2"]]
  fisher_t0 <- restored$linking$anchored_joint$fisher_t0_by_spoke[["2"]]
  expect_identical(accepted$anchored_joint_init_state_method, "artifact_copy_init")
  expect_true(isTRUE(fisher_t0$I_s_t0_zero))
  expect_identical(fisher_t0$n_link_active_pairs, 0L)
  expect_equal(
    restored$linking$phase_a$artifacts[["1"]]$phase_a_within_set_evidence$A_item,
    "a1"
  )
})

test_that("save/load preserves anchored-joint accepted-state provenance and audit fields", {
  state <- make_anchored_joint_resume_state()
  accepted <- state$linking$anchored_joint$accepted_state_by_spoke[["2"]]
  state$linking$anchored_joint$fisher_t0_by_spoke[["2"]] <- list(
    free_block_dim = 2L,
    I_s_t0_zero = TRUE,
    n_link_active_pairs = 3L,
    anchored_joint_init_state_method = accepted$anchored_joint_init_state_method
  )
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    pairwiseLLM:::new_link_stage_log(),
    list(
      refit_id = 3L,
      spoke_id = 2L,
      hub_id = 1L,
      link_epoch_id = 1L,
      link_estimation_mode = "anchored_joint",
      hub_lock_mode = "hard_lock",
      link_fit_method = "map_laplace",
      link_uncertainty_approximation = "laplace_hessian",
      phase_b_global_metric_uncertainty_approximation = "laplace_hessian_marginal_quantiles",
      reliability_link_global = 0.91,
      linking_identified = TRUE,
      link_stop_eligible = FALSE,
      link_stop_pass = FALSE,
      link_state_frozen = FALSE,
      n_pairs_cross_set_done = 3L,
      n_unique_cross_pairs_seen = 3L,
      n_probe_pairs_since_last_refit = 0L,
      n_cross_edges_active_since_last_refit = 1L,
      n_cross_edges_probe_since_last_refit = 0L,
      n_cross_edges_total_since_last_refit = 1L,
      coverage_bins_used = 2L,
      B_spoke_refit_budget = 1L,
      B_spoke_refit_budget_source = "single_spoke_controller",
      stage_target_anchor_link = 1L,
      stage_target_long_link = 0L,
      stage_target_mid_link = 0L,
      stage_target_local_link = 0L,
      feasible_stage_capacity_anchor_link = 1L,
      feasible_stage_capacity_long_link = 0L,
      feasible_stage_capacity_mid_link = 0L,
      feasible_stage_capacity_local_link = 0L,
      feasibility_budget_released = 0L,
      feasibility_reallocation_used = FALSE,
      feasibility_reallocation_rule = "none",
      stage_realized_anchor_link = 1L,
      stage_realized_long_link = 0L,
      stage_realized_mid_link = 0L,
      stage_realized_local_link = 0L,
      stage_shortfall_anchor_link = 0L,
      stage_shortfall_long_link = 0L,
      stage_shortfall_mid_link = 0L,
      stage_shortfall_local_link = 0L,
      stage_reallocation_used = FALSE,
      stage_reallocation_rule_used = "none",
      stage_budget_unfilled = 0L,
      probe_edges_realized_before_refit = 0L,
      probe_edges_realized_delta_since_last_refit = 0L,
      probe_shortfall_reason = "none",
      probe_brier = NA_real_,
      probe_brier_max_used = NA_real_,
      probe_brier_pass = NA,
      probe_pred_rmse_lagged = NA_real_,
      probe_pred_rmse_max_used = NA_real_,
      probe_pred_rmse_pass = NA,
      theta_global_rmse_scope = "direct_evidence_spoke",
      phase_a_within_edges_hub_used = 1L,
      phase_a_within_edges_spoke_used = 1L,
      phase_b_active_edges_used = 1L,
      anchored_joint_hub_items_fixed_count = 2L,
      theta_global_rmse_lagged = NA_real_,
      theta_global_rmse_max_used = NA_real_,
      theta_global_rmse_pass = NA,
      probe_edges_min_for_stop_used = 30L,
      anchored_joint_init_state_method = accepted$anchored_joint_init_state_method,
      anchored_joint_spoke_prior_scale_used = 1.0,
      anchored_joint_sd_floor_used = 0.02,
      anchored_joint_spoke_prior_fallback_used = FALSE,
      anchored_joint_spoke_prior_fallback_sd_used = 1.0,
      judge_params_fixed_for_anchored_joint = TRUE,
      anchored_joint_free_block_dim = 2L,
      resumed_from_session = FALSE
    )
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir, overwrite = TRUE)
  restored <- load_adaptive_session(session_dir)

  expect_identical(restored$linking$anchored_joint$accepted_state_by_spoke[["2"]], accepted)
  expect_identical(
    restored$linking$anchored_joint$fisher_t0_by_spoke[["2"]]$n_link_active_pairs,
    3L
  )
  row <- restored$link_stage_log[1L, , drop = FALSE]
  expect_identical(as.character(row$anchored_joint_init_state_method[[1L]]), "artifact_copy_init")
  expect_identical(row$phase_a_within_edges_hub_used[[1L]], 1L)
  expect_identical(row$phase_a_within_edges_spoke_used[[1L]], 1L)
  expect_identical(row$phase_b_active_edges_used[[1L]], 1L)
  expect_true(isTRUE(row$judge_params_fixed_for_anchored_joint[[1L]]))
  expect_identical(
    as.character(row$phase_b_global_metric_uncertainty_approximation[[1L]]),
    "laplace_hessian_marginal_quantiles"
  )
  print_line <- pairwiseLLM:::.adaptive_print_link_state_line(
    restored,
    list(stopped_spokes = integer())
  )
  expect_true(any(grepl("global_metric_uncertainty=laplace_hessian_marginal_quantiles", print_line)))
  item_log <- pairwiseLLM:::.adaptive_build_item_log_refit(restored, refit_id = 3L)
  expect_equal(
    item_log$theta_link_eap[item_log$item_id == "a1"],
    accepted$theta_hub_fixed[["a1"]]
  )
  expect_equal(
    item_log$theta_link_eap[item_log$item_id == "b1"],
    accepted$theta_spoke_global_mean[["b1"]]
  )
  expect_equal(item_log$theta_link_sd[item_log$item_id == "a1"], 0)
})

test_that("load_adaptive_session aborts on anchored-joint accepted-state provenance drift", {
  state <- make_anchored_joint_resume_state()
  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir, overwrite = TRUE)

  broken <- readRDS(file.path(session_dir, "state.rds"))
  broken$linking$anchored_joint$accepted_state_by_spoke[["2"]]$phase_a_evidence_hash_hub <- "bad_hash"
  saveRDS(broken, file.path(session_dir, "state.rds"))

  expect_error(
    load_adaptive_session(session_dir),
    "persisted accepted-state scaffolding could not be preserved"
  )
})
