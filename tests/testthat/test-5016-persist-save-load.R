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
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_refit_mode = "shift_only",
      hub_lock_mode = "soft_lock",
      link_stop_pass = FALSE,
      transform_frozen = FALSE
    )
  )
  state
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
  state$controller$link_transform_frozen_by_spoke <- list(`2` = TRUE)
  state$controller$link_transform_frozen_refit_id_by_spoke <- list(`2` = 3L)
  state$controller$link_epoch_id_by_spoke <- list(`2` = 4L)
  state$controller$link_epoch_start_step_by_spoke <- list(`2` = 8L)
  state$controller$link_escalation_consecutive_pass_count_by_spoke <- list(`2` = 1L)
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_policy = "auto",
      link_transform_state = "shift_scale",
      link_epoch_id = 4L,
      transform_frozen = TRUE,
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
  expect_true(isTRUE(restored$controller$link_transform_frozen_by_spoke[["2"]]))
  expect_identical(restored$controller$link_transform_frozen_refit_id_by_spoke[["2"]], 3L)
  expect_identical(restored$controller$link_epoch_id_by_spoke[["2"]], 4L)
  expect_identical(restored$controller$link_epoch_start_step_by_spoke[["2"]], 8L)
  expect_identical(restored$controller$link_escalation_consecutive_pass_count_by_spoke[["2"]], 1L)
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
    reliability_EAP_link = 0.9,
    linking_identified = TRUE,
    link_stop_eligible = FALSE,
    link_stop_pass = FALSE,
    transform_frozen = FALSE,
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
  expect_identical(as.character(restored$link_stage_log$link_transform_policy[[1L]]), "fixed_shift_only")
  expect_identical(as.character(restored$link_stage_log$link_transform_state[[1L]]), "shift_only")
})

test_that("save/load preserves feasibility and blocker explanation fields in link_stage_log", {
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
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_refit_mode = "shift_only",
      hub_lock_mode = "soft_lock",
      reliability_EAP_link = 0.9,
      linking_identified = TRUE,
      link_stop_eligible = FALSE,
      link_stop_pass = FALSE,
      transform_frozen = FALSE,
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
      blocker_probe_panel_shortfall_weight = 0.5,
      blocker_probe_brier_weight = 1,
      blocker_probe_pred_rmse_weight = 1,
      blocker_theta_global_rmse_weight = 0.5,
      blocker_delta_spoke_sd_weight = 0.25,
      blocker_reweighting_rule = "canonical_metric_excess_ratio_v1"
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
  expect_equal(row$blocker_probe_panel_shortfall_weight[[1L]], 0.5, tolerance = 1e-12)
  expect_identical(
    as.character(row$blocker_reweighting_rule[[1L]]),
    "canonical_metric_excess_ratio_v1"
  )
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
})

test_that("resume preserves probe panel identity, epoch, and realized counts across a chunk boundary", {
  state <- make_probe_resume_state()
  state <- pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins"))

  panel_before <- pairwiseLLM:::.adaptive_link_probe_panel_for_spoke(state, spoke_id = 2L, epoch_id = 1L)
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
      transform_frozen = FALSE,
      link_epoch_id = 1L,
      probe_panel_id = as.character(panel_before$probe_panel_id[[1L]]),
      probe_edges_planned = as.integer(nrow(panel_before)),
      probe_edges_realized = as.integer(realized_before),
      probe_panel_shortfall = as.integer(nrow(panel_before) - realized_before)
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

  resumed <- pairwiseLLM:::run_one_step(restored, make_deterministic_judge("i_wins"))
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
      transform_frozen = FALSE,
      link_epoch_id = 1L,
      probe_panel_id = as.character(panel$probe_panel_id[[1L]]),
      probe_edges_planned = as.integer(nrow(panel)),
      probe_edges_realized = 0L,
      probe_panel_shortfall = as.integer(nrow(panel))
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

test_that("resume aborts when persisted probe state disagrees with canonical logs or controller epoch", {
  state <- make_probe_resume_state()
  state <- pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins"))
  panel <- pairwiseLLM:::.adaptive_link_probe_panel_for_spoke(state, spoke_id = 2L, epoch_id = 1L)
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
      transform_frozen = FALSE,
      link_epoch_id = 1L,
      probe_panel_id = as.character(panel$probe_panel_id[[1L]]),
      probe_edges_planned = as.integer(nrow(panel)),
      probe_edges_realized = as.integer(realized),
      probe_panel_shortfall = as.integer(nrow(panel) - realized)
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
