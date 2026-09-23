make_probe_resume_state <- function() {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22"),
    set_id = c(1L, 1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22")
  )
  state <- adaptive_rank_start(
    items,
    seed = 61L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L
    )
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
  state$linking$phase_a$artifacts[["1"]] <- add_test_phase_a_evidence(
    state$linking$phase_a$artifacts[["1"]],
    state = state,
    set_id = 1L
  )
  state$linking$phase_a$artifacts[["2"]] <- add_test_phase_a_evidence(
    state$linking$phase_a$artifacts[["2"]],
    state = state,
    set_id = 2L
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
      phase_a_mode = "import"
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
  art1 <- add_test_phase_a_evidence(art1, state = state, set_id = 1L)
  art2 <- add_test_phase_a_evidence(art2, state = state, set_id = 2L)
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

test_that("save/load rebuilds the Phase A committed-pair cache from canonical history", {
  state <- make_anchored_joint_resume_state()
  state$linking$phase_a$phase <- "phase_a"
  state$refit_meta$phase_a_committed_pairs_by_set <- c(`1` = 99L, `2` = 0L)

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)
  restored <- load_adaptive_session(session_dir)

  expect_identical(restored$refit_meta$phase_a_committed_pairs_by_set, c(`1` = 1L, `2` = 1L))
})

test_that("save/load preserves tasklist 01 step_log audit fields exactly", {
  items <- make_test_items(3)
  state <- adaptive_rank_start(items)
  judge <- function(A, B, state, ...) {
    list(
      is_valid = TRUE,
      Y = 1L,
      backend = "openai",
      model = "gpt-5.1",
      endpoint = "responses",
      status_code = 200L,
      error_message = NA_character_,
      custom_id = "persist-custom",
      prompt_tokens = 13,
      completion_tokens = 5,
      total_tokens = 18,
      raw_response = list(ok = TRUE, picked = as.character(A$item_id[[1L]]))
    )
  }

  withr::local_seed(1)
  state <- adaptive_rank_run_live(state, judge, n_steps = 1L, progress = "none")

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)
  loaded <- load_adaptive_session(session_dir)

  cols <- c(
    "i_id", "j_id", "A_id", "B_id", "unordered_key", "ordered_key",
    "judge_backend", "judge_model", "judge_endpoint", "judge_valid",
    "judge_invalid_reason", "llm_status_code", "llm_error_message",
    "llm_custom_id", "prompt_tokens", "completion_tokens", "total_tokens",
    "raw_response_json"
  )
  expect_identical(loaded$step_log[cols], state$step_log[cols])
})

test_that("load_adaptive_session backfills tasklist 01 step_log audit fields", {
  state <- adaptive_rank_start(make_test_items(3), seed = 2L)
  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir, overwrite = TRUE)

  step_path <- file.path(session_dir, "step_log.rds")
  step_log <- readRDS(step_path)
  drop_cols <- c(
    "i_id", "j_id", "A_id", "B_id", "unordered_key", "ordered_key",
    "judge_backend", "judge_model", "judge_endpoint", "judge_valid",
    "judge_invalid_reason", "llm_status_code", "llm_error_message",
    "llm_custom_id", "prompt_tokens", "completion_tokens", "total_tokens",
    "raw_response_json"
  )
  step_log <- step_log[, setdiff(names(step_log), drop_cols), drop = FALSE]
  saveRDS(step_log, step_path)

  expect_error(validate_session_dir(session_dir), "missing required columns")
  loaded <- load_adaptive_session(session_dir)
  expect_true(all(drop_cols %in% names(loaded$step_log)))
  expect_true(all(vapply(drop_cols, function(col) all(is.na(loaded$step_log[[col]])), logical(1L))))
  expect_true(is.character(loaded$step_log$raw_response_json))
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

test_that("save_adaptive_session overwrite preserves existing item-log files", {
  items <- make_test_items(6)
  judge <- make_deterministic_judge("i_wins")
  stub <- make_deterministic_fit_fn(items$item_id)

  state_one_refit <- adaptive_rank_start(items, persist_item_log = TRUE)
  withr::local_seed(1)
  state_one_refit <- adaptive_rank_run_live(
    state_one_refit,
    judge,
    n_steps = 2L,
    fit_fn = stub$fit_fn,
    btl_config = list(refit_pairs_target = 2L),
    progress = "none"
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state_one_refit, session_dir, overwrite = TRUE)

  refit_1_path <- file.path(session_dir, "item_log", "refit_0001.rds")
  expect_true(file.exists(refit_1_path))
  refit_1_mtime <- file.info(refit_1_path)$mtime

  Sys.sleep(1.1)

  state_two_refits <- adaptive_rank_start(items, persist_item_log = TRUE)
  withr::local_seed(1)
  state_two_refits <- adaptive_rank_run_live(
    state_two_refits,
    judge,
    n_steps = 4L,
    fit_fn = stub$fit_fn,
    btl_config = list(refit_pairs_target = 2L),
    progress = "none"
  )

  save_adaptive_session(state_two_refits, session_dir, overwrite = TRUE)

  expect_true(file.exists(refit_1_path))
  expect_true(file.exists(file.path(session_dir, "item_log", "refit_0002.rds")))
  expect_identical(file.info(refit_1_path)$mtime, refit_1_mtime)
})

test_that("save_adaptive_session overwrite preserves existing phase-a artifact files", {
  state <- make_anchored_joint_resume_state()
  state$linking$phase_a$phase <- "phase_a"
  session_dir <- withr::local_tempdir()

  save_adaptive_session(state, session_dir, overwrite = TRUE)

  artifact_path <- file.path(session_dir, "phase_a_artifacts", "set_0001.rds")
  expect_true(file.exists(artifact_path))
  artifact_mtime <- file.info(artifact_path)$mtime

  Sys.sleep(1.1)

  save_adaptive_session(state, session_dir, overwrite = TRUE)

  expect_true(file.exists(artifact_path))
  expect_identical(file.info(artifact_path)$mtime, artifact_mtime)
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

test_that("save/load preserves legacy broad-surface Phase A artifact reuse without manual hash allowlists", {
  state <- make_anchored_joint_resume_state()
  state$linking$phase_a$phase <- "phase_a"
  artifacts <- state$linking$phase_a$artifacts

  legacyize <- function(artifact) {
    legacy_surface <- list(
      set_id = as.integer(artifact$set_id %||% NA_integer_),
      judge_param_mode = as.character(artifact$judge_param_mode %||% "global_shared"),
      model_variant = as.character(artifact$fit_model_id %||% "btl_e_b"),
      link_refit_mode = "shift_only",
      shift_only_theta_treatment = "fixed_eap_plugin_var",
      link_transform_policy = "auto",
      hub_lock_mode = "soft_lock",
      hub_lock_kappa = 0.75,
      cross_set_utility = "linking_d_optimal"
    )
    artifact$fit_config_surface <- legacy_surface
    artifact$fit_config_hash <- pairwiseLLM:::.adaptive_phase_a_hash_object(legacy_surface)
    artifact
  }

  state$linking$phase_a$artifacts <- lapply(artifacts, legacyize)
  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir, overwrite = TRUE)

  restored <- load_adaptive_session(session_dir)
  restored$btl_fit <- NULL
  restored <- .adaptive_apply_controller_config(
    restored,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = list()
    )
  )

  expect_no_error(
    restored <- .adaptive_phase_a_prepare(restored)
  )
  status <- tibble::as_tibble(restored$linking$phase_a$set_status)
  expect_true(all(status$status == "ready"))
})

test_that("legacy Phase B files reject before schema backfill or posterior reconstruction", {
  state <- make_anchored_joint_resume_state()
  expect_error(save_adaptive_session(state, withr::local_tempdir()),
    class = "pairwiseLLM_unsupported_legacy_link_state")
  # Start with a complete valid Phase A directory, then reproduce old Phase B
  # state metadata. Logs may have obsolete columns: rejection must precede repair.
  phase_a <- state
  phase_a$linking$phase_a$phase <- "phase_a"
  dir <- withr::local_tempdir()
  save_adaptive_session(phase_a, dir)
  saveRDS(state, file.path(dir, "state.rds"))
  saveRDS(data.frame(anchored_joint_init_state_method = "phase_b_refit"),
    file.path(dir, "link_stage_log.rds"))
  expect_error(load_adaptive_session(dir), "Restart linking from compatible Phase A",
    class = "pairwiseLLM_unsupported_legacy_link_state")
  expect_error(validate_session_dir(dir), class = "pairwiseLLM_unsupported_legacy_link_state")
  expect_error(adaptive_rank_resume(dir), class = "pairwiseLLM_unsupported_legacy_link_state")
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
      phase_a_mode = "import"
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

  expect_identical(restored$controller$link_transform_state_by_spoke, list())
  expect_true(isTRUE(restored$controller$link_state_frozen_by_spoke[["2"]]))
  expect_identical(restored$controller$link_state_frozen_refit_id_by_spoke[["2"]], 3L)
  expect_identical(restored$controller$link_epoch_id_by_spoke[["2"]], 4L)
  expect_identical(restored$controller$link_epoch_start_step_by_spoke[["2"]], 8L)
  expect_identical(restored$controller$link_escalation_recent_pass_window_by_spoke[["2"]], c(TRUE))
})


test_that("save/load strips runtime-only refit caches from persisted state", {
  state <- adaptive_rank_start(c("h1", "h2", "s21"))
  state$refit_meta$committed_results_cache <- tibble::tibble(
    pair_id = 1L,
    step_id = 1L,
    A_id = "h1",
    B_id = "h2",
    Y = 1L,
    timestamp = as.POSIXct("2026-01-02 00:00:00", tz = "UTC"),
    is_cross_set = FALSE
  )
  state$refit_meta$committed_results_cache_built <- TRUE
  state$refit_meta$link_cross_edges_by_spoke <- list(
    `2` = tibble::tibble(
      spoke_item = "s21",
      hub_item = "h1",
      y_spoke = 1L,
      step_id = 2L,
      spoke_in_A = FALSE,
      run_mode = "link_probe_holdout",
      is_probe_step = TRUE,
      link_stage = "anchor_link",
      fallback_used = "probe_panel_acceleration"
    )
  )
  state$refit_meta$link_cross_edges_cache_built <- TRUE

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  persisted_state <- readRDS(file.path(session_dir, "state.rds"))
  expect_null(persisted_state$refit_meta[["committed_results_cache", exact = TRUE]])
  expect_false(isTRUE(persisted_state$refit_meta$committed_results_cache_built))
  expect_null(persisted_state$refit_meta[["link_cross_edges_by_spoke", exact = TRUE]])
  expect_false(isTRUE(persisted_state$refit_meta$link_cross_edges_cache_built))

  restored <- load_adaptive_session(session_dir)
  expect_null(restored$refit_meta[["committed_results_cache", exact = TRUE]])
  expect_false(isTRUE(restored$refit_meta$committed_results_cache_built))
  expect_null(restored$refit_meta[["link_cross_edges_by_spoke", exact = TRUE]])
  expect_false(isTRUE(restored$refit_meta$link_cross_edges_cache_built))
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
      hub_id = 1L
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
