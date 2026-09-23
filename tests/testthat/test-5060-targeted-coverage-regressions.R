make_covr_multiset_items <- function() {
  tibble::tibble(
    item_id = c("a1", "a2", "b1", "b2"),
    text = c("a1", "a2", "b1", "b2"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("g_a1", "g_a2", "g_b1", "g_b2")
  )
}

make_covr_phase_a_ready_state <- function() {
  state <- adaptive_rank_start(make_covr_multiset_items(), seed = 1L)
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
  state
}

make_covr_phase_a_ready_state_with_evidence <- function() {
  state <- make_covr_phase_a_ready_state()
  ts0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  state$step_log <- append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      timestamp = ts0,
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
  state$step_log <- append_step_log(
    state$step_log,
    list(
      step_id = 2L,
      timestamp = ts0 + 1,
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
  state$round_log <- append_round_log(
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
  state$round_log <- append_round_log(
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
  state
}

make_covr_probe_resume_state <- function() {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22"),
    set_id = c(1L, 1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22")
  )
  state <- adaptive_rank_start(
    items,
    seed = 61L,
    adaptive_config = list(run_mode = "link_one_spoke", link_estimation_mode = "fixed_shape_offset", hub_id = 1L)
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
    artifacts = list(),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L),
    set_stop_pass_by_set = list(`1` = TRUE, `2` = TRUE),
    phase = "phase_b",
    ready_spokes = 2L,
    active_phase_a_set = NA_integer_,
    phase_b_started_at_step = 1L
  )
  state$link_stage_log <- append_link_stage_log(
    new_link_stage_log(),
    list(
      refit_id = 1L,
      spoke_id = 2L,
      hub_id = 1L,
      link_transform_state = "shift_only",
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )
  state
}

make_covr_link_probe_state <- function() {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs31", "gs32")
  )
  state <- adaptive_rank_start(
    items,
    seed = 101L,
    adaptive_config = list(run_mode = "link_multi_spoke", link_estimation_mode = "fixed_shape_offset", hub_id = 1L)
  )
  state$controller$probe_pairs_per_refit_per_spoke <- 0L
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L, 3L),
      source = c("run", "run", "run"),
      status = c("ready", "ready", "ready"),
      validation_message = c("ok", "ok", "ok"),
      artifact_path = c(NA_character_, NA_character_, NA_character_)
    ),
    artifacts = list(),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    phase = "phase_b",
    ready_spokes = c(2L, 3L),
    active_spokes = c(2L, 3L)
  )
  state
}

make_phase_a_anchored_import_state <- function() {
  state <- .adaptive_apply_controller_config(
    make_covr_phase_a_ready_state_with_evidence(),
    adaptive_config = list(
      run_mode = "link_one_spoke", link_estimation_mode = "fixed_shape_offset",
      hub_id = 1L,
      phase_a_mode = "import"
    )
  )
  art1 <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  art2 <- .adaptive_phase_a_build_artifact(state, set_id = 2L)
  art1$quality_gate_accepted <- TRUE
  art2$quality_gate_accepted <- TRUE
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("import", "import"),
      status = c("ready", "ready"),
      validation_message = c("imported", "imported"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(`1` = art1, `2` = art2),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L),
    set_stop_pass_by_set = list(`1` = TRUE, `2` = TRUE),
    phase = "phase_b",
    ready_spokes = 2L,
    active_phase_a_set = NA_integer_
  )
  state
}

test_that("Phase A validators reject incompatible artifacts", {
  state <- make_covr_phase_a_ready_state_with_evidence()
  state_link <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(run_mode = "link_one_spoke", link_estimation_mode = "fixed_shape_offset", hub_id = 1L)
  )

  state_link$linking$phase_a <- list(set_status = tibble::tibble(), phase = "phase_a")
  expect_identical(.adaptive_phase_a_pending_run_sets(state_link), integer())

  state_link$linking$phase_a$phase <- "phase_b"
  ctx <- .adaptive_link_phase_context(
    state_link,
    controller = .adaptive_controller_resolve(state_link)
  )
  expect_identical(ctx$ready_spokes, 2L)

  draws_no_names <- state$btl_fit$btl_posterior_draws
  colnames(draws_no_names) <- NULL
  state$btl_fit$btl_posterior_draws <- draws_no_names
  expect_identical(
    colnames(.adaptive_phase_a_extract_set_draws(state, set_id = 1L)),
    c("a1", "a2")
  )

  expect_identical(
    nrow(.adaptive_phase_a_within_set_evidence_from_state(list(step_log = tibble::tibble()), set_id = 1L)),
    0L
  )

  invalid_step_state <- make_covr_phase_a_ready_state()
  invalid_step_state$step_log <- tibble::tibble(
    pair_id = 1L,
    step_id = 1L,
    A = 99L,
    B = 98L,
    Y = 2L,
    set_i = 1L,
    set_j = 1L
  )
  expect_identical(
    nrow(.adaptive_phase_a_within_set_evidence_from_state(invalid_step_state, set_id = 1L)),
    0L
  )

  valid_evidence <- tibble::tibble(
    pair_id = 1L,
    step_id = 1L,
    A_item = "a1",
    B_item = "a2",
    y_A = 1L
  )
  bad_cases <- list(
    list(
      evidence = valid_evidence[, c("pair_id", "step_id", "A_item", "B_item"), drop = FALSE],
      set_id = 1L,
      expected_n_pairs = NULL,
      pattern = "missing required columns"
    ),
    list(
      evidence = dplyr::mutate(valid_evidence, pair_id = 0L),
      set_id = 1L,
      expected_n_pairs = NULL,
      pattern = "positive integer `pair_id`"
    ),
    list(
      evidence = dplyr::mutate(valid_evidence, step_id = 0L),
      set_id = 1L,
      expected_n_pairs = NULL,
      pattern = "positive integer `step_id`"
    ),
    list(
      evidence = dplyr::bind_rows(valid_evidence, valid_evidence),
      set_id = 1L,
      expected_n_pairs = NULL,
      pattern = "duplicate `pair_id`"
    ),
    list(
      evidence = dplyr::bind_rows(valid_evidence, dplyr::mutate(valid_evidence, pair_id = 2L)),
      set_id = 1L,
      expected_n_pairs = NULL,
      pattern = "duplicate `step_id`"
    ),
    list(
      evidence = dplyr::mutate(valid_evidence, A_item = ""),
      set_id = 1L,
      expected_n_pairs = NULL,
      pattern = "non-empty `A_item`/`B_item`"
    ),
    list(
      evidence = dplyr::mutate(valid_evidence, y_A = 2L),
      set_id = 1L,
      expected_n_pairs = NULL,
      pattern = "encode `y_A` in \\{0, 1\\}"
    ),
    list(
      evidence = valid_evidence,
      set_id = 99L,
      expected_n_pairs = NULL,
      pattern = "No items found for set_id 99"
    ),
    list(
      evidence = dplyr::mutate(valid_evidence, B_item = "b1"),
      set_id = 1L,
      expected_n_pairs = NULL,
      pattern = "outside the set domain"
    ),
    list(
      evidence = valid_evidence,
      set_id = 1L,
      expected_n_pairs = 2L,
      pattern = "did not reconcile to `n_pairs_committed`"
    )
  )
  for (case in bad_cases) {
    expect_error(
      .adaptive_phase_a_validate_within_set_evidence(
        evidence = case$evidence,
        state = make_covr_phase_a_ready_state(),
        set_id = case$set_id,
        expected_n_pairs = case$expected_n_pairs,
        label = "Evidence"
      ),
      case$pattern
    )
  }

  expect_true(nzchar(.adaptive_phase_a_within_set_evidence_hash(NULL)))


})

test_that("persistence resume helpers cover legacy mode inference and resume invariants", {
  aligned_step <- .adaptive_align_log_schema_for_resume(
    tibble::tibble(
      is_cross_set = c(FALSE, TRUE, FALSE),
      run_mode = c(NA_character_, "", "link_probe_holdout")
    ),
    schema_step_log,
    "step_log"
  )
  expect_identical(
    as.character(aligned_step$link_estimation_mode),
    c(NA_character_, "transform", "transform")
  )

  aligned_stage <- .adaptive_align_log_schema_for_resume(
    tibble::tibble(
      transform_frozen = TRUE,
      transform_frozen_refit_id = 7L
    ),
    schema_link_stage_log,
    "link_stage_log"
  )
  expect_true(isTRUE(aligned_stage$link_state_frozen[[1L]]))
  expect_identical(aligned_stage$link_state_frozen_refit_id[[1L]], 7L)
  expect_true(is.na(aligned_stage$link_transform_policy[[1L]]))

  reconciled_missing_pair <- .adaptive_resume_reconcile_refit_meta(
    state = list(item_ids = c("a", "b"), refit_meta = list(last_refit_M_done = 9L)),
    step_log = tibble::tibble(step_id = c(1L, 2L)),
    round_log = tibble::tibble(refit_id = 2L, step_id_at_refit = 3L, total_pairs_done = 0L)
  )
  expect_identical(reconciled_missing_pair$refit_meta$last_refit_M_done, 9L)
  expect_identical(reconciled_missing_pair$refit_meta$last_refit_step, 3L)

  reconciled_holdout_only <- .adaptive_resume_reconcile_refit_meta(
    state = list(item_ids = c("a", "b"), refit_meta = list()),
    step_log = tibble::tibble(
      step_id = 1L,
      pair_id = 1L,
      A = 1L,
      B = 2L,
      run_mode = "link_probe_holdout"
    ),
    round_log = tibble::tibble(refit_id = 1L, step_id_at_refit = 1L, total_pairs_done = 0L)
  )
  expect_identical(reconciled_holdout_only$refit_meta$last_refit_M_done, 0L)

  expect_error(
    .adaptive_resume_reconcile_refit_meta(
      state = list(item_ids = c("a", "b"), refit_meta = list()),
      step_log = tibble::tibble(
        step_id = 1L,
        pair_id = 1L,
        A = 1L,
        B = 2L,
        run_mode = "within_set"
      ),
      round_log = tibble::tibble(refit_id = 1L, step_id_at_refit = 9L, total_pairs_done = 1L)
    ),
    "step_id_at_refit` is out of range"
  )

  expect_identical(.adaptive_read_item_log_files(withr::local_tempdir()), list())

  session_dir <- withr::local_tempdir()
  save_adaptive_session(adaptive_rank_start(make_test_items(3), seed = 1L), session_dir, overwrite = TRUE)
  unlink(file.path(session_dir, "link_stage_log.rds"))
  expect_silent(validate_session_dir(session_dir))

  resume_state <- make_covr_probe_resume_state()
  resume_state$linking$probe <- .adaptive_link_probe_empty_state()
  resume_state$linking$probe$panels_by_spoke <- list(
    `2` = tibble::tibble(
      spoke_id = c(2L, 2L),
      link_epoch_id = c(1L, NA_integer_),
      probe_panel_id = c("panel-a", "panel-a"),
      pair_key = c("p1", "p2")
    )
  )
  expect_error(
    .adaptive_link_probe_resume_validate_spoke(resume_state, spoke_id = 2L),
    "exactly one non-missing `link_epoch_id`"
  )

  resume_state$linking$probe$panels_by_spoke[["2"]] <- tibble::tibble(
    spoke_id = c(2L, 2L),
    link_epoch_id = c(1L, 1L),
    probe_panel_id = c("", NA_character_),
    pair_key = c("p1", "p2")
  )
  expect_error(
    .adaptive_link_probe_resume_validate_spoke(resume_state, spoke_id = 2L),
    "exactly one non-empty `probe_panel_id`"
  )

  resume_state$linking$probe$panels_by_spoke[["2"]] <- tibble::tibble(
    spoke_id = c(2L, 2L),
    link_epoch_id = c(1L, 1L),
    probe_panel_id = c("panel-a", "panel-a"),
    pair_key = c("p1", "p2")
  )
  resume_state$link_stage_log <- append_link_stage_log(
    new_link_stage_log(),
    list(
      refit_id = 1L,
      spoke_id = 2L,
      hub_id = 1L,
      link_transform_state = "shift_only",
      link_stop_pass = FALSE,
      link_state_frozen = FALSE,
      link_epoch_id = 99L,
      probe_panel_id = "panel-a"
    )
  )
  expect_error(
    .adaptive_link_probe_resume_validate_spoke(resume_state, spoke_id = 2L),
    "does not match persisted panel epoch"
  )

  resume_state$link_stage_log$link_epoch_id[[1L]] <- 1L
  resume_state$link_stage_log$probe_panel_id[[1L]] <- "wrong-panel"
  expect_error(
    .adaptive_link_probe_resume_validate_spoke(resume_state, spoke_id = 2L),
    "does not match persisted panel id"
  )
})

test_that("adaptive run helper fallbacks cover remaining probe and phase-scope branches", {
  state <- .adaptive_apply_controller_config(
    adaptive_rank_start(make_covr_multiset_items(), seed = 3L),
    adaptive_config = list(run_mode = "link_one_spoke", link_estimation_mode = "fixed_shape_offset", hub_id = 1L)
  )
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = 99L,
      source = "run",
      status = "pending_finalization",
      validation_message = NA_character_,
      artifact_path = NA_character_
    ),
    phase = "phase_a"
  )
  scope <- .adaptive_link_phase_a_scope(state)
  expect_true(is.na(scope$active_set_n))

  expect_identical(.adaptive_link_refit_shortfalls_map(list(refit_meta = list(), round = list())), list())
  expect_identical(.adaptive_link_refit_exhausted_map(list(refit_meta = list(), round = list())), list())

  probe_state <- .adaptive_link_probe_state(list(linking = list(probe = 1L)))
  expect_true(is.list(probe_state$panels_by_spoke))
  expect_true(is.character(.adaptive_link_probe_panel_id(tibble::tibble())))
  expect_identical(
    .adaptive_link_is_holdout_probe_rows(tibble::tibble(is_holdout_probe_step = TRUE)),
    TRUE
  )

  state_probe <- make_covr_link_probe_state()
  state_probe$linking$probe <- list(
    panels_by_spoke = list(
      `2` = tibble::tibble(
        spoke_id = c(2L, 2L),
        link_epoch_id = c(3L, 3L),
        probe_panel_id = c("panel-a", "panel-b"),
        pair_key = c("p1", "p2")
      )
    ),
    prediction_cache = .adaptive_link_probe_empty_cache(),
    realized_edges = tibble::tibble(
      step_id = 1L,
      pair_id = 1L,
      run_mode = "link_probe_holdout",
      spoke_id = 2L,
      link_epoch_id = 3L,
      probe_panel_id = "panel-a",
      hub_item_id = "h1",
      spoke_item_id = "s21",
      pair_key = "p1",
      Y = 1L
    ),
    collect_holdout_now_by_spoke = list()
  )
  expect_error(
    .adaptive_link_probe_panel_for_spoke(state_probe, spoke_id = 2L, epoch_id = 3L),
    "multiple `probe_panel_id`"
  )

  state_probe$linking$probe$panels_by_spoke[["2"]] <- tibble::tibble(
    spoke_id = 2L,
    link_epoch_id = 3L,
    probe_panel_id = "panel-a",
    pair_key = "p1"
  )
  state_probe$linking$probe$realized_edges <- tibble::tibble(
    step_id = 1L,
    pair_id = 1L,
    run_mode = "link_probe_holdout",
    spoke_id = 2L,
    link_epoch_id = 3L,
    probe_panel_id = "wrong-panel",
    hub_item_id = "h1",
    spoke_item_id = "s21",
    pair_key = "p1",
    Y = 1L
  )
  expect_error(
    .adaptive_link_probe_panel_for_spoke(state_probe, spoke_id = 2L, epoch_id = 3L),
    "does not match the current panel"
  )

  state_independent <- make_covr_link_probe_state()
  state_independent$controller$multi_spoke_mode <- "independent"
  state_independent$controller$link_budget_refit_id <- .adaptive_link_refit_window_id(state_independent)
  state_independent$controller$link_budget_map <- list(
    `2` = list(B_spoke_refit_budget = 1L, B_spoke_refit_budget_source = "a"),
    `3` = list(B_spoke_refit_budget = 1L, B_spoke_refit_budget_source = "b")
  )
  state_independent$link_stage_log <- append_link_stage_log(
    new_link_stage_log(),
    list(
      refit_id = 1L,
      spoke_id = 2L,
      hub_id = 1L,
      link_transform_state = "shift_only",
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )
  state_independent$link_stage_log <- append_link_stage_log(
    state_independent$link_stage_log,
    list(
      refit_id = 1L,
      spoke_id = 3L,
      hub_id = 1L,
      link_transform_state = "shift_only",
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )
  expect_error(
    .adaptive_link_probe_next_holdout_spoke(
      state_independent,
      controller = state_independent$controller,
      eligible_spoke_ids = c(2L, 3L)
    ),
    "common E1--E3"
  )

  expect_null(.adaptive_link_probe_select_holdout(make_covr_link_probe_state(), step_id = 1L, spoke_id = 2L))

  registered <- .adaptive_link_probe_register_commit(
    make_covr_link_probe_state(),
    tibble::tibble(
      step_id = 1L,
      pair_id = 1L,
      run_mode = "link_probe_holdout",
      is_probe_step = TRUE,
      link_spoke_id = NA_integer_
    )
  )
  expect_true(is.list(registered$linking$probe))

  phase_b_empty <- .adaptive_apply_controller_config(
    adaptive_rank_start(make_covr_multiset_items(), seed = 4L),
    adaptive_config = list(run_mode = "link_one_spoke", link_estimation_mode = "fixed_shape_offset", hub_id = 1L)
  )
  phase_b_empty$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("ready", "ready"),
      validation_message = c("ok", "ok"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    phase = "phase_b",
    ready_spokes = integer(),
    active_spokes = integer()
  )
  expect_false(.adaptive_link_all_spokes_stopped(phase_b_empty))
  expect_identical(
    .adaptive_link_effective_active_spokes(phase_b_empty, refit_id = NA_integer_, exclude_exhausted = TRUE),
    2L
  )
  expect_false(.adaptive_link_all_spokes_exhausted(phase_b_empty, refit_id = NA_integer_))
  expect_false(.adaptive_link_phase_b_window_exhausted(
    adaptive_rank_start(make_test_items(3), seed = 5L),
    refit_id = 1L
  ))
})
