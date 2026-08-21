matrix_two_set_items <- function() {
  tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs23")
  )
}

matrix_two_set_linking_items <- function() {
  tibble::tibble(
    item_id = c(paste0("h", seq_len(10L)), paste0("s2", seq_len(6L))),
    set_id = c(rep.int(1L, 10L), rep.int(2L, 6L)),
    global_item_id = c(paste0("gh", seq_len(10L)), paste0("gs2", seq_len(6L)))
  )
}

matrix_three_set_items <- function() {
  tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23", "s31", "s32", "s33"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs23", "gs31", "gs32", "gs33")
  )
}

matrix_three_set_linking_items <- function() {
  tibble::tibble(
    item_id = c(
      paste0("h", seq_len(10L)),
      paste0("s2", seq_len(6L)),
      paste0("s3", seq_len(6L))
    ),
    set_id = c(rep.int(1L, 10L), rep.int(2L, 6L), rep.int(3L, 6L)),
    global_item_id = c(
      paste0("gh", seq_len(10L)),
      paste0("gs2", seq_len(6L)),
      paste0("gs3", seq_len(6L))
    )
  )
}

matrix_score_judge <- function(scores) {
  score_names <- names(scores)
  scores <- as.double(scores)
  names(scores) <- score_names
  function(A, B, state, ...) {
    a <- as.character(A$item_id[[1L]])
    b <- as.character(B$item_id[[1L]])
    y <- as.integer(scores[[a]] >= scores[[b]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }
}

matrix_import_artifacts <- function(state, spoke_shift = -1.0) {
  ids <- as.character(state$item_ids)
  draws <- matrix(seq_along(ids), nrow = 4L, ncol = length(ids), byrow = TRUE)
  colnames(draws) <- ids
  state$btl_fit <- make_test_btl_fit(ids, draws = draws, model_variant = "btl_e_b")
  set_ids <- sort(unique(as.integer(state$items$set_id)))
  out <- lapply(set_ids, function(set_id) {
    art <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, set_id = as.integer(set_id))
    if (!identical(as.integer(set_id), 1L)) {
      art$items$theta_raw_mean <- as.double(art$items$theta_raw_mean + spoke_shift)
    }
    art <- add_test_phase_a_evidence(art, state = state, set_id = set_id)
    art$quality_gate_accepted <- TRUE
    art
  })
  names(out) <- as.character(set_ids)
  out
}

test_that("regression matrix smoke covers baseline/linking modes and resume paths", {
  withr::local_seed(20260214)

  scenarios <- list(
    list(
      name = "single_set",
      items = make_test_items(8),
      adaptive_config = list(run_mode = "within_set"),
      linking = FALSE
    ),
    list(
      name = "link_one_spoke",
      items = matrix_two_set_linking_items(),
      adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, phase_a_mode = "import"),
      linking = TRUE
    ),
    list(
      name = "link_multi_independent",
      items = matrix_three_set_linking_items(),
      adaptive_config = list(
        run_mode = "link_multi_spoke",
        hub_id = 1L,
        multi_spoke_mode = "independent",
        phase_a_mode = "import"
      ),
      linking = TRUE
    ),
    list(
      name = "link_multi_concurrent",
      items = matrix_three_set_linking_items(),
      adaptive_config = list(
        run_mode = "link_multi_spoke",
        hub_id = 1L,
        multi_spoke_mode = "concurrent",
        min_cross_set_pairs_per_spoke_per_refit = 1L,
        phase_a_mode = "import"
      ),
      linking = TRUE
    )
  )

  for (sc in scenarios) {
    session_dir <- file.path(withr::local_tempdir(), sc$name)
    state <- adaptive_rank_start(sc$items, seed = 42L)
    fit_stub <- make_deterministic_fit_fn(as.character(state$item_ids))
    judge <- if (isTRUE(sc$linking)) {
      matrix_score_judge(stats::setNames(seq_len(nrow(sc$items)), as.character(sc$items$item_id)))
    } else {
      make_deterministic_judge("i_wins")
    }

    cfg <- sc$adaptive_config
    if (isTRUE(sc$linking)) {
      cfg <- utils::modifyList(
        cfg,
        list(
          probe_panel_edges = 18L,
          probe_pairs_per_refit_per_spoke = 1L,
          probe_edges_min_for_stop = 2L,
          probe_active_floor_min = 1L,
          probe_active_floor_frac = 0,
          probe_active_floor_requires_anchor_progress = FALSE,
          link_refit_pairs_per_spoke_rule = "fixed"
        )
      )
      state$warm_start_done <- TRUE
      state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
      cfg$phase_a_artifacts <- matrix_import_artifacts(state, spoke_shift = -1)
    }

    first <- adaptive_rank_run_live(
      state = state,
      judge = judge,
      n_steps = 8L,
      fit_fn = fit_stub$fit_fn,
      adaptive_config = cfg,
      btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
      session_dir = session_dir,
      progress = "none"
    )
    save_adaptive_session(first, session_dir = session_dir, overwrite = TRUE)
    resumed <- adaptive_rank_resume(session_dir = session_dir)
    prev <- resumed$step_log
    prev_link_rows <- nrow(resumed$link_stage_log %||% tibble::tibble())

    second <- adaptive_rank_run_live(
      state = resumed,
      judge = judge,
      n_steps = if (isTRUE(sc$linking)) 50L else 4L,
      fit_fn = fit_stub$fit_fn,
      adaptive_config = cfg,
      btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
      session_dir = session_dir,
      progress = "none"
    )
    expect_true(nrow(second$step_log) >= nrow(prev))
    expect_equal(second$step_log[seq_len(nrow(prev)), , drop = FALSE], prev)

    if (isTRUE(sc$linking)) {
      expect_true(any(second$step_log$is_cross_set %in% TRUE))
      expect_true(nrow(second$link_stage_log) >= 1L)
      appended_link_rows <- second$link_stage_log[
        seq.int(from = prev_link_rows + 1L, to = nrow(second$link_stage_log)),
        ,
        drop = FALSE
      ]
      appended_link_rows <- appended_link_rows[!is.na(appended_link_rows$refit_id), , drop = FALSE]
      expect_true(nrow(appended_link_rows) >= 1L)
      complete_probe_rows <- appended_link_rows[
        !is.na(appended_link_rows$probe_edges_realized_before_refit) &
          !is.na(appended_link_rows$probe_edges_realized_delta_since_last_refit) &
          !is.na(appended_link_rows$probe_edges_realized),
        ,
        drop = FALSE
      ]
      if (nrow(complete_probe_rows) >= 1L) {
        expect_true(all(
          as.integer(complete_probe_rows$probe_edges_realized_before_refit) +
            as.integer(complete_probe_rows$probe_edges_realized_delta_since_last_refit) ==
            as.integer(complete_probe_rows$probe_edges_realized)
        ))
      }
    }
  }
})

test_that("phase A workflow matrix executes run/import/mixed paths", {
  withr::local_seed(20260214)
  items <- matrix_two_set_items()
  judge <- matrix_score_judge(c(h1 = -0.4, h2 = 0.0, h3 = 0.6, s21 = -0.3, s22 = 0.2, s23 = 0.8))
  base <- adaptive_rank_start(items, seed = 91L)
  base$warm_start_done <- TRUE
  base$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts <- matrix_import_artifacts(base, spoke_shift = -1)
  fit_stub <- make_deterministic_fit_fn(as.character(base$item_ids))

  # import
  out_import <- adaptive_rank_run_live(
    state = base,
    judge = judge,
    n_steps = 10L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
    progress = "none"
  )
  status_import <- tibble::as_tibble(out_import$linking$phase_a$set_status)
  expect_true(all(status_import$source == "import"))

  # run
  out_run <- adaptive_rank_run_live(
    state = adaptive_rank_start(items, seed = 92L),
    judge = judge,
    n_steps = 10L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "run"
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
    progress = "none"
  )
  status_run <- tibble::as_tibble(out_run$linking$phase_a$set_status)
  expect_true(all(status_run$source == "run"))

  # mixed
  out_mixed <- adaptive_rank_run_live(
    state = adaptive_rank_start(items, seed = 93L),
    judge = judge,
    n_steps = 10L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "mixed",
      phase_a_set_source = c(`1` = "import", `2` = "run"),
      phase_a_artifacts = list(`1` = artifacts[["1"]])
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
    progress = "none"
  )
  status_mixed <- tibble::as_tibble(out_mixed$linking$phase_a$set_status)
  expect_identical(status_mixed$source[match(1L, status_mixed$set_id)], "import")
  expect_identical(status_mixed$source[match(2L, status_mixed$set_id)], "run")
})

test_that("phase-a scoped lag eligibility resets by active set domain history", {
  items <- matrix_two_set_items()
  state <- adaptive_rank_start(
    items,
    seed = 121L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, phase_a_mode = "run")
  )
  ids <- as.character(state$item_ids)
  draws <- matrix(
    c(
      0.8, 0.5, 0.2, -0.4, -0.6, -0.8,
      0.9, 0.6, 0.1, -0.3, -0.5, -0.7,
      0.7, 0.4, 0.3, -0.5, -0.7, -0.9,
      1.0, 0.7, 0.0, -0.2, -0.4, -0.6
    ),
    nrow = 4,
    byrow = TRUE
  )
  colnames(draws) <- ids
  state$btl_fit <- make_test_btl_fit(ids, draws = draws, model_variant = "btl_e_b")
  state$linking$phase_a$set_status <- tibble::tibble(
    set_id = c(1L, 2L),
    source = c("run", "run"),
    status = c("ready", "pending_finalization"),
    validation_message = c("ok", "pending"),
    artifact_path = c(NA_character_, NA_character_)
  )

  state$refit_meta$theta_mean_history <- list(
    stats::setNames(c(0.5, 0.4, 0.3, -0.2, -0.3, -0.4), ids),
    stats::setNames(c(0.55, 0.45, 0.35, -0.15, -0.25, -0.35), ids),
    stats::setNames(c(0.6, 0.5, 0.4, -0.1, -0.2, -0.3), ids)
  )
  state$refit_meta$theta_mean_history_by_phase_a_set <- list(
    `2` = list(
      stats::setNames(c(0.6, 0.5, 0.4, -0.1, -0.2, -0.3), ids)
    )
  )

  metrics <- pairwiseLLM:::compute_stop_metrics(
    state = state,
    config = list(stability_lag = 1L)
  )
  expect_true(isTRUE(metrics$lag_eligible))
  expect_false(isTRUE(metrics$lag_eligible_scope))
})

test_that("freeze state in regression matrix remains one-way across subsequent updates", {
  items <- matrix_two_set_items()
  state <- adaptive_rank_start(
    items,
    seed = 141L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, phase_a_mode = "import")
  )
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  state$linking$phase_a$phase <- "phase_b"
  state$linking$phase_a$ready_spokes <- 2L

  frozen_once <- pairwiseLLM:::.adaptive_link_apply_stop_state(
    state,
    tibble::tibble(
      refit_id = 1L,
      spoke_id = 2L,
      link_stop_pass = TRUE,
      link_transform_state = "shift_only",
      delta_spoke_mean = 0.22,
      log_alpha_spoke_mean = NA_real_
    )
  )
  frozen_twice <- pairwiseLLM:::.adaptive_link_apply_stop_state(
    frozen_once,
    tibble::tibble(
      refit_id = 2L,
      spoke_id = 2L,
      link_stop_pass = FALSE
    )
  )

  expect_true(isTRUE(frozen_twice$controller$link_state_frozen_by_spoke[["2"]]))
  expect_identical(frozen_twice$controller$link_state_frozen_refit_id_by_spoke[["2"]], 1L)
  expect_equal(frozen_twice$controller$link_transform_frozen_delta_by_spoke[["2"]], 0.22, tolerance = 1e-12)
})

test_that("anchored-joint frozen spokes are removed from active and probe routing", {
  state <- adaptive_rank_start(
    matrix_two_set_items(),
    seed = 142L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, phase_a_mode = "import")
  )
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  state$linking$phase_a$phase <- "phase_b"
  state$linking$phase_a$ready_spokes <- 2L
  state$linking$phase_a$active_spokes <- 2L
  state$controller$probe_edges_min_for_stop <- 1L
  state$controller$link_epoch_id_by_spoke <- list(`2` = 1L)
  state$linking$probe <- pairwiseLLM:::.adaptive_link_probe_empty_state()
  state$linking$probe$panels_by_spoke[["2"]] <- tibble::tibble(
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
  state$controller$link_estimation_mode <- "anchored_joint"
  state$controller$hub_lock_mode <- "hard_lock"

  frozen <- pairwiseLLM:::.adaptive_link_apply_stop_state(
    state,
    tibble::tibble(
      refit_id = 3L,
      spoke_id = 2L,
      link_stop_pass = TRUE,
      link_estimation_mode = "anchored_joint"
    )
  )

  expect_true(isTRUE(frozen$controller$link_state_frozen_by_spoke[["2"]]))
  expect_identical(pairwiseLLM:::.adaptive_link_effective_active_spokes(frozen), integer())
  expect_true(is.na(pairwiseLLM:::.adaptive_link_probe_next_holdout_spoke(
    frozen,
    controller = frozen$controller
  )))
  expect_true(isTRUE(pairwiseLLM:::.adaptive_link_all_spokes_stopped(frozen)))
})
