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

test_that("regression matrix preserves ordinary ranking and explicit E1--E3 resume", {
  withr::local_seed(20260214)
  state <- adaptive_rank_start(make_test_items(8), seed = 42L)
  fit_stub <- make_deterministic_fit_fn(state$item_ids)
  judge <- make_deterministic_judge("i_wins")
  first <- adaptive_rank_run_live(state, judge, n_steps = 8L, fit_fn = fit_stub$fit_fn,
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)), progress = "none")
  dir <- withr::local_tempdir()
  save_adaptive_session(first, dir)
  resumed <- adaptive_rank_resume(dir)
  second <- adaptive_rank_run_live(resumed, judge, n_steps = 4L, fit_fn = fit_stub$fit_fn,
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)), progress = "none")
  expect_equal(second$step_log[seq_len(nrow(first$step_log)), ], first$step_log)
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    for (n_spokes in 1:2) {
      inputs <- lapply(seq_len(n_spokes), function(k) {
        args <- link_contract_args(id, 1L)
        args$spoke$set_id <- as.character(k)
        args$cross$B_set <- as.character(k)
        if (id == "joint_offset") {
          args$phase_a$spoke$observations$A_set <- as.character(k)
          args$phase_a$spoke$observations$B_set <- as.character(k)
        }
        do.call(prepare_link_input, args)
      })
      first <- start_link_session(inputs)
      dir <- withr::local_tempdir()
      save_adaptive_session(first, dir)
      resumed <- adaptive_rank_resume(dir)
      expect_identical(resumed, first)
      for (input in inputs) expect_identical(resume_link_session(resumed, input), first)
      expect_equal(nrow(summary(resumed)), n_spokes)
      expect_true(all(summary(resumed)$fit_valid))
      expect_identical(adaptive_get_logs(resumed)$link_stage_log, first$link_stage_log)
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
  expect_error(adaptive_rank_run_live(
    state = base,
    judge = judge,
    n_steps = 1L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke", link_estimation_mode = "fixed_shape_offset",
      hub_id = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
    progress = "none"
  ), class = "pairwiseLLM_link_selector_unvalidated")

  # run
  out_run <- adaptive_rank_run_live(
    state = adaptive_rank_start(items, seed = 92L),
    judge = judge,
    n_steps = 1L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke", link_estimation_mode = "fixed_shape_offset",
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
    n_steps = 1L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke", link_estimation_mode = "fixed_shape_offset",
      hub_id = 1L,
      phase_a_mode = "mixed",
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
    adaptive_config = list(run_mode = "link_one_spoke", link_estimation_mode = "fixed_shape_offset", hub_id = 1L, phase_a_mode = "run")
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
    adaptive_config = list(run_mode = "link_one_spoke", link_estimation_mode = "fixed_shape_offset", hub_id = 1L, phase_a_mode = "import")
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

test_that("E1 frozen spokes are removed from active and probe routing", {
  state <- adaptive_rank_start(
    matrix_two_set_items(),
    seed = 142L,
    adaptive_config = list(run_mode = "link_one_spoke", link_estimation_mode = "fixed_shape_offset", hub_id = 1L, phase_a_mode = "import")
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
  state$controller$link_estimation_mode <- "fixed_shape_offset"

  frozen <- pairwiseLLM:::.adaptive_link_apply_stop_state(
    state,
    tibble::tibble(
      refit_id = 3L,
      spoke_id = 2L,
      link_stop_pass = TRUE
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
