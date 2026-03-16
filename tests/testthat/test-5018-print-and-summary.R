test_that("print and summarize_adaptive use adaptive logs", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  judge <- make_deterministic_judge("i_wins")

  withr::local_seed(1)
  state <- adaptive_rank_run_live(state, judge, n_steps = 1L, progress = "none")

  output <- capture.output(print(state))
  expect_true(any(grepl("items", output)))
  expect_true(any(grepl("steps", output)))
  expect_false(any(grepl("batch_log", output)))

  summary <- summarize_adaptive(state)
  expect_true(tibble::is_tibble(summary))
  expect_true(all(c(
    "n_items",
    "steps_attempted",
    "committed_pairs",
    "n_refits",
    "last_stop_decision",
    "last_stop_reason"
  ) %in% names(summary)))
})

test_that("print.adaptive_state exposes linking phase and controller state concisely", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22")
  )
  state <- adaptive_rank_start(
    items,
    seed = 9L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$linking$phase_a$phase <- "phase_b"
  state$linking$phase_a$ready_spokes <- 2L
  state$linking$phase_a$ready_for_phase_b <- TRUE
  state$linking$phase_a$set_status <- tibble::tibble(
    set_id = c(1L, 2L),
    source = c("run", "run"),
    status = c("ready", "ready"),
    validation_message = c("ok", "ok"),
    artifact_path = c(NA_character_, NA_character_)
  )
  state$controller$link_transform_state_by_spoke <- list(`2` = "shift_only")
  state$controller$link_epoch_id_by_spoke <- list(`2` = 3L)
  state$controller$link_state_frozen_by_spoke <- list(`2` = TRUE)
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    state$link_stage_log,
    list(
      refit_id = 2L,
      spoke_id = 2L,
      hub_id = 1L,
      link_estimation_mode = "transform",
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_refit_mode = "shift_only",
      hub_lock_mode = "soft_lock",
      link_epoch_id = 3L,
      probe_panel_id = "panel-epoch-3",
      link_fit_method = "cmdstan_hmc",
      link_uncertainty_approximation = "cmdstan_posterior_draws",
      probe_edges_planned = 30L,
      probe_edges_realized = 18L,
      link_lag_eligible = TRUE,
      link_stop_gate_open = FALSE,
      link_state_frozen = TRUE,
      stop_blocker_codes = "probe_pred_rmse_lagged,theta_global_rmse_lagged"
    )
  )
  output <- capture.output(print(state))

  expect_true(any(grepl("^linking: phase_b", output)))
  expect_true(any(grepl("transform_policy=auto", output)))
  expect_true(any(grepl("transform_state=shift_only", output)))
  expect_true(any(grepl("link_epoch=3", output)))
  expect_true(any(grepl("frozen_spokes=2", output)))
  expect_true(any(grepl("^link review: ", output)))
  expect_true(any(grepl("fit_method=cmdstan_hmc", output)))
  expect_true(any(grepl("probe_panel_id=panel-epoch-3", output)))
  expect_true(any(grepl("probe_edges=18/30", output)))
  expect_true(any(grepl("mode=transform", output)))
  expect_true(any(grepl("stop_blockers=probe_pred_rmse_lagged,theta_global_rmse_lagged", output)))
})

test_that("print.adaptive_state names anchored-joint mode without transform-only fields", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22")
  )
  state <- adaptive_rank_start(
    items,
    seed = 10L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      link_estimation_mode = "anchored_joint",
      hub_lock_mode = "hard_lock"
    )
  )
  state$linking$phase_a$phase <- "phase_b"
  state$linking$phase_a$ready_spokes <- 2L
  state$linking$phase_a$ready_for_phase_b <- TRUE
  state$linking$phase_a$set_status <- tibble::tibble(
    set_id = c(1L, 2L),
    source = c("run", "run"),
    status = c("ready", "ready"),
    validation_message = c("ok", "ok"),
    artifact_path = c(NA_character_, NA_character_)
  )
  state$controller$link_epoch_id_by_spoke <- list(`2` = 4L)
  state$controller$link_state_frozen_by_spoke <- list(`2` = TRUE)
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    state$link_stage_log,
    list(
      refit_id = 3L,
      spoke_id = 2L,
      hub_id = 1L,
      link_estimation_mode = "anchored_joint",
      link_transform_policy = NA_character_,
      link_transform_state = NA_character_,
      link_refit_mode = NA_character_,
      hub_lock_mode = "hard_lock",
      link_epoch_id = 4L,
      probe_panel_id = "panel-epoch-4",
      link_fit_method = "map_laplace",
      link_uncertainty_approximation = "laplace_hessian",
      probe_edges_planned = 30L,
      probe_edges_realized = 30L,
      link_lag_eligible = TRUE,
      link_stop_gate_open = TRUE,
      link_state_frozen = TRUE,
      anchored_joint_init_state_method = "artifact_copy_init",
      phase_a_within_edges_hub_used = 11L,
      phase_a_within_edges_spoke_used = 7L,
      phase_b_active_edges_used = 5L
    )
  )

  output <- capture.output(print(state))

  expect_true(any(grepl("estimation_mode=anchored_joint", output)))
  expect_true(any(grepl("mode=anchored_joint", output)))
  expect_true(any(grepl("init_state=artifact_copy_init", output)))
  expect_true(any(grepl("frozen_spokes=2", output)))
  expect_true(any(grepl("evidence_edges=11\\+7\\+5", output)))
  expect_false(any(grepl("transform_policy=", output)))
  expect_false(any(grepl("transform_state=", output)))
})

test_that("adaptive_get_logs and print preserve free hub-lock mode", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22")
  )
  state <- adaptive_rank_start(
    items,
    seed = 18L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      link_refit_mode = "joint_refit",
      hub_lock_mode = "free"
    )
  )
  state$linking$phase_a$phase <- "phase_b"
  state$linking$phase_a$ready_spokes <- 2L
  state$linking$phase_a$ready_for_phase_b <- TRUE
  state$linking$phase_a$set_status <- tibble::tibble(
    set_id = c(1L, 2L),
    source = c("run", "run"),
    status = c("ready", "ready"),
    validation_message = c("ok", "ok"),
    artifact_path = c(NA_character_, NA_character_)
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
      link_refit_mode = "joint_refit",
      hub_lock_mode = "free",
      link_fit_method = "cmdstan_hmc",
      link_uncertainty_approximation = "cmdstan_posterior_draws",
      hub_anchored = FALSE,
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )

  logs <- adaptive_get_logs(state)
  expect_identical(as.character(logs$link_stage_log$hub_lock_mode[[1L]]), "free")
  expect_no_error(capture.output(print(state)))
})
