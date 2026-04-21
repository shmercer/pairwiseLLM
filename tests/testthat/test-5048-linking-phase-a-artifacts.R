make_multiset_items <- function() {
  tibble::tibble(
    item_id = c("a1", "a2", "b1", "b2"),
    text = c("a1", "a2", "b1", "b2"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("g_a1", "g_a2", "g_b1", "g_b2")
  )
}

make_phase_a_ready_state <- function() {
  items <- make_multiset_items()
  state <- adaptive_rank_start(items, seed = 1L)
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

make_phase_a_ready_state_with_evidence <- function() {
  state <- make_phase_a_ready_state()
  ts0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  state$step_log <- pairwiseLLM:::append_step_log(
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
  state$step_log <- pairwiseLLM:::append_step_log(
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
  state
}

make_legacy_phase_a_artifact <- function(artifact,
                                         link_refit_mode = "shift_only",
                                         shift_only_theta_treatment = "fixed_eap_plugin_var",
                                         link_transform_policy = "auto",
                                         hub_lock_mode = "soft_lock",
                                         hub_lock_kappa = 0.75,
                                         cross_set_utility = "linking_d_optimal") {
  legacy_surface <- list(
    set_id = as.integer(artifact$set_id %||% NA_integer_),
    judge_param_mode = as.character(artifact$judge_param_mode %||% "global_shared"),
    model_variant = as.character(artifact$fit_model_id %||% "btl_e_b"),
    link_refit_mode = as.character(link_refit_mode),
    shift_only_theta_treatment = as.character(shift_only_theta_treatment),
    link_transform_policy = as.character(link_transform_policy),
    hub_lock_mode = as.character(hub_lock_mode),
    hub_lock_kappa = if (identical(hub_lock_mode, "soft_lock")) {
      as.double(hub_lock_kappa)
    } else {
      NA_real_
    },
    cross_set_utility = as.character(cross_set_utility)
  )

  legacy <- artifact
  legacy$fit_config_surface <- legacy_surface
  legacy$fit_config_hash <- .adaptive_phase_a_hash_object(legacy_surface)
  legacy
}

test_that("phase A artifacts carry exact within-set evidence surfaces", {
  state <- make_phase_a_ready_state_with_evidence()

  art1 <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  art2 <- .adaptive_phase_a_build_artifact(state, set_id = 2L)

  expect_equal(art1$n_pairs_committed, 1L)
  expect_equal(art2$n_pairs_committed, 1L)
  expect_equal(
    art1$phase_a_within_set_evidence,
    tibble::tibble(
      pair_id = 1L,
      step_id = 1L,
      A_item = "a1",
      B_item = "a2",
      y_A = 1L
    )
  )
  expect_equal(
    art2$phase_a_within_set_evidence,
    tibble::tibble(
      pair_id = 2L,
      step_id = 2L,
      A_item = "b1",
      B_item = "b2",
      y_A = 0L
    )
  )
  expect_true(is.character(art1$phase_a_within_set_evidence_hash))
  expect_identical(art1$phase_a_within_set_evidence_source, "canonical_committed_step_log")
})

test_that("phase A artifacts round-trip through persistence", {
  state <- make_phase_a_ready_state_with_evidence()
  art1 <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  art2 <- .adaptive_phase_a_build_artifact(state, set_id = 2L)
  art1$quality_gate_accepted <- TRUE
  art2$quality_gate_accepted <- TRUE

  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("import", "import"),
      status = c("ready", "ready"),
      validation_message = c("built_in_run", "built_in_run"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(`1` = art1, `2` = art2),
    ready_for_phase_b = TRUE,
    phase = "phase_b"
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir = session_dir)

  expect_true(file.exists(file.path(session_dir, "phase_a_artifacts", "set_0001.rds")))
  expect_true(file.exists(file.path(session_dir, "phase_a_artifacts", "set_0002.rds")))

  restored <- load_adaptive_session(session_dir)
  expect_true(is.list(restored$linking$phase_a$artifacts))
  expect_true(all(c("1", "2") %in% names(restored$linking$phase_a$artifacts)))

  r1 <- restored$linking$phase_a$artifacts[["1"]]
  expect_equal(as.integer(r1$set_id), 1L)
  expect_equal(as.integer(r1$refit_id), 1L)
  expect_true(all(c("global_item_id", "theta_raw_mean", "theta_raw_sd") %in% names(r1$items)))
  expect_equal(nrow(r1$phase_a_within_set_evidence), 1L)
})

test_that("anchored-joint import validation rejects summary-only artifacts without exact evidence", {
  state <- make_phase_a_ready_state()
  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      link_estimation_mode = "anchored_joint",
      hub_lock_mode = "hard_lock"
    )
  )
  source_state <- .adaptive_apply_controller_config(
    make_phase_a_ready_state_with_evidence(),
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      link_estimation_mode = "anchored_joint",
      hub_lock_mode = "hard_lock"
    )
  )
  artifact <- .adaptive_phase_a_build_artifact(source_state, set_id = 1L)
  artifact$quality_gate_accepted <- TRUE
  artifact$phase_a_within_set_evidence <- NULL
  artifact$phase_a_within_set_evidence_hash <- NULL
  artifact$phase_a_within_set_evidence_source <- NULL
  artifact$n_pairs_committed <- 1L
  controller <- .adaptive_controller_resolve(state)

  expect_error(
    .adaptive_phase_a_validate_imported_artifact(
      artifact,
      state,
      set_id = 1L,
      controller = controller
    ),
    "exact within-set committed-edge history is unavailable"
  )
})

test_that("phase_a_prepare scaffolds anchored-joint artifact-copy accepted states", {
  state <- .adaptive_apply_controller_config(
    make_phase_a_ready_state_with_evidence(),
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      link_estimation_mode = "anchored_joint",
      hub_lock_mode = "hard_lock"
    )
  )
  art1 <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  art2 <- .adaptive_phase_a_build_artifact(state, set_id = 2L)
  art1$quality_gate_accepted <- TRUE
  art2$quality_gate_accepted <- TRUE

  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(phase_a_artifacts = list(`1` = art1, `2` = art2))
  )

  state <- .adaptive_phase_a_prepare(state)
  accepted <- state$linking$anchored_joint$accepted_state_by_spoke[["2"]]
  fisher_t0 <- state$linking$anchored_joint$fisher_t0_by_spoke[["2"]]

  expect_identical(state$linking$phase_a$phase, "phase_b")
  expect_identical(accepted$anchored_joint_init_state_method, "artifact_copy_init")
  expect_equal(unname(accepted$theta_hub_fixed[c("a1", "a2")]), c(1.05, 0.85), tolerance = 1e-8)
  expect_equal(unname(accepted$theta_spoke_global_mean[c("b1", "b2")]), c(-0.45, -0.65), tolerance = 1e-8)
  expect_true(isTRUE(fisher_t0$I_s_t0_zero))
  expect_identical(fisher_t0$n_link_active_pairs, 0L)
  expect_identical(fisher_t0$anchored_joint_init_state_method, "artifact_copy_init")
})

test_that("phase A import validation rejects each required failure mode", {
  state <- make_phase_a_ready_state()
  controller <- .adaptive_controller_resolve(state)
  valid <- .adaptive_phase_a_build_artifact(state, set_id = 1L)

  bad_model <- valid
  bad_model$fit_model_id <- "btl"
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(bad_model, state, set_id = 1L, controller = controller),
    "likelihood/model incompatibility"
  )

  bad_identity <- valid
  bad_identity$items$global_item_id[[1L]] <- "wrong_global"
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(bad_identity, state, set_id = 1L, controller = controller),
    "global_item_id mapping mismatch"
  )

  bad_complete <- valid
  bad_complete$items$theta_raw_sd <- NULL
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(bad_complete, state, set_id = 1L, controller = controller),
    "completeness failure"
  )
  bad_rank <- valid
  bad_rank$items$rank_mu_raw <- NULL
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(bad_rank, state, set_id = 1L, controller = controller),
    "missing rank_mu_raw"
  )

  bad_n_items <- valid
  bad_n_items$n_items <- bad_n_items$n_items + 1L
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(bad_n_items, state, set_id = 1L, controller = controller),
    "item-count metadata mismatch"
  )

  bad_pairs_count <- valid
  bad_pairs_count$n_pairs_committed <- NULL
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(bad_pairs_count, state, set_id = 1L, controller = controller),
    "missing `n_pairs_committed`"
  )

  bad_diag <- valid
  bad_diag$diagnostics <- list()
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(bad_diag, state, set_id = 1L, controller = controller),
    "missing diagnostics metadata"
  )

  bad_quality <- valid
  bad_quality$diagnostics$reliability_EAP_within <- NA_real_
  bad_quality$quality_gate_accepted <- FALSE
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(bad_quality, state, set_id = 1L, controller = controller),
    "missing reliability_EAP_within"
  )

  bad_hash <- valid
  bad_hash$judge_param_mode <- "phase_specific"
  bad_hash$fit_config_surface$judge_param_mode <- "phase_specific"
  bad_hash$fit_config_hash <- .adaptive_phase_a_hash_object(bad_hash$fit_config_surface)
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(bad_hash, state, set_id = 1L, controller = controller),
    "within-set fit incompatibility"
  )

  bad_sd_negative <- valid
  bad_sd_negative$items$theta_raw_sd[[1L]] <- -0.01
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(bad_sd_negative, state, set_id = 1L, controller = controller),
    "theta_raw_sd.*non-negative"
  )

  bad_sd_na <- valid
  bad_sd_na$items$theta_raw_sd[[1L]] <- NA_real_
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(bad_sd_na, state, set_id = 1L, controller = controller),
    "completeness failure"
  )

  bad_sd_nan <- valid
  bad_sd_nan$items$theta_raw_sd[[1L]] <- NaN
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(bad_sd_nan, state, set_id = 1L, controller = controller),
    "(completeness failure|theta_raw_sd.*finite)"
  )

  bad_sd_inf <- valid
  bad_sd_inf$items$theta_raw_sd[[1L]] <- Inf
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(bad_sd_inf, state, set_id = 1L, controller = controller),
    "theta_raw_sd.*finite"
  )
})

test_that("phase A import hash rejects inference-setting mismatch and supports allowlisted compatibility", {
  state <- make_phase_a_ready_state()
  artifact <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  artifact$quality_gate_accepted <- TRUE

  state_joint <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(judge_param_mode = "phase_specific")
  )
  controller_joint <- .adaptive_controller_resolve(state_joint)
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(
      artifact,
      state_joint,
      set_id = 1L,
      controller = controller_joint
    ),
    "judge_param_mode"
  )

  controller_allow <- controller_joint
  controller_allow$phase_a_compatible_config_hashes <- artifact$fit_config_hash
  expect_no_error(
    .adaptive_phase_a_validate_imported_artifact(
      artifact,
      state_joint,
      set_id = 1L,
      controller = controller_allow
    )
  )
})

test_that("phase A import ignores Phase B-only legacy surface mismatches", {
  state <- make_phase_a_ready_state()
  artifact <- make_legacy_phase_a_artifact(
    .adaptive_phase_a_build_artifact(state, set_id = 1L),
    link_refit_mode = "shift_only",
    hub_lock_mode = "soft_lock",
    hub_lock_kappa = 0.75
  )
  artifact$quality_gate_accepted <- TRUE

  state_joint <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(
      link_estimation_mode = "transform",
      link_refit_mode = "joint_refit",
      hub_lock_mode = "soft_lock",
      hub_lock_kappa = 0.6
    )
  )
  controller_joint <- .adaptive_controller_resolve(state_joint)
  expect_no_error(
    .adaptive_phase_a_validate_imported_artifact(
      artifact,
      state_joint,
      set_id = 1L,
      controller = controller_joint
    )
  )
})

test_that("phase A run artifacts prefer the latest available refit for the set", {
  state <- make_phase_a_ready_state()
  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "run",
      phase_a_required_reliability_min = 0
    )
  )
  state$history_pairs <- tibble::tibble(A_id = "a1", B_id = "a2")
  stale_artifact <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  stale_artifact$refit_id <- 1L
  stale_artifact$quality_gate_accepted <- TRUE
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("ready", "pending_finalization"),
      validation_message = c("built_in_run_refit_1", "pending_finalization"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(`1` = stale_artifact),
    ready_for_phase_b = FALSE,
    phase = "phase_a",
    active_phase_a_set = 1L
  )
  state$round_log <- pairwiseLLM:::append_round_log(
    state$round_log,
    list(
      refit_id = 2L,
      round_id_at_refit = 3L,
      step_id_at_refit = 12L,
      diagnostics_pass = TRUE,
      phase_scope = "phase_a_set",
      phase_scope_set_id = 1L
    )
  )

  prepared <- .adaptive_phase_a_prepare(state)
  refreshed <- prepared$linking$phase_a$artifacts[["1"]]

  expect_equal(as.integer(refreshed$refit_id), 2L)
  expect_match(
    prepared$linking$phase_a$set_status$validation_message[
      match(1L, prepared$linking$phase_a$set_status$set_id)
    ],
    "built_in_run_refit_2"
  )
})

test_that("phase A prepare memo reuses unchanged run-set preparation results", {
  state <- make_phase_a_ready_state_with_evidence()
  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "run",
      phase_a_required_reliability_min = 0
    )
  )
  state <- .adaptive_phase_a_prepare(state)

  ns <- asNamespace("pairwiseLLM")
  orig_build <- get(".adaptive_phase_a_build_artifact", envir = ns, inherits = FALSE)
  build_calls <- 0L
  reused <- testthat::with_mocked_bindings(
    .adaptive_phase_a_build_artifact = function(...) {
      build_calls <<- build_calls + 1L
      orig_build(...)
    },
    .env = ns,
    {
      .adaptive_phase_a_prepare(state)
    }
  )

  expect_identical(build_calls, 0L)
  expect_equal(
    tibble::as_tibble(reused$linking$phase_a$set_status),
    tibble::as_tibble(state$linking$phase_a$set_status)
  )
  expect_identical(
    reused$linking$phase_a$prepare_context_by_set,
    state$linking$phase_a$prepare_context_by_set
  )
})

test_that("phase A prepare invalidates memo on committed within-set evidence changes", {
  state <- make_phase_a_ready_state_with_evidence()
  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "run",
      phase_a_required_reliability_min = 0
    )
  )
  state <- .adaptive_phase_a_prepare(state)

  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 3L,
      timestamp = as.POSIXct("2026-01-01 00:00:02", tz = "UTC"),
      pair_id = 3L,
      A = 1L,
      B = 2L,
      Y = 0L,
      set_i = 1L,
      set_j = 1L,
      is_cross_set = FALSE,
      run_mode = "within_set"
    )
  )
  state$history_pairs <- tibble::add_row(state$history_pairs, A_id = "a1", B_id = "a2")

  ns <- asNamespace("pairwiseLLM")
  orig_build <- get(".adaptive_phase_a_build_artifact", envir = ns, inherits = FALSE)
  build_calls <- 0L
  updated <- testthat::with_mocked_bindings(
    .adaptive_phase_a_build_artifact = function(...) {
      build_calls <<- build_calls + 1L
      orig_build(...)
    },
    .env = ns,
    {
      .adaptive_phase_a_prepare(state)
    }
  )

  expect_identical(build_calls, 1L)
  expect_equal(updated$linking$phase_a$artifacts[["1"]]$n_pairs_committed, 2L)
  expect_equal(updated$linking$phase_a$artifacts[["2"]]$n_pairs_committed, 1L)
})

test_that("phase A prepare invalidates memo on set-local refit changes", {
  state <- make_phase_a_ready_state_with_evidence()
  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "run",
      phase_a_required_reliability_min = 0
    )
  )
  state <- .adaptive_phase_a_prepare(state)
  state$round_log <- pairwiseLLM:::append_round_log(
    state$round_log,
    list(
      refit_id = 3L,
      round_id_at_refit = 4L,
      step_id_at_refit = 20L,
      diagnostics_pass = TRUE,
      phase_scope = "phase_a_set",
      phase_scope_set_id = 1L
    )
  )

  ns <- asNamespace("pairwiseLLM")
  orig_build <- get(".adaptive_phase_a_build_artifact", envir = ns, inherits = FALSE)
  build_calls <- 0L
  updated <- testthat::with_mocked_bindings(
    .adaptive_phase_a_build_artifact = function(...) {
      build_calls <<- build_calls + 1L
      orig_build(...)
    },
    .env = ns,
    {
      .adaptive_phase_a_prepare(state)
    }
  )

  expect_identical(build_calls, 1L)
  expect_equal(as.integer(updated$linking$phase_a$artifacts[["1"]]$refit_id), 3L)
  expect_equal(as.integer(updated$linking$phase_a$artifacts[["2"]]$refit_id), 2L)
})

test_that("phase A prepare invalidates memo on imported artifact replacement", {
  state <- make_phase_a_ready_state_with_evidence()
  art1 <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  art2 <- .adaptive_phase_a_build_artifact(state, set_id = 2L)
  art1$quality_gate_accepted <- TRUE
  art2$quality_gate_accepted <- TRUE
  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      phase_a_required_reliability_min = 0,
      phase_a_artifacts = list(`1` = art1, `2` = art2)
    )
  )
  state <- .adaptive_phase_a_prepare(state)

  replacement <- art1
  replacement$diagnostics$ts_btl_rank_spearman <- 0.99
  state$controller$phase_a_artifacts[["1"]] <- replacement

  ns <- asNamespace("pairwiseLLM")
  orig_validate <- get(".adaptive_phase_a_validate_imported_artifact", envir = ns, inherits = FALSE)
  validate_calls <- 0L
  updated <- testthat::with_mocked_bindings(
    .adaptive_phase_a_validate_imported_artifact = function(...) {
      validate_calls <<- validate_calls + 1L
      orig_validate(...)
    },
    .env = ns,
    {
      .adaptive_phase_a_prepare(state)
    }
  )

  expect_gte(validate_calls, 1L)
  status <- tibble::as_tibble(updated$linking$phase_a$set_status)
  expect_true(all(status$status == "ready"))
})

test_that("phase A prepare invalidates memo on config changes that affect acceptance", {
  state <- make_phase_a_ready_state_with_evidence()
  art1 <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  art2 <- .adaptive_phase_a_build_artifact(state, set_id = 2L)
  art1$quality_gate_accepted <- TRUE
  art2$quality_gate_accepted <- TRUE
  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      phase_a_required_reliability_min = 0,
      phase_a_artifacts = list(`1` = art1, `2` = art2)
    )
  )
  state <- .adaptive_phase_a_prepare(state)
  state$controller$judge_param_mode <- "phase_specific"

  updated <- .adaptive_phase_a_prepare(state)
  status <- tibble::as_tibble(updated$linking$phase_a$set_status)
  expect_true(all(status$status == "failed"))
  expect_true(all(grepl("fit incompatibility", status$validation_message, fixed = TRUE)))
})

test_that("phase A import hash is stable for fresh sessions without local btl_fit", {
  state_built <- make_phase_a_ready_state()
  artifact <- .adaptive_phase_a_build_artifact(state_built, set_id = 1L)
  artifact$quality_gate_accepted <- TRUE

  state_fresh <- adaptive_rank_start(make_multiset_items(), seed = 11L)
  state_fresh$btl_fit <- NULL
  controller_fresh <- .adaptive_controller_resolve(state_fresh)

  expect_no_error(
    .adaptive_phase_a_validate_imported_artifact(
      artifact,
      state_fresh,
      set_id = 1L,
      controller = controller_fresh
    )
  )
})

test_that("phase A mixed mode supports import and run per set", {
  state <- make_phase_a_ready_state()
  import_set1 <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  import_set1$quality_gate_accepted <- TRUE

  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(
      run_mode = "within_set",
      phase_a_mode = "mixed",
      phase_a_artifacts = list(`1` = import_set1),
      phase_a_set_source = c(`1` = "import", `2` = "run")
    )
  )

  prepared <- .adaptive_phase_a_prepare(state)
  status <- tibble::as_tibble(prepared$linking$phase_a$set_status)

  expect_equal(status$status[match(1L, status$set_id)], "ready")
  expect_equal(status$status[match(2L, status$set_id)], "pending_finalization")
  expect_equal(status$source[match(1L, status$set_id)], "import")
  expect_equal(status$source[match(2L, status$set_id)], "run")
  expect_true(all(c("1", "2") %in% names(prepared$linking$phase_a$artifacts)))
})

test_that("phase A import fallback policy switches to run mode when configured", {
  state <- make_phase_a_ready_state()
  bad_import <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  bad_import$judge_param_mode <- "phase_specific"
  bad_import$fit_config_surface$judge_param_mode <- "phase_specific"
  bad_import$fit_config_hash <- .adaptive_phase_a_hash_object(bad_import$fit_config_surface)
  import_set2 <- .adaptive_phase_a_build_artifact(state, set_id = 2L)
  import_set2$quality_gate_accepted <- TRUE

  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(
      run_mode = "within_set",
      phase_a_mode = "import",
      phase_a_import_failure_policy = "fallback_to_run",
      phase_a_artifacts = list(`1` = bad_import, `2` = import_set2)
    )
  )

  prepared <- .adaptive_phase_a_prepare(state)
  status <- tibble::as_tibble(prepared$linking$phase_a$set_status)

  expect_equal(status$status[match(1L, status$set_id)], "pending_finalization")
  expect_equal(status$status[match(2L, status$set_id)], "ready")
  expect_equal(status$source[match(1L, status$set_id)], "run")
  expect_match(status$validation_message[match(1L, status$set_id)], "pending_finalization")
})

test_that("phase_a_mode=run executes Phase A within-set steps before Phase B", {
  state <- adaptive_rank_start(make_multiset_items(), seed = 1L)
  judge <- make_deterministic_judge("i_wins")

  out <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 1L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "run"
    ),
    progress = "none"
  )

  expect_equal(nrow(out$step_log), 1L)
  expect_true(all(out$step_log$is_cross_set %in% FALSE))
  expect_true(all(out$step_log$set_i == out$step_log$set_j))
  expect_true(all(is.na(out$step_log$link_stage)))
  expect_true(all(is.na(out$step_log$delta_spoke_estimate_pre)))
  expect_true(all(is.na(out$step_log$delta_spoke_sd_pre)))
  expect_true(all(is.na(out$step_log$posterior_win_prob_pre)))
  expect_true(all(is.na(out$step_log$link_transform_policy)))
  expect_true(all(is.na(out$step_log$link_transform_state)))
  expect_true(all(is.na(out$step_log$cross_set_utility_pre)))
  expect_true(all(out$step_log$utility_mode %in% c("pairing_trueskill_u0")))
  expect_true(all(is.na(out$step_log$log_alpha_spoke_estimate_pre)))
  expect_true(all(is.na(out$step_log$log_alpha_spoke_sd_pre)))
  expect_true(all(is.na(out$step_log$hub_lock_mode)))
  expect_true(all(is.na(out$step_log$hub_lock_kappa)))
})

test_that("phase A gate allows pending run sets and blocks failed imports", {
  state <- adaptive_rank_start(make_multiset_items(), seed = 1L)
  state <- .adaptive_apply_controller_config(state, adaptive_config = list(
    run_mode = "link_one_spoke",
    hub_id = 1L,
    phase_a_mode = "run"
  ))
  state <- .adaptive_phase_a_prepare(state)
  expect_no_error(.adaptive_phase_a_gate_or_abort(state))

  status_tbl <- tibble::as_tibble(state$linking$phase_a$set_status)
  status_tbl$status[] <- "failed"
  status_tbl$source[] <- "import"
  status_tbl$validation_message[] <- "bad"
  state$linking$phase_a$set_status <- status_tbl
  expect_error(.adaptive_phase_a_gate_or_abort(state), "cannot start until valid Phase A artifacts")
})

test_that("phase_specific judge mode respects Phase A to Phase B boundary gating", {
  state <- make_phase_a_ready_state()
  judge <- make_deterministic_judge("i_wins")

  art1 <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  art2 <- .adaptive_phase_a_build_artifact(state, set_id = 2L)
  art1$quality_gate_accepted <- TRUE
  art2$quality_gate_accepted <- TRUE

  out <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 1L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      judge_param_mode = "phase_specific",
      phase_a_mode = "import",
      phase_a_artifacts = list(`1` = art1, `2` = art2),
      phase_a_compatible_config_hashes = c(art1$fit_config_hash, art2$fit_config_hash)
    ),
    progress = "none"
  )

  expect_identical(out$linking$phase_a$phase, "phase_b")
  expect_true(isTRUE(out$linking$phase_a$ready_for_phase_b))
  expect_equal(nrow(out$step_log), 1L)
})

test_that("phase_specific Phase B startup falls back deterministically without link judge estimates", {
  items <- tibble::tibble(
    item_id = c(paste0("h", seq_len(10L)), paste0("s2", seq_len(6L))),
    text = c(paste0("h", seq_len(10L)), paste0("s2", seq_len(6L))),
    set_id = c(rep(1L, 10L), rep(2L, 6L)),
    global_item_id = c(paste0("gh", seq_len(10L)), paste0("gs2", seq_len(6L)))
  )
  state <- adaptive_rank_start(items, seed = 1L)
  draws <- matrix(
    rep(seq_along(state$item_ids), each = 4L),
    nrow = 4L,
    byrow = TRUE
  )
  colnames(draws) <- as.character(state$item_ids)
  state$btl_fit <- make_test_btl_fit(state$item_ids, draws = draws, model_variant = "btl_e_b")
  judge <- make_deterministic_judge("i_wins")

  art1 <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  art2 <- .adaptive_phase_a_build_artifact(state, set_id = 2L)
  art1$quality_gate_accepted <- TRUE
  art2$quality_gate_accepted <- TRUE

  state$btl_fit$beta_link_mean <- NULL
  state$btl_fit$epsilon_link_mean <- NULL
  state$btl_fit$beta_within_mean <- 0.05
  state$btl_fit$epsilon_within_mean <- 0.02

  out <- expect_no_error(adaptive_rank_run_live(
    state,
    judge,
    n_steps = 1L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      judge_param_mode = "phase_specific",
      phase_a_mode = "import",
      phase_a_artifacts = list(`1` = art1, `2` = art2),
      phase_a_compatible_config_hashes = c(art1$fit_config_hash, art2$fit_config_hash)
    ),
    progress = "none"
  ))

  row <- out$step_log[1L, , drop = FALSE]
  expect_true(isTRUE(row$is_cross_set[[1L]]))
  expect_true(is.finite(row$posterior_win_prob_pre[[1L]]))
  expect_true(is.finite(row$cross_set_utility_pre[[1L]]))
  expect_true(is.character(row$link_stage))
  expect_identical(as.character(row$link_stage[[1L]]), as.character(row$round_stage[[1L]]))
  expect_true(is.integer(out$step_log$link_spoke_id))
})

test_that("phase A helper branch guards and edge paths are exercised", {
  state <- make_phase_a_ready_state()

  no_set <- .adaptive_phase_a_extract_set_draws(state, set_id = 99L)
  expect_null(no_set)

  bad_dim <- state
  bad_dim$btl_fit$btl_posterior_draws <- matrix(1, nrow = 2, ncol = 3)
  expect_null(.adaptive_phase_a_extract_set_draws(bad_dim, set_id = 1L))

  bad_names <- state
  draws <- state$btl_fit$btl_posterior_draws
  colnames(draws) <- c("x1", "x2", "x3", "x4")
  bad_names$btl_fit$btl_posterior_draws <- draws
  expect_null(.adaptive_phase_a_extract_set_draws(bad_names, set_id = 1L))

  expect_error(.adaptive_phase_a_build_artifact(state, set_id = 999L), "No items found for set_id")

  expect_error(.adaptive_phase_a_read_import_artifact("missing-path.rds"), "does not exist")
  expect_error(.adaptive_phase_a_read_import_artifact(1L), "must be a list or .rds path")

  artifact <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  expect_error(.adaptive_phase_a_validate_imported_artifact(1L, state, 1L, .adaptive_controller_resolve(state)))

  bad_set <- artifact
  bad_set$set_id <- 2L
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(bad_set, state, 1L, .adaptive_controller_resolve(state)),
    "set_id mismatch"
  )

  bad_missing_hash <- artifact
  bad_missing_hash$fit_config_hash <- ""
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(
      bad_missing_hash,
      state,
      1L,
      .adaptive_controller_resolve(state)
    ),
    "missing fit_config_hash"
  )

  bad_dup_global <- artifact
  bad_dup_global$items$global_item_id[[2L]] <- bad_dup_global$items$global_item_id[[1L]]
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(
      bad_dup_global,
      state,
      1L,
      .adaptive_controller_resolve(state)
    ),
    "identity duplicates"
  )

  bad_item_map <- artifact
  bad_item_map$items$item_id[[1L]] <- "wrong_item"
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(
      bad_item_map,
      state,
      1L,
      .adaptive_controller_resolve(state)
    ),
    "item_id mapping mismatch"
  )

  bad_reliability <- artifact
  bad_reliability$diagnostics$reliability_EAP_within <- 0
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(
      bad_reliability,
      state,
      1L,
      .adaptive_controller_resolve(state)
    ),
    "reliability gate failed"
  )

  expect_error(.adaptive_phase_a_collect_import_map(list(phase_a_artifacts = 1L)), "named list")
  expect_error(
    .adaptive_phase_a_collect_import_map(list(phase_a_artifacts = list(list(a = 1L)))),
    "Unable to resolve set_id"
  )

  expect_error(
    .adaptive_phase_a_resolve_set_sources(
      list(phase_a_mode = "mixed", phase_a_set_source = c("run")),
      set_ids = c(1L, 2L),
      import_map = list()
    ),
    "named character vector"
  )
  expect_error(
    .adaptive_phase_a_resolve_set_sources(
      list(phase_a_mode = "mixed", phase_a_set_source = c(`1` = "bad")),
      set_ids = c(1L, 2L),
      import_map = list()
    ),
    "values must be `run` or `import`"
  )

  state$linking$phase_a <- list(
    set_status = tibble::tibble(),
    artifacts = list(),
    ready_for_phase_b = FALSE,
    phase = "phase_a"
  )
  state <- .adaptive_apply_controller_config(state, adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L))
  expect_error(.adaptive_phase_a_gate_or_abort(state), "cannot start until valid Phase A artifacts")

  empty_dir <- withr::local_tempdir()
  empty_child <- file.path(empty_dir, "child")
  dir.create(empty_child, recursive = TRUE, showWarnings = FALSE)
  expect_equal(.adaptive_read_phase_a_artifacts(empty_child), list())

  artifact_dir <- withr::local_tempdir()
  .adaptive_write_phase_a_artifacts(list(`x` = list(set_id = NA_integer_)), artifact_dir)
  expect_equal(length(list.files(artifact_dir, full.names = TRUE)), 0L)
})

test_that("resume preserves persisted phase A artifacts for linking gate", {
  state <- make_phase_a_ready_state_with_evidence()

  art1 <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  art2 <- .adaptive_phase_a_build_artifact(state, set_id = 2L)
  art1$quality_gate_accepted <- TRUE
  art2$quality_gate_accepted <- TRUE

  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("import", "import"),
      status = c("ready", "ready"),
      validation_message = c("built_in_run", "built_in_run"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(`1` = art1, `2` = art2),
    ready_for_phase_b = TRUE,
    phase = "phase_b"
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir = session_dir, overwrite = TRUE)
  restored <- load_adaptive_session(session_dir)
  restored$btl_fit <- NULL

  restored <- .adaptive_apply_controller_config(
    restored,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      phase_a_required_reliability_min = 0,
      phase_a_artifacts = list()
    )
  )
  expect_no_error(
    restored <- .adaptive_phase_a_prepare(restored)
  )
  expect_no_error(.adaptive_phase_a_gate_or_abort(restored))
  status <- tibble::as_tibble(restored$linking$phase_a$set_status)
  expect_true(all(status$status == "ready"))
})

test_that("phase A prepare memo is runtime-only across save and load", {
  state <- make_phase_a_ready_state_with_evidence()
  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "run",
      phase_a_required_reliability_min = 0
    )
  )
  state <- .adaptive_phase_a_prepare(state)
  expect_true(length(state$linking$phase_a$prepare_context_by_set) > 0L)
  state$linking$phase_a$within_set_evidence_by_set <- list(
    `1` = tibble::tibble(
      pair_id = 1L,
      step_id = 1L,
      A_item = "a1",
      B_item = "a2",
      y_A = 1L
    )
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir = session_dir, overwrite = TRUE)
  raw_state <- readRDS(file.path(session_dir, "state.rds"))
  expect_null(raw_state$linking$phase_a$prepare_context_by_set)
  expect_null(raw_state$linking$phase_a$within_set_evidence_by_set)
  restored <- load_adaptive_session(session_dir)

  restored <- .adaptive_phase_a_prepare(restored)
  expect_true(length(restored$linking$phase_a$prepare_context_by_set) > 0L)
})

test_that("phase A within-set evidence resolver reuses runtime cache when counts match", {
  state <- make_phase_a_ready_state_with_evidence()
  cached_evidence <- list(
    `1` = tibble::tibble(
      pair_id = 1L,
      step_id = 1L,
      A_item = "a1",
      B_item = "a2",
      y_A = 1L
    )
  )
  state$linking$phase_a$within_set_evidence_by_set <- cached_evidence
  state$refit_meta$phase_a_committed_pairs_by_set <- c(`1` = 1L, `2` = 1L)
  state$refit_meta$phase_a_committed_pairs_history_n <- 2L

  resolved <- .adaptive_phase_a_within_set_evidence_resolve(state, set_id = 1L)

  expect_equal(resolved, cached_evidence[["1"]])
  expect_identical(as.integer(nrow(resolved)), 1L)
})

test_that("resume preserves Phase A pending/ready semantics and warm-start state", {
  state <- make_phase_a_ready_state_with_evidence()
  art1 <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  art2 <- .adaptive_phase_a_build_artifact(state, set_id = 2L)
  art1$quality_gate_accepted <- TRUE
  art2$quality_gate_accepted <- TRUE

  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "run",
      phase_a_required_reliability_min = 0
    )
  )
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "import"),
      status = c("pending_finalization", "ready"),
      validation_message = c(
        "pending_finalization: within-set stop criteria not yet met",
        "imported"
      ),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(`1` = art1, `2` = art2),
    ready_for_phase_b = FALSE,
    phase = "phase_a",
    ready_spokes = integer(),
    active_phase_a_set = 1L
  )
  state$warm_start_done <- FALSE
  state$warm_start_pairs <- tibble::tibble(i_id = c("a1"), j_id = c("a2"))
  state$warm_start_idx <- 1L

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir = session_dir, overwrite = TRUE)
  restored <- load_adaptive_session(session_dir)

  status <- tibble::as_tibble(restored$linking$phase_a$set_status)
  expect_equal(status$status[match(1L, status$set_id)], "pending_finalization")
  expect_equal(status$status[match(2L, status$set_id)], "ready")
  expect_false(isTRUE(restored$linking$phase_a$ready_for_phase_b))
  expect_false(isTRUE(restored$warm_start_done))
  expect_equal(nrow(restored$warm_start_pairs), 1L)
})

test_that("phase A helper utilities cover fallback and phase-context branches", {
  state <- adaptive_rank_start(make_multiset_items(), seed = 9L)
  controller <- .adaptive_controller_resolve(state)

  # ready set fallback uses artifact names when set_status has no ready rows.
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("import", "import"),
      status = c("pending", "pending"),
      validation_message = c("x", "y"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(`2` = list(set_id = 2L))
  )
  expect_identical(.adaptive_phase_a_ready_sets(state), integer())

  # Non-link mode has no pending run sets.
  expect_identical(
    .adaptive_phase_a_pending_run_sets(
      state,
      controller = utils::modifyList(controller, list(run_mode = "within_set"))
    ),
    integer()
  )

  # Unresolved (all NA status/source) remains in phase_a until artifacts are prepared.
  state$linking$phase_a$set_status <- .adaptive_phase_a_empty_state(c(1L, 2L))
  ctx <- .adaptive_link_phase_context(
    state,
    controller = utils::modifyList(controller, list(run_mode = "link_one_spoke", hub_id = 1L))
  )
  expect_identical(ctx$phase, "phase_a")
  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  expect_error(.adaptive_phase_a_gate_or_abort(state), "cannot start until valid Phase A artifacts")

  # collect_import_map handles NULL and read_import_artifact reads existing .rds files.
  expect_equal(.adaptive_phase_a_collect_import_map(list(phase_a_artifacts = NULL)), list())
  path <- file.path(withr::local_tempdir(), "art.rds")
  write_log(list(set_id = 1L), path)
  expect_identical(.adaptive_phase_a_read_import_artifact(path)$set_id, 1L)

  # Additional validation failures.
  valid <- .adaptive_phase_a_build_artifact(make_phase_a_ready_state(), set_id = 1L)
  bad_identity <- valid
  bad_identity$items$global_item_id[[1L]] <- NA_character_
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(
      bad_identity,
      make_phase_a_ready_state(),
      1L,
      .adaptive_controller_resolve(make_phase_a_ready_state())
    ),
    "item identity failure"
  )
  bad_missing <- valid
  bad_missing$items$theta_raw_mean[[1L]] <- NA_real_
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(
      bad_missing,
      make_phase_a_ready_state(),
      1L,
      .adaptive_controller_resolve(make_phase_a_ready_state())
    ),
    "completeness failure"
  )

  # Import mode with missing artifacts marks failed rows.
  state2 <- make_phase_a_ready_state()
  state2 <- .adaptive_apply_controller_config(
    state2,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import"
    )
  )
  prepared <- .adaptive_phase_a_prepare(state2)
  status <- tibble::as_tibble(prepared$linking$phase_a$set_status)
  expect_true(all(status$status == "failed"))

  # Gate emits missing-artifact message when not ready and no pending run sets.
  state3 <- state
  state3 <- .adaptive_apply_controller_config(
    state3,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state3$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("import", "import"),
      status = c("pending", "pending"),
      validation_message = c("missing", "missing"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(),
    ready_for_phase_b = FALSE,
    phase = "phase_a"
  )
  expect_error(.adaptive_phase_a_gate_or_abort(state3), "cannot start until valid Phase A artifacts")
})

test_that("phase_a_mode=run does not mark set ready before within-set finalization", {
  state <- make_phase_a_ready_state()
  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, phase_a_mode = "run")
  )

  prepared <- .adaptive_phase_a_prepare(state)
  status <- tibble::as_tibble(prepared$linking$phase_a$set_status)
  expect_true(all(status$source == "run"))
  expect_true(all(status$status == "pending_finalization"))
  expect_true(all(grepl("pending_finalization", status$validation_message)))
  expect_false(isTRUE(prepared$linking$phase_a$ready_for_phase_b))
})

test_that("run set transition to ready overwrites stale pending_finalization message", {
  state <- make_phase_a_ready_state()
  state$round_log <- tibble::tibble(
    diagnostics_pass = TRUE,
    ts_btl_rank_spearman = 0.95
  )
  state$history_pairs <- tibble::tibble(
    A_id = c("a1", "b1"),
    B_id = c("a2", "b2")
  )
  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, phase_a_mode = "run")
  )

  art1 <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  art1$n_pairs_committed <- 0L
  art2 <- .adaptive_phase_a_build_artifact(state, set_id = 2L)
  art2$n_pairs_committed <- 0L
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("pending_finalization", "pending_finalization"),
      validation_message = c(
        "pending_finalization: within-set stop criteria not yet met",
        "pending_finalization: within-set stop criteria not yet met"
      ),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(`1` = art1, `2` = art2),
    ready_for_phase_b = FALSE,
    phase = "phase_a",
    ready_spokes = integer(),
    active_phase_a_set = 1L
  )

  prepared <- .adaptive_phase_a_prepare(state)
  status <- tibble::as_tibble(prepared$linking$phase_a$set_status)
  expect_true(all(status$validation_message[status$status == "ready"] == "built_in_run"))
})

test_that("phase B gate aborts when hub/spoke artifacts are missing", {
  state <- make_phase_a_ready_state()
  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("import", "import"),
      status = c("ready", "ready"),
      validation_message = c("imported", "imported"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(`1` = list(set_id = 1L)),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L),
    set_stop_pass_by_set = c(`1` = TRUE, `2` = TRUE),
    phase = "phase_b",
    ready_spokes = 2L,
    active_phase_a_set = NA_integer_
  )
  expect_error(
    .adaptive_phase_a_gate_or_abort(state),
    "missing artifacts for set_id"
  )
})

test_that("linking warm-start sync helper covers phase and scope transitions", {
  state <- adaptive_rank_start(make_multiset_items(), seed = 5L)

  # Non-link mode is a no-op.
  no_link <- pairwiseLLM:::.adaptive_link_sync_warm_start(state)
  expect_identical(no_link$warm_start_done, state$warm_start_done)

  # Build valid import artifacts for phase_b branch checks.
  ready <- make_phase_a_ready_state()
  art1 <- .adaptive_phase_a_build_artifact(ready, set_id = 1L)
  art2 <- .adaptive_phase_a_build_artifact(ready, set_id = 2L)
  art1$quality_gate_accepted <- TRUE
  art2$quality_gate_accepted <- TRUE
  ready <- .adaptive_apply_controller_config(
    ready,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, phase_a_mode = "import")
  )
  ready$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("import", "import"),
      status = c("ready", "ready"),
      validation_message = c("imported", "imported"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(`1` = art1, `2` = art2),
    ready_for_phase_b = TRUE,
    phase = "phase_b",
    ready_spokes = 2L,
    active_phase_a_set = NA_integer_
  )
  ready$warm_start_done <- FALSE
  ready$warm_start_pairs <- tibble::tibble(i_id = "a1", j_id = "a2")
  out_b <- pairwiseLLM:::.adaptive_link_sync_warm_start(ready)
  expect_true(isTRUE(out_b$warm_start_done))
  expect_equal(nrow(out_b$warm_start_pairs), 0L)
  expect_true(is.na(out_b$linking$phase_a$warm_start_scope_set))

  # Phase_a branch should scope warm-start to active set.
  phase_a <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, phase_a_mode = "run")
  )
  phase_a$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("pending_finalization", "pending_finalization"),
      validation_message = c("x", "y"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(),
    ready_for_phase_b = FALSE,
    phase = "phase_a",
    ready_spokes = integer(),
    active_phase_a_set = 1L
  )
  phase_a$warm_start_done <- FALSE
  phase_a$warm_start_pairs <- tibble::tibble(i_id = c("a1", "b1"), j_id = c("a2", "b2"))
  phase_a$linking$phase_a$warm_start_scope_set <- 1L
  out_a <- pairwiseLLM:::.adaptive_link_sync_warm_start(phase_a)
  expect_true(all(out_a$warm_start_pairs$i_id %in% c("a1", "a2")))
  expect_true(all(out_a$warm_start_pairs$j_id %in% c("a1", "a2")))

  # Switching active set reseeds and resets warm-start queue to that set.
  phase_a$linking$phase_a$warm_start_scope_set <- 2L
  switched <- pairwiseLLM:::.adaptive_link_sync_warm_start(phase_a)
  expect_identical(switched$linking$phase_a$warm_start_scope_set, 1L)
  expect_equal(switched$warm_start_idx, 1L)
})

test_that("phase A prepare preserves warm-start scope metadata", {
  items <- tibble::tibble(
    item_id = c("a1", "a2", "a3", "b1", "b2"),
    text = c("a1", "a2", "a3", "b1", "b2"),
    set_id = c(1L, 1L, 1L, 2L, 2L),
    global_item_id = c("g_a1", "g_a2", "g_a3", "g_b1", "g_b2")
  )
  state <- adaptive_rank_start(items, seed = 5L)
  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, phase_a_mode = "run")
  )
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("pending_finalization", "pending_finalization"),
      validation_message = c("x", "y"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(),
    ready_for_phase_b = FALSE,
    phase = "phase_a",
    ready_spokes = integer(),
    active_phase_a_set = 1L,
    warm_start_scope_set = 1L
  )
  state$warm_start_done <- FALSE
  state$warm_start_pairs <- tibble::tibble(i_id = c("a1", "a2"), j_id = c("a2", "a3"))
  state$warm_start_idx <- 2L

  prepared <- pairwiseLLM:::.adaptive_phase_a_prepare(state)
  expect_identical(prepared$linking$phase_a$warm_start_scope_set, 1L)

  synced <- pairwiseLLM:::.adaptive_link_sync_warm_start(prepared)
  expect_identical(synced$linking$phase_a$warm_start_scope_set, 1L)
  expect_identical(synced$warm_start_idx, 2L)
})

test_that("phase A finalize reuses prepared stop-pass state when complete", {
  state <- make_phase_a_ready_state_with_evidence()
  state <- .adaptive_apply_controller_config(
    state,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "run",
      phase_a_required_reliability_min = 0
    )
  )
  prepared <- .adaptive_phase_a_prepare(state)
  ns <- asNamespace("pairwiseLLM")
  orig_stop_pass <- get(".adaptive_phase_a_set_stop_passed", envir = ns, inherits = FALSE)
  stop_pass_calls <- 0L

  testthat::with_mocked_bindings(
    .adaptive_phase_a_set_stop_passed = function(...) {
      stop_pass_calls <<- stop_pass_calls + 1L
      orig_stop_pass(...)
    },
    .env = ns,
    {
      expect_no_error(.adaptive_phase_a_finalize_if_ready(prepared))
    }
  )
  expect_identical(stop_pass_calls, 0L)
})

test_that("phase A helpers cover non-link mode and stale-summary fallback behavior", {
  state <- make_phase_a_ready_state()
  expect_identical(pairwiseLLM:::.adaptive_phase_a_pending_run_sets(state), integer())

  stale <- tibble::tibble(
    item_id = as.character(state$item_ids),
    theta_raw_eap = c(NA_real_, 0.2, -0.1, 0.1),
    theta_raw_sd = c(0.1, -0.2, 0.3, 0.4),
    rank_raw = c(1L, 2L, 3L, 4L)
  )
  state$item_log <- list(stale)
  art <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, set_id = 1L)
  expect_true(all(is.finite(art$items$theta_raw_mean)))
  expect_true(all(art$items$theta_raw_sd >= 0))
  expect_true("fit_config_surface" %in% names(art))
})

test_that("phase A validation and gate exercise failure branches for edge completeness", {
  state <- make_phase_a_ready_state()
  controller <- pairwiseLLM:::.adaptive_controller_resolve(state)
  valid <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, set_id = 1L)
  valid$n_pairs_committed <- -1L
  expect_error(
    pairwiseLLM:::.adaptive_phase_a_validate_imported_artifact(valid, state, set_id = 1L, controller = controller),
    "`n_pairs_committed` must be >= 0"
  )

  ctl_empty <- pairwiseLLM:::.adaptive_controller_defaults(length(state$item_ids))
  ctl_empty$phase_a_artifacts <- NULL
  expect_identical(pairwiseLLM:::.adaptive_phase_a_collect_import_map(ctl_empty), list())

  state_link <- pairwiseLLM:::.adaptive_apply_controller_config(
    state,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state_link$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("import", "import"),
      status = c("ready", "ready"),
      validation_message = c("imported", "imported"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(`1` = pairwiseLLM:::.adaptive_phase_a_build_artifact(state_link, set_id = 1L), `2` = NULL),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L),
    set_stop_pass_by_set = c(`1` = TRUE, `2` = TRUE),
    phase = "phase_b"
  )
  state_link$linking$phase_a$artifacts[["1"]]$quality_gate_accepted <- TRUE
  expect_error(
    pairwiseLLM:::.adaptive_phase_a_gate_or_abort(state_link),
    "missing artifact for set_id: 2"
  )

  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_link_phase_context = function(state, controller = NULL) {
        list(
          phase = "phase_b",
          pending_run_sets = integer(),
          ready_spokes = integer(),
          active_spokes = integer(),
          stopped_spokes = integer(),
          active_phase_a_set = NA_integer_
        )
      },
      .package = "pairwiseLLM",
      {
        pairwiseLLM:::.adaptive_phase_a_gate_or_abort(state_link)
      }
    ),
    "phase marked phase_b but no ready spokes are available"
  )
})
