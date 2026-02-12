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

test_that("phase A artifacts round-trip through persistence", {
  state <- make_phase_a_ready_state()
  art1 <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  art2 <- .adaptive_phase_a_build_artifact(state, set_id = 2L)
  art1$quality_gate_accepted <- TRUE
  art2$quality_gate_accepted <- TRUE

  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
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
  expect_true(all(c("global_item_id", "theta_raw_mean", "theta_raw_sd") %in% names(r1$items)))
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

  bad_quality <- valid
  bad_quality$diagnostics$reliability_EAP_within <- NA_real_
  bad_quality$quality_gate_accepted <- FALSE
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(bad_quality, state, set_id = 1L, controller = controller),
    "missing reliability_EAP_within"
  )

  bad_hash <- valid
  bad_hash$fit_config_hash <- "hash_mismatch"
  expect_error(
    .adaptive_phase_a_validate_imported_artifact(bad_hash, state, set_id = 1L, controller = controller),
    "config hash incompatibility"
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

  expect_true(all(status$status == "ready"))
  expect_equal(status$source[match(1L, status$set_id)], "import")
  expect_equal(status$source[match(2L, status$set_id)], "run")
  expect_true(all(c("1", "2") %in% names(prepared$linking$phase_a$artifacts)))
})

test_that("phase A import fallback policy switches to run mode when configured", {
  state <- make_phase_a_ready_state()
  bad_import <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  bad_import$fit_config_hash <- "bad_hash"
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

  expect_true(all(status$status == "ready"))
  expect_equal(status$source[match(1L, status$set_id)], "run")
  expect_match(status$validation_message[match(1L, status$set_id)], "import_failed_fallback_to_run")
})

test_that("Phase B cannot start without valid Phase A artifacts", {
  state <- adaptive_rank_start(make_multiset_items(), seed = 1L)
  judge <- make_deterministic_judge("i_wins")

  expect_error(
    adaptive_rank_run_live(
      state,
      judge,
      n_steps = 1L,
      adaptive_config = list(
        run_mode = "link_one_spoke",
        hub_id = 1L,
        phase_a_mode = "run"
      ),
      progress = "none"
    ),
    "Phase B linking cannot start"
  )
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
  expect_error(.adaptive_phase_a_gate_or_abort(state), "phase_a_artifacts_missing")

  empty_dir <- withr::local_tempdir()
  empty_child <- file.path(empty_dir, "child")
  dir.create(empty_child, recursive = TRUE, showWarnings = FALSE)
  expect_equal(.adaptive_read_phase_a_artifacts(empty_child), list())

  artifact_dir <- withr::local_tempdir()
  .adaptive_write_phase_a_artifacts(list(`x` = list(set_id = NA_integer_)), artifact_dir)
  expect_equal(length(list.files(artifact_dir, full.names = TRUE)), 0L)
})

test_that("resume preserves persisted phase A artifacts for linking gate", {
  state <- make_phase_a_ready_state()
  judge <- make_deterministic_judge("i_wins")

  art1 <- .adaptive_phase_a_build_artifact(state, set_id = 1L)
  art2 <- .adaptive_phase_a_build_artifact(state, set_id = 2L)
  art1$quality_gate_accepted <- TRUE
  art2$quality_gate_accepted <- TRUE

  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
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

  expect_no_error(
    adaptive_rank_run_live(
      restored,
      judge,
      n_steps = 1L,
      adaptive_config = list(
        run_mode = "link_one_spoke",
        hub_id = 1L,
        phase_a_mode = "import",
        phase_a_artifacts = list()
      ),
      progress = "none"
    )
  )
})
