phase_identity_items <- function() {
  tibble::tibble(item_id = letters[1:6], set_id = rep(1:2, each = 3L),
    global_item_id = paste0("global_", letters[1:6]))
}

phase_identity_state <- function(mode = "cold", strategy = "hybrid", scores = 1:6) {
  prior <- if (mode == "cold") NULL else make_warm_start_prior(stats::setNames(scores, letters[1:6]))
  adaptive_rank_start(phase_identity_items(), seed = 42L, warm_start_mode = mode,
    warm_start_prior = prior, adaptive_config = list(pairing_strategy = strategy))
}

phase_identity_artifacts <- function(state) {
  state$btl_fit <- make_test_btl_fit(state$item_ids)
  artifacts <- lapply(1:2, function(set_id) {
    artifact <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, set_id)
    artifact <- add_test_phase_a_evidence(artifact, state, set_id)
    artifact$phase_a_within_set_evidence <- pairwiseLLM:::.adaptive_phase_a_artifact_resolve_within_set_evidence(
      artifact, state, set_id, state$controller)
    artifact$phase_a_within_set_evidence_hash <- pairwiseLLM:::.adaptive_phase_a_hash_object(
      artifact$phase_a_within_set_evidence)
    artifact$quality_gate_accepted <- TRUE
    artifact
  })
  stats::setNames(artifacts, c("1", "2"))
}

phase_identity_link <- function(state, artifacts = list(), mode = "import") {
  pairwiseLLM:::.adaptive_apply_controller_config(state, list(
    run_mode = "link_one_spoke", phase_a_mode = mode, phase_a_artifacts = artifacts,
    phase_a_required_reliability_min = 0
  ))
}

test_that("Phase A identity distinguishes the study ceiling and retains explicit imports", {
  ordinary <- phase_identity_state()
  study <- pairwiseLLM:::.adaptive_apply_controller_config(ordinary, list(dup_max_obs_relaxed = 2L))
  surface <- pairwiseLLM:::.adaptive_phase_a_required_config_surface(study, 1L)
  expect_identical(surface$dup_max_obs_relaxed, 2L)
  expect_null(pairwiseLLM:::.adaptive_phase_a_required_config_surface(ordinary, 1L)$dup_max_obs_relaxed)
  ordinary_artifact <- phase_identity_artifacts(ordinary)[[1L]]
  study_artifact <- phase_identity_artifacts(study)[[1L]]
  expect_false(identical(ordinary_artifact$fit_config_hash, study_artifact$fit_config_hash))
  validate <- function(artifact, state, source = "run") {
    pairwiseLLM:::.adaptive_phase_a_validate_imported_artifact(
      artifact, state, 1L, state$controller, source = source)
  }
  expect_identical(validate(study_artifact, study), study_artifact)
  expect_error(validate(ordinary_artifact, study), "relaxed duplicate ceiling")
  expect_error(validate(study_artifact, ordinary), "relaxed duplicate ceiling")
  expect_identical(validate(study_artifact, ordinary, "import"), study_artifact)
  expect_identical(validate(ordinary_artifact, study, "import"), ordinary_artifact)
  for (bad in list(NA, 2.5, "2", c(2, 3))) {
    malformed <- study_artifact
    malformed$fit_config_surface$dup_max_obs_relaxed <- bad
    expect_error(validate(malformed, study), "dup_max_obs_relaxed")
  }
  context <- function(state, source) {
    pairwiseLLM:::.adaptive_phase_a_prepare_context_hash(
      state, 1L, source, state$controller, import_artifact = ordinary_artifact)
  }
  expect_identical(context(ordinary, "import"), context(study, "import"))
  expect_false(identical(context(ordinary, "run"), context(study, "run")))
  linked <- phase_identity_link(study, phase_identity_artifacts(ordinary))
  prepared <- pairwiseLLM:::.adaptive_phase_a_prepare(linked)
  expect_true(all(prepared$linking$phase_a$set_status$status == "ready"))
  expect_identical(prepared$linking$phase_a$phase, "phase_b")
})

test_that("Phase A generation hashes distinguish warm destinations and pairing policies", {
  modes <- c("cold", "btl_only", "trueskill_only", "both")
  strategies <- c("hybrid", "random", "trueskill_p50", "trueskill_pollitt")
  hashes <- character()
  for (mode in modes) {
    for (strategy in strategies) {
      state <- phase_identity_state(mode, strategy)
      surface <- pairwiseLLM:::.adaptive_phase_a_required_config_surface(state, 1L)
      effective_mode <- pairwiseLLM:::.warm_start_mode(surface$warm_start_mode,
        !is.null(surface$predictive_prior_digest))
      expect_identical(effective_mode, mode)
      expect_identical(pairwiseLLM:::.adaptive_pairing_strategy(surface), strategy)
      hashes <- c(hashes, pairwiseLLM:::.adaptive_phase_a_required_config_hash(state, 1L))
    }
  }
  expect_identical(length(unique(hashes)), 16L)
  cold_surface <- pairwiseLLM:::.adaptive_phase_a_required_config_surface(phase_identity_state(), 1L)
  expect_identical(cold_surface, list(judge_param_mode = "global_shared", model_variant = "btl_e_b"))

  warm <- phase_identity_state("both")
  changed_local <- phase_identity_state("both", scores = c(1, 2, 4, 4, 5, 6))
  changed_other <- phase_identity_state("both", scores = c(1, 2, 3, 4, 5, 8))
  key <- function(state) pairwiseLLM:::.adaptive_phase_a_required_config_hash(state, 1L)
  expect_false(identical(key(warm), key(changed_local)))
  expect_identical(key(warm), key(changed_other))
})

test_that("run artifact validation rejects incompatible generation identity", {
  btl <- phase_identity_state("btl_only")
  artifact <- phase_identity_artifacts(btl)[[1L]]
  validate <- function(state, art = artifact) {
    pairwiseLLM:::.adaptive_phase_a_validate_imported_artifact(art, state, 1L, state$controller)
  }
  expect_identical(validate(btl), artifact)
  expect_error(validate(phase_identity_state("both")), "warm-start mode configuration")
  expect_error(validate(phase_identity_state("trueskill_only")), "warm-start mode configuration")
  expect_error(validate(phase_identity_state("btl_only", "random")), "pairing strategy configuration")
  expect_error(validate(phase_identity_state("btl_only", scores = c(1, 2, 4, 4, 5, 6))),
    "predictive prior configuration")
  forged <- artifact
  forged$fit_config_hash <- pairwiseLLM:::.adaptive_phase_a_required_config_hash(
    phase_identity_state("both"), 1L)
  expect_error(validate(phase_identity_state("both"), forged), "warm-start mode configuration")

  legacy <- artifact
  legacy$fit_config_surface$warm_start_mode <- NULL
  legacy$fit_config_surface$pairing_strategy <- NULL
  legacy$fit_config_hash <- pairwiseLLM:::.adaptive_phase_a_hash_object(legacy$fit_config_surface)
  expect_identical(validate(btl, legacy), legacy)
  expect_error(validate(phase_identity_state("both"), legacy), "warm-start mode configuration")
  malformed <- artifact
  malformed$fit_config_surface$warm_start_mode <- "cold"
  expect_error(validate(btl, malformed), "cold.*predictive input")
  malformed <- artifact
  malformed$fit_config_surface$pairing_strategy <- "unknown"
  expect_error(validate(btl, malformed), "pairing_strategy")

  state <- phase_identity_link(phase_identity_state("both"), mode = "run")
  state$linking$phase_a$artifacts <- phase_identity_artifacts(btl)
  state$linking$phase_a$set_status <- tibble::tibble(set_id = 1:2,
    source = "run", status = "ready", validation_message = "persisted")
  prepared <- pairwiseLLM:::.adaptive_phase_a_prepare(state)
  expect_identical(prepared$linking$phase_a$set_status$source, c("run", "run"))
  expect_identical(prepared$linking$phase_a$set_status$status,
    c("pending_finalization", "pending_finalization"))
  expect_length(prepared$linking$phase_a$artifacts, 0L)
})

test_that("explicit imports retain generation identity across session warm modes", {
  artifacts <- phase_identity_artifacts(phase_identity_state())
  for (mode in c("cold", "btl_only", "trueskill_only", "both")) {
    state <- phase_identity_link(phase_identity_state(mode), artifacts)
    original_ts <- state$trueskill_state
    prepared <- pairwiseLLM:::.adaptive_phase_a_prepare(state)
    expect_identical(prepared$linking$phase_a$artifacts, artifacts)
    expect_true(all(prepared$linking$phase_a$set_status$status == "ready"))
    expect_identical(prepared$linking$phase_a$phase, "phase_b")
    expect_identical(prepared$trueskill_state, original_ts)
    expect_identical(nrow(prepared$step_log), 0L)
    expect_no_error(pairwiseLLM:::.adaptive_phase_a_gate_or_abort(prepared))
  }
  # A direct within-set artifact can be explicitly imported into hybrid linking;
  # the receiving controller's policy does not describe how that evidence arose.
  direct <- phase_identity_artifacts(phase_identity_state("both", "trueskill_pollitt"))
  state <- phase_identity_link(phase_identity_state(), direct)
  expect_identical(pairwiseLLM:::.adaptive_phase_a_prepare(state)$linking$phase_a$artifacts, direct)

  incompatible <- artifacts
  incompatible[[1L]]$fit_config_surface$model_variant <- "btl"
  state <- phase_identity_link(phase_identity_state("both"), incompatible)
  rejected <- pairwiseLLM:::.adaptive_phase_a_prepare(state)
  expect_identical(rejected$linking$phase_a$set_status$status[[1L]], "failed")
  expect_error(pairwiseLLM:::.adaptive_phase_a_gate_or_abort(rejected), "likelihood/model incompatibility")
})

test_that("mixed Phase A keeps imports and warms only run-required generation identity", {
  artifacts <- phase_identity_artifacts(phase_identity_state())
  state <- phase_identity_link(phase_identity_state("both"), artifacts["1"], mode = "mixed")
  prepared <- pairwiseLLM:::.adaptive_phase_a_prepare(state)
  status <- prepared$linking$phase_a$set_status
  expect_identical(status$source, c("import", "run"))
  expect_identical(status$status, c("ready", "pending_finalization"))
  expect_identical(prepared$linking$phase_a$artifacts[["1"]], artifacts[["1"]])
  expect_null(prepared$linking$phase_a$artifacts[["2"]])
  expect_identical(prepared$linking$phase_a$active_phase_a_set, 2L)
  out <- adaptive_rank_run_live(prepared, make_deterministic_judge("i_wins"),
    n_steps = 1L, progress = "none")
  expect_identical(out$linking$phase_a$artifacts[["1"]], artifacts[["1"]])
  expect_true(all(out$step_log$set_i == 2L & out$step_log$set_j == 2L))
  expect_identical(pairwiseLLM:::.adaptive_phase_a_required_config_surface(out, 2L)$warm_start_mode, "both")
  expect_identical(out$linking$phase_a$warm_start_scope_set, 2L)
  expect_false(out$warm_start_done)
  session <- withr::local_tempdir()
  save_adaptive_session(out, session)
  restored <- load_adaptive_session(session)
  expect_identical(restored$warm_start_pairs, out$warm_start_pairs)
  expect_identical(restored$warm_start_idx, out$warm_start_idx)
  expect_identical(restored$linking$phase_a$warm_start_scope_set, 2L)
  expect_identical(restored$linking$phase_a$artifacts[["1"]], out$linking$phase_a$artifacts[["1"]])
  expected <- adaptive_rank_run_live(out, make_deterministic_judge("i_wins"),
    n_steps = 1L, progress = "none")
  actual <- adaptive_rank_run_live(restored, make_deterministic_judge("i_wins"),
    n_steps = 1L, progress = "none")
  expect_identical(actual$history_pairs[c("A_id", "B_id")], expected$history_pairs[c("A_id", "B_id")])
  expect_identical(actual$history_pairs$is_probe_step %in% TRUE,
    expected$history_pairs$is_probe_step %in% TRUE)
  expect_identical(actual$trueskill_state, expected$trueskill_state)
  expect_identical(actual$warm_start_pairs, expected$warm_start_pairs)
  expect_identical(actual$warm_start_idx, expected$warm_start_idx)
  expect_identical(actual$step_log[c("A", "B", "Y")], expected$step_log[c("A", "B", "Y")])
})

test_that("Phase A memo identity follows source and installs explicit replacements", {
  artifacts <- phase_identity_artifacts(phase_identity_state())
  btl <- phase_identity_link(phase_identity_state("btl_only"), artifacts)
  both <- phase_identity_link(phase_identity_state("both"), artifacts)
  context <- function(state, source) {
    pairwiseLLM:::.adaptive_phase_a_prepare_context_hash(
      state, 1L, source, state$controller, import_artifact = artifacts[[1L]])
  }
  expect_identical(context(btl, "import"), context(both, "import"))
  expect_false(identical(context(btl, "run"), context(both, "run")))

  prepared <- pairwiseLLM:::.adaptive_phase_a_prepare(both)
  replacement <- artifacts[[1L]]
  replacement$items$theta_raw_mean <- replacement$items$theta_raw_mean + 0.25
  prepared$controller$phase_a_artifacts[["1"]] <- replacement
  replaced <- pairwiseLLM:::.adaptive_phase_a_prepare(prepared)
  expect_identical(replaced$linking$phase_a$artifacts[["1"]], replacement)
  expect_false(identical(replaced$linking$phase_a$prepare_context_by_set[["1"]],
    prepared$linking$phase_a$prepare_context_by_set[["1"]]))
  expect_identical(replaced$linking$phase_a$prepare_context_by_set[["2"]],
    prepared$linking$phase_a$prepare_context_by_set[["2"]])
  replaced$controller$phase_a_artifacts[["1"]]$items$theta_raw_sd <- -1
  rejected <- pairwiseLLM:::.adaptive_phase_a_prepare(replaced)
  expect_identical(rejected$linking$phase_a$set_status$status[[1L]], "failed")
  expect_null(rejected$linking$phase_a$artifacts[["1"]])

  stale <- pairwiseLLM:::.adaptive_phase_a_prepare(both)
  stale$controller$phase_a_artifacts[["1"]]$phase_a_within_set_evidence$y_A[[1L]] <- 2L
  expect_identical(pairwiseLLM:::.adaptive_phase_a_artifact_memo_hash(
    stale$controller$phase_a_artifacts[["1"]]),
    pairwiseLLM:::.adaptive_phase_a_artifact_memo_hash(artifacts[[1L]]))
  rejected <- pairwiseLLM:::.adaptive_phase_a_prepare(stale)
  expect_identical(rejected$linking$phase_a$set_status$status[[1L]], "failed")
  expect_null(rejected$linking$phase_a$artifacts[["1"]])
  expect_error(pairwiseLLM:::.adaptive_phase_a_gate_or_abort(rejected), "y_A")
})

test_that("Phase A memo reuse restores absent stop flags without regenerating imported work", {
  artifacts <- phase_identity_artifacts(phase_identity_state())
  state <- phase_identity_link(phase_identity_state("both"), artifacts)
  prepared <- pairwiseLLM:::.adaptive_phase_a_prepare(state)
  prepared$linking$phase_a$set_stop_pass_by_set <- NULL
  reused <- pairwiseLLM:::.adaptive_phase_a_prepare(prepared)
  expect_identical(reused$linking$phase_a$artifacts, artifacts)
  expect_true(all(unlist(reused$linking$phase_a$set_stop_pass_by_set)))
  expect_identical(reused$trueskill_state, prepared$trueskill_state)
  expect_identical(reused$history_pairs, prepared$history_pairs)
  reused$linking$phase_a$set_stop_pass_by_set <- NULL
  finalized <- pairwiseLLM:::.adaptive_phase_a_finalize_if_ready(reused)
  expect_true(all(unlist(finalized$linking$phase_a$set_stop_pass_by_set)))
  expect_identical(finalized$linking$phase_a$phase, "phase_b")
  expect_identical(finalized$linking$phase_a$artifacts, artifacts)

  artifact <- artifacts[[1L]]
  artifact$phase_a_within_set_evidence_hash <- NULL
  surface <- pairwiseLLM:::.adaptive_phase_a_artifact_memo_surface(artifact)
  expect_identical(surface$phase_a_within_set_evidence_hash,
    pairwiseLLM:::.adaptive_phase_a_hash_object(artifact$phase_a_within_set_evidence))
  changed <- artifact
  changed$phase_a_within_set_evidence$y_A[1L] <- 0L
  expect_false(identical(pairwiseLLM:::.adaptive_phase_a_artifact_memo_hash(changed),
    pairwiseLLM:::.adaptive_phase_a_artifact_memo_hash(artifact)))
  path <- file.path(withr::local_tempdir(), "single-artifact.rds")
  saveRDS(artifact, path)
  expect_identical(pairwiseLLM:::.adaptive_rank_normalize_phase_a_artifacts(path), list(`1` = artifact))
})

test_that("Phase A committed cache rejects malformed counts and generation contracts", {
  validate <- pairwiseLLM:::.adaptive_phase_a_committed_pairs_validate
  expect_error(validate(c(`1` = 0, `2` = 0), 1:2), "integer vector")
  expect_error(validate(c(`2` = 0L, `1` = 0L), 1:2), "set-id names")
  for (value in c(NA_integer_, -1L)) {
    expect_error(validate(c(`1` = value, `2` = 0L), 1:2), "non-missing and non-negative")
  }
  expect_true(validate(c(`1` = 1L, `2` = 2L), 1:2))
  surface <- pairwiseLLM:::.adaptive_phase_a_fit_contract_surface
  expect_error(surface(c("global_shared", "phase_specific"), "btl_e_b"), "judge_param_mode")
  expect_identical(surface("", "btl_e_b")$judge_param_mode, "global_shared")
  expect_error(surface("global_shared", "unknown"), "model_variant")
})

test_that("Phase B pooled evidence and anchored initialization ignore session warm metadata", {
  artifacts <- phase_identity_artifacts(phase_identity_state())
  outputs <- lapply(c("cold", "btl_only", "trueskill_only", "both"), function(mode) {
    state <- phase_identity_link(phase_identity_state(mode), artifacts)
    state <- pairwiseLLM:::.adaptive_phase_a_prepare(state)
    expect_no_error(pairwiseLLM:::.adaptive_phase_a_gate_or_abort(state))
    list(
      pooled = pairwiseLLM:::.adaptive_phase_a_pooled_judge_results(state, artifacts, 1:2, state$controller),
      anchored = pairwiseLLM:::.adaptive_anchored_joint_artifact_copy_init(state, 2L)
    )
  })
  for (out in outputs[-1L]) expect_identical(out, outputs[[1L]])
  for (mode in c("run", "import", "mixed")) {
    for (strategy in c("random", "trueskill_p50", "trueskill_pollitt")) {
      expect_error(adaptive_rank_start(phase_identity_items(), adaptive_config = list(
        run_mode = "link_one_spoke", phase_a_mode = mode, pairing_strategy = strategy)),
        "requires.*within_set")
    }
  }
})
