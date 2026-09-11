test_that("session metadata identifies every mode and strategy without duplicating predictions", {
  withr::local_seed(104)
  rng <- .Random.seed
  root <- withr::local_tempdir()
  ids <- letters[1:4]
  prior <- make_warm_start_prior(stats::setNames(c(-1, -0.2, 0.2, 1), ids))
  for (mode in c("cold", "btl_only", "trueskill_only", "both")) {
    for (strategy in c("hybrid", "random", "trueskill_p50", "trueskill_pollitt")) {
      state <- adaptive_rank_start(ids, seed = 51L, warm_start_mode = mode,
        warm_start_prior = if (mode == "cold") NULL else prior,
        adaptive_config = list(pairing_strategy = strategy))
      state <- adaptive_rank_run_live(state, function(...) list(is_valid = TRUE, Y = 1L),
        n_steps = 1L, progress = "none")
      session <- file.path(root, paste(mode, strategy, sep = "-"))
      save_adaptive_session(state, session)
      metadata <- validate_session_dir(session)
      expect_identical(metadata, readRDS(file.path(session, "metadata.rds")))
      expect_identical(metadata$warm_start_mode, mode)
      expect_identical(metadata$pairing_strategy, strategy)
      expect_false(any(c("prior_mean", "prior_sd", "predictive_prior") %in% names(metadata)))
      loaded <- load_adaptive_session(session)
      expect_identical(loaded$meta$warm_start_mode, mode)
      expect_identical(loaded$meta$trueskill_initialized_from_predictive,
        mode %in% c("trueskill_only", "both"))
      for (field in c("predictive_prior", "trueskill_state", "warm_start_pairs",
        "warm_start_idx", "warm_start_done", "round", "controller", "step_log", "round_log")) {
        expect_identical(loaded[[field]], state[[field]], info = paste(mode, strategy, field))
      }
      expect_identical(loaded$step_log$pairing_strategy, strategy)
    }
  }
  expect_identical(.Random.seed, rng)
})

test_that("legacy migration preserves historical TrueSkill and the partially consumed queue", {
  root <- withr::local_tempdir()
  ids <- letters[1:5]
  prior <- make_warm_start_prior(stats::setNames(seq(-1, 1, length.out = 5), ids))
  local_mocked_bindings(
    load_warm_start_model = function(...) stop("must not load a model"),
    extract_warm_start_features = function(...) stop("must not extract features"),
    .warm_start_prior_resolve_model = function(...) stop("must not resolve a model"),
    .package = "pairwiseLLM")
  for (has_prior in c(FALSE, TRUE)) {
    state <- adaptive_rank_start(ids, seed = 70L,
      warm_start_prior = if (has_prior) prior else NULL)
    state <- adaptive_rank_run_live(state, function(...) list(is_valid = TRUE, Y = 0L),
      n_steps = 2L, progress = "none")
    session <- file.path(root, as.character(has_prior))
    save_adaptive_session(state, session)
    legacy <- readRDS(file.path(session, "state.rds"))
    legacy$meta$warm_start_mode <- NULL
    legacy$meta$trueskill_initialized_from_predictive <- NULL
    legacy$controller$pairing_strategy <- NULL
    legacy$step_log$pairing_strategy <- legacy$step_log$target_distance <- NULL
    saveRDS(legacy, file.path(session, "state.rds"))
    saveRDS(legacy$step_log, file.path(session, "step_log.rds"))
    metadata <- readRDS(file.path(session, "metadata.rds"))
    metadata$warm_start_mode <- metadata$pairing_strategy <- NULL
    saveRDS(metadata, file.path(session, "metadata.rds"))

    expected_mode <- if (has_prior) "btl_only" else "cold"
    audit <- validate_session_dir(session)
    expect_identical(audit$warm_start_mode, expected_mode)
    expect_identical(audit$pairing_strategy, "hybrid")
    loaded <- load_adaptive_session(session)
    expect_identical(loaded$meta$warm_start_mode, expected_mode)
    expect_false(loaded$meta$trueskill_initialized_from_predictive)
    expect_null(loaded$meta$trueskill_warm_scale)
    expect_identical(loaded$controller$pairing_strategy, "hybrid")
    for (field in c("trueskill_state", "predictive_prior", "warm_start_pairs",
      "warm_start_idx", "warm_start_done", "round")) {
      expect_identical(loaded[[field]], legacy[[field]], info = field)
    }
    expect_identical(loaded$meta$predictive_prior_digest, legacy$meta$predictive_prior_digest)
    expect_identical(pairwiseLLM:::.warm_start_btl_prior_for_state(loaded), state$predictive_prior)
    expect_identical(loaded$step_log$pairing_strategy, rep("hybrid", 2L))
    expect_identical(loaded$step_log$target_distance, rep(NA_real_, 2L))
    expect_identical(names(loaded$step_log), names(pairwiseLLM:::schema_step_log))
    # Load/preflight migrate in memory; the historical artifacts are not rewritten.
    expect_null(readRDS(file.path(session, "state.rds"))$meta$warm_start_mode)
    expect_null(readRDS(file.path(session, "metadata.rds"))$warm_start_mode)
    save_adaptive_session(loaded, session, overwrite = TRUE)
    expect_identical(validate_session_dir(session)$warm_start_mode, expected_mode)
    expect_identical(load_adaptive_session(session)$trueskill_state, state$trueskill_state)
  }
})

test_that("intermediate warm sessions retain their explicit mode when disk audit fields are absent", {
  session <- withr::local_tempdir()
  prior <- make_warm_start_prior(c(a = -1, b = 0, c = 1))
  state <- adaptive_rank_start(letters[1:3], warm_start_prior = prior, warm_start_mode = "both",
    adaptive_config = list(pairing_strategy = "trueskill_p50"))
  save_adaptive_session(state, session)
  metadata <- readRDS(file.path(session, "metadata.rds"))
  metadata$warm_start_mode <- metadata$pairing_strategy <- NULL
  saveRDS(metadata, file.path(session, "metadata.rds"))
  # Missing fixed-mapping audit fields can be restored from an explicit saved warm mode.
  stored <- readRDS(file.path(session, "state.rds"))
  fields <- c("trueskill_initialized_from_predictive", "trueskill_warm_scale",
    "trueskill_mu0_used", "trueskill_sigma0_used")
  stored$meta[fields] <- NULL
  saveRDS(stored, file.path(session, "state.rds"))
  loaded <- load_adaptive_session(session)
  expect_identical(loaded$meta$warm_start_mode, "both")
  expect_identical(loaded$controller$pairing_strategy, "trueskill_p50")
  expect_identical(loaded$meta[fields], state$meta[fields])
  expect_identical(loaded$trueskill_state, state$trueskill_state)
  expect_identical(validate_session_dir(session)$warm_start_mode, "both")
})

test_that("session behavior metadata fails on malformed values and disagreement", {
  session <- withr::local_tempdir()
  state <- adaptive_rank_start(letters[1:3], warm_start_mode = "both",
    warm_start_prior = make_warm_start_prior(c(a = -1, b = 0, c = 1)),
    adaptive_config = list(pairing_strategy = "random"))
  save_adaptive_session(state, session)
  metadata_path <- file.path(session, "metadata.rds")
  metadata <- readRDS(metadata_path)
  for (field in c("warm_start_mode", "pairing_strategy")) {
    invalid <- list("", NA_character_, 1L, c("cold", "both"), matrix("both"), "unknown")
    for (value in invalid) {
      bad <- metadata
      bad[[field]] <- value
      saveRDS(bad, metadata_path)
      expect_error(validate_session_dir(session), field)
      expect_error(load_adaptive_session(session), field)
    }
    bad <- metadata
    bad[[field]] <- if (field == "warm_start_mode") "btl_only" else "hybrid"
    saveRDS(bad, metadata_path)
    expect_error(validate_session_dir(session), paste0(field, "` integrity mismatch"))
    expect_error(load_adaptive_session(session), paste0(field, "` integrity mismatch"))
  }
  saveRDS(metadata, metadata_path)
  saved <- readRDS(file.path(session, "state.rds"))
  bad <- saved
  bad$meta$warm_start_mode <- "cold"
  expect_error(save_adaptive_session(bad, session, overwrite = TRUE), "cold.*predictive")
  bad$meta$warm_start_mode <- NULL
  expect_error(save_adaptive_session(bad, session, overwrite = TRUE), "initialization metadata")
  bad$meta$trueskill_initialized_from_predictive <- NULL
  expect_error(save_adaptive_session(bad, session, overwrite = TRUE), "mapping metadata")
  for (mode in list("", NA_character_, "unknown", c("cold", "both"))) {
    bad <- saved
    bad$meta$warm_start_mode <- mode
    saveRDS(bad, file.path(session, "state.rds"))
    expect_error(load_adaptive_session(session), "warm_start_mode")
  }
  saveRDS(saved, file.path(session, "state.rds"))
  for (field in c("trueskill_warm_scale", "trueskill_mu0_used", "trueskill_sigma0_used")) {
    for (value in list(NA_real_, 0, "1", c(1, 2), matrix(1))) {
      bad <- saved
      bad$meta[[field]] <- value
      expect_error(save_adaptive_session(bad, session, overwrite = TRUE), field)
    }
  }
  # All rejected saves leave the valid on-disk session available.
  expect_identical(load_adaptive_session(session)$trueskill_state, saved$trueskill_state)
})

test_that("state validation precedes legacy controller repair and preserves linking restrictions", {
  session <- withr::local_tempdir()
  state <- adaptive_rank_start(letters[1:3])
  save_adaptive_session(state, session)
  path <- file.path(session, "state.rds")
  saved <- readRDS(path)
  for (controller in list("invalid", list(pairing_strategy = ""),
    list(pairing_strategy = NA_character_), list(pairing_strategy = c("hybrid", "random")))) {
    bad <- saved
    bad$controller <- controller
    saveRDS(bad, path)
    expect_error(load_adaptive_session(session), "controller|pairing_strategy")
  }
  bad <- saved
  bad$controller <- list(run_mode = "link_one_spoke", pairing_strategy = "random")
  saveRDS(bad, path)
  expect_error(load_adaptive_session(session), "within_set")
  bad <- saved
  bad$meta <- NULL
  saveRDS(bad, path)
  expect_error(load_adaptive_session(session), "missing required fields: meta")
  expect_error(validate_session_dir(session), "meta.*controller")
  bad <- saved
  bad$controller <- NULL
  saveRDS(bad, path)
  expect_identical(load_adaptive_session(session)$controller$pairing_strategy, "hybrid")
  bad <- saved
  bad$meta$warm_start_mode <- "both"
  saveRDS(bad, path)
  expect_error(load_adaptive_session(session), "requires.*warm_start_model")
  # A raw constructor has no predictive metadata; saving it uses legacy cold defaults.
  raw <- pairwiseLLM:::new_adaptive_state(letters[1:3])
  save_adaptive_session(raw, session, overwrite = TRUE)
  expect_identical(validate_session_dir(session)$warm_start_mode, "cold")
})
