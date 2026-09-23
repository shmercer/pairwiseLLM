test_that("fixed and adaptive completed results share score semantics across all variants", {
  withr::local_seed(200)
  normalize <- pairwiseLLM:::.rubric_normalize_cj
  for (variant in c("btl", "btl_e", "btl_b", "btl_e_b")) {
    fixed <- normalize(rubric_test_fixed(variant), "organization")
    state <- rubric_test_adaptive(variant)
    adaptive <- normalize(list(state = state), "organization")
    expect_equal(fixed$items, adaptive$items[c("item_id", "theta", "theta_sd")])
    expect_equal(fixed$posterior_draws, adaptive$posterior_draws)
    expect_identical(fixed$model_variant, variant)
    expect_identical(adaptive$model_variant, variant)
    expect_identical(fixed$scale_status, "within_set")
    expect_identical(adaptive$estimation_mode, "adaptive")
    expect_identical(adaptive$reliability, 0.95)
    expect_identical(state, rubric_test_adaptive(variant))
  }
})

test_that("fixed refit selection and optional provenance do not change scoring rules", {
  normalize <- pairwiseLLM:::.rubric_normalize_cj
  live <- batch <- rubric_test_fixed()
  live$provenance <- list(collection_mode = "live", backend = "example")
  batch$provenance <- list(collection_mode = "batch", backend = "another")
  a <- normalize(live, "trait")
  b <- normalize(batch, "trait")
  expect_equal(a$items, b$items)
  expect_equal(a$posterior_draws, b$posterior_draws)
  expect_false(identical(a$provenance$collection, b$provenance$collection))
  last <- rubric_test_fixed(shift = 5)
  live$fit <- NULL
  live$fits[[2L]] <- last$fit
  live$item_log_list[[2L]] <- last$item_log_list[[1L]]
  live$item_log_list[[2L]]$refit_id <- 2L
  live$round_log <- dplyr::bind_rows(live$round_log, last$round_log)
  live$round_log$round_id <- 1:2
  expect_equal(normalize(live, "trait")$items$theta, unname(last$fit$theta_mean))
  expect_null(normalize(live, "trait", include_draws = FALSE)$posterior_draws)
  expect_error(normalize(live, "trait", include_draws = NA), "include_draws")
})

test_that("normalization rejects malformed fixed data and inconsistent metadata", {
  normalize <- pairwiseLLM:::.rubric_normalize_cj
  fixed <- rubric_test_fixed()
  expect_error(normalize(fixed), "Supply `trait`")
  expect_error(normalize(list()), "Unsupported")
  expect_error(normalize(data.frame(theta = 1)), "completed CJ")
  bad <- fixed
  bad$fits[[1L]]$model_variant <- "elo"
  expect_error(normalize(bad, "trait"), "model_variant")
  bad <- fixed
  bad$round_log$model_variant <- "btl"
  expect_error(normalize(bad, "trait"), "model_variant")
  bad <- fixed
  bad$fits[[1L]]$theta_draws[1L, 1L] <- NA_real_
  expect_error(normalize(bad, "trait"), "finite numeric matrix")
  bad <- fixed
  bad$fits[[1L]]$theta_mean[[1L]] <- Inf
  expect_error(normalize(bad, "trait"), "finite")
  bad <- fixed
  bad$item_log_list[[1L]]$ID[[2L]] <- "a"
  expect_error(normalize(bad, "trait"), "unique")
  bad$item_log_list[[1L]]$ID[[2L]] <- NA_character_
  expect_error(normalize(bad, "trait"), "nonmissing")
  bad <- fixed
  bad$item_log_list[[1L]]$theta_mean[[1L]] <- 8
  expect_error(normalize(bad, "trait"), "accepted fit")
  bad <- fixed
  bad$item_log_list[[1L]]$refit_id <- NULL
  expect_error(normalize(bad, "trait"), "refit IDs")
  bad <- fixed
  bad$item_log_list[[1L]]$refit_id[[1L]] <- NA_integer_
  expect_error(normalize(bad, "trait"), "refit IDs")
  bad <- fixed
  bad$trait <- "organization"
  expect_error(normalize(bad, "mechanics"), "Trait mismatch")
  expect_identical(normalize(bad)$trait, "organization")
  bad$orientation <- "lower_is_better"
  expect_error(normalize(bad), "orientation")
  expect_error(normalize(fixed, "trait", scale_status = "phase_b_linked"), "inappropriate")
  bad <- fixed
  bad$fits[[1L]]$diagnostics_pass <- FALSE
  expect_warning(out <- normalize(bad, "trait"), class = "pairwiseLLM_rubric_cj_diagnostics")
  expect_false(out$diagnostics$diagnostics_pass)
})

test_that("adaptive acceptance requires terminal evidence but preserves a completed post-stop fit", {
  withr::local_seed(201)
  normalize <- pairwiseLLM:::.rubric_normalize_cj
  state <- rubric_test_adaptive()
  bad <- state
  bad$meta$stop_decision <- FALSE
  expect_error(normalize(bad, "trait"), "incomplete")
  bad <- state
  bad$meta$stop_reason <- "candidate_starvation"
  expect_error(normalize(bad, "trait"), "stop_reason")
  bad <- state
  bad$refit_meta$last_refit_step <- 2L
  expect_error(normalize(bad, "trait"), "inconsistent")
  bad <- state
  bad$config$btl_config$model_variant <- "btl"
  expect_error(normalize(bad, "trait"), "configured model_variant")
  bad <- state
  bad$item_log[[1L]]$refit_id <- NULL
  expect_error(normalize(bad, "trait"), "accepted refit")
  bad <- state
  bad$items$orientation <- "lower_is_better"
  expect_error(normalize(bad, "trait"), "orientation")
  bad <- state
  bad$items$trait <- c("a", "a", "b", "b")
  expect_error(normalize(bad, "a"), "Trait mismatch")
  bad <- state
  bad$items$set_id <- c(1L, 1L, 2L, 2L)
  expect_error(normalize(bad, "trait"), "Independent Phase A")
  state$meta$stop_reason <- "max_pairs_after_stop_exhausted"
  state$meta$stop_boundary_refit_id <- 1L
  state$step_log$pair_id <- 1:3
  state$step_log <- dplyr::bind_rows(state$step_log, tibble::tibble(pair_id = 4L))
  out <- normalize(state, "trait")
  expect_identical(out$provenance$fitted_comparisons, 3L)
  expect_equal(out$provenance$collected_comparisons, 4)
  state$meta$stop_boundary_refit_id <- 2L
  expect_error(normalize(state, "trait"), "stop boundary")
})

test_that("draws and canonical identities align by name without imputing uncertainty", {
  items <- pairwiseLLM:::.rubric_items(c("a", "b"), c(1, 2), global_ids = c("ga", "gb"))
  expect_true(all(is.na(items$theta_sd)))
  expect_identical(items$item_id, c("ga", "gb"))
  draws <- matrix(1:4, 2, dimnames = list(NULL, c("b", "a")))
  aligned <- pairwiseLLM:::.rubric_draws(draws, c("a", "b"), c("ga", "gb"))
  expect_equal(unname(aligned), unname(draws[, 2:1]))
  expect_identical(colnames(aligned), c("ga", "gb"))
  draws[1L, 1L] <- Inf
  expect_error(pairwiseLLM:::.rubric_draws(draws, c("a", "b"), c("ga", "gb")), "finite")
  expect_error(pairwiseLLM:::.rubric_items("a", 1, -1), "nonnegative")
  expect_error(pairwiseLLM:::.rubric_items("a", 1, NaN), "nonnegative")
  expect_error(pairwiseLLM:::.rubric_ids(c("a", " ")), "nonmissing")
})

test_that("Phase A import readiness retains reference metadata and rejects unready artifacts", {
  withr::local_seed(202)
  normalize <- pairwiseLLM:::.rubric_normalize_cj
  state <- rubric_test_link_state(2L)
  artifact <- state$linking$phase_a$artifacts[["1"]]
  out <- normalize(artifact, "trait")
  expect_identical(out$scale_status, "phase_a_reference")
  expect_identical(out$provenance$finalization, "validated_import")
  expect_identical(out$items$item_id, artifact$items$global_item_id)
  expect_equal(out$items$theta, artifact$items$theta_raw_mean)
  expect_identical(out$reference$fit_contract_hash, artifact$fit_config_hash)
  expect_identical(colnames(out$posterior_draws), out$items$item_id)
  bad <- artifact
  bad$diagnostics$reliability_EAP_within <- 0.2
  expect_error(normalize(bad, "trait"), "reliability gate")
  bad$quality_gate_accepted <- TRUE
  expect_true(normalize(bad, "trait")$provenance$quality_gate_accepted)
  bad <- artifact
  bad$diagnostics$diagnostics_pass <- FALSE
  expect_error(normalize(bad, "trait"), "not import-ready")
  bad <- artifact
  bad$fit_config_surface$model_variant <- NULL
  expect_error(normalize(bad, "trait"), "model_variant")
  bad <- artifact
  bad$phase_a_within_set_evidence <- NULL
  expect_error(normalize(bad, "trait"), "evidence")
  bad <- artifact
  bad$phase_a_within_set_evidence_hash <- "wrong"
  expect_error(normalize(bad, "trait"), "evidence hash")
  expect_error(normalize(artifact, "trait", scale_status = "phase_b_linked"), "inappropriate")
})

test_that("old Phase B posterior state is rejected rather than reinterpreted for rubric scoring", {
  withr::local_seed(203)
  testthat::local_mocked_bindings(
    .adaptive_linking_refit_update_state = function(...) stop("Rejection fixtures must not refit"),
    .package = "pairwiseLLM"
  )
  for (n_sets in 2:3) {
    state <- rubric_test_legacy_linked(n_sets)
    expect_error(pairwiseLLM:::.rubric_normalize_cj(state, "trait"),
      "Restart linking from compatible Phase A", class = "pairwiseLLM_unsupported_legacy_link_state")
  }
})

test_that("current fixed and adaptive entry points produce accepted rubric inputs offline", {
  testthat::local_mocked_bindings(
    .btl_mcmc_available_cores = function() 2L, .package = "pairwiseLLM"
  )
  withr::local_seed(206)
  fit <- rubric_test_fit()
  testthat::local_mocked_bindings(.fit_bayes_btl_mcmc_adaptive = function(bt_data, config, seed = NULL) {
    list(draws = list(theta = fit$theta_draws, epsilon = fit$epsilon_draws, beta = fit$beta_draws),
      model_variant = fit$model_variant, diagnostics = fit$diagnostics, mcmc_config_used = fit$mcmc_config_used)
  }, .package = "pairwiseLLM")
  results <- tibble::tibble(pair_uid = c("a:b#1", "b:c#1", "c:d#1"),
    unordered_key = c("a:b", "b:c", "c:d"), ordered_key = c("a:b", "b:c", "c:d"),
    A_id = letters[1:3], B_id = letters[2:4], better_id = letters[2:4], winner_pos = 2L,
    phase = "phase2", iter = 1L, received_at = as.POSIXct("2026-09-12", tz = "UTC"),
    backend = "offline", model = "fixture")
  fixed <- pairwiseLLM::fit_bayes_btl_mcmc(results, ids = letters[1:4])
  expect_equal(pairwiseLLM:::.rubric_normalize_cj(fixed, "trait")$items$theta, unname(fit$theta_mean))
  for (extra in c(0L, 2L)) {
    state <- pairwiseLLM::adaptive_rank_start(tibble::tibble(item_id = letters[1:4]), seed = 206L,
      now_fn = function() as.POSIXct("2026-09-12", tz = "UTC"))
    state <- pairwiseLLM::adaptive_rank_run_live(state, make_deterministic_judge("i_wins"),
      n_steps = 30L, fit_fn = function(state, config) pairwiseLLM:::.adaptive_btl_adapt_fit(fit),
      adaptive_config = list(max_pairs_after_stop = extra),
      btl_config = list(refit_pairs_target = 1L, ess_bulk_min = 100, ess_bulk_min_near_stop = 100,
        max_rhat = 1.01, divergences_max = 0L, eap_reliability_min = 0.90, stability_lag = 1L,
        theta_corr_min = 0.90, theta_sd_rel_change_max = 0.20, rank_spearman_min = 0.90), progress = "none")
    expect_true(state$meta$stop_decision)
    out <- pairwiseLLM:::.rubric_normalize_cj(state, "trait")
    expect_equal(out$items$theta, unname(fit$theta_mean))
    expect_identical(out$provenance$finalization,
      if (extra == 0L) "btl_converged" else "max_pairs_after_stop_exhausted")
  }
})

test_that("Phase A rubric imports verify stored evidence before table normalization", {
  state <- rubric_test_link_state(2L)
  original <- state$linking$phase_a$artifacts[["1"]]
  normalize <- pairwiseLLM:::.rubric_normalize_cj
  hash <- pairwiseLLM:::.adaptive_phase_a_hash_object
  expected <- normalize(original, "trait")
  for (table_class in c("tibble", "data.frame")) {
    for (field in c("phase_a_within_set_evidence", "within_set_evidence")) {
      artifact <- original
      rows <- artifact$phase_a_within_set_evidence
      if (table_class == "data.frame") rows <- as.data.frame(rows)
      # Equal observations can have different serialized table attributes.
      attributes(rows) <- rev(attributes(rows))
      artifact$phase_a_within_set_evidence <- NULL
      artifact[[field]] <- rows
      artifact$phase_a_within_set_evidence_hash <- hash(rows)
      canonical <- pairwiseLLM:::.adaptive_phase_a_artifact_resolve_within_set_evidence(
        artifact, state, 1L, state$controller)
      expect_false(identical(hash(rows), hash(canonical)))
      before <- serialize(artifact, NULL)
      out <- normalize(artifact, "trait")
      expect_identical(out$items, expected$items)
      expect_identical(out$reference$evidence, canonical)
      expect_identical(out$reference$evidence_hash, hash(canonical))
      expect_no_error(pairwiseLLM:::.rubric_reference_identity(out$reference))
      expect_identical(serialize(artifact, NULL), before)
      artifact[[field]]$y_A <- 1L - artifact[[field]]$y_A
      expect_error(normalize(artifact, "trait"), "evidence hash does not match")
    }
  }
})
