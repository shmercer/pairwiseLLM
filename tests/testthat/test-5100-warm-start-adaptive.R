test_that("adaptive initialization preserves pairing queues and scopes saved priors", {
  ids <- c("a", "b", "c")
  cold <- adaptive_rank_start(ids, seed = 12)
  prior <- make_warm_start_prior(c(a = 2, b = 4, c = 9))
  state <- adaptive_rank_start(ids, seed = 12, warm_start_prior = prior)
  expect_identical(state$warm_start_pairs, cold$warm_start_pairs)
  expect_identical(state$warm_start_idx, cold$warm_start_idx)
  expect_identical(state$trueskill_state, cold$trueskill_state)
  expect_identical(state$predictive_prior, prior)
  testthat::local_mocked_bindings(
    .adaptive_stop_metric_scope = function(...) list(scope_ids = c("b", "a")),
    .adaptive_results_from_step_log = function(...) {
      build_btl_results_data(data.frame(ID1 = "a", ID2 = "b", better_id = "a"))
    },
    fit_bayes_btl_mcmc = function(results, ids, model_variant, cmdstan, warm_start_prior) {
      expect_identical(ids, c("b", "a"))
      expect_identical(warm_start_prior$prior_mean, c(1, -1))
      list(fit = list(theta_draws = matrix(0, 2, 2), predictive_prior = warm_start_prior))
    }, .package = "pairwiseLLM")
  fit <- pairwiseLLM:::default_btl_fit_fn(state, list())
  expect_identical(fit$predictive_prior$prior_sd, c(0.5, 0.5))
})

test_that("model input resolves once with features or one extraction", {
  features <- warm_core_features()
  items <- data.frame(item_id = features$item_id, text = "Synthetic text")
  model <- warm_bundle_model()
  extracted <- 0L
  testthat::local_mocked_bindings(extract_warm_start_features = function(texts, ids, schema, python) {
    extracted <<- extracted + 1L
    expect_identical(ids, features$item_id)
    expect_identical(python, "explicit-python")
    features
  }, .package = "pairwiseLLM")
  state <- adaptive_rank_start(items, warm_start_model = model, warm_start_python = "explicit-python")
  expect_identical(extracted, 1L)
  state2 <- adaptive_rank_start(items, warm_start_model = model, warm_start_features = features,
    warm_start_prior_sd = 0.8)
  expect_identical(extracted, 1L)
  expect_equal(state$predictive_prior$scores, state2$predictive_prior$scores)
  expect_identical(state2$predictive_prior$prior_sd, rep(0.8, nrow(items)))
  expect_identical(state$predictive_prior$provenance$artifact$source, "object")
  expect_error(adaptive_rank_start(items, warm_start_model = model, warm_start_prior = state$predictive_prior),
    "only one")
  expect_error(adaptive_rank_start(items, warm_start_features = features), "require")
  expect_error(adaptive_rank_start(items, warm_start_model = model, warm_start_features = features,
    warm_start_python = "python"), "only to text")
  expect_error(adaptive_rank_start(items$item_id, warm_start_model = model), "texts or features")
  expect_error(adaptive_rank_start(items, warm_start_model = list(bogus = 1)), "reference")
  expect_error(adaptive_rank_start(items, warm_start_model = warm_core_model(), warm_start_features = features),
    "calibration")
  bad <- features
  attr(bad, "warm_start_schema") <- "wrong"
  expect_error(adaptive_rank_start(items, warm_start_model = model, warm_start_features = bad), "schema")
  bad <- features[, -2]
  attr(bad, "warm_start_schema") <- attr(features, "warm_start_schema")
  expect_error(adaptive_rank_start(items, warm_start_model = model, warm_start_features = bad), "Missing")
})

test_that("saved priors resume without lookup, extraction or fitting and reject changes", {
  features <- warm_core_features()
  items <- data.frame(item_id = features$item_id, text = "Synthetic text")
  model <- warm_bundle_model()
  root <- withr::local_tempdir()
  path <- file.path(root, "model.rds")
  save_warm_start_model(model, path)
  session <- file.path(root, "session")
  state <- adaptive_rank_start(items, warm_start_model = path, warm_start_features = features,
    session_dir = session)
  unlink(path)
  testthat::local_mocked_bindings(load_warm_start_model = function(...) stop("No lookup"),
    extract_warm_start_features = function(...) stop("No Python"),
    fit_warm_start_model = function(...) stop("No glmnet"), .package = "pairwiseLLM")
  restored <- adaptive_rank_resume(session)
  expect_identical(restored$predictive_prior, state$predictive_prior)
  expect_identical(restored$warm_start_pairs, state$warm_start_pairs)
  expect_null(restored$predictive_prior$provenance$artifact$reference$model)
  testthat::local_mocked_bindings(adaptive_rank_run_live = function(state, ...) state, .package = "pairwiseLLM")
  out <- adaptive_rank(items, session_dir = session, judge = function(...) NULL, progress = "none")
  expect_identical(out$state$predictive_prior %||% out$predictive_prior, state$predictive_prior)
  for (arg in c("warm_start_model", "warm_start_prior", "warm_start_features", "warm_start_python",
                "warm_start_prior_sd")) {
    args <- list(data = items, session_dir = session, judge = function(...) NULL, progress = "none")
    args[[arg]] <- "replacement"
    expect_error(do.call(adaptive_rank, args), "Omit all warm-start")
  }
  expect_error(adaptive_rank_resume(session, warm_start_model = "replacement"), "must be empty")
  corrupt <- state
  corrupt$predictive_prior$scores[1] <- 999
  expect_error(save_adaptive_session(corrupt, session, overwrite = TRUE), "centering")
  corrupt <- state
  corrupt$predictive_prior <- NULL
  expect_error(save_adaptive_session(corrupt, session, overwrite = TRUE), "integrity")
  corrupt <- state
  corrupt$predictive_prior <- make_warm_start_prior(stats::setNames(seq_len(nrow(items)), items$item_id))
  expect_error(save_adaptive_session(corrupt, session, overwrite = TRUE), "integrity")
})

test_that("legacy sessions backfill predictive log fields and retain cold-start state", {
  root <- withr::local_tempdir()
  state <- adaptive_rank_start(c("a", "b", "c"), seed = 22)
  save_adaptive_session(state, root, overwrite = TRUE)
  log <- readRDS(file.path(root, "round_log.rds"))
  log$predictive_prior_digest <- NULL
  saveRDS(log, file.path(root, "round_log.rds"))
  restored <- adaptive_rank_resume(root)
  expect_null(restored$predictive_prior)
  expect_identical(restored$warm_start_pairs, state$warm_start_pairs)
  expect_identical(restored$warm_start_idx, state$warm_start_idx)
  expect_identical(restored$config$btl_config, state$config$btl_config)
  expect_true("predictive_prior_digest" %in% names(restored$round_log))
})

test_that("Phase A contracts distinguish predictive configurations without changing cold hashes", {
  ids <- c("a", "b", "c")
  cold <- adaptive_rank_start(ids)
  warm <- adaptive_rank_start(ids, warm_start_prior = make_warm_start_prior(c(a = 1, b = 3, c = 2)))
  surface <- pairwiseLLM:::.adaptive_phase_a_required_config_surface(cold, 1L)
  expect_identical(names(surface), c("judge_param_mode", "model_variant"))
  warm_surface <- pairwiseLLM:::.adaptive_phase_a_required_config_surface(warm, 1L)
  expect_identical(warm_surface$predictive_prior_digest, warm$predictive_prior$digest)
  artifact <- list(set_id = 1L, fit_config_surface = surface)
  expect_error(pairwiseLLM:::.adaptive_phase_a_validate_imported_artifact(artifact, warm, 1L, warm$controller),
    "predictive prior configuration")
  artifact$fit_config_surface <- warm_surface
  expect_error(pairwiseLLM:::.adaptive_phase_a_validate_imported_artifact(artifact, cold, 1L, cold$controller),
    "predictive prior configuration")
  expect_identical(pairwiseLLM:::.adaptive_phase_a_artifact_fit_contract_surface(artifact), warm_surface)
})

test_that("registry and bundle references persist their verified identity independently", {
  root <- withr::local_tempdir()
  user <- file.path(root, "user")
  bundle <- file.path(root, "bundle")
  dir.create(user)
  dir.create(bundle)
  testthat::local_mocked_bindings(.warm_start_registry_root = function(source) {
    if (source == "bundled") bundle else user
  }, .package = "pairwiseLLM")
  model <- warm_bundle_model()
  features <- warm_core_features()
  register_warm_start_model(model, name = "synthetic-test")
  manifest <- warm_bundle_write(bundle, model)
  expect_error(adaptive_rank_start(features$item_id,
    warm_start_model = list(name = "synthetic-test"), warm_start_features = features), "Ambiguous")
  for (source in c("user", "bundled")) {
    session <- file.path(root, paste0("session-", source))
    state <- adaptive_rank_start(features$item_id,
      warm_start_model = list(name = "synthetic-test", source = source),
      warm_start_features = features, session_dir = session)
    identity <- state$predictive_prior$provenance$artifact
    expect_identical(identity$source, source)
    expect_identical(identity$name, "synthetic-test")
    expect_identical(identity$version, "test-1")
    expect_identical(identity$format_version, 2L)
    if (source == "bundled") expect_identical(identity$manifest$checksum, manifest$artifacts[[1]]$checksum)
    replacement <- model
    replacement$intercept <- replacement$intercept + 5
    if (source == "user") {
      register_warm_start_model(replacement, name = "synthetic-test", overwrite = TRUE)
    } else {
      warm_bundle_write(bundle, replacement)
    }
    expect_identical(adaptive_rank_resume(session)$predictive_prior, state$predictive_prior)
    if (source == "user") {
      remove_warm_start_model("synthetic-test")
    } else {
      unlink(file.path(bundle, "synthetic-test.rds"))
      unlink(file.path(bundle, "manifest.json"))
    }
    expect_identical(adaptive_rank_resume(session)$predictive_prior, state$predictive_prior)
  }
  warm_bundle_write(bundle, model)
  saveRDS(list(corrupt = TRUE), file.path(bundle, "synthetic-test.rds"))
  expect_error(adaptive_rank_start(features$item_id,
    warm_start_model = list(name = "synthetic-test", source = "bundled"),
    warm_start_features = features), "checksum")
  model$metadata <- NULL
  state <- adaptive_rank_start(features$item_id, warm_start_model = model, warm_start_features = features)
  expect_null(state$predictive_prior$provenance$artifact$name)
  expect_null(state$predictive_prior$provenance$artifact$version)
  expect_error(pairwiseLLM:::.warm_start_prior_resolve_model(list()), "reference")
})

test_that("fresh adaptive_rank forwards model settings and persistence detects metadata corruption", {
  features <- warm_core_features()
  items <- data.frame(item_id = features$item_id, text = "Synthetic text")
  session <- file.path(withr::local_tempdir(), "session")
  testthat::local_mocked_bindings(adaptive_rank_run_live = function(state, ...) state, .package = "pairwiseLLM")
  out <- adaptive_rank(items, judge = function(...) NULL, warm_start_model = warm_bundle_model(),
    warm_start_features = features, session_dir = session, warm_start_prior_sd = 0.7, progress = "none")
  state <- out$state %||% out
  expect_identical(state$predictive_prior$prior_sd, rep(0.7, nrow(items)))
  metadata <- readRDS(file.path(session, "metadata.rds"))
  metadata$predictive_prior_digest <- "wrong"
  saveRDS(metadata, file.path(session, "metadata.rds"))
  expect_error(adaptive_rank_resume(session), "metadata predictive prior")
})

test_that("real calibrated full and reduced artifacts share prior values", {
  skip_if_not_installed("glmnet")
  features <- warm_core_features(15)
  model <- fit_warm_start_model(features = features, ids = features$item_id, theta = warm_core_theta(features),
    task_id = "synthetic-prior-test", alpha_grid = c(0, 1), outer_folds = 3, inner_folds = 3, seed = 88)
  reduced <- prepare_warm_start_model(model, omit_audit = TRUE)
  full_state <- adaptive_rank_start(features$item_id, warm_start_model = model, warm_start_features = features)
  reduced_state <- adaptive_rank_start(features$item_id, warm_start_model = reduced, warm_start_features = features)
  expect_identical(full_state$predictive_prior$scores, reduced_state$predictive_prior$scores)
  expect_identical(full_state$predictive_prior$prior_mean, reduced_state$predictive_prior$prior_mean)
  ensemble <- ensemble_warm_start_models(full = model, reduced = reduced)
  state <- adaptive_rank_start(features$item_id, warm_start_model = ensemble, warm_start_features = features)
  expect_equal(state$predictive_prior$scores, full_state$predictive_prior$scores)
  expect_identical(state$predictive_prior$diagnostics$ensemble_sd, rep(0, 15))
})

test_that("real optional Python text initialization predicts once and persists numeric priors", {
  python <- Sys.getenv("PAIRWISELLM_TEST_PYTHON")
  skip_if(!nzchar(python), "Audited Python environment not explicitly selected")
  items <- data.frame(item_id = c("a", "b"), text = c(
    "The writer describes a small garden. Flowers grow beside the house.",
    "An argument should present clear evidence and explain why the evidence matters."))
  state <- withCallingHandlers(
    adaptive_rank_start(items, warm_start_model = warm_bundle_model(), warm_start_python = python),
    warning = function(w) {
      if (startsWith(conditionMessage(w), "Importing 'parser.split_arg_string' is deprecated")) {
        invokeRestart("muffleWarning")
      }
    })
  expect_length(state$predictive_prior$scores, 2)
  expect_true(all(is.finite(state$predictive_prior$scores)))
  expect_equal(sum(state$predictive_prior$prior_mean), 0, tolerance = 1e-10)
})
