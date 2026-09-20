test_that("algorithm means initialize BTL and TrueSkill with authoritative prior SD and resume", {
  f <- warm_algorithm_fixture()
  root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = root)
  reduced <- prepare_warm_start_model(f$ensemble, omit_audit = TRUE)
  path <- file.path(root, "algorithm.rds")
  save_warm_start_model(reduced, path)
  register_warm_start_model(reduced, "algorithm")
  p <- predict(f$ensemble, f$x)
  prior <- make_warm_start_prior(p, prior_sd = 0.7)
  expect_identical(prior$scores, p$ensemble_mean)
  expect_equal(prior$prior_mean, p$ensemble_mean - mean(p$ensemble_mean))
  expect_identical(prior$prior_sd, rep(0.7, nrow(f$x)))
  expect_identical(make_warm_start_prior(p)$prior_sd, rep(0.5, nrow(f$x)))
  expect_identical(prior$provenance$model$artifact_type, "algorithm_ensemble")
  expect_identical(prior$diagnostics$ensemble_sd, p$ensemble_sd)
  # Perturb component disagreement while holding each row's mean fixed.
  changed <- p
  changed$component_glmnet <- p$component_glmnet + 10
  changed$component_pls <- p$component_pls - 10
  columns <- unname(attr(p, "component_columns"))
  changed$ensemble_mean <- rowMeans(as.matrix(changed[, columns]))
  changed$ensemble_sd <- apply(as.matrix(changed[, columns]), 1, sd)
  other <- make_warm_start_prior(changed, prior_sd = 0.7)
  expect_equal(other$prior_mean, prior$prior_mean)
  expect_identical(other$prior_sd, prior$prior_sd)
  expect_false(identical(other$diagnostics$ensemble_sd, prior$diagnostics$ensemble_sd))
  inputs <- list(btl_only = f$ensemble, trueskill_only = path,
    both = list(name = "algorithm", source = "user"))
  for (mode in names(inputs)) {
    session <- file.path(root, mode)
    state <- adaptive_rank_start(f$x$item_id, warm_start_model = inputs[[mode]],
      warm_start_features = f$x, warm_start_mode = mode, session_dir = session)
    expect_equal(state$predictive_prior$prior_mean, prior$prior_mean)
    expect_identical(state$predictive_prior$prior_sd, rep(0.5, nrow(f$x)))
    expect_identical(state$meta$warm_start_mode, mode)
    expect_identical(state$meta$trueskill_initialized_from_predictive, mode != "btl_only")
    if (mode != "btl_only") {
      defaults <- .trueskill_defaults()
      expect_equal(state$trueskill_state$items$mu, defaults$mu0 + defaults$sigma0 * prior$prior_mean)
    }
    with_mocked_bindings({
      resumed <- adaptive_rank_resume(session)
      expect_identical(resumed$predictive_prior, state$predictive_prior)
      expect_identical(resumed$trueskill_state, state$trueskill_state)
      expect_identical(resumed$warm_start_pairs, state$warm_start_pairs)
    }, load_warm_start_model = function(...) stop("must use saved prior"),
      extract_warm_start_features = function(...) stop("must not extract"), .package = "pairwiseLLM")
  }
  results <- build_btl_results_data(data.frame(ID1 = "1", ID2 = "2", better_id = "1"))
  data <- .btl_mcmc_prepare_bt_data(results, rev(f$x$item_id), prior)
  expect_equal(data$prior_mean, rev(prior$prior_mean))
  expect_identical(data$prior_sd, rep(0.7, nrow(f$x)))
})

test_that("algorithm prediction metadata rejects incompatible components before prior creation", {
  f <- warm_algorithm_fixture(engines = c("glmnet", "pls"))
  p <- predict(f$ensemble, f$x)
  for (field in c("cv_digest", "task_id", "format_version")) {
    bad <- p
    meta <- attr(bad, "warm_start_model")
    meta$components$pls[[field]] <- "wrong"
    attr(bad, "warm_start_model") <- meta
    expect_error(make_warm_start_prior(bad), "format|identity")
  }
  for (field in names(f$ensemble$cv_identity)) {
    bad <- p
    attr(bad, "warm_start_model")$cv_identity[field] <- list(NULL)
    expect_error(make_warm_start_prior(bad), info = field)
  }
  corrupt <- list(format_version = 2L, n = 2L, seed = -1L, outer_folds = 1L,
    inner_folds = 100L, rng_kind = "unknown", digest = "wrong", outcome_digest = "wrong")
  for (field in names(corrupt)) {
    bad <- p
    attr(bad, "warm_start_model")$cv_identity[[field]] <- corrupt[[field]]
    expect_error(make_warm_start_prior(bad), info = field)
  }
  bad <- p
  attr(bad, "warm_start_model")$cv_identity <- NULL
  expect_error(make_warm_start_prior(bad), "identity")
})
