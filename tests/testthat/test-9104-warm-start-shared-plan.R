test_that("a shared plan survives independent fits, feature alignment and warm-prior deployment", {
  skip_if_not_installed("glmnet")
  f <- warm_phase2_fixture()
  first <- warm_phase2_fit(f)
  other <- f
  other$x$token_length_mean <- other$x$token_length_mean * 2 + 3
  other$x <- other$x[rev(seq_len(nrow(other$x))), ]
  second <- fit_warm_start_model(f$x$item_id, f$theta, "phase2", features = other$x,
    alpha_grid = c(0, 1), cv_plan = f$plan)
  expect_identical(first$cv_plan, second$cv_plan)
  expect_identical(first$cv_identity, second$cv_identity)
  expect_identical(first$validation$predictions$fold, second$validation$predictions$fold)
  root <- withr::local_tempdir()
  path <- file.path(root, "model.rds")
  save_warm_start_model(prepare_warm_start_model(first, omit_audit = TRUE), path)
  predictions <- predict(load_warm_start_model(path), f$x)
  prior <- make_warm_start_prior(predictions, prior_sd = 0.7)
  expect_equal(prior$prior_mean, predictions$calibrated_prediction - mean(predictions$calibrated_prediction))
  expect_identical(prior$prior_sd, rep(0.7, nrow(f$x)))
  session <- file.path(root, "session")
  state <- adaptive_rank_start(f$x$item_id, warm_start_model = path,
    warm_start_features = f$x, session_dir = session)
  unlink(path)
  local_mocked_bindings(load_warm_start_model = function(...) stop("must resume stored prior"),
    extract_warm_start_features = function(...) stop("must not extract"), .package = "pairwiseLLM")
  resumed <- adaptive_rank_resume(session)
  expect_identical(resumed$predictive_prior, state$predictive_prior)
  expect_identical(resumed$warm_start_pairs, state$warm_start_pairs)
  results <- build_btl_results_data(data.frame(ID1 = "1", ID2 = "2", better_id = "1"))
  data <- .btl_mcmc_prepare_bt_data(results, rev(f$x$item_id), prior)
  expect_equal(data$prior_mean, rev(prior$prior_mean))
  expect_identical(data$prior_sd, rep(0.7, nrow(f$x)))
})

test_that("PLS and glmnet reuse exact evaluation partitions and PLS initializes adaptive priors", {
  skip_if_not_installed("pls")
  skip_if_not_installed("glmnet")
  f <- warm_pls_fixture()
  pls <- warm_pls_fit(f, engine_control = list(ncomp = 1:2))
  en <- fit_warm_start_model(f$x$item_id, f$theta, "phase4", features = f$x,
    alpha_grid = c(0, 1), cv_plan = f$plan)
  expect_identical(pls$cv_plan, en$cv_plan)
  expect_identical(pls$cv_identity, en$cv_identity)
  expect_identical(pls$tuning$foldid, en$tuning$foldid)
  expect_identical(pls$validation$predictions$fold, en$validation$predictions$fold)
  for (fold in 1:5) {
    expect_identical(pls$validation$folds[[fold]]$tuning$foldid, en$validation$folds[[fold]]$tuning$foldid)
    expect_identical(pls$validation$folds[[fold]]$outcome, en$validation$folds[[fold]]$outcome)
  }
  root <- withr::local_tempdir()
  path <- file.path(root, "pls.rds")
  save_warm_start_model(prepare_warm_start_model(pls, omit_audit = TRUE), path)
  local_mocked_bindings(.warm_start_require_pls = function() stop("must not fit"), .package = "pairwiseLLM")
  for (mode in c("btl_only", "trueskill_only", "both")) {
    session <- file.path(root, mode)
    state <- adaptive_rank_start(f$x$item_id, warm_start_mode = mode,
      warm_start_model = path, warm_start_features = f$x, session_dir = session)
    resumed <- adaptive_rank_resume(session)
    expect_identical(resumed$predictive_prior, state$predictive_prior)
    expect_identical(resumed$warm_start_pairs, state$warm_start_pairs)
    expect_identical(resumed$trueskill_state, state$trueskill_state)
  }
})
