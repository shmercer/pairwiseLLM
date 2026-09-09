test_that("public training through stored ensemble priors survives artifact removal", {
  skip_if_not_installed("glmnet")
  features <- warm_core_features(15)
  other <- warm_core_features(18)
  other$item_id <- paste0("other-", other$item_id)
  model <- fit_warm_start_model(features$item_id, warm_core_theta(features), "synthetic-a",
    features = features, alpha_grid = c(0, 1))
  second <- fit_warm_start_model(other$item_id, warm_core_theta(other), "synthetic-b",
    features = other, alpha_grid = c(0, 1))
  expect_identical(model$validation$predictions$item_id, features$item_id)
  expect_identical(second$validation$predictions$item_id, other$item_id)
  expect_true(all(is.finite(model$validation$predictions$calibrated_prediction)))
  root <- withr::local_tempdir()
  path <- file.path(root, "model.rds")
  save_warm_start_model(model, path)
  restored <- load_warm_start_model(path)
  expect_identical(restored$validation, model$validation)
  ensemble <- ensemble_warm_start_models(first = restored, second = second)
  predictions <- predict(ensemble, features)
  expect_equal(predictions$ensemble_mean,
    (predictions$component_first + predictions$component_second) / 2)
  expect_equal(predictions$ensemble_sd,
    abs(predictions$component_first - predictions$component_second) / sqrt(2))
  reduced <- prepare_warm_start_model(ensemble, omit_audit = TRUE)
  reduced_predictions <- predict(reduced, features)
  expect_identical(reduced_predictions$ensemble_mean, predictions$ensemble_mean)
  artifact <- file.path(root, "ensemble.rds")
  save_warm_start_model(reduced, artifact)
  prior <- make_warm_start_prior(predict(load_warm_start_model(artifact), features))
  results <- build_btl_results_data(data.frame(ID1 = "1", ID2 = "2", better_id = "1"))
  ids <- rev(features$item_id)
  data <- pairwiseLLM:::.btl_mcmc_prepare_bt_data(results, ids, prior)
  expect_identical(data$item_id, ids)
  expect_equal(data$prior_mean, rev(predictions$ensemble_mean - mean(predictions$ensemble_mean)))
  expect_identical(data$prior_sd, rep(0.5, length(ids)))
  session <- file.path(root, "session")
  state <- adaptive_rank_start(features$item_id, warm_start_model = artifact,
    warm_start_features = features, session_dir = session)
  unlink(c(path, artifact))
  testthat::local_mocked_bindings(
    load_warm_start_model = function(...) rlang::abort("Artifact lookup attempted"),
    extract_warm_start_features = function(...) rlang::abort("Extraction attempted"),
    fit_warm_start_model = function(...) rlang::abort("Training attempted"),
    .package = "pairwiseLLM"
  )
  resumed <- adaptive_rank_resume(session)
  expect_identical(resumed$predictive_prior, state$predictive_prior)
  expect_identical(resumed$warm_start_pairs, state$warm_start_pairs)
})
