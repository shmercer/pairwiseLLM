test_that("optional dependencies fail explicitly before I/O or model development", {
  original <- base::requireNamespace
  unavailable <- "cmdstanr"
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) {
    if (package %in% unavailable) FALSE else original(package, ...)
  }, .package = "base")
  expect_error(.btl_mcmc_require_cmdstanr(), "CmdStanR is required")
  unavailable <- "readr"
  expect_error(.llm_multi_require_readr(), "readr.*required")
  expect_error(llm_submit_pairs_multi_batch(write_registry = TRUE), "readr.*required")
  expect_error(llm_resume_multi_batches(write_results_csv = TRUE), "readr.*required")
  pairs <- tibble::tibble(ID1 = "a", ID2 = "b", text1 = "one", text2 = "two")
  for (provider in c("openai", "anthropic", "gemini", "together", "ollama")) {
    submit <- get(paste0("submit_", provider, "_pairs_live"), asNamespace("pairwiseLLM"))
    unavailable <- "readr"
    expect_error(submit(pairs, "fixture", "quality", "quality", save_path = "unused.csv"),
      "readr.*required")
    unavailable <- "future"
    expect_error(submit(pairs, "fixture", "quality", "quality", parallel = TRUE, workers = 2L),
      "future.*required")
  }
  unavailable <- "withr"
  testthat::local_mocked_bindings(.warm_start_require_glmnet = function() NULL, .package = "pairwiseLLM")
  features <- warm_core_features(12)
  expect_error(fit_warm_start_model(features$item_id, warm_core_theta(features), "fixture",
    features = features, outer_folds = 2L, inner_folds = 2L), "optional package 'withr'")
})

test_that("a missing CmdStan installation is reported without invoking an installer", {
  skip_if_not_installed("cmdstanr")
  testthat::local_mocked_bindings(cmdstan_path = function(...) stop("not installed"),
    .package = "cmdstanr")
  expect_error(.btl_mcmc_require_cmdstanr(), "CmdStan is not available")
})
