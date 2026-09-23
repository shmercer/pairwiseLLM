# Explicit opt-in real CmdStan smoke: synthetic comparisons only, no providers.
# Run: PAIRWISELLM_TEST_STANDALONE_STAN=true Rscript scripts/standalone-rubric-reference-stan-smoke.R
if (Sys.getenv("PAIRWISELLM_TEST_STANDALONE_STAN") != "true") {
  stop("Set PAIRWISELLM_TEST_STANDALONE_STAN=true to run the real standalone CmdStan smoke.")
}
pkgload::load_all(quiet = TRUE)
testthat::test_that("real standalone CmdStan draws produce a serializable verified reference", {
  output <- withr::local_tempdir()
  evidence <- build_btl_results_data(data.frame(
    ID1 = rep(c("a", "a", "b"), each = 12L),
    ID2 = rep(c("b", "c", "c"), each = 12L),
    better_id = c(rep(c("a", "b"), 6L), rep(c("a", "c"), 6L), rep(c("b", "c"), 6L))))
  completed <- fit_bayes_btl_mcmc(evidence, c("a", "b", "c"), model_variant = "btl_e_b",
    cmdstan = list(chains = 2L, parallel_chains = 2L, iter_warmup = 500L,
      iter_sampling = 500L, seed = 292L, output_dir = output))
  fit <- completed$fits[[1L]]
  testthat::expect_silent(pairwiseLLM:::validate_btl_fit_contract(fit, c("a", "b", "c")))
  testthat::expect_false(is.object(fit$theta_draws))
  testthat::expect_true(all(is.finite(fit$theta_draws)))
  reference <- prepare_linked_rubric_reference(completed, evidence, "H", trait = "synthetic")
  testthat::expect_true(pairwiseLLM:::.link_data_only(unclass(reference)))
  testthat::expect_identical(reference$fit_evidence, fit$evidence_identity)
  testthat::expect_identical(as.vector(reference$posterior_draws), as.vector(fit$theta_draws))
  path <- file.path(output, "reference.rds")
  saveRDS(reference, path)
  testthat::expect_identical(readRDS(path), reference)

  # Recover the same posterior from CSVs; retain the completed fit's exact
  # evidence/configuration records rather than inferring identity from draws.
  saved <- cmdstanr::as_cmdstan_fit(list.files(output, pattern = "[.]csv$", full.names = TRUE))
  draws <- saved$draws(variables = c("theta", "epsilon", "beta"), format = "matrix")
  theta <- draws[, paste0("theta[", 1:3, "]"), drop = FALSE]
  colnames(theta) <- c("a", "b", "c")
  testthat::expect_true(inherits(theta, "draws_matrix"))
  testthat::local_mocked_bindings(.fit_bayes_btl_mcmc_adaptive = function(...) stop("must not resample"),
    .package = "pairwiseLLM")
  recovered <- pairwiseLLM:::as_btl_fit_contract_from_mcmc(list(
    draws = list(theta = theta, epsilon = draws[, "epsilon"], beta = draws[, "beta"]),
    model_variant = fit$model_variant, diagnostics = fit$diagnostics,
    mcmc_config_used = fit$mcmc_config_used, theta_prior = fit$theta_prior), c("a", "b", "c"))
  testthat::expect_identical(recovered$theta_draws, fit$theta_draws)
  testthat::expect_identical(recovered$epsilon_draws, fit$epsilon_draws)
  testthat::expect_identical(recovered$beta_draws, fit$beta_draws)
  for (field in c("inference_contract", "diagnostics_pass", "evidence_identity", "reference_fit_config")) {
    recovered[[field]] <- fit[[field]]
  }
  completed$fits[[1L]] <- recovered
  testthat::expect_identical(
    prepare_linked_rubric_reference(completed, evidence, "H", trait = "synthetic"), reference)
})
