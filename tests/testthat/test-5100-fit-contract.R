testthat::test_that("as_v3_fit_contract_from_mcmc returns v3 contract shape", {
  ids <- c("id1", "id2", "id3")
  theta_draws <- matrix(as.double(1:12), nrow = 4, ncol = 3)
  colnames(theta_draws) <- c("id2", "id3", "id1")

  mcmc_fit <- list(
    draws = list(
      theta = theta_draws,
      epsilon = c(0.1, 0.2, 0.3, 0.4)
    ),
    theta_summary = tibble::tibble(
      item_id = c("id3", "id1", "id2"),
      theta_mean = c(0.3, 0.1, 0.2)
    ),
    epsilon_summary = tibble::tibble(
      epsilon_mean = 0.2,
      epsilon_p2.5 = 0.12,
      epsilon_p5 = 0.14,
      epsilon_p50 = 0.2,
      epsilon_p95 = 0.28,
      epsilon_p97.5 = 0.3
    ),
    diagnostics = list(max_rhat = 1.01),
    model_variant = "btl_e"
  )

  fit <- pairwiseLLM:::as_v3_fit_contract_from_mcmc(mcmc_fit, ids)

  testthat::expect_true(is.list(fit))
  testthat::expect_true(all(c(
    "theta_draws", "theta_mean", "theta_sd",
    "epsilon_draws", "epsilon_mean", "epsilon_p50",
    "beta_draws", "beta_mean", "beta_p50",
    "diagnostics", "diagnostics_pass",
    "n_items", "n_draws", "model_variant", "mcmc_config_used"
  ) %in% names(fit)))
  testthat::expect_identical(colnames(fit$theta_draws), ids)
  testthat::expect_identical(names(fit$theta_mean), ids)
  testthat::expect_true(is.numeric(fit$epsilon_mean) && length(fit$epsilon_mean) == 1L)
  testthat::expect_equal(fit$n_items, length(ids))
  testthat::expect_equal(fit$n_draws, nrow(fit$theta_draws))
})

testthat::test_that("validate_v3_fit_contract rejects malformed fits", {
  ids <- c("A", "B")
  theta_draws <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  colnames(theta_draws) <- ids
  base_fit <- make_v3_fit_contract(ids, theta_draws = theta_draws)

  missing_cols <- base_fit
  colnames(missing_cols$theta_draws) <- NULL
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(missing_cols, ids),
    "column names"
  )

  wrong_order <- base_fit
  colnames(wrong_order$theta_draws) <- rev(ids)
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(wrong_order, ids),
    "ids"
  )

  unnamed_mean <- base_fit
  names(unnamed_mean$theta_mean) <- NULL
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(unnamed_mean, ids),
    "theta_mean"
  )

  missing_diagnostics <- base_fit
  missing_diagnostics$diagnostics <- NULL
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(missing_diagnostics, ids),
    "diagnostics"
  )
})
