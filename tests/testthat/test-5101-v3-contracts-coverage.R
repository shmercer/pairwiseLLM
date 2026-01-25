testthat::test_that("reorder_theta_draws validates inputs", {
  ids <- c("a", "b")
  draws <- matrix(1:4, nrow = 2, ncol = 2)

  testthat::expect_error(
    pairwiseLLM:::reorder_theta_draws(1, ids),
    "numeric matrix"
  )
  testthat::expect_error(
    pairwiseLLM:::reorder_theta_draws(draws, character()),
    "non-empty"
  )
  testthat::expect_error(
    pairwiseLLM:::reorder_theta_draws(draws, c("a", "a")),
    "unique"
  )

  colnames(draws) <- c(NA_character_, "b")
  testthat::expect_error(
    pairwiseLLM:::reorder_theta_draws(draws, ids),
    "column names"
  )

  colnames(draws) <- ids
  testthat::expect_error(
    pairwiseLLM:::reorder_theta_draws(draws, c("a", "c")),
    "match"
  )

  colnames(draws) <- rev(ids)
  reordered <- pairwiseLLM:::reorder_theta_draws(draws, ids)
  testthat::expect_identical(colnames(reordered), ids)
})

testthat::test_that("validate_v3_fit_contract catches schema violations", {
  ids <- c("a", "b")
  theta_draws <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2, ncol = 2)
  colnames(theta_draws) <- ids
  base_fit <- make_v3_fit_contract(ids, theta_draws = theta_draws)

  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract("bad", ids),
    "list"
  )
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(base_fit, character()),
    "non-empty"
  )
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(base_fit, c("a", "a")),
    "unique"
  )

  bad_draws <- base_fit
  bad_draws$theta_draws <- NULL
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(bad_draws, ids),
    "theta_draws"
  )

  short_draws <- base_fit
  short_draws$theta_draws <- theta_draws[1, , drop = FALSE]
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(short_draws, ids),
    "at least two draws"
  )

  wrong_cols <- base_fit
  wrong_cols$theta_draws <- matrix(1:6, nrow = 3, ncol = 2)
  colnames(wrong_cols$theta_draws) <- ids
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(wrong_cols, c("a", "b", "c")),
    "one column"
  )

  bad_mean <- base_fit
  bad_mean$theta_mean <- "nope"
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(bad_mean, ids),
    "theta_mean"
  )

  short_mean <- base_fit
  short_mean$theta_mean <- stats::setNames(0.1, "a")
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(short_mean, ids),
    "length"
  )

  unnamed_mean <- base_fit
  names(unnamed_mean$theta_mean) <- NULL
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(unnamed_mean, ids),
    "named"
  )

  wrong_names <- base_fit
  names(wrong_names$theta_mean) <- rev(ids)
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(wrong_names, ids),
    "names"
  )
})

testthat::test_that("build_v3_fit_contract validates column names", {
  theta_draws <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2, ncol = 2)
  testthat::expect_error(
    pairwiseLLM:::build_v3_fit_contract(theta_draws),
    "column names"
  )
})

testthat::test_that("v3 contract vector summary validates draws", {
  testthat::expect_error(
    pairwiseLLM:::.v3_contract_vector_summary("bad", "epsilon_draws", bounds = NULL, probs = c(0.5)),
    "numeric"
  )
  testthat::expect_warning(
    testthat::expect_error(
      pairwiseLLM:::.v3_contract_vector_summary(c(1, NA_real_), "epsilon_draws", bounds = NULL, probs = c(0.5)),
      "at least two"
    ),
    "Non-finite"
  )
})

testthat::test_that("v3 contract diagnostics and mcmc defaults validate inputs", {
  defaults <- pairwiseLLM:::.v3_contract_diagnostics_defaults(NULL)
  testthat::expect_true(all(c("divergences", "max_rhat", "min_ess_bulk") %in% names(defaults)))
  testthat::expect_error(pairwiseLLM:::.v3_contract_diagnostics_defaults("bad"), "diagnostics")

  testthat::expect_true(is.na(pairwiseLLM:::.v3_contract_diagnostics_pass(NULL)))
  testthat::expect_error(pairwiseLLM:::.v3_contract_diagnostics_pass(c(TRUE, FALSE)), "diagnostics_pass")

  testthat::expect_error(pairwiseLLM:::.v3_contract_mcmc_defaults("bad"), "mcmc_config_used")
})

testthat::test_that("validate_v3_fit_contract checks scalars and metadata", {
  ids <- c("a", "b")
  theta_draws <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2, ncol = 2)
  colnames(theta_draws) <- ids
  base_fit <- make_v3_fit_contract(ids, theta_draws = theta_draws)

  bad_mean <- base_fit
  bad_mean$theta_mean[[1L]] <- NA_real_
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(bad_mean, ids),
    "theta_mean"
  )

  bad_sd_type <- base_fit
  bad_sd_type$theta_sd <- "bad"
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(bad_sd_type, ids),
    "theta_sd"
  )

  bad_sd_len <- base_fit
  bad_sd_len$theta_sd <- stats::setNames(0.1, "a")
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(bad_sd_len, ids),
    "length"
  )

  bad_sd_names <- base_fit
  names(bad_sd_names$theta_sd) <- NULL
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(bad_sd_names, ids),
    "named"
  )

  bad_sd_order <- base_fit
  names(bad_sd_order$theta_sd) <- rev(ids)
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(bad_sd_order, ids),
    "names"
  )

  bad_sd_finite <- base_fit
  bad_sd_finite$theta_sd[[1L]] <- NA_real_
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(bad_sd_finite, ids),
    "theta_sd"
  )

  bad_n <- base_fit
  bad_n$n_items <- "bad"
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(bad_n, ids),
    "n_items"
  )

  bad_items <- base_fit
  bad_items$n_items <- base_fit$n_items + 1L
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(bad_items, ids),
    "n_items"
  )

  bad_draws <- base_fit
  bad_draws$n_draws <- base_fit$n_draws + 1L
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(bad_draws, ids),
    "n_draws"
  )

  epsilon_bad_type <- base_fit
  epsilon_bad_type["epsilon_draws"] <- list(NULL)
  epsilon_bad_type$epsilon_mean <- "bad"
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(epsilon_bad_type, ids),
    "epsilon_mean"
  )

  epsilon_bad_finite <- base_fit
  epsilon_bad_finite["epsilon_draws"] <- list(NULL)
  epsilon_bad_finite$epsilon_mean <- Inf
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(epsilon_bad_finite, ids),
    "epsilon_mean"
  )

  epsilon_not_numeric <- base_fit
  epsilon_not_numeric$epsilon_draws <- "bad"
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(epsilon_not_numeric, ids),
    "epsilon_draws"
  )

  epsilon_too_short <- base_fit
  epsilon_too_short$epsilon_draws <- 0.1
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(epsilon_too_short, ids),
    "epsilon_draws"
  )

  epsilon_bounds <- base_fit
  epsilon_bounds$epsilon_draws <- c(0.1, 1.2)
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(epsilon_bounds, ids),
    "within \\[0, 1\\]"
  )

  epsilon_mean_na <- base_fit
  epsilon_mean_na$epsilon_mean <- NA_real_
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(epsilon_mean_na, ids),
    "epsilon_mean"
  )

  b_not_numeric <- base_fit
  b_not_numeric$b_draws <- "bad"
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(b_not_numeric, ids),
    "b_draws"
  )

  b_too_short <- base_fit
  b_too_short$b_draws <- 0.1
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(b_too_short, ids),
    "b_draws"
  )

  bad_diag <- base_fit
  bad_diag$diagnostics <- "bad"
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(bad_diag, ids),
    "diagnostics"
  )

  missing_diag <- base_fit
  missing_diag$diagnostics <- list(divergences = 0L)
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(missing_diag, ids),
    "diagnostics"
  )

  bad_pass <- base_fit
  bad_pass$diagnostics_pass <- "bad"
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(bad_pass, ids),
    "diagnostics_pass"
  )

  bad_variant <- base_fit
  bad_variant$model_variant <- 1
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(bad_variant, ids),
    "model_variant"
  )

  bad_mcmc <- base_fit
  bad_mcmc$mcmc_config_used <- "bad"
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(bad_mcmc, ids),
    "mcmc_config_used"
  )

  missing_mcmc <- base_fit
  missing_mcmc$mcmc_config_used <- list(chains = 2L)
  testthat::expect_error(
    pairwiseLLM:::validate_v3_fit_contract(missing_mcmc, ids),
    "mcmc_config_used"
  )
})
