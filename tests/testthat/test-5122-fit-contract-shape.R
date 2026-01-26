testthat::test_that("fit contract respects variant parameter presence", {
  ids <- c("a", "b", "c")
  theta_draws <- matrix(seq_len(6), nrow = 2, ncol = 3)
  colnames(theta_draws) <- ids
  epsilon_draws <- c(0.1, 0.2)
  beta_draws <- c(-0.2, 0.3)

  fit_btl <- pairwiseLLM:::build_v3_fit_contract(
    theta_draws = theta_draws,
    epsilon_draws = NULL,
    beta_draws = NULL,
    model_variant = "btl"
  )
  testthat::expect_null(fit_btl$epsilon_draws)
  testthat::expect_null(fit_btl$beta_draws)
  testthat::expect_true(all(is.na(c(fit_btl$epsilon_mean, fit_btl$epsilon_p50))))
  testthat::expect_true(all(is.na(c(fit_btl$beta_mean, fit_btl$beta_p50))))

  fit_btl_e <- pairwiseLLM:::build_v3_fit_contract(
    theta_draws = theta_draws,
    epsilon_draws = epsilon_draws,
    beta_draws = NULL,
    model_variant = "btl_e"
  )
  testthat::expect_true(is.numeric(fit_btl_e$epsilon_draws))
  testthat::expect_null(fit_btl_e$beta_draws)
  testthat::expect_true(is.finite(fit_btl_e$epsilon_mean))
  testthat::expect_true(is.na(fit_btl_e$beta_mean))

  fit_btl_b <- pairwiseLLM:::build_v3_fit_contract(
    theta_draws = theta_draws,
    epsilon_draws = NULL,
    beta_draws = beta_draws,
    model_variant = "btl_b"
  )
  testthat::expect_null(fit_btl_b$epsilon_draws)
  testthat::expect_true(is.numeric(fit_btl_b$beta_draws))
  testthat::expect_true(is.na(fit_btl_b$epsilon_mean))
  testthat::expect_true(is.finite(fit_btl_b$beta_mean))

  fit_btl_e_b <- pairwiseLLM:::build_v3_fit_contract(
    theta_draws = theta_draws,
    epsilon_draws = epsilon_draws,
    beta_draws = beta_draws,
    model_variant = "btl_e_b"
  )
  testthat::expect_true(is.numeric(fit_btl_e_b$epsilon_draws))
  testthat::expect_true(is.numeric(fit_btl_e_b$beta_draws))
  testthat::expect_true(is.finite(fit_btl_e_b$epsilon_p95))
  testthat::expect_true(is.finite(fit_btl_e_b$beta_p95))
})
