testthat::test_that(".fit_bayes_btl_mcmc_adaptive is deterministic with fixed seed", {
  testthat::skip_if_not_installed("cmdstanr")
  cmdstan_path <- tryCatch(cmdstanr::cmdstan_path(), error = function(e) "")
  if (!nzchar(cmdstan_path)) {
    testthat::skip("CmdStan is not installed for MCMC test.")
  }

  out_dir <- withr::local_tempdir()
  bt_data <- list(
    A = c(1L, 2L, 1L),
    B = c(2L, 3L, 3L),
    Y = c(1L, 0L, 1L),
    N = 3L,
    item_id = c("A", "B", "C")
  )
  config <- pairwiseLLM:::adaptive_v3_config(
    bt_data$N,
    list(
      thin_draws = 1L,
      cmdstan = list(
        chains = 2,
        iter_warmup = 50,
        iter_sampling = 50,
        core_fraction = 0.5,
        output_dir = out_dir
      )
    )
  )

  withr::local_seed(202)
  fit1 <- tryCatch(
    pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data = bt_data, config = config, seed = 99),
    error = function(e) {
      testthat::skip(paste("CmdStan not usable for MCMC test:", conditionMessage(e)))
    }
  )

  withr::local_seed(202)
  fit2 <- tryCatch(
    pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data = bt_data, config = config, seed = 99),
    error = function(e) {
      testthat::skip(paste("CmdStan not usable for MCMC test:", conditionMessage(e)))
    }
  )

  expect_equal(fit1$theta_summary, fit2$theta_summary, tolerance = 1e-8)
  expect_equal(fit1$epsilon_summary, fit2$epsilon_summary, tolerance = 1e-8)
})
