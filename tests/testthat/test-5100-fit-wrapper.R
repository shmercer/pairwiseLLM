make_bt_data_v3 <- function() {
  list(
    A = c(1L, 1L, 2L, 3L),
    B = c(2L, 3L, 3L, 1L),
    Y = c(1L, 0L, 1L, 0L),
    N = 3L,
    item_id = c("A", "B", "C")
  )
}

testthat::test_that(".fit_bayes_btl_mcmc_adaptive returns required outputs", {
  testthat::skip_if_not_installed("cmdstanr")
  cmdstan_path <- tryCatch(cmdstanr::cmdstan_path(), error = function(e) "")
  if (!nzchar(cmdstan_path)) {
    testthat::skip("CmdStan is not installed for MCMC test.")
  }

  out_dir <- withr::local_tempdir()
  bt_data <- make_bt_data_v3()
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

  withr::local_seed(101)
  fit <- tryCatch(
    withCallingHandlers(
      pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data = bt_data, config = config, seed = 101),
      warning = function(w) {
        msg <- conditionMessage(w)
        if (grepl("threads_per_chain", msg, fixed = TRUE) &&
          grepl("stan_threads", msg, fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    ),
    error = function(e) {
      testthat::skip(paste("CmdStan not usable for MCMC test:", conditionMessage(e)))
    }
  )

  expect_true(all(c("draws", "theta_summary", "epsilon_summary", "diagnostics") %in% names(fit)))
  expect_true(is.list(fit$draws))
  expect_equal(nrow(fit$theta_summary), 3L)
  expect_equal(nrow(fit$epsilon_summary), 1L)

  theta_cols <- c(
    "item_id", "theta_mean", "theta_sd",
    "theta_ci90_low", "theta_ci90_high",
    "theta_ci95_low", "theta_ci95_high"
  )
  expect_true(all(theta_cols %in% names(fit$theta_summary)))
  expect_true(is.character(fit$theta_summary$item_id))
  expect_true(is.numeric(fit$theta_summary$theta_mean))

  epsilon_cols <- c(
    "epsilon_mean",
    "epsilon_p2.5", "epsilon_p5", "epsilon_p50",
    "epsilon_p95", "epsilon_p97.5"
  )
  expect_true(all(epsilon_cols %in% names(fit$epsilon_summary)))
  expect_true(is.numeric(fit$epsilon_summary$epsilon_mean))
})
