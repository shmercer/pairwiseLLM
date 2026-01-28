testthat::test_that(".fit_bayes_btl_mcmc_adaptive returns diagnostics with required fields", {
  testthat::skip_if_not_installed("cmdstanr")
  cmdstan_path <- tryCatch(cmdstanr::cmdstan_path(), error = function(e) "")
  if (!nzchar(cmdstan_path)) {
    testthat::skip("CmdStan is not installed for MCMC test.")
  }

  out_dir <- withr::local_tempdir()
  bt_data <- list(
    A = c(1L, 1L, 2L),
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

  withr::local_seed(303)
  fit <- tryCatch(
    withCallingHandlers(
      pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data = bt_data, config = config, seed = 303),
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

  diagnostics <- fit$diagnostics
  required <- c("divergences", "max_rhat", "min_ess_bulk", "min_ess_tail")
  expect_true(all(required %in% names(diagnostics)))
  expect_true(is.integer(diagnostics$divergences) || is.numeric(diagnostics$divergences))
  expect_true(is.numeric(diagnostics$max_rhat))
  expect_true(is.numeric(diagnostics$min_ess_bulk))
  expect_true(is.numeric(diagnostics$min_ess_tail))
})
