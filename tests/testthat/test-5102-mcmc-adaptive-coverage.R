testthat::test_that("btl mcmc adaptive helpers validate inputs", {
  bad_AB <- list(A = "a", B = 1, Y = 1, N = 2)
  testthat::expect_error(
    pairwiseLLM:::.btl_mcmc_v3_validate_bt_data(bad_AB),
    "A` and `bt_data\\$B"
  )

  bad_Y <- list(A = 1, B = 2, Y = "a", N = 2)
  testthat::expect_error(
    pairwiseLLM:::.btl_mcmc_v3_validate_bt_data(bad_Y),
    "bt_data\\$Y"
  )
})

local_rebind_namespace <- function(ns, name, value) {
  env <- asNamespace(ns)
  has_old <- exists(name, envir = env, inherits = FALSE)
  old <- if (has_old) get(name, envir = env, inherits = FALSE) else NULL
  locked <- if (has_old) bindingIsLocked(name, env) else FALSE
  if (locked) {
    unlockBinding(name, env)
  }
  assign(name, value, envir = env)
  if (locked) {
    lockBinding(name, env)
  }
  function() {
    if (locked) {
      unlockBinding(name, env)
    }
    if (has_old) {
      assign(name, old, envir = env)
    } else if (exists(name, envir = env, inherits = FALSE)) {
      rm(list = name, envir = env)
    }
    if (locked) {
      lockBinding(name, env)
    }
  }
}

testthat::test_that("model code lookup aborts when stan file missing", {
  testthat::expect_error(
    pairwiseLLM:::.btl_mcmc_v3_model_code(path_override = 1),
    "path_override"
  )
  testthat::expect_error(
    pairwiseLLM:::.btl_mcmc_v3_model_code(path_override = ""),
    "not found"
  )
})

testthat::test_that("summarize_draws warns and aborts on insufficient epsilon draws", {
  draws <- list(
    theta = matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2, ncol = 2),
    epsilon = c(NA_real_, 0.1)
  )
  colnames(draws$theta) <- c("a", "b")

  testthat::expect_warning(
    testthat::expect_error(
      pairwiseLLM:::summarize_draws(draws),
      "at least two finite values"
    ),
    "Non-finite"
  )
})

testthat::test_that("as_v3_fit_contract_from_mcmc rejects bad shapes", {
  ids <- c("a", "b")
  theta_draws <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2, ncol = 2)
  colnames(theta_draws) <- ids

  base_fit <- list(
    draws = list(theta = theta_draws, epsilon = c(0.1, 0.2)),
    theta_summary = tibble::tibble(item_id = ids, theta_mean = c(0.1, 0.2)),
    epsilon_summary = tibble::tibble(
      epsilon_mean = 0.2,
      epsilon_p2.5 = 0.12,
      epsilon_p5 = 0.14,
      epsilon_p50 = 0.2,
      epsilon_p95 = 0.28,
      epsilon_p97.5 = 0.3
    ),
    diagnostics = list()
  )

  testthat::expect_error(
    pairwiseLLM:::as_v3_fit_contract_from_mcmc("bad", ids),
    "mcmc_fit"
  )
  testthat::expect_error(
    pairwiseLLM:::as_v3_fit_contract_from_mcmc(base_fit, character()),
    "non-empty"
  )
  testthat::expect_error(
    pairwiseLLM:::as_v3_fit_contract_from_mcmc(base_fit, c("a", "a")),
    "unique"
  )

  bad_draws <- base_fit
  bad_draws$draws <- "nope"
  testthat::expect_error(
    pairwiseLLM:::as_v3_fit_contract_from_mcmc(bad_draws, ids),
    "draws"
  )

  bad_theta <- base_fit
  bad_theta$draws$theta <- 1
  testthat::expect_error(
    pairwiseLLM:::as_v3_fit_contract_from_mcmc(bad_theta, ids),
    "theta` must be a numeric matrix"
  )

  no_colnames <- base_fit
  colnames(no_colnames$draws$theta) <- NULL
  testthat::expect_error(
    pairwiseLLM:::as_v3_fit_contract_from_mcmc(no_colnames, ids),
    "column names"
  )

  bad_summary <- base_fit
  bad_summary$theta_summary <- "nope"
  testthat::expect_error(
    pairwiseLLM:::as_v3_fit_contract_from_mcmc(bad_summary, ids),
    "theta_summary"
  )

  missing_ids <- base_fit
  missing_ids$theta_summary$item_id <- c("a", NA_character_)
  testthat::expect_error(
    pairwiseLLM:::as_v3_fit_contract_from_mcmc(missing_ids, ids),
    "non-missing"
  )

  wrong_ids <- base_fit
  wrong_ids$theta_summary$item_id <- c("a", "c")
  testthat::expect_error(
    pairwiseLLM:::as_v3_fit_contract_from_mcmc(wrong_ids, ids),
    "cover all"
  )

  bad_eps_summary <- base_fit
  bad_eps_summary$epsilon_summary <- "nope"
  testthat::expect_error(
    pairwiseLLM:::as_v3_fit_contract_from_mcmc(bad_eps_summary, ids),
    "epsilon_summary"
  )

  missing_eps <- base_fit
  missing_eps$epsilon_summary <- tibble::tibble(other = 0.2)
  testthat::expect_error(
    pairwiseLLM:::as_v3_fit_contract_from_mcmc(missing_eps, ids),
    "epsilon_mean"
  )

  bad_eps_value <- base_fit
  bad_eps_value$epsilon_summary <- tibble::tibble(
    epsilon_mean = NA_real_,
    epsilon_p2.5 = 0.12,
    epsilon_p5 = 0.14,
    epsilon_p50 = 0.2,
    epsilon_p95 = 0.28,
    epsilon_p97.5 = 0.3
  )
  testthat::expect_error(
    pairwiseLLM:::as_v3_fit_contract_from_mcmc(bad_eps_value, ids),
    "finite numeric scalar"
  )
})

testthat::test_that("fit_bayes_btl_mcmc_adaptive validates config and thin draws", {
  bt_data <- list(A = 1L, B = 2L, Y = 1L, N = 2L, item_id = c("a", "b"))

  restore <- local_rebind_namespace(
    "pairwiseLLM",
    ".btl_mcmc_require_cmdstanr",
    function() invisible(TRUE)
  )
  on.exit(restore(), add = TRUE)
  testthat::expect_error(
    pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config = 1),
    "config"
  )

  config_bad <- pairwiseLLM:::adaptive_v3_config(2)
  config_bad$cmdstan <- list(chains = 0L, iter_warmup = 1L, iter_sampling = 1L)
  testthat::expect_error(
    pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config = config_bad),
    "positive integers"
  )
})

testthat::test_that("fit_bayes_btl_mcmc_adaptive handles draw matrix checks", {
  bt_data <- list(A = 1L, B = 2L, Y = 1L, N = 2L, item_id = c("a", "b"))
  config <- pairwiseLLM:::adaptive_v3_config(2)
  config$cmdstan <- list(output_dir = tempdir())

  restore_req <- local_rebind_namespace(
    "pairwiseLLM",
    ".btl_mcmc_require_cmdstanr",
    function() invisible(TRUE)
  )
  restore_write <- local_rebind_namespace("cmdstanr", "write_stan_file", function(...) "fake.stan")
  stub_model <- function(...) {
    list(sample = function(...) {
      list(draws = function(...) {
        matrix(0.1, nrow = 2, ncol = 1, dimnames = list(NULL, "epsilon"))
      })
    })
  }
  restore_model <- local_rebind_namespace("cmdstanr", "cmdstan_model", stub_model)
  on.exit({
    restore_model()
    restore_write()
    restore_req()
  }, add = TRUE)
  testthat::expect_true(identical(cmdstanr::cmdstan_model, stub_model))

  testthat::expect_error(
    pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config = config),
    "missing theta draws"
  )
})

testthat::test_that("fit_bayes_btl_mcmc_adaptive validates thin_draws", {
  bt_data <- list(A = 1L, B = 2L, Y = 1L, N = 2L, item_id = c("a", "b"))
  config <- pairwiseLLM:::adaptive_v3_config(2)
  config$thin_draws <- 0L

  restore_req <- local_rebind_namespace(
    "pairwiseLLM",
    ".btl_mcmc_require_cmdstanr",
    function() invisible(TRUE)
  )
  restore_write <- local_rebind_namespace("cmdstanr", "write_stan_file", function(...) "fake.stan")
  restore_model <- local_rebind_namespace("cmdstanr", "cmdstan_model", function(...) {
    list(sample = function(...) {
      list(draws = function(...) {
        matrix(
          c(0.1, 0.2, 0.3, 0.4, 0.2, 0.2),
          nrow = 2,
          ncol = 3,
          dimnames = list(NULL, c("theta[1]", "theta[2]", "epsilon"))
        )
      })
    })
  })
  on.exit({
    restore_model()
    restore_write()
    restore_req()
  }, add = TRUE)

  testthat::expect_error(
    pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config = config),
    "thin_draws"
  )
})

testthat::test_that("fit_bayes_btl_mcmc_adaptive thins draws deterministically", {
  bt_data <- list(A = 1L, B = 2L, Y = 1L, N = 2L, item_id = c("a", "b"))
  config <- pairwiseLLM:::adaptive_v3_config(2)
  config$thin_draws <- 2L

  draws_matrix <- matrix(
    c(
      0.1, 0.2, 0.3, 0.4,
      0.5, 0.6, 0.7, 0.8,
      0.05, 0.06, 0.07, 0.08
    ),
    nrow = 4,
    ncol = 3,
    dimnames = list(NULL, c("theta[1]", "theta[2]", "epsilon"))
  )

  restore_req <- local_rebind_namespace(
    "pairwiseLLM",
    ".btl_mcmc_require_cmdstanr",
    function() invisible(TRUE)
  )
  restore_write <- local_rebind_namespace("cmdstanr", "write_stan_file", function(...) "fake.stan")
  restore_model <- local_rebind_namespace("cmdstanr", "cmdstan_model", function(...) {
    list(sample = function(...) {
      list(
        draws = function(...) draws_matrix,
        diagnostic_summary = function() tibble::tibble(num_divergent = 0),
        summary = function(...) tibble::tibble(rhat = 1, ess_bulk = 500, ess_tail = 500)
      )
    })
  })
  on.exit({
    restore_model()
    restore_write()
    restore_req()
  }, add = TRUE)

  out <- pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config = config)
  testthat::expect_equal(nrow(out$draws$theta), 2L)
  testthat::expect_equal(length(out$draws$epsilon), 2L)
})
