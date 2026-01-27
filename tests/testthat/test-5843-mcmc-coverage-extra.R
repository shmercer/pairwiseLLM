testthat::test_that("adaptive_v3_config normalizes NULL overrides", {
  config <- pairwiseLLM:::adaptive_v3_config(4L, NULL)
  testthat::expect_equal(config$N, 4L)
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

testthat::test_that("round log defaults handle POSIXct columns", {
  schema <- tibble::tibble(
    created_at = as.POSIXct(character(), tz = "UTC"),
    count = integer()
  )
  defaults <- pairwiseLLM:::.adaptive_log_defaults_from_schema(schema)

  testthat::expect_true(inherits(defaults$created_at, "POSIXct"))
})

testthat::test_that("cmdstan config rejects non-list inputs", {
  testthat::expect_error(
    pairwiseLLM:::.btl_mcmc_resolve_cmdstan_config(1),
    "`cmdstan` must be a list"
  )
})

testthat::test_that("id validation rejects missing and duplicate ids", {
  testthat::expect_error(
    pairwiseLLM:::.btl_validate_ids("a"),
    "at least two"
  )
  testthat::expect_error(
    pairwiseLLM:::.btl_validate_ids(c("a", NA_character_)),
    "missing"
  )
  testthat::expect_error(
    pairwiseLLM:::.btl_validate_ids(c("a", "a")),
    "unique"
  )
})

testthat::test_that("fit_bayes_btl_mcmc_adaptive rejects invalid thinning", {
  testthat::skip_if_not_installed("cmdstanr")
  fn <- pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive
  if (!any(grepl("thin_draws", deparse(body(fn))))) {
    testthat::skip("mcmc fit helper is mocked; skipping thin_draws validation.")
  }
  bt_data <- list(A = 1L, B = 2L, Y = 1L, N = 2L, item_id = c("a", "b"))
  config <- pairwiseLLM:::adaptive_v3_config(2L, list(model_variant = "btl_e"))
  config$thin_draws <- NA_integer_
  config$cmdstan <- list(chains = 1L, iter_warmup = 1L, iter_sampling = 1L)

  fake_fit <- list(
    draws = function(variables = NULL, format = NULL) {
      mat <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.05, 0.06), nrow = 2, byrow = TRUE)
      colnames(mat) <- c("theta[1]", "theta[2]", "epsilon")
      mat
    },
    diagnostic_summary = function() tibble::tibble(num_divergent = 0L),
    summary = function(variables = NULL) {
      tibble::tibble(rhat = 1, ess_bulk = 100, ess_tail = 100)
    }
  )
  fake_model <- list(sample = function(...) fake_fit)

  restore_req <- local_rebind_namespace(
    "pairwiseLLM",
    ".btl_mcmc_require_cmdstanr",
    function() NULL
  )
  restore_stan <- local_rebind_namespace(
    "pairwiseLLM",
    "stan_file_for_variant",
    function(...) "fake.stan"
  )
  restore_model <- local_rebind_namespace(
    "cmdstanr",
    "cmdstan_model",
    function(...) fake_model
  )
  on.exit({
    restore_model()
    restore_stan()
    restore_req()
  }, add = TRUE)

  testthat::expect_error(
    pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config),
    "thin_draws"
  )
})

testthat::test_that("fit_bayes_btl_mcmc_adaptive validates parallel chains", {
  testthat::skip_if_not_installed("cmdstanr")
  fn <- pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive
  if (!any(grepl("parallel_chains", deparse(body(fn))))) {
    testthat::skip("mcmc fit helper is mocked; skipping parallel_chains validation.")
  }
  bt_data <- list(A = 1L, B = 2L, Y = 1L, N = 2L, item_id = c("a", "b"))
  config <- pairwiseLLM:::adaptive_v3_config(2L, list(model_variant = "btl_e"))
  config$cmdstan <- list(iter_warmup = 1L, iter_sampling = 1L)

  restore_req <- local_rebind_namespace(
    "pairwiseLLM",
    ".btl_mcmc_require_cmdstanr",
    function() NULL
  )
  restore_resolve <- local_rebind_namespace(
    "pairwiseLLM",
    ".btl_mcmc_resolve_cmdstan_config",
    function(...) {
      list(
        chains = 1L,
        parallel_chains = NA_integer_,
        core_fraction = 0.8,
        cores_detected_physical = 1L,
        cores_detected_logical = 1L,
        threads_per_chain = NA_integer_,
        cmdstanr_version = NA_character_
      )
    }
  )
  on.exit({
    restore_resolve()
    restore_req()
  }, add = TRUE)

  testthat::expect_error(
    pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config),
    "parallel_chains"
  )
})

testthat::test_that("cmdstanr availability check returns TRUE when configured", {
  testthat::skip_if_not_installed("cmdstanr")

  restore_path <- local_rebind_namespace(
    "cmdstanr",
    "cmdstan_path",
    function() "fake"
  )
  on.exit(restore_path(), add = TRUE)

  testthat::expect_silent(pairwiseLLM:::.btl_mcmc_require_cmdstanr())
})

testthat::test_that("fit_bayes_btl_mcmc rejects invalid parallel chains", {
  testthat::skip_if_not_installed("cmdstanr")
  results <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2026-01-02 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test"
  )

  restore_req <- local_rebind_namespace(
    "pairwiseLLM",
    ".btl_mcmc_require_cmdstanr",
    function() TRUE
  )
  restore_resolve <- local_rebind_namespace(
    "pairwiseLLM",
    ".btl_mcmc_resolve_cmdstan_config",
    function(...) {
      list(
        chains = 1L,
        parallel_chains = NA_integer_,
        core_fraction = 0.8,
        cores_detected_physical = 1L,
        cores_detected_logical = 1L,
        threads_per_chain = NA_integer_,
        cmdstanr_version = NA_character_
      )
    }
  )
  on.exit({
    restore_resolve()
    restore_req()
  }, add = TRUE)

  testthat::expect_error(
    pairwiseLLM:::fit_bayes_btl_mcmc(
      results,
      ids = c("A", "B"),
      cmdstan = list(iter_warmup = 1L, iter_sampling = 1L)
    ),
    "parallel_chains"
  )
})

testthat::test_that("fit_bayes_btl_mcmc_adaptive validates cmdstan inputs and outputs", {
  testthat::skip_if_not_installed("cmdstanr")
  fn <- pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive
  if (!any(grepl("cmdstan", deparse(body(fn))))) {
    testthat::skip("mcmc fit helper is mocked; skipping cmdstan validation.")
  }
  bt_data <- list(A = 1L, B = 2L, Y = 1L, N = 2L, item_id = c("a", "b"))
  config <- pairwiseLLM:::adaptive_v3_config(2L, list(model_variant = "btl_b"))

  restore_req <- local_rebind_namespace(
    "pairwiseLLM",
    ".btl_mcmc_require_cmdstanr",
    function() NULL
  )
  on.exit(restore_req(), add = TRUE)

  config_bad <- config
  config_bad$cmdstan <- 1
  testthat::expect_error(
    pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config_bad),
    "cmdstan"
  )

  fake_fit <- list(
    draws = function(variables = NULL, format = NULL) {
      mat <- matrix(c(0.1, 0.2, 0.05, 0.3, 0.4, 0.06), nrow = 2, byrow = TRUE)
      colnames(mat) <- c("theta[1]", "theta[2]", "beta")
      mat
    },
    diagnostic_summary = function() tibble::tibble(num_divergent = 0L),
    summary = function(variables = NULL) {
      tibble::tibble(rhat = 1, ess_bulk = 100, ess_tail = 100)
    }
  )
  fake_model <- list(sample = function(...) fake_fit)

  restore_resolve <- local_rebind_namespace(
    "pairwiseLLM",
    ".btl_mcmc_resolve_cmdstan_config",
    function(...) {
      list(
        chains = 1L,
        parallel_chains = 1L,
        core_fraction = 0.8,
        cores_detected_physical = 1L,
        cores_detected_logical = 1L,
        threads_per_chain = NA_integer_,
        cmdstanr_version = NA_character_
      )
    }
  )
  restore_stan <- local_rebind_namespace(
    "pairwiseLLM",
    "stan_file_for_variant",
    function(...) "fake.stan"
  )
  restore_model <- local_rebind_namespace(
    "cmdstanr",
    "cmdstan_model",
    function(...) fake_model
  )
  on.exit({
    restore_model()
    restore_stan()
    restore_resolve()
  }, add = TRUE)

  config_outdir <- config
  config_outdir$cmdstan <- list(chains = 1L, iter_warmup = 1L, iter_sampling = 1L, output_dir = 1)
  testthat::expect_error(
    pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config_outdir),
    "output_dir"
  )

  bad_fit <- list(
    draws = function(variables = NULL, format = NULL) {
      mat <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2, byrow = TRUE)
      colnames(mat) <- c("theta[1]", "theta[2]")
      mat
    },
    diagnostic_summary = function() tibble::tibble(num_divergent = 0L),
    summary = function(variables = NULL) {
      tibble::tibble(rhat = 1, ess_bulk = 100, ess_tail = 100)
    }
  )
  restore_model_bad <- local_rebind_namespace(
    "cmdstanr",
    "cmdstan_model",
    function(...) list(sample = function(...) bad_fit)
  )
  on.exit(restore_model_bad(), add = TRUE)
  config_bad_beta <- config
  config_bad_beta$cmdstan <- list(chains = 1L, iter_warmup = 1L, iter_sampling = 1L)
  testthat::expect_error(
    pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config_bad_beta),
    "beta"
  )

  restore_model_bad()
  restore_model <- local_rebind_namespace(
    "cmdstanr",
    "cmdstan_model",
    function(...) fake_model
  )
  on.exit(restore_model(), add = TRUE)

  config_thin <- config
  config_thin$cmdstan <- list(chains = 1L, iter_warmup = 1L, iter_sampling = 1L)
  config_thin$thin_draws <- 2L
  out <- pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config_thin)
  testthat::expect_null(out$draws$epsilon)
  testthat::expect_equal(length(out$draws$beta), 1L)
})
