testthat::test_that("adaptive_v3_config normalizes NULL overrides", {
  config <- pairwiseLLM:::adaptive_v3_config(4L, NULL)
  testthat::expect_equal(config$N, 4L)
})

testthat::test_that("round log defaults handle POSIXct columns", {
  defaults <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_round_log_defaults(),
    round_log_schema = function() {
      tibble::tibble(
        created_at = as.POSIXct(character(), tz = "UTC"),
        count = integer()
      )
    },
    .env = asNamespace("pairwiseLLM")
  )

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
  bt_data <- list(A = 1L, B = 2L, Y = 1L, N = 2L, item_id = c("a", "b"))
  config <- pairwiseLLM:::adaptive_v3_config(2L)
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

  testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      testthat::expect_error(
        pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config),
        "thin_draws"
      ),
      .btl_mcmc_require_cmdstanr = function() NULL,
      .package = "pairwiseLLM"
    ),
    cmdstan_model = function(file) fake_model,
    write_stan_file = function(code) "fake.stan",
    .package = "cmdstanr"
  )
})

testthat::test_that("fit_bayes_btl_mcmc_adaptive validates parallel chains", {
  bt_data <- list(A = 1L, B = 2L, Y = 1L, N = 2L, item_id = c("a", "b"))
  config <- pairwiseLLM:::adaptive_v3_config(2L)
  config$cmdstan <- list(iter_warmup = 1L, iter_sampling = 1L)

  testthat::with_mocked_bindings(
    testthat::expect_error(
      pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config),
      "parallel_chains"
    ),
    .btl_mcmc_require_cmdstanr = function() NULL,
    .btl_mcmc_resolve_cmdstan_config = function(...) {
      list(
        chains = 1L,
        parallel_chains = NA_integer_,
        core_fraction = 0.6,
        cores_detected_physical = 1L,
        cores_detected_logical = 1L,
        threads_per_chain = NA_integer_,
        cmdstanr_version = NA_character_
      )
    },
    .env = asNamespace("pairwiseLLM")
  )
})

testthat::test_that("cmdstanr availability check returns TRUE when configured", {
  testthat::skip_if_not_installed("cmdstanr")

  testthat::with_mocked_bindings(
    testthat::expect_silent(pairwiseLLM:::.btl_mcmc_require_cmdstanr()),
    cmdstan_path = function() "fake",
    .package = "cmdstanr"
  )
})

testthat::test_that("fit_bayes_btl_mcmc rejects invalid parallel chains", {
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

  testthat::with_mocked_bindings(
    testthat::expect_error(
      pairwiseLLM:::fit_bayes_btl_mcmc(
        results,
        ids = c("A", "B"),
        cmdstan = list(iter_warmup = 1L, iter_sampling = 1L)
      ),
      "parallel_chains"
    ),
    .btl_mcmc_require_cmdstanr = function() TRUE,
    .btl_mcmc_resolve_cmdstan_config = function(...) {
      list(
        chains = 1L,
        parallel_chains = NA_integer_,
        core_fraction = 0.6,
        cores_detected_physical = 1L,
        cores_detected_logical = 1L,
        threads_per_chain = NA_integer_,
        cmdstanr_version = NA_character_
      )
    },
    .env = asNamespace("pairwiseLLM")
  )
})
