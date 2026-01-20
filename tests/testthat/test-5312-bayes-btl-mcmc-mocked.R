testthat::test_that(".btl_mcmc_require_cmdstanr aborts when cmdstanr is missing", {
  testthat::with_mocked_bindings(
    requireNamespace = function(...) FALSE,
    {
      testthat::expect_error(
        pairwiseLLM:::.btl_mcmc_require_cmdstanr(),
        "CmdStanR is required"
      )
    },
    .package = "base"
  )
})

testthat::test_that(".btl_mcmc_require_cmdstanr aborts when CmdStan is unavailable", {
  testthat::with_mocked_bindings(
    cmdstan_path = function() "",
    {
      testthat::expect_error(
        pairwiseLLM:::.btl_mcmc_require_cmdstanr(),
        "CmdStan is not available"
      )
    },
    .package = "cmdstanr"
  )
})

testthat::test_that("fit_bayes_btl_mcmc validates cmdstan inputs before sampling", {
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

  testthat::expect_error(
    pairwiseLLM:::fit_bayes_btl_mcmc(results, ids = c("A", "B"), cmdstan = 1),
    "`cmdstan` must be a list"
  )

  testthat::expect_error(
    pairwiseLLM:::fit_bayes_btl_mcmc(results, ids = c("A", "B"), cmdstan = list(output_dir = NA_character_)),
    "length-1 character path"
  )
})

testthat::test_that("fit_bayes_btl_mcmc rejects invalid CmdStan integer settings", {
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
    .btl_mcmc_require_cmdstanr = function() TRUE,
    {
      testthat::expect_error(
        pairwiseLLM:::fit_bayes_btl_mcmc(
          results,
          ids = c("A", "B"),
          cmdstan = list(chains = 0L, iter_warmup = 1L, iter_sampling = 1L)
        ),
        "positive integers"
      )
    },
    .env = asNamespace("pairwiseLLM")
  )
})

testthat::test_that("fit_bayes_btl_mcmc returns theta_draws with mocked cmdstanr", {
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

  seen <- rlang::env(sample_args = NULL)
  fake_fit <- list(
    draws = function(variables, format) {
      testthat::expect_equal(variables, "theta")
      testthat::expect_equal(format, "matrix")
      m <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
      colnames(m) <- c("theta[1]", "theta[2]")
      m
    },
    diagnostic_summary = function() tibble::tibble(name = "ok", value = 1)
  )

  fake_model <- list(
    sample = function(...) {
      seen$sample_args <- list(...)
      fake_fit
    }
  )

  testthat::with_mocked_bindings(
    .btl_mcmc_require_cmdstanr = function() TRUE,
    {
      testthat::with_mocked_bindings(
        write_stan_file = function(code) {
          testthat::expect_true(is.character(code))
          "fake.stan"
        },
        cmdstan_model = function(stan_file) {
          testthat::expect_equal(stan_file, "fake.stan")
          fake_model
        },
        {
          out <- pairwiseLLM:::fit_bayes_btl_mcmc(
            results,
            ids = c("A", "B"),
            cmdstan = list(
              chains = 2L,
              iter_warmup = 3L,
              iter_sampling = 4L,
              seed = 123,
              core_fraction = 0.5,
              output_dir = "out"
            )
          )
          testthat::expect_true(is.matrix(out$theta_draws))
          testthat::expect_equal(colnames(out$theta_draws), c("A", "B"))
          testthat::expect_equal(out$fit_meta$converged, TRUE)

          testthat::expect_equal(seen$sample_args$refresh, 0)
          testthat::expect_equal(seen$sample_args$chains, 2L)
          testthat::expect_equal(seen$sample_args$iter_warmup, 3L)
          testthat::expect_equal(seen$sample_args$iter_sampling, 4L)
          testthat::expect_equal(seen$sample_args$seed, 123)
          testthat::expect_equal(seen$sample_args$output_dir, "out")
        },
        .package = "cmdstanr"
      )
    },
    .env = asNamespace("pairwiseLLM")
  )
})

testthat::test_that("fit_bayes_btl_mcmc aborts when CmdStan output misses theta columns", {
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

  fake_fit <- list(
    draws = function(...) {
      m <- matrix(0.1, nrow = 1)
      colnames(m) <- "theta[1]"
      m
    },
    diagnostic_summary = function() tibble::tibble(name = "ok", value = 1)
  )

  fake_model <- list(sample = function(...) fake_fit)

  testthat::with_mocked_bindings(
    .btl_mcmc_require_cmdstanr = function() TRUE,
    {
      testthat::with_mocked_bindings(
        write_stan_file = function(...) "fake.stan",
        cmdstan_model = function(...) fake_model,
        {
          testthat::expect_error(
            pairwiseLLM:::fit_bayes_btl_mcmc(
              results,
              ids = c("A", "B"),
              cmdstan = list(iter_warmup = 1L, iter_sampling = 1L, chains = 1L)
            ),
            "missing theta draws"
          )
        },
        .package = "cmdstanr"
      )
    },
    .env = asNamespace("pairwiseLLM")
  )
})
