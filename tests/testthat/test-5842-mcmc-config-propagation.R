make_v3_bt_data_5842 <- function() {
  list(
    A = c(1L, 2L),
    B = c(2L, 1L),
    Y = c(1L, 0L),
    N = 2L,
    item_id = c("A", "B")
  )
}

testthat::test_that("cmdstanr sampling receives resolved chain settings", {
  bt_data <- make_v3_bt_data_5842()
  config <- pairwiseLLM:::adaptive_v3_config(2L, list(model_variant = "btl_e"))
  config$cmdstan <- list(
    chains = 4L,
    parallel_chains = 7L,
    iter_warmup = 2L,
    iter_sampling = 2L
  )

  capture_env <- rlang::env(args = NULL)
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
  fake_model <- list(sample = function(...) {
    capture_env$args <- list(...)
    fake_fit
  })

  out <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      pairwiseLLM:::.fit_bayes_btl_mcmc_adaptive(bt_data, config),
      .btl_mcmc_require_cmdstanr = function() NULL,
      stan_file_for_variant = function(...) "fake.stan",
      .package = "pairwiseLLM"
    ),
    cmdstan_model = function(file) fake_model,
    .package = "cmdstanr"
  )

  testthat::expect_equal(capture_env$args$chains, 4L)
  testthat::expect_equal(capture_env$args$parallel_chains, 4L)
  testthat::expect_true(is.list(out$mcmc_config_used))
  testthat::expect_equal(out$mcmc_config_used$parallel_chains, 4L)
})

testthat::test_that("round log rows include mcmc config used", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list())
  state$posterior$mcmc_config_used <- list(
    chains = 6L,
    parallel_chains = 4L,
    core_fraction = 0.5,
    cores_detected_physical = 12L,
    cores_detected_logical = 16L,
    threads_per_chain = 2L,
    cmdstanr_version = "0.0.0"
  )

  row <- pairwiseLLM:::build_round_log_row(state = state, config = state$config$v3)

  testthat::expect_equal(row$mcmc_chains[[1L]], 6L)
  testthat::expect_equal(row$mcmc_parallel_chains[[1L]], 4L)
  testthat::expect_equal(row$mcmc_core_fraction[[1L]], 0.5)
  testthat::expect_equal(row$mcmc_cmdstanr_version[[1L]], "0.0.0")
})

testthat::test_that("refit stores mcmc config used on state", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list())
  state$history_results <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase1",
    iter = 1L,
    received_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test"
  )
  state$comparisons_observed <- 1L
  state$new_since_refit <- 1L

  used <- list(chains = 2L, parallel_chains = 2L, core_fraction = 0.8)
  make_mcmc_fit <- function(ids) {
    theta_draws <- matrix(
      0,
      nrow = 2,
      ncol = length(ids),
      dimnames = list(NULL, ids)
    )
    list(
      draws = list(theta = theta_draws, epsilon = c(0.1, 0.12)),
      theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
      epsilon_summary = tibble::tibble(
        epsilon_mean = 0.1,
        epsilon_p2.5 = 0.01,
        epsilon_p5 = 0.02,
        epsilon_p50 = 0.1,
        epsilon_p95 = 0.2,
        epsilon_p97.5 = 0.21
      ),
      diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000),
      model_variant = "btl_e"
    )
  }
  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_get_refit_fit(
      state,
      adaptive = list(),
      batch_size = 1L,
      seed = NULL
    ),
    .fit_bayes_btl_mcmc_adaptive = function(...) {
      ids <- state$ids
      mcmc_fit <- make_mcmc_fit(ids)
      mcmc_fit$mcmc_config_used <- used
      mcmc_fit
    },
    .env = asNamespace("pairwiseLLM")
  )

  testthat::expect_equal(out$state$posterior$mcmc_config_used$chains, 2L)
})
