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

make_results_tbl <- function() {
  tibble::tibble(
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
}

fake_mcmc_fit <- function(ids) {
  theta <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
  colnames(theta) <- ids
  list(
    draws = list(theta = theta),
    model_variant = "btl",
    diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 100),
    mcmc_config_used = list(
      chains = 1L,
      parallel_chains = 1L,
      core_fraction = 0.8,
      cores_detected_physical = 1L,
      cores_detected_logical = 1L,
      threads_per_chain = 1L,
      cmdstanr_version = "0.1"
    )
  )
}

testthat::test_that("fit_bayes_btl_mcmc validates cmdstan inputs before sampling", {
  results <- make_results_tbl()

  testthat::expect_error(
    pairwiseLLM:::fit_bayes_btl_mcmc(results, ids = c("A", "B"), cmdstan = 1),
    "`cmdstan` must be a list"
  )

  testthat::expect_error(
    pairwiseLLM:::fit_bayes_btl_mcmc(
      results,
      ids = c("A", "B"),
      cmdstan = list(output_dir = NA_character_)
    ),
    "length-1 character path"
  )
})

testthat::test_that("fit_bayes_btl_mcmc returns adaptive outputs with mocked MCMC", {
  results <- make_results_tbl()
  ids <- c("A", "B")

  testthat::with_mocked_bindings(
    .fit_bayes_btl_mcmc_adaptive = function(...) fake_mcmc_fit(ids),
    {
      out <- pairwiseLLM:::fit_bayes_btl_mcmc(
        results,
        ids = ids,
        model_variant = "btl"
      )
      testthat::expect_true(is.data.frame(out$item_summary))
      testthat::expect_true(is.data.frame(out$round_log))
      testthat::expect_true(is.list(out$fit))
      testthat::expect_true(all(c("refit_id", "theta_p2.5", "rank_p50") %in% names(out$item_summary)))
      testthat::expect_true(all(c("epsilon_mean", "beta_mean") %in% names(out$round_log)))
      testthat::expect_true(is.na(out$round_log$epsilon_mean[[1L]]))
      testthat::expect_true(is.na(out$round_log$beta_mean[[1L]]))
    },
    .env = asNamespace("pairwiseLLM")
  )
})
