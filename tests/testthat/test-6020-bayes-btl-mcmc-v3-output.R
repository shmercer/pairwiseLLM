mock_btl_mcmc_fit <- function(bt_data, config, seed = NULL) {
  model_variant <- config$model_variant %||% "btl_e_b"
  ids <- bt_data$item_id %||% as.character(seq_len(bt_data$N))
  theta <- matrix(seq_len(2L * length(ids)), nrow = 2L)
  colnames(theta) <- ids
  draws <- list(theta = theta)
  if (pairwiseLLM:::model_has_e(model_variant)) {
    draws$epsilon <- c(0.05, 0.06)
  }
  if (pairwiseLLM:::model_has_b(model_variant)) {
    draws$beta <- c(0.1, -0.1)
  }
  list(
    draws = draws,
    model_variant = model_variant,
    diagnostics = list(divergences = 0L, max_rhat = 1.01, min_ess_bulk = 200),
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

make_results_tbl_multi <- function() {
  tibble::tibble(
    pair_uid = c("A:B#1", "A:C#1", "B:C#1"),
    unordered_key = c("A:B", "A:C", "B:C"),
    ordered_key = c("A:B", "A:C", "B:C"),
    A_id = c("A", "A", "B"),
    B_id = c("B", "C", "C"),
    better_id = c("A", "C", "B"),
    winner_pos = c(1L, 2L, 1L),
    phase = c("phase2", "phase2", "phase2"),
    iter = c(1L, 1L, 1L),
    received_at = as.POSIXct(
      c("2026-01-02 00:00:00", "2026-01-02 00:00:01", "2026-01-02 00:00:02"),
      tz = "UTC"
    ),
    backend = c("openai", "openai", "openai"),
    model = c("gpt-test", "gpt-test", "gpt-test")
  )
}

testthat::test_that("fit_bayes_btl_mcmc returns schema-stable outputs across variants", {
  results <- make_results_tbl_multi()
  ids <- c("A", "B", "C")
  variants <- c("btl", "btl_e", "btl_b", "btl_e_b")

  testthat::with_mocked_bindings(
    .fit_bayes_btl_mcmc_adaptive = mock_btl_mcmc_fit,
    {
      for (variant in variants) {
        out <- pairwiseLLM:::fit_bayes_btl_mcmc(
          results,
          ids = ids,
          model_variant = variant
        )
        testthat::expect_true(all(c("theta_p2.5", "theta_p50", "theta_p97.5") %in% names(out$item_summary)))
        testthat::expect_true(all(c("rank_p2.5", "rank_p50", "rank_p97.5") %in% names(out$item_summary)))
        testthat::expect_true(all(c("epsilon_mean", "beta_mean") %in% names(out$round_log)))

        if (!pairwiseLLM:::model_has_e(variant)) {
          testthat::expect_true(is.na(out$round_log$epsilon_mean[[1L]]))
        }
        if (!pairwiseLLM:::model_has_b(variant)) {
          testthat::expect_true(is.na(out$round_log$beta_mean[[1L]]))
        }
      }
    },
    .env = asNamespace("pairwiseLLM")
  )
})

testthat::test_that("fit_bayes_btl_mcmc subset fitting is deterministic with seed", {
  results <- make_results_tbl_multi()
  ids <- c("A", "B", "C")

  testthat::with_mocked_bindings(
    .fit_bayes_btl_mcmc_adaptive = mock_btl_mcmc_fit,
    {
      out1 <- pairwiseLLM:::fit_bayes_btl_mcmc(
        results,
        ids = ids,
        model_variant = "btl_e_b",
        pair_counts = c(1, 2),
        subset_method = "sample",
        seed = 101
      )
      out2 <- pairwiseLLM:::fit_bayes_btl_mcmc(
        results,
        ids = ids,
        model_variant = "btl_e_b",
        pair_counts = c(1, 2),
        subset_method = "sample",
        seed = 101
      )

      testthat::expect_equal(nrow(out1$round_log), 2L)
      testthat::expect_equal(out1$round_log$n_unique_pairs_seen, out2$round_log$n_unique_pairs_seen)
      testthat::expect_equal(out1$item_summary$deg, out2$item_summary$deg)
      testthat::expect_equal(out1$item_summary$refit_id, out2$item_summary$refit_id)
    },
    .env = asNamespace("pairwiseLLM")
  )
})
