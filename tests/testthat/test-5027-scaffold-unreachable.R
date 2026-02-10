mock_fit_for_reachability <- function(bt_data, config, seed = NULL) {
  ids <- bt_data$item_id %||% as.character(seq_len(bt_data$N))
  model_variant <- config$model_variant %||% "btl_e_b"
  theta <- matrix(seq_len(2L * length(ids)), nrow = 2L)
  colnames(theta) <- ids
  draws <- list(theta = theta)
  if (pairwiseLLM:::model_has_e(model_variant)) {
    draws$epsilon <- c(0.05, 0.06)
  }
  if (pairwiseLLM:::model_has_b(model_variant)) {
    draws$beta <- c(0.10, -0.10)
  }
  list(
    draws = draws,
    model_variant = model_variant,
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 300),
    mcmc_config_used = list(
      chains = 1L,
      parallel_chains = 1L,
      core_fraction = 0.8,
      cores_detected_physical = 1L,
      cores_detected_logical = 1L,
      threads_per_chain = 1L,
      cmdstanr_version = "test"
    )
  )
}

make_items_reachability <- function(n = 3L) {
  tibble::tibble(
    item_id = seq_len(n),
    text = paste("item", seq_len(n))
  )
}

test_that("adaptive canonical entrypoints do not call legacy scaffold constructors", {
  testthat::with_mocked_bindings(
    btl_mcmc_state_new = function(...) {
      rlang::abort("legacy constructor reached")
    },
    {
      state <- adaptive_rank_start(make_items_reachability(3), seed = 11L)
      judge <- make_deterministic_judge("i_wins")
      out <- adaptive_rank_run_live(state, judge, n_steps = 2L, progress = "none")
      expect_s3_class(out, "adaptive_state")
      expect_equal(nrow(out$step_log), 2L)
    },
    .env = asNamespace("pairwiseLLM")
  )
})

test_that("fit_bayes_btl_mcmc does not call legacy scaffold constructors", {
  results <- tibble::tibble(
    pair_uid = c("A:B#1", "A:C#1"),
    unordered_key = c("A:B", "A:C"),
    ordered_key = c("A:B", "A:C"),
    A_id = c("A", "A"),
    B_id = c("B", "C"),
    better_id = c("A", "C"),
    winner_pos = c(1L, 2L),
    phase = c("phase2", "phase2"),
    iter = c(1L, 2L),
    received_at = as.POSIXct(c("2026-01-01 00:00:00", "2026-01-01 00:00:01"), tz = "UTC"),
    backend = c("openai", "openai"),
    model = c("gpt-test", "gpt-test")
  )

  testthat::with_mocked_bindings(
    .fit_bayes_btl_mcmc_adaptive = mock_fit_for_reachability,
    btl_mcmc_state_new = function(...) {
      rlang::abort("legacy constructor reached")
    },
    {
      out <- pairwiseLLM:::fit_bayes_btl_mcmc(
        results = results,
        ids = c("A", "B", "C"),
        model_variant = "btl_e_b"
      )
      expect_true(is.list(out))
      expect_equal(nrow(out$round_log), 1L)
    },
    .env = asNamespace("pairwiseLLM")
  )
})
