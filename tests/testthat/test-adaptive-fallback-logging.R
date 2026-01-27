testthat::test_that("fallback logging uses best stage when ladder is exhausted", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  config$batch_size <- 2L
  config$explore_rate <- 0

  pair_ids <- utils::combn(state$ids, 2)
  keys <- pairwiseLLM:::make_unordered_key(pair_ids[1L, ], pair_ids[2L, ])
  counts <- stats::setNames(rep.int(2L, length(keys)), keys)
  counts[["A:B"]] <- 0L
  state$pair_count <- counts

  withr::local_seed(123)
  theta_draws <- matrix(seq_len(2L * state$N), nrow = 2L, ncol = state$N)
  colnames(theta_draws) <- state$ids
  fit <- make_v3_fit_contract(state$ids, theta_draws = theta_draws)
  theta_summary <- pairwiseLLM:::.adaptive_theta_summary_from_fit(fit, state)
  candidates <- pairwiseLLM:::generate_candidates(theta_summary, state, config)
  epsilon_mean <- pairwiseLLM:::.adaptive_epsilon_mean_from_state(state, fit)
  utilities <- pairwiseLLM:::compute_pair_utility(fit$theta_draws, candidates, epsilon_mean)
  utilities <- pairwiseLLM:::apply_degree_penalty(utilities, state)

  out <- pairwiseLLM:::.adaptive_select_batch_by_ladder(
    state = state,
    fit = fit,
    theta_summary = theta_summary,
    config = config,
    candidates_with_utility = utilities,
    n_candidates_generated = nrow(candidates),
    seed = 123,
    exploration_only = FALSE
  )

  testthat::expect_true(out$candidate_starved)
  testthat::expect_true(out$fallback_exhausted)
  testthat::expect_equal(nrow(out$selection), 1L)
  testthat::expect_identical(out$fallback_used, "base_window")
  testthat::expect_identical(out$fallback_stage, "base_window")
  testthat::expect_identical(
    out$fallback_path,
    c(
      "base_window",
      "expand_2x",
      "expand_4x",
      "uncertainty_pool",
      "dup_relax",
      "global_safe"
    )
  )
})
