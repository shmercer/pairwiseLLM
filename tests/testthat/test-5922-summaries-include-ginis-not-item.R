test_that("summaries include gini fields alongside reliability_EAP", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 10L)
  )
  state <- pairwiseLLM:::record_exposure(state, "A", "B")

  theta_draws <- matrix(
    c(0.1, 0.2, 0.3, 0.2, 0.1, 0.4),
    nrow = 2L,
    ncol = 3L,
    byrow = TRUE,
    dimnames = list(NULL, state$ids)
  )
  round_row <- pairwiseLLM:::build_round_log_row(
    state = state,
    fit = list(theta_draws = theta_draws),
    config = list(batch_size = 2L, W = 2L, explore_rate = 0.2, U_abs = 0.1)
  )
  state$config$round_log <- round_row

  state$batch_log <- tibble::tibble(
    iter = 1L,
    phase = "phase2",
    mode = "adaptive",
    created_at = as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
    batch_size_target = 2L,
    n_pairs_selected = 2L,
    n_pairs_completed = 2L,
    candidate_starved = FALSE,
    reason_short_batch = NA_character_,
    n_explore_selected = 1L,
    n_exploit_selected = 1L,
    reliability_EAP = 0.7
  )

  iter_summary <- pairwiseLLM::summarize_iterations(state)
  refit_summary <- pairwiseLLM::summarize_refits(state)
  item_summary <- pairwiseLLM::summarize_items(state, include_optional = FALSE)

  expect_true(all(c("gini_degree", "gini_pos_A") %in% names(iter_summary)))
  expect_true(all(c("gini_degree", "gini_pos_A") %in% names(refit_summary)))
  expect_false(any(c("gini_degree", "gini_pos_A") %in% names(item_summary)))
})
