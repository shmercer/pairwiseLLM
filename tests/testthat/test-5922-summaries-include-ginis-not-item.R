test_that("summaries exclude gini fields in outputs", {
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
    config = list(batch_size = 2L, W = 2L, explore_rate = 0.2)
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

  item_log <- tibble::tibble(
    ID = state$ids,
    theta_mean = c(0.2, -0.1, 0.0),
    theta_sd = c(0.1, 0.2, 0.3),
    theta_p2.5 = c(-0.2, -0.3, -0.4),
    theta_p5 = c(-0.1, -0.2, -0.3),
    theta_p50 = c(0.1, -0.05, 0.0),
    theta_p95 = c(0.3, 0.2, 0.1),
    theta_p97.5 = c(0.4, 0.3, 0.2),
    rank_mean = c(1.0, 2.0, 3.0),
    rank_p2.5 = c(1.0, 1.8, 2.7),
    rank_p5 = c(1.0, 1.9, 2.8),
    rank_p50 = c(1.0, 2.0, 3.0),
    rank_p95 = c(1.2, 2.1, 3.2),
    rank_p97.5 = c(1.3, 2.2, 3.3),
    rank_sd = c(0.1, 0.2, 0.3),
    deg = c(1L, 2L, 3L),
    posA_prop = c(1.0, 0.5, 0.0)
  )
  state$logs <- list(
    item_log_list = list(
      dplyr::relocate(dplyr::mutate(item_log, refit_id = 1L), refit_id, .before = 1L)
    )
  )

  iter_summary <- pairwiseLLM::summarize_iterations(state)
  refit_summary <- pairwiseLLM::summarize_refits(state)
  item_log <- pairwiseLLM::summarize_items(state, include_optional = FALSE)

  expect_false(any(c("gini_degree", "gini_pos_A") %in% names(iter_summary)))
  expect_false(any(c("gini_degree", "gini_pos_A") %in% names(refit_summary)))
  expect_false(any(c("gini_degree", "gini_pos_A") %in% names(item_log)))
})
