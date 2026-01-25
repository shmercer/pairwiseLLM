testthat::test_that("rank stability metrics use adjacent posterior probabilities", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 10L)
  )
  config_v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(S_subset = 3L, K_top = 2L, U_abs = 0.2)
  )
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE

  draws <- matrix(c(
    3, 2, 1, 0,
    3, 2, 1, 0,
    3, 0, 2, 1
  ), nrow = 3, byrow = TRUE)
  colnames(draws) <- state$ids
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  utilities <- tibble::tibble(utility = 0.1)
  metrics <- pairwiseLLM:::compute_stop_metrics(state, fit, utilities, config_v3)

  testthat::expect_equal(metrics$frac_weak_adj, 1 / 3)
  testthat::expect_equal(metrics$min_adj_prob, 2 / 3)
  testthat::expect_false(metrics$rank_stability_pass)
  testthat::expect_equal(metrics$scheduled_pairs, 0L)
  testthat::expect_equal(metrics$completed_pairs, 0L)
  testthat::expect_equal(metrics$proposed_pairs, 1L)
  testthat::expect_equal(metrics$weak_adj_threshold, config_v3$rank_weak_adj_threshold)
  testthat::expect_equal(metrics$weak_adj_frac_max, config_v3$rank_weak_adj_frac_max)
  testthat::expect_equal(metrics$min_adj_prob_threshold, config_v3$rank_min_adj_prob)

  strong_draws <- matrix(c(
    4, 3, 2, 1,
    4, 3, 2, 1,
    4, 3, 2, 1
  ), nrow = 3, byrow = TRUE)
  colnames(strong_draws) <- state$ids
  strong_fit <- make_v3_fit_contract(state$ids, theta_draws = strong_draws)

  strong_metrics <- pairwiseLLM:::compute_stop_metrics(state, strong_fit, utilities, config_v3)
  testthat::expect_true(strong_metrics$rank_stability_pass)
})

testthat::test_that("weak boundary tolerance allows exactly five percent", {
  n_items <- 21L
  ids <- paste0("I", seq_len(n_items))
  samples <- tibble::tibble(
    ID = ids,
    text = paste0("text", seq_len(n_items))
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 300L)
  )
  config_v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(U_abs = 0.2))
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE

  n_draws <- 10L
  base_means <- rev(seq_len(n_items))
  draws <- matrix(rep(base_means, each = n_draws), nrow = n_draws)
  colnames(draws) <- ids

  draws[1, "I10"] <- base_means[10] - 3
  draws[1, "I11"] <- base_means[11] + 3

  fit_pass <- make_v3_fit_contract(ids, theta_draws = draws)
  metrics_pass <- pairwiseLLM:::compute_stop_metrics(
    state,
    fit_pass,
    tibble::tibble(utility = 0.1),
    config_v3
  )
  testthat::expect_equal(metrics_pass$frac_weak_adj, 1 / 20)
  testthat::expect_true(metrics_pass$rank_stability_pass)

  draws[1, "I12"] <- base_means[12] - 3
  draws[1, "I13"] <- base_means[13] + 3
  fit_fail <- make_v3_fit_contract(ids, theta_draws = draws)
  metrics_fail <- pairwiseLLM:::compute_stop_metrics(
    state,
    fit_fail,
    tibble::tibble(utility = 0.1),
    config_v3
  )
  testthat::expect_equal(metrics_fail$frac_weak_adj, 2 / 20)
  testthat::expect_false(metrics_fail$rank_stability_pass)
})

testthat::test_that("stop checks do not advance without new information", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 0L, budget_max = 6L)
  )
  A_id <- state$ids[[1L]]
  B_id <- state$ids[[2L]]
  unordered_key <- pairwiseLLM:::make_unordered_key(A_id, B_id)
  ordered_key <- pairwiseLLM:::make_ordered_key(A_id, B_id)
  created_at <- as.POSIXct("2024-01-01", tz = "UTC")
  A_text <- state$texts[[A_id]]
  B_text <- state$texts[[B_id]]

  pair_rows <- tibble::tibble(
    pair_uid = c(paste0(unordered_key, "#1"), paste0(unordered_key, "#2")),
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = A_id,
    B_id = B_id,
    A_text = rep(A_text, 2L),
    B_text = rep(B_text, 2L),
    phase = "phase2",
    iter = 0L,
    created_at = created_at
  )
  result_rows <- tibble::tibble(
    pair_uid = pair_rows$pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = A_id,
    B_id = B_id,
    better_id = A_id,
    winner_pos = 1L,
    phase = "phase2",
    iter = 0L,
    received_at = created_at,
    backend = "test",
    model = "test"
  )

  state$history_pairs <- pair_rows
  state$history_results <- result_rows
  state$comparisons_scheduled <- as.integer(nrow(pair_rows))
  state$comparisons_observed <- as.integer(nrow(result_rows))
  state$last_check_at <- 1L
  state$checks_passed_in_row <- 1L

  config_v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    checks_passed_target = 3L,
    min_new_pairs_for_check = 3L
  ))
  metrics <- list(
    hard_cap_reached = FALSE,
    diagnostics_pass = TRUE,
    theta_sd_pass = TRUE,
    U_pass = TRUE,
    rank_stability_pass = TRUE,
    U0 = 0.1,
    refit_performed = FALSE
  )

  out <- pairwiseLLM:::should_stop(metrics, state, config_v3)
  testthat::expect_false(out$stop_decision)
  testthat::expect_equal(out$state$checks_passed_in_row, 1L)
  testthat::expect_equal(out$state$last_check_at, 1L)
  testthat::expect_false(identical(out$state$stop_reason, "v3_converged"))

  state_next <- out$state
  extra_pairs <- pair_rows
  extra_pairs$pair_uid <- c(paste0(unordered_key, "#3"), paste0(unordered_key, "#4"))
  extra_results <- result_rows
  extra_results$pair_uid <- extra_pairs$pair_uid
  state_next$history_pairs <- dplyr::bind_rows(state_next$history_pairs, extra_pairs)
  state_next$history_results <- dplyr::bind_rows(state_next$history_results, extra_results)
  state_next$comparisons_scheduled <- as.integer(nrow(state_next$history_pairs))
  state_next$comparisons_observed <- as.integer(nrow(state_next$history_results))
  out_next <- pairwiseLLM:::should_stop(metrics, state_next, config_v3)
  testthat::expect_false(out_next$stop_decision)
  testthat::expect_equal(out_next$state$checks_passed_in_row, 2L)
  testthat::expect_equal(out_next$state$last_check_at, 4L)
})
