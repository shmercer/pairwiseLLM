testthat::test_that("compute_stop_metrics reports theta_sd_eap and counts", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 12L)
  )
  config_v3 <- pairwiseLLM:::adaptive_v3_config(state$N)
  state$config$v3 <- config_v3
  state$posterior$diagnostics_pass <- TRUE

  pair_rows <- tibble::tibble(
    pair_uid = paste0("A:B#", 1:5),
    unordered_key = rep("A:B", 5L),
    ordered_key = rep("A:B", 5L),
    A_id = rep("A", 5L),
    B_id = rep("B", 5L),
    A_text = rep(state$texts[["A"]], 5L),
    B_text = rep(state$texts[["B"]], 5L),
    phase = rep("phase2", 5L),
    iter = rep(1L, 5L),
    created_at = as.POSIXct(rep("2026-01-01 00:00:00", 5L), tz = "UTC")
  )
  result_rows <- tibble::tibble(
    pair_uid = pair_rows$pair_uid[1:3],
    unordered_key = rep("A:B", 3L),
    ordered_key = rep("A:B", 3L),
    A_id = rep("A", 3L),
    B_id = rep("B", 3L),
    better_id = rep("A", 3L),
    winner_pos = rep(1L, 3L),
    phase = rep("phase2", 3L),
    iter = rep(1L, 3L),
    received_at = as.POSIXct(rep("2026-01-01 00:00:00", 3L), tz = "UTC"),
    backend = rep("test", 3L),
    model = rep("test", 3L)
  )
  state$history_pairs <- pair_rows
  state$history_results <- result_rows
  state$comparisons_scheduled <- as.integer(nrow(pair_rows))
  state$comparisons_observed <- as.integer(nrow(result_rows))
  state$last_refit_at <- 0L
  state$new_since_refit <- as.integer(state$comparisons_observed)

  draws <- matrix(
    c(1, 0, -1,
      1.2, 0.2, -0.8),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(NULL, state$ids)
  )
  fit <- make_v3_fit_contract(state$ids, theta_draws = draws)

  metrics <- pairwiseLLM:::compute_stop_metrics(
    state,
    fit,
    tibble::tibble(),
    config_v3
  )

  testthat::expect_true(is.finite(metrics$theta_sd_eap))
  testthat::expect_equal(metrics$scheduled_pairs, 5L)
  testthat::expect_equal(metrics$completed_pairs, 3L)
})
