testthat::test_that("adaptive stopping checks do not stop on minimal state", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )

  add_results <- function(state, n_pairs) {
    ids <- state$ids
    A_id <- rep(ids[1], n_pairs)
    B_id <- rep(ids[2], n_pairs)
    unordered_key <- pairwiseLLM:::make_unordered_key(A_id, B_id)
    ordered_key <- pairwiseLLM:::make_ordered_key(A_id, B_id)
    pair_uid <- paste0(unordered_key, "#", seq_len(n_pairs))

    state$history_pairs <- tibble::tibble(
      pair_uid = pair_uid,
      unordered_key = unordered_key,
      ordered_key = ordered_key,
      A_id = A_id,
      B_id = B_id,
      A_text = rep(state$texts[[ids[1]]], n_pairs),
      B_text = rep(state$texts[[ids[2]]], n_pairs),
      phase = "phase2",
      iter = 1L,
      created_at = as.POSIXct(rep("2026-01-01 00:00:00", n_pairs), tz = "UTC")
    )

    state$history_results <- tibble::tibble(
      pair_uid = pair_uid,
      unordered_key = unordered_key,
      ordered_key = ordered_key,
      A_id = A_id,
      B_id = B_id,
      better_id = A_id,
      winner_pos = 1L,
      phase = "phase2",
      iter = 1L,
      received_at = as.POSIXct(rep("2026-01-02 00:00:00", n_pairs), tz = "UTC"),
      backend = "openai",
      model = "gpt-test"
    )

    state$comparisons_scheduled <- as.integer(n_pairs)
    state$comparisons_observed <- as.integer(n_pairs)
    state
  }

  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 1L, M1_target = 1L, budget_max = 10L),
    seed = 1
  )
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N)
  state$phase <- "phase2"
  state$config$CW <- 1L
  state$U0 <- 1

  theta_mean <- stats::setNames(c(2, 1, 0), state$ids)
  draws <- matrix(rep(theta_mean, each = 4), nrow = 4, byrow = FALSE)
  colnames(draws) <- state$ids
  state$fit <- list(
    theta_mean = theta_mean,
    theta_draws = draws,
    epsilon_mean = 0.1,
    diagnostics = list()
  )
  state$config$allow_refit <- FALSE

  state <- add_results(state, 2L)
  state$config$last_refit_at <- as.integer(state$comparisons_observed)
  out <- pairwiseLLM:::.adaptive_run_stopping_checks(
    state,
    adaptive = list(exploration_frac = 0.05)
  )

  expect_false(identical(out$state$mode, "stopped"))
  expect_true(is.na(out$state$stop_reason))
})
