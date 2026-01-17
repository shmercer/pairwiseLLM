testthat::test_that("adaptive stopping checks record MCMC error on failure", {
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
  state$fast_fit <- list(theta_mean = theta_mean, theta_draws = draws)

  state <- add_results(state, 2L)
  state$config$last_refit_at <- as.integer(state$comparisons_observed)
  out1 <- pairwiseLLM:::.adaptive_run_stopping_checks(
    state,
    adaptive = list(exploration_frac = 0.05)
  )

  state2 <- add_results(out1$state, 3L)
  state2$config$last_refit_at <- as.integer(state2$comparisons_observed)
  state2$last_check_at <- 0L
  state2$config$stop_candidate_at <- 0L
  state2$config$mcmc_attempted_at <- -1L
  state2$config$stop_confirmed <- FALSE
  state2$checks_passed_in_row <- 2L

  mock_mcmc_error <- function(bt_data, config, seed = NULL) {
    rlang::abort("mock cmdstan failure")
  }

  mock_stopping_check <- function(state, fast_fit, ranking_ids, candidates, utilities_tbl) {
    state$stop_candidate <- TRUE
    state$checks_passed_in_row <- 2L
    state$last_check_at <- as.integer(state$comparisons_observed)
    list(
      state = state,
      check_performed = TRUE,
      condition_A = TRUE,
      condition_B = TRUE,
      q_summary = list(median = 0.99, p10 = 0.9),
      U_max = 0.01,
      U_0 = state$U0,
      stop_candidate = TRUE,
      checks_passed_in_row = 2L
    )
  }

  out2 <- testthat::expect_warning(
    testthat::with_mocked_bindings(
      pairwiseLLM:::.adaptive_run_stopping_checks(
        state2,
        adaptive = list(exploration_frac = 0.05)
      ),
      fit_bayes_btl_mcmc_v3 = mock_mcmc_error,
      stopping_check = mock_stopping_check,
      generate_candidates_v3 = function(theta_summary, state, config) {
        i_id <- state$ids[1L]
        j_id <- state$ids[2L]
        tibble::tibble(
          i = i_id,
          j = j_id
        )
      }
    ),
    "MCMC confirmation failed"
  )

  expect_false(isTRUE(out2$state$config$stop_confirmed))
  if (!is.null(out2$state$config$mcmc_error)) {
    expect_match(out2$state$config$mcmc_error, "mock cmdstan failure")
  }
})
