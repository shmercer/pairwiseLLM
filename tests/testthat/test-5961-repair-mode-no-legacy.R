make_state_with_result <- function() {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples,
    config = list(budget_max = 10L, M1_target = 2L, d1 = 2L)
  )
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    explore_rate = 0,
    batch_size = 1L
  ))
  state$mode <- "adaptive"
  state$phase <- "phase2"
  state$iter <- 0L
  state$config$skip_stop_checks <- TRUE

  A_id <- "A"
  B_id <- "B"
  unordered_key <- pairwiseLLM:::make_unordered_key(A_id, B_id)
  ordered_key <- pairwiseLLM:::make_ordered_key(A_id, B_id)
  pair_uid <- pairwiseLLM:::pair_uid_from_state(state, unordered_key)
  created_at <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")

  pair_row <- tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = A_id,
    B_id = B_id,
    A_text = state$texts[[A_id]],
    B_text = state$texts[[B_id]],
    phase = "phase2",
    iter = 0L,
    created_at = created_at
  )
  state <- pairwiseLLM:::record_exposure(state, A_id, B_id)
  state$history_pairs <- dplyr::bind_rows(state$history_pairs, pair_row)
  state$comparisons_scheduled <- as.integer(nrow(state$history_pairs))

  results_row <- tibble::tibble(
    pair_uid = pair_uid,
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
  state$history_results <- dplyr::bind_rows(state$history_results, results_row)
  state$comparisons_observed <- as.integer(nrow(state$history_results))
  state$new_since_refit <- 1L
  state
}

testthat::test_that("repair mode uses exploration-only selection and skips legacy calls", {
  state <- make_state_with_result()
  ns <- asNamespace("pairwiseLLM")

  fit <- list(
    theta_draws = matrix(0, nrow = 2L, ncol = state$N, dimnames = list(NULL, state$ids)),
    theta_mean = stats::setNames(rep(0, state$N), state$ids),
    epsilon_mean = 0.1,
    diagnostics = list(divergences = 0L, max_rhat = 1.20, min_ess_bulk = 1000)
  )
  candidate_tbl <- tibble::tibble(
    unordered_key = "A:B",
    i = "A",
    j = "B"
  )
  utilities_tbl <- tibble::tibble(
    unordered_key = "A:B",
    i_id = "A",
    j_id = "B",
    p_mean = 0.5,
    utility = 0.4,
    utility_raw = 0.4
  )
  selection_tbl <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = "A:B",
    utility = 0.4,
    utility_raw = 0.4,
    p_mean = 0.5,
    A_id = "A",
    B_id = "B"
  )

  out <- NULL
  testthat::expect_warning(
    {
      out <- testthat::with_mocked_bindings(
        pairwiseLLM:::.adaptive_schedule_next_pairs(state, 1L, adaptive = list(), seed = 1),
        .fit_bayes_btl_mcmc_adaptive = function(...) list(),
        as_v3_fit_contract_from_mcmc = function(...) fit,
        diagnostics_gate = function(...) FALSE,
        generate_candidates = function(...) candidate_tbl,
        compute_pair_utility = function(...) utilities_tbl,
        apply_degree_penalty = function(utility_tbl, state) utility_tbl,
        select_exploitation_pairs = function(...) rlang::abort("exploitation called"),
        .adaptive_schedule_repair_pairs = function(...) {
          rlang::abort("legacy repair scheduler called")
        },
        .adaptive_select_exploration_only = function(state, candidates_with_utility, config, seed = NULL) {
          testthat::expect_equal(config$dup_max_count, 0L)
          selection_tbl
        },
        .package = "pairwiseLLM"
      )
    },
    "Diagnostics gate failed; entering repair mode"
  )

  testthat::expect_equal(out$state$mode, "repair")
  testthat::expect_equal(nrow(out$pairs), 1L)
  testthat::expect_equal(out$state$iter, 1L)
  testthat::expect_equal(out$state$comparisons_scheduled, 2L)
})

testthat::test_that("repair mode stops after bounded retry failures", {
  state <- make_state_with_result()
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    explore_rate = 0,
    batch_size = 1L,
    refit_B = 1L,
    repair_max_cycles = 1L
  ))
  ns <- asNamespace("pairwiseLLM")

  fit <- list(
    theta_draws = matrix(0, nrow = 2L, ncol = state$N, dimnames = list(NULL, state$ids)),
    theta_mean = stats::setNames(rep(0, state$N), state$ids),
    epsilon_mean = 0.1,
    diagnostics = list(divergences = 0L, max_rhat = 1.20, min_ess_bulk = 1000)
  )
  candidate_tbl <- tibble::tibble(
    unordered_key = "A:B",
    i = "A",
    j = "B"
  )
  utilities_tbl <- tibble::tibble(
    unordered_key = "A:B",
    i_id = "A",
    j_id = "B",
    p_mean = 0.5,
    utility = 0.4,
    utility_raw = 0.4
  )

  out1 <- NULL
  testthat::expect_warning(
    {
      out1 <- testthat::with_mocked_bindings(
        pairwiseLLM:::.adaptive_schedule_next_pairs(state, 1L, adaptive = list(), seed = 1),
        .fit_bayes_btl_mcmc_adaptive = function(...) list(),
        as_v3_fit_contract_from_mcmc = function(...) fit,
        diagnostics_gate = function(...) FALSE,
        generate_candidates = function(...) candidate_tbl,
        compute_pair_utility = function(...) utilities_tbl,
        apply_degree_penalty = function(utility_tbl, state) utility_tbl,
        .adaptive_select_exploration_only = function(state, candidates_with_utility, config, seed = NULL) {
          candidates_with_utility[0, , drop = FALSE]
        },
        .package = "pairwiseLLM"
      )
    },
    "Diagnostics gate failed; entering repair mode"
  )
  state <- out1$state
  state$new_since_refit <- 1L

  out2 <- NULL
  testthat::expect_silent(
    {
      out2 <- testthat::with_mocked_bindings(
        pairwiseLLM:::.adaptive_schedule_next_pairs(state, 1L, adaptive = list(), seed = 1),
        .fit_bayes_btl_mcmc_adaptive = function(...) list(),
        as_v3_fit_contract_from_mcmc = function(...) fit,
        diagnostics_gate = function(...) FALSE,
        generate_candidates = function(...) candidate_tbl,
        compute_pair_utility = function(...) utilities_tbl,
        apply_degree_penalty = function(utility_tbl, state) utility_tbl,
        .adaptive_select_exploration_only = function(state, candidates_with_utility, config, seed = NULL) {
          candidates_with_utility[0, , drop = FALSE]
        },
        .package = "pairwiseLLM"
      )
    }
  )

  testthat::expect_equal(out2$state$mode, "stopped")
  testthat::expect_equal(out2$state$stop_reason, "candidate_starvation")
})
