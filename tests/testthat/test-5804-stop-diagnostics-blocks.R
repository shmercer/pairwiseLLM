make_state_with_result <- function() {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 10L)
  )
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    batch_size = 1L,
    explore_rate = 0
  ))
  state$mode <- "adaptive"
  state$phase <- "phase2"
  state$iter <- 0L

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

testthat::test_that("diagnostics failure blocks convergence stopping and enters repair", {
  withr::local_seed(123)
  state <- make_state_with_result()
  ns <- asNamespace("pairwiseLLM")

  fit <- list(
    theta_draws = matrix(0, nrow = 2L, ncol = state$N, dimnames = list(NULL, state$ids)),
    theta_mean = stats::setNames(rep(0, state$N), state$ids),
    epsilon_mean = 0.1,
    diagnostics = list(divergences = 0L, max_rhat = 1.20, min_ess_bulk = 1000)
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

  testthat::local_mocked_bindings(
    .fit_bayes_btl_mcmc_adaptive = function(...) list(),
    as_v3_fit_contract_from_mcmc = function(...) fit,
    generate_candidates = function(...) tibble::tibble(i = "A", j = "B"),
    compute_pair_utility = function(...) utilities_tbl,
    apply_degree_penalty = function(utility_tbl, state) utility_tbl,
    .adaptive_select_exploration_only = function(...) selection_tbl,
    .env = ns
  )

  out <- NULL
  testthat::expect_warning(
    {
      out <- pairwiseLLM:::.adaptive_schedule_next_pairs(state, 1L, adaptive = list(), seed = 1)
    },
    "Diagnostics gate failed; entering repair mode"
  )

  testthat::expect_equal(out$state$mode, "repair")
  testthat::expect_false(identical(out$state$stop_reason, "v3_converged"))
})

testthat::test_that("diagnostics pass does not block convergence checks", {
  state <- make_state_with_result()
  ns <- asNamespace("pairwiseLLM")
  called <- FALSE

  fit <- list(
    theta_draws = matrix(0, nrow = 2L, ncol = state$N, dimnames = list(NULL, state$ids)),
    theta_mean = stats::setNames(rep(0, state$N), state$ids),
    epsilon_mean = 0.1,
    diagnostics = list(divergences = 0L, max_rhat = 1.00, min_ess_bulk = 1000)
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
    A_id = "A",
    B_id = "B",
    unordered_key = "A:B",
    utility = 0.4,
    utility_raw = 0.4,
    p_mean = 0.5
  )

  testthat::local_mocked_bindings(
    .fit_bayes_btl_mcmc_adaptive = function(...) list(),
    as_v3_fit_contract_from_mcmc = function(...) fit,
    generate_candidates = function(...) tibble::tibble(i = "A", j = "B"),
    compute_pair_utility = function(...) utilities_tbl,
    apply_degree_penalty = function(utility_tbl, state) utility_tbl,
    should_stop = function(metrics, state, config) {
      testthat::expect_true(isTRUE(metrics$diagnostics_pass))
      called <<- TRUE
      list(state = state, stop_decision = FALSE, stop_reason = NA_character_)
    },
    select_batch = function(...) selection_tbl,
    .env = ns
  )

  out <- pairwiseLLM:::.adaptive_schedule_next_pairs(state, 1L, adaptive = list(), seed = 1)
  testthat::expect_true(called)
  testthat::expect_equal(out$state$mode, "adaptive")
})
