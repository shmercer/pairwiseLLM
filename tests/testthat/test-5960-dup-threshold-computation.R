testthat::test_that("U_dup_threshold uses quantile for >= 50 candidates", {
  samples <- tibble::tibble(
    ID = LETTERS[1:12],
    text = paste0("text-", seq_len(12))
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$mode <- "adaptive"
  state$phase <- "phase2"
  state$iter <- 0L
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    explore_rate = 0,
    batch_size = 1L
  ))
  state$config$skip_stop_checks <- TRUE

  candidate_keys <- head(pairwiseLLM:::.adaptive_unordered_keys(state$ids), 60)
  candidate_tbl <- tibble::tibble(
    unordered_key = candidate_keys,
    i = sub(":.*", "", candidate_keys),
    j = sub(".*:", "", candidate_keys)
  )
  U_all <- seq(0.01, 0.60, length.out = length(candidate_keys))
  utilities_tbl <- tibble::tibble(
    unordered_key = candidate_keys,
    i_id = candidate_tbl$i,
    j_id = candidate_tbl$j,
    p_mean = rep(0.5, length(candidate_keys)),
    utility = U_all,
    utility_raw = U_all
  )

  dup_key <- candidate_keys[[1L]]
  state$pair_count[[dup_key]] <- 1L
  state$history_pairs <- tibble::tibble(
    pair_uid = paste0(dup_key, "#1"),
    unordered_key = dup_key,
    ordered_key = dup_key,
    A_id = candidate_tbl$i[[1L]],
    B_id = candidate_tbl$j[[1L]],
    A_text = state$texts[[candidate_tbl$i[[1L]]]],
    B_text = state$texts[[candidate_tbl$j[[1L]]]],
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )
  state$comparisons_scheduled <- 1L

  fit <- make_v3_fit_contract(
    state$ids,
    theta_draws = matrix(0, nrow = 2L, ncol = state$N, dimnames = list(NULL, state$ids)),
    diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000)
  )

  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_schedule_next_pairs(state, 1L, adaptive = list(), seed = 1),
    .adaptive_get_refit_fit = function(state, adaptive, batch_size, seed, allow_refit = TRUE) {
      list(state = state, fit = fit, refit_performed = TRUE)
    },
    generate_candidates = function(theta_summary, state, config, allow_repeats = FALSE) candidate_tbl,
    compute_pair_utility = function(draws, candidates, epsilon_mean) utilities_tbl,
    apply_degree_penalty = function(utility_tbl, state) utility_tbl
  )

  expected_threshold <- stats::quantile(U_all, 0.90, type = 7, na.rm = TRUE)[[1L]]
  testthat::expect_equal(out$state$posterior$U_dup_threshold, expected_threshold)
  testthat::expect_equal(nrow(out$pairs), 1L)
  testthat::expect_false(out$pairs$unordered_key[[1L]] == dup_key)
})

testthat::test_that("U_dup_threshold uses max for < 50 candidates", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste0("text-", seq_len(6))
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$mode <- "adaptive"
  state$phase <- "phase2"
  state$iter <- 0L
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(
    explore_rate = 0,
    batch_size = 1L
  ))
  state$config$skip_stop_checks <- TRUE

  candidate_keys <- head(pairwiseLLM:::.adaptive_unordered_keys(state$ids), 10)
  candidate_tbl <- tibble::tibble(
    unordered_key = candidate_keys,
    i = sub(":.*", "", candidate_keys),
    j = sub(".*:", "", candidate_keys)
  )
  U_all <- seq(0.1, 0.9, length.out = length(candidate_keys))
  U_all[[1L]] <- 0.95
  utilities_tbl <- tibble::tibble(
    unordered_key = candidate_keys,
    i_id = candidate_tbl$i,
    j_id = candidate_tbl$j,
    p_mean = rep(0.5, length(candidate_keys)),
    utility = U_all,
    utility_raw = U_all
  )

  dup_key <- candidate_keys[[1L]]
  state$pair_count[[dup_key]] <- 1L
  state$history_pairs <- tibble::tibble(
    pair_uid = paste0(dup_key, "#1"),
    unordered_key = dup_key,
    ordered_key = dup_key,
    A_id = candidate_tbl$i[[1L]],
    B_id = candidate_tbl$j[[1L]],
    A_text = state$texts[[candidate_tbl$i[[1L]]]],
    B_text = state$texts[[candidate_tbl$j[[1L]]]],
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )
  state$comparisons_scheduled <- 1L

  fit <- make_v3_fit_contract(
    state$ids,
    theta_draws = matrix(0, nrow = 2L, ncol = state$N, dimnames = list(NULL, state$ids)),
    diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000)
  )

  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_schedule_next_pairs(state, 1L, adaptive = list(), seed = 1),
    .adaptive_get_refit_fit = function(state, adaptive, batch_size, seed, allow_refit = TRUE) {
      list(state = state, fit = fit, refit_performed = TRUE)
    },
    generate_candidates = function(theta_summary, state, config, allow_repeats = FALSE) candidate_tbl,
    compute_pair_utility = function(draws, candidates, epsilon_mean) utilities_tbl,
    apply_degree_penalty = function(utility_tbl, state) utility_tbl
  )

  expected_threshold <- max(U_all, na.rm = TRUE)
  testthat::expect_equal(out$state$posterior$U_dup_threshold, expected_threshold)
  testthat::expect_equal(nrow(out$pairs), 1L)
  testthat::expect_equal(out$pairs$unordered_key[[1L]], dup_key)
})
