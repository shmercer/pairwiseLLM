make_state_for_stopping <- function() {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 10L),
    seed = 1
  )
}

make_fast_fit <- function(ids) {
  draws <- matrix(
    c(2, 1, 0,
      2, 1, 0,
      2, 1, 0,
      2, 1, 0),
    nrow = 4,
    byrow = TRUE
  )
  colnames(draws) <- ids
  list(theta_draws = draws)
}

populate_state_counts <- function(state, n_pairs) {
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

test_that("stopping_check respects CW spacing and updates last_check_at", {
  state <- make_state_for_stopping()
  state <- populate_state_counts(state, 4L)
  state$last_check_at <- 3L
  state$config$CW <- 2L

  fast_fit <- make_fast_fit(state$ids)
  ranking_ids <- state$ids
  candidates <- tibble::tibble(i_id = "A", j_id = "B")
  utilities <- tibble::tibble(utility_raw = 0.2)

  out <- pairwiseLLM:::stopping_check(state, fast_fit, ranking_ids, candidates, utilities)
  expect_false(out$check_performed)
  expect_equal(out$state$last_check_at, 3L)

  state$last_check_at <- 2L
  out2 <- pairwiseLLM:::stopping_check(state, fast_fit, ranking_ids, candidates, utilities)
  expect_true(out2$check_performed)
  expect_equal(out2$state$last_check_at, 4L)
})

test_that("stopping_check updates conditions and two-check confirmation", {
  state <- make_state_for_stopping()
  state$config$CW <- 2L
  state <- populate_state_counts(state, 4L)
  state$last_check_at <- 2L
  state$U0 <- 1
  state$comparisons_scheduled <- state$comparisons_observed

  fast_fit <- make_fast_fit(state$ids)
  ranking_ids <- state$ids
  candidates <- tibble::tibble(i_id = "A", j_id = "B")
  utilities <- tibble::tibble(utility_raw = 0.01)

  out1 <- pairwiseLLM:::stopping_check(state, fast_fit, ranking_ids, candidates, utilities)
  expect_true(out1$condition_A)
  expect_true(out1$condition_B)
  expect_true(out1$state$stop_candidate)
  expect_equal(out1$state$checks_passed_in_row, 1L)

  state2 <- out1$state
  state2 <- populate_state_counts(state2, 6L)
  out2 <- pairwiseLLM:::stopping_check(state2, fast_fit, ranking_ids, candidates, utilities)
  expect_equal(out2$state$checks_passed_in_row, 2L)
})

test_that("stopping_check initializes U0 exactly once", {
  state <- make_state_for_stopping()
  state$config$CW <- 1L
  state <- populate_state_counts(state, 2L)
  state$last_check_at <- 1L
  state$U0 <- NA_real_
  state$comparisons_scheduled <- state$comparisons_observed

  fast_fit <- make_fast_fit(state$ids)
  ranking_ids <- state$ids
  candidates <- tibble::tibble(i_id = "A", j_id = "B")

  utilities1 <- tibble::tibble(utility_raw = 0.5)
  out1 <- pairwiseLLM:::stopping_check(state, fast_fit, ranking_ids, candidates, utilities1)
  expect_equal(out1$state$U0, 0.5)

  state2 <- out1$state
  state2 <- populate_state_counts(state2, 3L)
  utilities2 <- tibble::tibble(utility_raw = 0.2)
  out2 <- pairwiseLLM:::stopping_check(state2, fast_fit, ranking_ids, candidates, utilities2)
  expect_equal(out2$state$U0, 0.5)
})

test_that("compute_Umax warns on empty utilities", {
  expect_warning(pairwiseLLM:::compute_Umax(tibble::tibble()), "U_max")
  expect_equal(suppressWarnings(pairwiseLLM:::compute_Umax(tibble::tibble())), 0)
})

test_that("summarize_theta and summarize_ranks return MCMC summaries", {
  theta_draws <- matrix(
    c(1, 2, 3,
      2, 3, 4),
    nrow = 2,
    byrow = TRUE
  )
  colnames(theta_draws) <- c("A", "B", "C")

  theta_sum <- pairwiseLLM:::summarize_theta(theta_draws)
  expect_true(all(c("ID", "mean", "sd", "median", "q05", "q95", "q025", "q975") %in% names(theta_sum)))
  expect_equal(theta_sum$mean[theta_sum$ID == "A"], 1.5)

  rank_sum <- pairwiseLLM:::summarize_ranks(theta_draws)
  expect_true(all(c("ID", "rank_mean", "rank_median", "rank_sd", "rank_q05", "rank_q95", "rank_q025", "rank_q975") %in% names(rank_sum)))
  expect_equal(rank_sum$rank_mean[rank_sum$ID == "C"], 1)
})

test_that("compute_adjacent_win_probs uses ranking order", {
  theta_draws <- matrix(
    c(2, 1, 0,
      3, 2, 1),
    nrow = 2,
    byrow = TRUE
  )
  colnames(theta_draws) <- c("A", "B", "C")
  ranking_ids <- c("A", "B", "C")

  out <- pairwiseLLM:::compute_adjacent_win_probs(theta_draws, ranking_ids)
  expect_equal(nrow(out), 2L)
  expect_true(all(out$win_prob >= 0))
  expect_true(all(out$win_prob_btl >= 0))
  expect_equal(out$A_id, c("A", "B"))
  expect_equal(out$B_id, c("B", "C"))
})

test_that("fit_bayes_btl_mcmc runs when CmdStan is available", {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    testthat::skip("CmdStanR is not available for MCMC test.")
  }
  cmdstan_path <- tryCatch(cmdstanr::cmdstan_path(), error = function(e) "")
  if (!nzchar(cmdstan_path)) {
    testthat::skip("CmdStan is not installed for MCMC test.")
  }

  results <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2026-01-02 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test"
  )

  fit <- pairwiseLLM:::fit_bayes_btl_mcmc(
    results,
    ids = c("A", "B"),
    cmdstan = list(
      chains = 2,
      iter_warmup = 200,
      iter_sampling = 200,
      seed = 123,
      core_fraction = 0.5
    )
  )
  expect_true(is.matrix(fit$theta_draws))
  expect_equal(colnames(fit$theta_draws), c("A", "B"))
  expect_true(nrow(fit$theta_draws) > 0L)
})
