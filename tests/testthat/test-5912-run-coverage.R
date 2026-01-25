testthat::test_that("adaptive_get_refit_fit aborts when fit contract is invalid", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list())
  state$new_since_refit <- 1L

  testthat::with_mocked_bindings(
    testthat::expect_error(
      pairwiseLLM:::.adaptive_get_refit_fit(state, adaptive = list(), batch_size = 1L, seed = 1L),
      "draws"
    ),
    .fit_bayes_btl_mcmc_adaptive = function(...) list(),
    .env = asNamespace("pairwiseLLM")
  )
})

testthat::test_that("adaptive_run_stopping_checks returns when draws are too few", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 2L)
  )
  state$phase <- "phase2"
  state$mode <- "adaptive"
  state$iter <- 1L
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list())

  unordered_key <- pairwiseLLM:::make_unordered_key("A", "B")
  ordered_key <- pairwiseLLM:::make_ordered_key("A", "B")
  pair_uid <- pairwiseLLM:::pair_uid_from_state(state, unordered_key)
  state <- pairwiseLLM:::record_exposure(state, "A", "B")
  state$history_pairs <- tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = "A",
    B_id = "B",
    A_text = state$texts[["A"]],
    B_text = state$texts[["B"]],
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2026-01-20", tz = "UTC")
  )
  state$comparisons_scheduled <- 1L
  state$history_results <- tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = "A",
    B_id = "B",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2026-01-20", tz = "UTC"),
    backend = "mock",
    model = "mock"
  )
  state$comparisons_observed <- 1L

  state$fit <- list(
    theta_draws = matrix(0, nrow = 1L, ncol = state$N, dimnames = list(NULL, state$ids)),
    theta_mean = stats::setNames(rep(0, state$N), state$ids),
    epsilon_mean = 0.1,
    diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000)
  )

  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_run_stopping_checks(
      state,
      adaptive = list(),
      seed = 1L,
      allow_refit = FALSE
    ),
    validate_state = function(x) x,
    .env = asNamespace("pairwiseLLM")
  )
  expect_s3_class(out$state, "adaptive_state")
  expect_identical(out$state$fit$theta_draws, state$fit$theta_draws)
})

testthat::test_that("select_exploration_only validates inputs", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  config <- list(batch_size = -1L)
  candidates <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = "A:B",
    utility = 0.1,
    utility_raw = 0.1,
    p_mean = 0.5
  )

  expect_error(
    pairwiseLLM:::.adaptive_select_exploration_only(state, candidates_with_utility = list(), config = config),
    "candidates_with_utility"
  )
  expect_error(
    pairwiseLLM:::.adaptive_select_exploration_only(state, candidates_with_utility = candidates, config = config),
    "batch_size"
  )
})

testthat::test_that("adaptive_filter_candidates_to_draws respects i_id/j_id columns", {
  candidates <- tibble::tibble(
    i_id = c("A", "B"),
    j_id = c("B", "A")
  )
  theta_draws <- matrix(0, nrow = 2L, ncol = 2L, dimnames = list(NULL, c("A", "B")))
  filtered <- pairwiseLLM:::.adaptive_filter_candidates_to_draws(candidates, theta_draws)
  expect_equal(nrow(filtered), 2L)
})

testthat::test_that("select_batch_by_ladder defaults n_candidates_generated", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  fit <- list(
    theta_draws = matrix(0, nrow = 2L, ncol = state$N, dimnames = list(NULL, state$ids))
  )
  candidates <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = "A:B",
    utility = 0.1,
    utility_raw = 0.1,
    p_mean = 0.5
  )
  config <- pairwiseLLM:::adaptive_v3_config(state$N, list(batch_size = 1L))

  out <- pairwiseLLM:::.adaptive_select_batch_by_ladder(
    state = state,
    fit = fit,
    theta_summary = tibble::tibble(item_id = state$ids, theta_mean = 0, theta_sd = 0),
    config = config,
    candidates_with_utility = candidates,
    n_candidates_generated = NULL,
    seed = 1L,
    exploration_only = FALSE
  )

  expect_equal(out$candidate_stats$n_candidates_generated, 1L)
  expect_equal(out$candidate_stats$n_candidates_after_filters, 1L)
})

testthat::test_that("sample_exploration_pairs uses zero count for unseen keys", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$pair_count <- list()

  candidates <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = "A:B",
    utility = 0.1,
    utility_raw = 0.1,
    p_mean = 0.5
  )
  config <- pairwiseLLM:::adaptive_v3_config(state$N, list())

  withr::local_seed(123)
  picked <- testthat::with_mocked_bindings(
    pairwiseLLM:::sample_exploration_pairs(
      state = state,
      candidates = candidates,
      n_explore = 1L,
      config = config
    ),
    validate_state = function(x) x,
    .env = asNamespace("pairwiseLLM")
  )
  expect_true(is.data.frame(picked))
})

testthat::test_that("schedule_next_pairs stops when stop decision is reached", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 1L, budget_max = 6L)
  )
  state$phase <- "phase2"
  state$mode <- "adaptive"
  state$iter <- 1L

  unordered_key <- pairwiseLLM:::make_unordered_key("A", "B")
  ordered_key <- pairwiseLLM:::make_ordered_key("A", "B")
  pair_uid <- pairwiseLLM:::pair_uid_from_state(state, unordered_key)
  state <- pairwiseLLM:::record_exposure(state, "A", "B")
  state$history_pairs <- tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = "A",
    B_id = "B",
    A_text = state$texts[["A"]],
    B_text = state$texts[["B"]],
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2026-01-20", tz = "UTC")
  )
  state$comparisons_scheduled <- 1L
  state$history_results <- tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = "A",
    B_id = "B",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2026-01-20", tz = "UTC"),
    backend = "mock",
    model = "mock"
  )
  state$comparisons_observed <- 1L

  fit <- list(
    theta_draws = matrix(0, nrow = 2L, ncol = state$N, dimnames = list(NULL, state$ids)),
    theta_mean = stats::setNames(rep(0, state$N), state$ids),
    epsilon_mean = 0.1,
    diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000)
  )
  state$fit <- fit

  v3_config <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(
      checks_passed_target = 1L,
      min_new_pairs_for_check = 1L,
      rank_weak_adj_threshold = 0.01,
      rank_weak_adj_frac_max = 1,
      rank_min_adj_prob = 0,
      U_abs = 1,
      refit_B = 100L
    )
  )
  state$config$v3 <- v3_config

  out <- pairwiseLLM:::.adaptive_schedule_next_pairs(
    state = state,
    target_pairs = 1L,
    adaptive = list(),
    seed = 1L
  )
  expect_equal(nrow(out$pairs), 0L)
  expect_true(identical(out$state$mode, "stopped"))
})
