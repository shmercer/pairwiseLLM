testthat::test_that("batch selection fills when feasible", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D", "E"),
    text = c("alpha", "bravo", "charlie", "delta", "echo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  config$batch_size <- 3L
  config$explore_rate <- 0

  theta_draws <- matrix(seq_len(2L * state$N), nrow = 2L, ncol = state$N)
  colnames(theta_draws) <- state$ids
  fit <- list(
    theta_draws = theta_draws,
    theta_mean = stats::setNames(colMeans(theta_draws), state$ids)
  )
  theta_summary <- pairwiseLLM:::.adaptive_theta_summary_from_fit(fit, state)
  candidates <- pairwiseLLM:::generate_candidates(theta_summary, state, config)
  epsilon_mean <- pairwiseLLM:::.adaptive_epsilon_mean_from_state(state, fit)
  utilities <- pairwiseLLM:::compute_pair_utility(fit$theta_draws, candidates, epsilon_mean)
  utilities <- pairwiseLLM:::apply_degree_penalty(utilities, state)

  selection_out <- pairwiseLLM:::.adaptive_select_batch_with_fallbacks(
    state = state,
    fit = fit,
    theta_summary = theta_summary,
    config = config,
    candidates_with_utility = utilities,
    n_candidates_generated = nrow(candidates),
    seed = NULL,
    exploration_only = FALSE
  )

  testthat::expect_equal(nrow(selection_out$selection), config$batch_size)
  testthat::expect_false(selection_out$candidate_starved)
  testthat::expect_identical(selection_out$fallback_stage, "base")
})

testthat::test_that("candidate starvation is flagged deterministically", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  config$batch_size <- 2L
  config$explore_rate <- 0

  pair_ids <- utils::combn(state$ids, 2)
  keys <- pairwiseLLM:::make_unordered_key(pair_ids[1L, ], pair_ids[2L, ])
  state$pair_count <- stats::setNames(rep.int(config$dup_max_count, length(keys)), keys)

  theta_draws <- matrix(seq_len(2L * state$N), nrow = 2L, ncol = state$N)
  colnames(theta_draws) <- state$ids
  fit <- list(
    theta_draws = theta_draws,
    theta_mean = stats::setNames(colMeans(theta_draws), state$ids)
  )
  theta_summary <- pairwiseLLM:::.adaptive_theta_summary_from_fit(fit, state)
  candidates <- pairwiseLLM:::generate_candidates(theta_summary, state, config)
  epsilon_mean <- pairwiseLLM:::.adaptive_epsilon_mean_from_state(state, fit)
  utilities <- pairwiseLLM:::compute_pair_utility(fit$theta_draws, candidates, epsilon_mean)
  utilities <- pairwiseLLM:::apply_degree_penalty(utilities, state)

  out1 <- pairwiseLLM:::.adaptive_select_batch_with_fallbacks(
    state = state,
    fit = fit,
    theta_summary = theta_summary,
    config = config,
    candidates_with_utility = utilities,
    n_candidates_generated = nrow(candidates),
    seed = 101,
    exploration_only = FALSE
  )
  out2 <- pairwiseLLM:::.adaptive_select_batch_with_fallbacks(
    state = state,
    fit = fit,
    theta_summary = theta_summary,
    config = config,
    candidates_with_utility = utilities,
    n_candidates_generated = nrow(candidates),
    seed = 101,
    exploration_only = FALSE
  )

  testthat::expect_true(out1$candidate_starved)
  testthat::expect_equal(nrow(out1$selection), 0L)
  testthat::expect_identical(out1$candidate_starved, out2$candidate_starved)
  testthat::expect_equal(nrow(out2$selection), nrow(out1$selection))
})

testthat::test_that("tiny batches require a starvation flag", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  config$batch_size <- 2L
  config$explore_rate <- 0

  pair_ids <- utils::combn(state$ids, 2)
  keys <- pairwiseLLM:::make_unordered_key(pair_ids[1L, ], pair_ids[2L, ])
  counts <- stats::setNames(rep.int(config$dup_max_count, length(keys)), keys)
  counts[["B:C"]] <- 0L
  state$pair_count <- counts

  theta_draws <- matrix(seq_len(2L * state$N), nrow = 2L, ncol = state$N)
  colnames(theta_draws) <- state$ids
  fit <- list(
    theta_draws = theta_draws,
    theta_mean = stats::setNames(colMeans(theta_draws), state$ids)
  )
  theta_summary <- pairwiseLLM:::.adaptive_theta_summary_from_fit(fit, state)
  candidates <- pairwiseLLM:::generate_candidates(theta_summary, state, config)
  epsilon_mean <- pairwiseLLM:::.adaptive_epsilon_mean_from_state(state, fit)
  utilities <- pairwiseLLM:::compute_pair_utility(fit$theta_draws, candidates, epsilon_mean)
  utilities <- pairwiseLLM:::apply_degree_penalty(utilities, state)

  selection_out <- pairwiseLLM:::.adaptive_select_batch_with_fallbacks(
    state = state,
    fit = fit,
    theta_summary = theta_summary,
    config = config,
    candidates_with_utility = utilities,
    n_candidates_generated = nrow(candidates),
    seed = NULL,
    exploration_only = FALSE
  )

  testthat::expect_equal(nrow(selection_out$selection), 1L)
  testthat::expect_true(selection_out$candidate_starved)
  testthat::expect_equal(selection_out$candidate_stats$n_pairs_requested, 2L)
})

testthat::test_that("fallbacks use relax constraints when anchors are blocked", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  config$batch_size <- 1L
  config$explore_rate <- 0
  config$A_anchors <- 1L
  config$W <- 1L

  pair_ids <- utils::combn(state$ids, 2)
  keys <- pairwiseLLM:::make_unordered_key(pair_ids[1L, ], pair_ids[2L, ])
  counts <- stats::setNames(rep.int(0L, length(keys)), keys)
  counts[names(counts) %in% c("A:B", "A:C", "A:D")] <- config$dup_max_count
  state$pair_count <- counts

  theta_draws <- matrix(
    c(4, 3, 2, 1,
      4, 3, 2, 1),
    nrow = 2L,
    byrow = TRUE
  )
  colnames(theta_draws) <- state$ids
  fit <- list(
    theta_draws = theta_draws,
    theta_mean = stats::setNames(colMeans(theta_draws), state$ids)
  )
  theta_summary <- pairwiseLLM:::.adaptive_theta_summary_from_fit(fit, state)

  base_utilities <- tibble::tibble(
    unordered_key = character(),
    i_id = character(),
    j_id = character(),
    i = character(),
    j = character(),
    mean_d = double(),
    var_d = double(),
    p_mean = double(),
    utility = double(),
    utility_raw = double()
  )

  out <- pairwiseLLM:::.adaptive_select_batch_with_fallbacks(
    state = state,
    fit = fit,
    theta_summary = theta_summary,
    config = config,
    candidates_with_utility = base_utilities,
    n_candidates_generated = 0L,
    seed = 1L,
    exploration_only = FALSE
  )

  testthat::expect_identical(out$fallback_stage, "relax_constraints")
  testthat::expect_equal(nrow(out$selection), config$batch_size)
})

testthat::test_that("fallbacks can complete at broadened window", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  config$batch_size <- 2L
  config$explore_rate <- 0
  config$W <- 1L

  theta_draws <- matrix(seq_len(2L * state$N), nrow = 2L, ncol = state$N)
  colnames(theta_draws) <- state$ids
  fit <- list(
    theta_draws = theta_draws,
    theta_mean = stats::setNames(colMeans(theta_draws), state$ids)
  )
  theta_summary <- pairwiseLLM:::.adaptive_theta_summary_from_fit(fit, state)

  base_utilities <- tibble::tibble(
    unordered_key = character(),
    i_id = character(),
    j_id = character(),
    i = character(),
    j = character(),
    mean_d = double(),
    var_d = double(),
    p_mean = double(),
    utility = double(),
    utility_raw = double()
  )

  out <- pairwiseLLM:::.adaptive_select_batch_with_fallbacks(
    state = state,
    fit = fit,
    theta_summary = theta_summary,
    config = config,
    candidates_with_utility = base_utilities,
    n_candidates_generated = 0L,
    seed = 1L,
    exploration_only = FALSE
  )

  testthat::expect_identical(out$fallback_stage, "broaden_window")
  testthat::expect_equal(nrow(out$selection), config$batch_size)
})

testthat::test_that("select_batch_with_fallbacks validates inputs", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)

  theta_draws <- matrix(seq_len(2L * state$N), nrow = 2L, ncol = state$N)
  colnames(theta_draws) <- state$ids
  fit <- list(theta_draws = theta_draws)
  utilities <- tibble::tibble(
    unordered_key = "A:B",
    i_id = "A",
    j_id = "B",
    utility = 0.1,
    utility_raw = 0.1,
    p_mean = 0.5
  )

  config_bad <- config
  config_bad$batch_size <- -1L
  testthat::expect_error(
    pairwiseLLM:::.adaptive_select_batch_with_fallbacks(
      state = state,
      fit = "bad",
      theta_summary = tibble::tibble(),
      config = config,
      candidates_with_utility = utilities
    ),
    "theta_draws"
  )
  testthat::expect_error(
    pairwiseLLM:::.adaptive_select_batch_with_fallbacks(
      state = state,
      fit = fit,
      theta_summary = tibble::tibble(),
      config = config,
      candidates_with_utility = "bad"
    ),
    "data frame"
  )
  testthat::expect_error(
    pairwiseLLM:::.adaptive_select_batch_with_fallbacks(
      state = state,
      fit = fit,
      theta_summary = tibble::tibble(),
      config = config_bad,
      candidates_with_utility = utilities
    ),
    "batch_size"
  )
  testthat::expect_error(
    pairwiseLLM:::.adaptive_select_batch_with_fallbacks(
      state = state,
      fit = fit,
      theta_summary = tibble::tibble(),
      config = config,
      candidates_with_utility = utilities,
      n_candidates_generated = -1L
    ),
    "n_candidates_generated"
  )
})
