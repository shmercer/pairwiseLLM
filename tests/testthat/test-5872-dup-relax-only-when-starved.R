testthat::test_that("dup_relax is skipped when earlier stage fills", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  config$batch_size <- 1L
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

  out <- pairwiseLLM:::.adaptive_select_batch_by_ladder(
    state = state,
    fit = fit,
    theta_summary = theta_summary,
    config = config,
    candidates_with_utility = utilities,
    n_candidates_generated = nrow(candidates),
    seed = NULL,
    exploration_only = FALSE
  )

  testthat::expect_identical(out$fallback_used, "base_window")
  testthat::expect_false("dup_relax" %in% out$fallback_path)
  attempt_tbl <- dplyr::bind_rows(out$stage_attempts)
  testthat::expect_true(all(attempt_tbl$dup_policy == "default"))
})

testthat::test_that("dup_relax is used only after starvation", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  config$batch_size <- 1L
  config$explore_rate <- 0

  pair_ids <- utils::combn(state$ids, 2)
  keys <- pairwiseLLM:::make_unordered_key(pair_ids[1L, ], pair_ids[2L, ])
  state$pair_count <- stats::setNames(rep.int(config$dup_max_count, length(keys)), keys)

  history_row <- tibble::tibble(
    pair_uid = "p1",
    unordered_key = "A:B",
    ordered_key = pairwiseLLM:::make_ordered_key("A", "B"),
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "bravo",
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2020-01-01", tz = "UTC")
  )
  state$history_pairs <- dplyr::bind_rows(state$history_pairs, history_row)
  state$comparisons_scheduled <- as.integer(nrow(state$history_pairs))
  state$posterior$U_dup_threshold <- 0

  theta_draws <- matrix(0, nrow = 2L, ncol = state$N)
  colnames(theta_draws) <- state$ids
  fit <- list(
    theta_draws = theta_draws,
    theta_mean = stats::setNames(rep(0, state$N), state$ids)
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

  out <- pairwiseLLM:::.adaptive_select_batch_by_ladder(
    state = state,
    fit = fit,
    theta_summary = theta_summary,
    config = config,
    candidates_with_utility = base_utilities,
    n_candidates_generated = 0L,
    seed = NULL,
    exploration_only = FALSE
  )

  testthat::expect_identical(out$fallback_used, "dup_relax")
  testthat::expect_true("dup_relax" %in% out$fallback_path)
  attempt_tbl <- dplyr::bind_rows(out$stage_attempts)
  testthat::expect_true(any(attempt_tbl$stage == "dup_relax" & attempt_tbl$dup_policy == "relaxed"))
  testthat::expect_equal(nrow(out$selection), config$batch_size)
})
