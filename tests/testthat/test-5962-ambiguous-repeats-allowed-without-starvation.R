testthat::test_that("ambiguous repeats can be selected in relaxed mode", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  unordered_key <- pairwiseLLM:::make_unordered_key("A", "B")
  ordered_key <- pairwiseLLM:::make_ordered_key("A", "B")
  state$pair_count[[unordered_key]] <- 2L
  state$posterior$U_dup_threshold <- 0.4

  testthat::expect_false(pairwiseLLM:::.adaptive_unordered_allowed(
    state,
    "A",
    "B",
    allow_repeats = TRUE
  ))
  testthat::expect_true(pairwiseLLM:::.adaptive_unordered_allowed(
    state,
    "A",
    "B",
    dup_policy = "relaxed",
    allow_repeats = TRUE
  ))

  history_rows <- tibble::tibble(
    pair_uid = c(paste0(unordered_key, "#1"), paste0(unordered_key, "#2")),
    unordered_key = unordered_key,
    ordered_key = c(ordered_key, "B:A"),
    A_id = c("A", "B"),
    B_id = c("B", "A"),
    A_text = c("alpha", "bravo"),
    B_text = c("bravo", "alpha"),
    phase = "phase2",
    iter = c(1L, 2L),
    created_at = as.POSIXct(c("2026-01-01 00:00:00", "2026-01-02 00:00:00"), tz = "UTC")
  )
  state$history_pairs <- dplyr::bind_rows(state$history_pairs, history_rows)
  state$comparisons_scheduled <- as.integer(nrow(state$history_pairs))
  state$history_results <- tibble::tibble(
    pair_uid = c(paste0(unordered_key, "#1"), paste0(unordered_key, "#2")),
    unordered_key = unordered_key,
    ordered_key = c(ordered_key, "B:A"),
    A_id = c("A", "B"),
    B_id = c("B", "A"),
    better_id = c("A", "B"),
    winner_pos = c(1L, 1L),
    phase = "phase2",
    iter = c(1L, 2L),
    received_at = as.POSIXct(c("2026-01-01 00:00:00", "2026-01-02 00:00:00"), tz = "UTC"),
    backend = "test",
    model = "test"
  )
  state$comparisons_observed <- as.integer(nrow(state$history_results))
  state$new_since_refit <- state$comparisons_observed - state$last_refit_at

  theta_summary <- tibble::tibble(
    item_id = state$ids,
    theta_mean = c(2, 1, 0),
    theta_sd = rep(0.2, state$N)
  )
  candidate_config <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(W = 2L, C_max = 10L)
  )
  candidate_pool <- pairwiseLLM:::generate_candidates_from_anchors(
    anchors = c("A", "B"),
    theta_summary = theta_summary,
    state = state,
    config = candidate_config,
    dup_policy = "relaxed",
    allow_repeats = TRUE
  )
  candidate_keys <- pairwiseLLM:::make_unordered_key(candidate_pool$i, candidate_pool$j)
  testthat::expect_true(unordered_key %in% candidate_keys)

  config <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(batch_size = 1L, explore_rate = 0, dup_p_margin = 0.1)
  )

  candidates <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = unordered_key,
    utility = 0.6,
    utility_raw = 0.6,
    p_mean = 0.52
  )

  filtered <- pairwiseLLM:::.adaptive_filter_duplicate_candidates(
    candidates,
    state,
    config,
    dup_policy = "relaxed"
  )
  testthat::expect_equal(nrow(filtered), 1L)
  testthat::expect_equal(filtered$unordered_key, unordered_key)

  selected <- pairwiseLLM:::select_batch(
    state = state,
    candidates_with_utility = filtered,
    config = config,
    seed = NULL,
    exploration_only = FALSE,
    dup_policy = "relaxed"
  )

  testthat::expect_equal(nrow(selected), 1L)
  testthat::expect_equal(selected$A_id, "A")
  testthat::expect_equal(selected$B_id, "B")
})
