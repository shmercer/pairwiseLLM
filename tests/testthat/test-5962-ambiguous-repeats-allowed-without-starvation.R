testthat::test_that("ambiguous repeats can be selected in default mode", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  unordered_key <- pairwiseLLM:::make_unordered_key("A", "B")
  ordered_key <- pairwiseLLM:::make_ordered_key("A", "B")
  state$unordered_count[[unordered_key]] <- 2L
  state$pair_count[[unordered_key]] <- 2L
  state$posterior$U_dup_threshold <- 0.4
  state$ordered_seen <- stats::setNames(TRUE, ordered_key)

  testthat::expect_true(pairwiseLLM:::.adaptive_unordered_allowed(
    state,
    "A",
    "B",
    allow_repeats = TRUE
  ))

  history_row <- tibble::tibble(
    pair_uid = paste0(unordered_key, "#2"),
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "bravo",
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )
  state$history_pairs <- dplyr::bind_rows(state$history_pairs, history_row)
  state$comparisons_scheduled <- as.integer(nrow(state$history_pairs))

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
    dup_policy = "default"
  )
  testthat::expect_equal(nrow(filtered), 1L)
  testthat::expect_equal(filtered$unordered_key, unordered_key)

  selected <- pairwiseLLM:::select_batch(
    state = state,
    candidates_with_utility = filtered,
    config = config,
    seed = NULL,
    exploration_only = FALSE,
    dup_policy = "default"
  )

  testthat::expect_equal(nrow(selected), 1L)
  testthat::expect_equal(selected$A_id, "B")
  testthat::expect_equal(selected$B_id, "A")
})
