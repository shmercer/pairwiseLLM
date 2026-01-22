testthat::test_that("non-ambiguous repeats are filtered in default mode", {
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
    p_mean = 0.8
  )

  filtered <- pairwiseLLM:::.adaptive_filter_duplicate_candidates(
    candidates,
    state,
    config,
    dup_policy = "default"
  )
  testthat::expect_equal(nrow(filtered), 0L)

  selected <- pairwiseLLM:::select_batch(
    state = state,
    candidates_with_utility = filtered,
    config = config,
    seed = NULL,
    exploration_only = FALSE,
    dup_policy = "default"
  )
  testthat::expect_equal(nrow(selected), 0L)
})
