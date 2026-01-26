testthat::test_that("duplicate policy enforces all locked conditions", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  state$posterior$U_dup_threshold <- 0.5

  unordered_key <- pairwiseLLM:::make_unordered_key("A", "B")
  ordered_key <- pairwiseLLM:::make_ordered_key("A", "B")
  state$pair_count[[unordered_key]] <- 1L
  state$history_pairs <- tibble::tibble(
    pair_uid = "A:B#1",
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
  state$history_results <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = "A",
    B_id = "B",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    backend = "test",
    model = "test"
  )
  state$comparisons_scheduled <- 1L
  state$comparisons_observed <- 1L

  allowed <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = unordered_key,
    utility = 0.1,
    utility_raw = 0.1,
    p_mean = 0.9
  )
  out_ok <- pairwiseLLM:::select_exploitation_pairs(
    candidates_with_utility = allowed,
    state = state,
    n_exploit = 1L,
    config = config
  )
  expect_equal(nrow(out_ok), 1L)

  blocked_count <- allowed
  state$pair_count[[unordered_key]] <- config$dup_max_count
  state$history_pairs <- dplyr::bind_rows(
    state$history_pairs,
    tibble::tibble(
      pair_uid = "A:B#2",
      unordered_key = unordered_key,
      ordered_key = "B:A",
      A_id = "B",
      B_id = "A",
      A_text = "bravo",
      B_text = "alpha",
      phase = "phase2",
      iter = 2L,
      created_at = as.POSIXct("2026-01-02 00:00:00", tz = "UTC")
    )
  )
  state$history_results <- dplyr::bind_rows(
    state$history_results,
    tibble::tibble(
      pair_uid = "A:B#2",
      unordered_key = unordered_key,
      ordered_key = "B:A",
      A_id = "B",
      B_id = "A",
      better_id = "B",
      winner_pos = 1L,
      phase = "phase2",
      iter = 2L,
      received_at = as.POSIXct("2026-01-02 00:00:00", tz = "UTC"),
      backend = "test",
      model = "test"
    )
  )
  state$comparisons_scheduled <- 2L
  state$comparisons_observed <- 2L
  out_count <- pairwiseLLM:::select_exploitation_pairs(
    candidates_with_utility = blocked_count,
    state = state,
    n_exploit = 1L,
    config = config
  )
  expect_equal(nrow(out_count), 0L)
})
