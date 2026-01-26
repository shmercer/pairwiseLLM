make_state_for_dup_tests <- function(ids = c("A", "B", "C")) {
  samples <- tibble::tibble(
    ID = ids,
    text = paste0("text_", ids)
  )
  pairwiseLLM:::adaptive_state_new(samples, config = list(), seed = 1L)
}

add_completed_pair <- function(state, A_id, B_id, pair_index = 1L, iter = 1L) {
  unordered_key <- pairwiseLLM:::make_unordered_key(A_id, B_id)
  ordered_key <- pairwiseLLM:::make_ordered_key(A_id, B_id)
  pair_uid <- paste0(unordered_key, "#", pair_index)

  pair_row <- tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = A_id,
    B_id = B_id,
    A_text = state$texts[[A_id]],
    B_text = state$texts[[B_id]],
    phase = "phase2",
    iter = as.integer(iter),
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )
  result_row <- tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = A_id,
    B_id = B_id,
    better_id = A_id,
    winner_pos = 1L,
    phase = "phase2",
    iter = as.integer(iter),
    received_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    backend = "test",
    model = "test"
  )

  state$history_pairs <- dplyr::bind_rows(state$history_pairs, pair_row)
  state$history_results <- dplyr::bind_rows(state$history_results, result_row)
  state$comparisons_scheduled <- as.integer(nrow(state$history_pairs))
  state$comparisons_observed <- as.integer(nrow(state$history_results))
  state$new_since_refit <- state$comparisons_observed - state$last_refit_at

  current <- state$pair_count[[unordered_key]]
  if (is.null(current) || is.na(current)) {
    current <- 0L
  }
  state$pair_count[[unordered_key]] <- as.integer(current + 1L)
  state
}

testthat::test_that("ordering reverses after completed history", {
  withr::local_seed(1)
  state <- make_state_for_dup_tests()
  state <- add_completed_pair(state, "A", "B", pair_index = 1L, iter = 1L)

  pairs <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = pairwiseLLM:::make_unordered_key("A", "B")
  )
  ordered <- pairwiseLLM:::assign_order(pairs, state)
  testthat::expect_equal(ordered$A_id, "B")
  testthat::expect_equal(ordered$B_id, "A")
})

testthat::test_that("ordering reverses symmetrically when last order is flipped", {
  withr::local_seed(1)
  state <- make_state_for_dup_tests()
  state <- add_completed_pair(state, "B", "A", pair_index = 1L, iter = 1L)

  pairs <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = pairwiseLLM:::make_unordered_key("A", "B")
  )
  ordered <- pairwiseLLM:::assign_order(pairs, state)
  testthat::expect_equal(ordered$A_id, "A")
  testthat::expect_equal(ordered$B_id, "B")
})

testthat::test_that("sorted unordered storage still allows reversal", {
  withr::local_seed(1)
  state <- make_state_for_dup_tests()
  state <- add_completed_pair(state, "C", "A", pair_index = 1L, iter = 1L)

  pairs <- tibble::tibble(
    i_id = "A",
    j_id = "C",
    unordered_key = pairwiseLLM:::make_unordered_key("A", "C")
  )
  ordered <- pairwiseLLM:::assign_order(pairs, state)
  testthat::expect_equal(ordered$A_id, "A")
  testthat::expect_equal(ordered$B_id, "C")
})

testthat::test_that("duplicate eligibility uses completed counts only", {
  withr::local_seed(1)
  state <- make_state_for_dup_tests()
  state <- add_completed_pair(state, "A", "B", pair_index = 1L, iter = 1L)

  testthat::expect_true(pairwiseLLM:::.adaptive_unordered_allowed(state, "A", "B"))

  state <- add_completed_pair(state, "B", "A", pair_index = 2L, iter = 2L)
  testthat::expect_false(pairwiseLLM:::.adaptive_unordered_allowed(state, "A", "B"))

  config <- pairwiseLLM:::adaptive_v3_config(state$N, list(dup_max_count = 3L, dup_p_margin = 0.2))
  state$posterior$U_dup_threshold <- 0
  allowed <- pairwiseLLM:::.adaptive_duplicate_allowed(
    state,
    pairwiseLLM:::make_unordered_key("A", "B"),
    p_mean = 0.5,
    utility = 1,
    config = config,
    policy = "relaxed"
  )
  blocked <- pairwiseLLM:::.adaptive_duplicate_allowed(
    state,
    pairwiseLLM:::make_unordered_key("A", "B"),
    p_mean = 0.9,
    utility = 1,
    config = config,
    policy = "relaxed"
  )
  testthat::expect_true(allowed)
  testthat::expect_false(blocked)
})

testthat::test_that("in-flight unordered pairs are not rescheduled", {
  withr::local_seed(1)
  state <- make_state_for_dup_tests()
  unordered_key <- pairwiseLLM:::make_unordered_key("A", "B")
  state$history_pairs <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = unordered_key,
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = state$texts[["A"]],
    B_text = state$texts[["B"]],
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )
  state$comparisons_scheduled <- 1L

  pairs <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = unordered_key
  )
  ordered <- pairwiseLLM:::assign_order(pairs, state)
  testthat::expect_equal(nrow(ordered), 0L)
})
