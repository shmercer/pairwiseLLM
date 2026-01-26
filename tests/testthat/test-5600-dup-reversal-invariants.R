testthat::test_that("assign_order reverses last completed ordering", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state <- pairwiseLLM:::record_judgment_exposure(state, "A", "B")

  history <- pairwiseLLM:::.adaptive_empty_results_tbl()
  history <- dplyr::bind_rows(history, tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = pairwiseLLM:::make_unordered_key("A", "B"),
    ordered_key = pairwiseLLM:::make_ordered_key("A", "B"),
    A_id = "A",
    B_id = "B",
    better_id = NA_character_,
    winner_pos = NA_integer_,
    phase = "phase1",
    iter = 1L,
    received_at = as.POSIXct("2020-01-01", tz = "UTC"),
    backend = "test",
    model = "test"
  ))
  state$history_results <- history

  pairs <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = "A:B"
  )
  out <- pairwiseLLM:::assign_order(pairs, state)
  testthat::expect_equal(out$A_id, "B")
  testthat::expect_equal(out$B_id, "A")
})

testthat::test_that("assign_order symmetry holds for reversed history", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state <- pairwiseLLM:::record_judgment_exposure(state, "A", "B")

  history <- pairwiseLLM:::.adaptive_empty_results_tbl()
  history <- dplyr::bind_rows(history, tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = pairwiseLLM:::make_unordered_key("A", "B"),
    ordered_key = pairwiseLLM:::make_ordered_key("B", "A"),
    A_id = "B",
    B_id = "A",
    better_id = NA_character_,
    winner_pos = NA_integer_,
    phase = "phase1",
    iter = 1L,
    received_at = as.POSIXct("2020-01-01", tz = "UTC"),
    backend = "test",
    model = "test"
  ))
  state$history_results <- history

  pairs <- tibble::tibble(
    i_id = "B",
    j_id = "A",
    unordered_key = "A:B"
  )
  out <- pairwiseLLM:::assign_order(pairs, state)
  testthat::expect_equal(out$A_id, "A")
  testthat::expect_equal(out$B_id, "B")
})

testthat::test_that("sorted candidate storage does not prevent reversal", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state <- pairwiseLLM:::record_judgment_exposure(state, "A", "B")

  history <- pairwiseLLM:::.adaptive_empty_results_tbl()
  history <- dplyr::bind_rows(history, tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = pairwiseLLM:::make_unordered_key("A", "B"),
    ordered_key = pairwiseLLM:::make_ordered_key("B", "A"),
    A_id = "B",
    B_id = "A",
    better_id = NA_character_,
    winner_pos = NA_integer_,
    phase = "phase1",
    iter = 1L,
    received_at = as.POSIXct("2020-01-01", tz = "UTC"),
    backend = "test",
    model = "test"
  ))
  state$history_results <- history

  pairs <- tibble::tibble(
    i_id = "A",
    j_id = "B",
    unordered_key = "A:B"
  )
  out <- pairwiseLLM:::assign_order(pairs, state)
  testthat::expect_equal(out$A_id, "A")
  testthat::expect_equal(out$B_id, "B")
})

testthat::test_that("completed-only duplicate counting ignores scheduled pairs", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  unordered_key <- pairwiseLLM:::make_unordered_key("A", "B")

  state <- pairwiseLLM:::record_presentation(state, "A", "B")
  testthat::expect_equal(state$pair_count[[unordered_key]], 0L)
  testthat::expect_true(pairwiseLLM:::.adaptive_unordered_allowed(state, "A", "B"))

  state <- pairwiseLLM:::rollback_presentation(state, "A", "B")
  testthat::expect_equal(state$pair_count[[unordered_key]], 0L)
  testthat::expect_equal(state$unordered_count[[unordered_key]], 0L)
})

testthat::test_that("in-flight backlog prevents re-scheduling by default", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "gamma")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  history_pairs <- pairwiseLLM:::.adaptive_empty_pairs_tbl()
  history_pairs <- dplyr::bind_rows(history_pairs, tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = pairwiseLLM:::make_unordered_key("A", "B"),
    ordered_key = pairwiseLLM:::make_ordered_key("A", "B"),
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "beta",
    phase = "phase1",
    iter = 1L,
    created_at = as.POSIXct("2020-01-01", tz = "UTC")
  ))
  state$history_pairs <- history_pairs

  candidates <- tibble::tibble(
    i_id = c("A", "A"),
    j_id = c("B", "C"),
    unordered_key = c("A:B", "A:C")
  )
  filtered <- pairwiseLLM:::.adaptive_filter_inflight_pairs(candidates, state)
  testthat::expect_equal(filtered$unordered_key, "A:C")
})
