testthat::test_that("rollback_presentation reverses presentation bookkeeping", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 3L)
  )
  baseline <- state

  state <- pairwiseLLM:::record_presentation(state, "A", "B")
  testthat::expect_equal(state$unordered_count[["A:B"]], 1L)
  testthat::expect_true(isTRUE(state$ordered_seen[["A:B"]]))
  testthat::expect_equal(state$pair_ordered_count[["A:B"]], 1L)

  state <- pairwiseLLM:::rollback_presentation(state, "A", "B")
  testthat::expect_identical(state$unordered_count, baseline$unordered_count)
  testthat::expect_identical(state$ordered_seen, baseline$ordered_seen)
  testthat::expect_identical(state$pair_ordered_count, baseline$pair_ordered_count)

  state <- baseline
  state <- pairwiseLLM:::record_presentation(state, "A", "B")
  state <- pairwiseLLM:::record_presentation(state, "B", "A")
  testthat::expect_equal(state$unordered_count[["A:B"]], 2L)
  testthat::expect_true(isTRUE(state$ordered_seen[["A:B"]]))
  testthat::expect_true(isTRUE(state$ordered_seen[["B:A"]]))
  testthat::expect_equal(state$pair_ordered_count[["A:B"]], 1L)
  testthat::expect_equal(state$pair_ordered_count[["B:A"]], 1L)

  state <- pairwiseLLM:::rollback_presentation(state, "B", "A")
  testthat::expect_equal(state$unordered_count[["A:B"]], 1L)
  testthat::expect_true(isTRUE(state$ordered_seen[["A:B"]]))
  testthat::expect_false("B:A" %in% names(state$ordered_seen))
  testthat::expect_equal(state$pair_ordered_count[["A:B"]], 1L)
  testthat::expect_false("B:A" %in% names(state$pair_ordered_count))

  state <- pairwiseLLM:::rollback_presentation(state, "A", "B")
  testthat::expect_identical(state$unordered_count, baseline$unordered_count)
  testthat::expect_identical(state$ordered_seen, baseline$ordered_seen)
  testthat::expect_identical(state$pair_ordered_count, baseline$pair_ordered_count)
})
