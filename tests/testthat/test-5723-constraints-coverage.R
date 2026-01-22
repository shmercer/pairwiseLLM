testthat::test_that("adaptive_unordered_allowed covers relaxed and env branches", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)

  state$unordered_count <- integer()
  testthat::expect_true(pairwiseLLM:::.adaptive_unordered_allowed(state, "A", "B"))

  state$unordered_count <- c("A:B" = 1L)
  seen <- new.env(parent = emptyenv())
  seen[["B:A"]] <- TRUE
  state$ordered_seen <- seen
  testthat::expect_true(pairwiseLLM:::.adaptive_unordered_allowed(state, "A", "B"))
})

testthat::test_that("rollback_presentation keeps ordered_seen when count remains", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)

  state$unordered_count <- c("A:B" = 1L)
  state$pair_ordered_count <- c("A:B" = 2L)
  state$ordered_seen <- c("A:B" = TRUE)

  out <- pairwiseLLM:::rollback_presentation(state, "A", "B")
  testthat::expect_true(isTRUE(out$ordered_seen[["A:B"]]))
})

testthat::test_that("record_judgment_exposure validates ids", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)

  testthat::expect_error(
    pairwiseLLM:::record_judgment_exposure(state, "A", "Z"),
    "exist in `state\\$ids`"
  )
})

testthat::test_that("choose_order_with_position_balance flips on seed", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)

  order <- pairwiseLLM:::choose_order_with_position_balance(state, "A", "B", seed = 4)
  testthat::expect_identical(order$A_id, "B")
  testthat::expect_identical(order$B_id, "A")
})
