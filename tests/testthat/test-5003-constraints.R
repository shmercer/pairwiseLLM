test_that("record_judgment_exposure initializes pos_count when missing", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$pos_count <- NULL

  updated <- pairwiseLLM:::record_judgment_exposure(state, "A", "B")

  expect_true(is.integer(updated$pos_count))
  expect_equal(updated$pos_count[["A"]], 1L)
  expect_equal(updated$pos_count[["B"]], 0L)
})

test_that("choose_order_with_position_balance rejects unknown ids", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  expect_error(
    pairwiseLLM:::choose_order_with_position_balance(state, "A", "C"),
    "exist in `state\\$ids`"
  )
})
