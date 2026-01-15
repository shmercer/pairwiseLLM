test_that("duplicate policy enforces reverse-only second occurrence", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  expect_true(pairwiseLLM:::duplicate_allowed(state, "A", "B"))
  expect_true(pairwiseLLM:::duplicate_allowed(state, "B", "A"))

  state1 <- pairwiseLLM:::record_exposure(state, "A", "B")
  expect_false(pairwiseLLM:::duplicate_allowed(state1, "A", "B"))
  expect_true(pairwiseLLM:::duplicate_allowed(state1, "B", "A"))

  state2 <- pairwiseLLM:::record_exposure(state1, "B", "A")
  expect_false(pairwiseLLM:::duplicate_allowed(state2, "A", "B"))
  expect_false(pairwiseLLM:::duplicate_allowed(state2, "B", "A"))
})

test_that("choose_order_with_position_balance uses imbalance rule", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$pos1[["A"]] <- 2L
  state$pos2[["A"]] <- 0L
  state$pos1[["B"]] <- 0L
  state$pos2[["B"]] <- 1L
  state$deg <- state$pos1 + state$pos2
  state$imb <- state$pos1 - state$pos2

  out <- pairwiseLLM:::choose_order_with_position_balance(state, "A", "B")
  expect_equal(out$A_id, "B")
  expect_equal(out$B_id, "A")
})

test_that("choose_order_with_position_balance is deterministic under seed", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  withr::local_seed(123)
  out1 <- pairwiseLLM:::choose_order_with_position_balance(state, "A", "B")
  withr::local_seed(123)
  out2 <- pairwiseLLM:::choose_order_with_position_balance(state, "A", "B")

  expect_equal(out1, out2)
})
