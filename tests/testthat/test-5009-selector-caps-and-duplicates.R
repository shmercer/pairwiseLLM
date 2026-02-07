test_that("selector respects star caps and duplicate limits", {
  items <- make_test_items(5)
  trueskill_state <- make_test_trueskill_state(items)

  history <- tibble::tibble(
    A_id = rep(1, 18),
    B_id = rep(2, 18)
  )
  state <- make_test_state(items, trueskill_state, history = history)

  out <- pairwiseLLM:::select_next_pair(state)

  expect_false(is.na(out$i))
  expect_false(is.na(out$j))
  expect_true(out$i != out$j)
  expect_true(out$i != 1L)
  expect_true(out$j != 1L)
})

test_that("selector reports starvation when no pairs are eligible", {
  items <- make_test_items(2)
  trueskill_state <- make_test_trueskill_state(items)
  history <- tibble::tibble(
    A_id = c(1, 2, 1),
    B_id = c(2, 1, 2)
  )
  state <- make_test_state(items, trueskill_state, history = history)

  out <- pairwiseLLM:::select_next_pair(state)

  expect_true(out$candidate_starved)
  expect_true(is.na(out$i))
  expect_true(is.na(out$j))
})

test_that("selector excludes items already used in round by default", {
  items <- make_test_items(6)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)
  state$round$staged_active <- TRUE
  state$round$per_round_item_uses[["1"]] <- 1L
  state$round$per_round_item_uses[["2"]] <- 1L
  state$round$repeat_in_round_used <- state$round$repeat_in_round_budget

  out <- pairwiseLLM:::select_next_pair(state, step_id = 1L)

  expect_false(out$i %in% c(1L, 2L))
  expect_false(out$j %in% c(1L, 2L))
})

test_that("selector allows limited repeats for underrepresented items within budget", {
  items <- make_test_items(6)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(
    items,
    trueskill_state,
    history = tibble::tibble(
      A_id = c("1", "2", "2", "3", "3"),
      B_id = c("4", "4", "5", "5", "6")
    )
  )
  state$round$staged_active <- TRUE
  state$round$per_round_item_uses[["1"]] <- 1L
  state$round$repeat_in_round_used <- 0L
  state$round$repeat_in_round_budget <- 2L

  out <- pairwiseLLM:::select_next_pair(state, step_id = 2L)

  expect_false(out$candidate_starved)
  expect_true(out$i %in% seq_len(6))
  expect_true(out$j %in% seq_len(6))
})
