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
