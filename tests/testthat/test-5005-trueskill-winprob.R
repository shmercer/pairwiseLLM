test_that("trueskill_win_probability is symmetric and monotone", {
  items <- tibble::tibble(item_id = c("1", "2"), mu = c(0, 0), sigma = c(1, 1))
  state <- pairwiseLLM:::new_trueskill_state(items)

  p12 <- pairwiseLLM:::trueskill_win_probability("1", "2", state)
  p21 <- pairwiseLLM:::trueskill_win_probability("2", "1", state)

  expect_equal(p12 + p21, 1, tolerance = 1e-12)
  expect_equal(p12, 0.5, tolerance = 1e-12)

  state_hi <- pairwiseLLM:::new_trueskill_state(
    tibble::tibble(item_id = c("1", "2"), mu = c(1, 0), sigma = c(1, 1))
  )
  state_higher <- pairwiseLLM:::new_trueskill_state(
    tibble::tibble(item_id = c("1", "2"), mu = c(2, 0), sigma = c(1, 1))
  )

  p_hi <- pairwiseLLM:::trueskill_win_probability("1", "2", state_hi)
  p_higher <- pairwiseLLM:::trueskill_win_probability("1", "2", state_higher)

  expect_gt(p_hi, 0.5)
  expect_gt(p_higher, p_hi)
})

test_that("trueskill_win_probability errors on invalid input", {
  items <- tibble::tibble(item_id = c("1", "2"), mu = c(0, 0), sigma = c(1, 1))
  state <- pairwiseLLM:::new_trueskill_state(items)

  expect_error(
    pairwiseLLM:::trueskill_win_probability("1", "1", state),
    "distinct"
  )
  expect_error(
    pairwiseLLM:::trueskill_win_probability("1", "3", state),
    "present"
  )
})

test_that("trueskill update validates ids and updates remain finite", {
  state <- pairwiseLLM:::new_trueskill_state(tibble::tibble(item_id = c("1", "2"), mu = c(0, 0), sigma = c(1, 1)))
  expect_error(pairwiseLLM:::update_trueskill_state(state, c("1", "2"), "2"), "length-1")
  expect_error(pairwiseLLM:::update_trueskill_state(state, "1", "1"), "distinct")
  expect_error(pairwiseLLM:::update_trueskill_state(state, "1", "3"), "must be present")

  updated <- pairwiseLLM:::update_trueskill_state(state, "1", "2")
  expect_true(all(is.finite(updated$items$mu)))
  expect_true(all(updated$items$sigma > 0))
})
