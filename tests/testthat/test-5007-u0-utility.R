test_that("compute_u0 reflects trueskill win probability", {
  items_equal <- tibble::tibble(
    item_id = c("1", "2"),
    mu = c(0, 0),
    sigma = c(1, 1)
  )
  state_equal <- pairwiseLLM:::new_trueskill_state(items_equal, mu0 = 0, sigma0 = 1, beta = 1)
  u0_equal <- pairwiseLLM:::compute_u0("1", "2", state_equal)

  expect_equal(u0_equal, 0.25, tolerance = 1e-6)

  items_diff <- tibble::tibble(
    item_id = c("1", "2"),
    mu = c(5, -5),
    sigma = c(1, 1)
  )
  state_diff <- pairwiseLLM:::new_trueskill_state(items_diff, mu0 = 0, sigma0 = 1, beta = 1)
  u0_diff <- pairwiseLLM:::compute_u0("1", "2", state_diff)

  expect_true(u0_diff < u0_equal)
  expect_true(u0_diff >= 0)
  expect_true(u0_equal <= 0.25 + 1e-8)
})

test_that("score_candidates_u0 adds utility and validates ids", {
  items <- tibble::tibble(
    item_id = c("1", "2", "3"),
    mu = c(0, 0, 1),
    sigma = c(1, 1, 1)
  )
  state <- pairwiseLLM:::new_trueskill_state(items, mu0 = 0, sigma0 = 1, beta = 1)

  candidates <- tibble::tibble(i = c("1", "2"), j = c("2", "3"))
  scored <- pairwiseLLM:::score_candidates_u0(candidates, state)

  expect_true("u0" %in% names(scored))
  expect_equal(nrow(scored), nrow(candidates))
  expect_true(all(scored$u0 >= 0 & scored$u0 <= 0.25 + 1e-8))

  bad_candidates <- tibble::tibble(i = "1", j = "999")
  expect_error(
    pairwiseLLM:::score_candidates_u0(bad_candidates, state),
    "must be present"
  )
})
