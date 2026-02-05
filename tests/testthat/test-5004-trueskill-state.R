make_test_items <- function(n) {
  tibble::tibble(item_id = seq_len(n))
}

test_that("new_trueskill_state builds a stable state", {
  items <- make_test_items(5)
  state <- pairwiseLLM:::new_trueskill_state(items)

  expect_s3_class(state, "trueskill_state")
  expect_true(is.data.frame(state$items))
  expect_equal(nrow(state$items), 5L)
  expect_true(all(c("item_id", "mu", "sigma") %in% names(state$items)))
  expect_true(is.numeric(state$beta))
  expect_equal(length(state$beta), 1L)
})

test_that("new_trueskill_state validates item ids and sigmas", {
  dup_items <- tibble::tibble(item_id = c("a", "a"))
  expect_error(
    pairwiseLLM:::new_trueskill_state(dup_items),
    "unique"
  )

  bad_sigma <- tibble::tibble(item_id = c("a", "b"), mu = c(0, 0), sigma = c(1, 0))
  expect_error(
    pairwiseLLM:::new_trueskill_state(bad_sigma),
    "sigma"
  )
})
