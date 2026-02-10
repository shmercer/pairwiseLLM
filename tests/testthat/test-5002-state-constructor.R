test_that("new_adaptive_state builds a stable adaptive scaffold", {
  items <- make_test_items(3)
  now_fn <- function() as.POSIXct("2000-01-01", tz = "UTC")

  state <- pairwiseLLM:::new_adaptive_state(items, now_fn = now_fn)

  expect_true(inherits(state, "adaptive_state"))
  expect_equal(state$meta$schema_version, "adaptive-session")
  expect_equal(state$meta$now_fn(), now_fn())
  expect_equal(state$item_ids, as.character(items$item_id))
  expect_equal(state$n_items, 3L)
  expect_true(tibble::is_tibble(state$history_pairs))
  expect_true(inherits(state$trueskill_state, "trueskill_state"))
  expect_true(tibble::is_tibble(state$step_log))
  expect_true(tibble::is_tibble(state$round_log))
  expect_true(is.list(state$item_log))
  expect_true(tibble::is_tibble(state$item_step_log))
})

test_that("new_adaptive_state rejects non-function now_fn", {
  items <- make_test_items(2)
  expect_error(
    pairwiseLLM:::new_adaptive_state(items, now_fn = NULL),
    "`now_fn` must be a function"
  )
  expect_error(
    pairwiseLLM:::new_adaptive_state(items, now_fn = Sys.time()),
    "`now_fn` must be a function"
  )
})

test_that("new_adaptive_state accepts character item_id values", {
  items <- tibble::tibble(item_id = c("S01", "S02"))
  state <- pairwiseLLM:::new_adaptive_state(items)
  expect_equal(state$item_ids, c("S01", "S02"))
  expect_equal(unname(state$item_index), c(1L, 2L))
})
