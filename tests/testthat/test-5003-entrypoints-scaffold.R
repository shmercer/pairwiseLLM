make_test_items <- function(n) {
  tibble::tibble(
    item_id = seq_len(n),
    text = paste("item", seq_len(n))
  )
}

test_that("adaptive_rank_start returns v2 state", {
  items <- make_test_items(2)
  now_fn <- function() as.POSIXct("2001-01-01", tz = "UTC")

  state <- adaptive_rank_start(items, now_fn = now_fn)

  expect_true(inherits(state, "adaptive_state"))
  expect_equal(state$meta$schema_version, "v2-0")
  expect_equal(state$meta$now_fn(), now_fn())
})

test_that("adaptive_rank_run_live and resume are explicit stubs", {
  expect_error(
    adaptive_rank_run_live(),
    "stepwise execution not implemented yet"
  )
  expect_error(
    adaptive_rank_resume(),
    "stepwise execution not implemented yet"
  )
})

test_that("adaptive_rank_start rejects unnamed extra arguments", {
  items <- make_test_items(2)
  expect_error(
    adaptive_rank_start(items, "oops"),
    "Only named `now_fn` is supported"
  )
})
