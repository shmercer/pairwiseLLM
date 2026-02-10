make_test_items <- function(n) {
  tibble::tibble(
    item_id = seq_len(n),
    text = paste("item", seq_len(n))
  )
}

test_that("new_step_log returns canonical schema with correct types", {
  step_log <- pairwiseLLM:::new_step_log()
  schema <- pairwiseLLM:::schema_step_log

  expect_true(tibble::is_tibble(step_log))
  expect_equal(names(step_log), names(schema))
  expect_equal(nrow(step_log), 0L)

  for (col in names(schema)) {
    type <- schema[[col]]
    value <- step_log[[col]]
    if (identical(type, "POSIXct")) {
      expect_true(inherits(value, "POSIXct"))
    } else {
      expect_equal(typeof(value), type)
    }
  }
})

test_that("append_step_log validates column names and fills missing columns", {
  step_log <- pairwiseLLM:::new_step_log()

  expect_error(
    pairwiseLLM:::append_step_log(step_log, list(unknown = 1L)),
    "unknown columns"
  )

  row <- list(
    step_id = 1L,
    timestamp = as.POSIXct("2020-01-01", tz = "UTC")
  )
  out <- pairwiseLLM:::append_step_log(step_log, row)
  expect_equal(nrow(out), 1L)
  expect_true(is.integer(out$pair_id))
  expect_true(is.na(out$pair_id))
  expect_true(is.character(out$status))
  expect_true(is.na(out$status))
  expect_true(is.logical(out$is_explore_step))
  expect_true(is.na(out$is_explore_step))
})

test_that("append_step_log rejects bad coercions and multirow input", {
  step_log <- pairwiseLLM:::new_step_log()

  expect_error(
    pairwiseLLM:::append_step_log(
      step_log,
      list(step_id = "not-an-int", timestamp = as.POSIXct("2020-01-01", tz = "UTC"))
    ),
    "integer-like"
  )
  expect_error(
    pairwiseLLM:::append_step_log(
      step_log,
      list(step_id = 1.5, timestamp = as.POSIXct("2020-01-01", tz = "UTC"))
    ),
    "integer-like"
  )

  multi <- tibble::tibble(
    step_id = c(1L, 2L),
    timestamp = as.POSIXct(c("2020-01-01", "2020-01-02"), tz = "UTC")
  )
  expect_error(pairwiseLLM:::append_step_log(step_log, multi), "exactly one row")
})

test_that("append_item_step_log allows multirow appends", {
  items <- make_test_items(2)
  item_log <- pairwiseLLM:::new_item_step_log(items)

  rows <- tibble::tibble(
    item_id = c(1L, 2L),
    timestamp = as.POSIXct(c("2020-01-01", "2020-01-02"), tz = "UTC")
  )
  out <- pairwiseLLM:::append_item_step_log(item_log, rows)
  expect_equal(nrow(out), 2L)
})
