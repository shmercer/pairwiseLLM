testthat::test_that("log schema helpers return typed defaults", {
  expect_true(is.integer(pairwiseLLM:::.adaptive_log_default_value(1L)))
  expect_true(is.na(pairwiseLLM:::.adaptive_log_default_value(1L)))
  expect_true(is.double(pairwiseLLM:::.adaptive_log_default_value(1)))
  expect_true(is.na(pairwiseLLM:::.adaptive_log_default_value(1)))
  expect_true(is.logical(pairwiseLLM:::.adaptive_log_default_value(TRUE)))
  expect_true(is.na(pairwiseLLM:::.adaptive_log_default_value(TRUE)))
  expect_true(is.character(pairwiseLLM:::.adaptive_log_default_value("x")))
  expect_true(is.na(pairwiseLLM:::.adaptive_log_default_value("x")))

  ts <- as.POSIXct(character(), tz = "UTC")
  ts_default <- pairwiseLLM:::.adaptive_log_default_value(ts)
  expect_true(inherits(ts_default, "POSIXct"))
  expect_true(is.na(ts_default))

  other_default <- pairwiseLLM:::.adaptive_log_default_value(list())
  expect_true(is.na(other_default))
})

testthat::test_that("log schema alignment fills missing columns", {
  schema <- tibble::tibble(
    a = integer(),
    b = double(),
    c = as.POSIXct(character(), tz = "UTC")
  )
  log_tbl <- tibble::tibble(a = 1L)
  aligned <- pairwiseLLM:::.adaptive_align_log_schema(log_tbl, schema)

  expect_identical(colnames(aligned), c("a", "b", "c"))
  expect_true(is.integer(aligned$a))
  expect_true(is.double(aligned$b))
  expect_true(inherits(aligned$c, "POSIXct"))
  expect_true(is.na(aligned$b[[1L]]))
  expect_true(is.na(aligned$c[[1L]]))

  empty_aligned <- pairwiseLLM:::.adaptive_align_log_schema(NULL, schema)
  expect_equal(nrow(empty_aligned), 0L)
  expect_identical(colnames(empty_aligned), c("a", "b", "c"))
})

testthat::test_that("state log initialization aligns stored logs", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  state$config$round_log <- tibble::tibble(round_id = 1L)
  state$batch_log <- tibble::tibble(iter = 1L)

  state <- pairwiseLLM:::.adaptive_state_init_logs(state)
  expect_identical(
    colnames(state$config$round_log),
    colnames(pairwiseLLM:::round_log_schema())
  )
  expect_identical(
    colnames(state$batch_log),
    colnames(pairwiseLLM:::batch_log_schema())
  )
})

testthat::test_that("log default builders handle POSIXct columns", {
  batch_defaults <- pairwiseLLM:::.adaptive_batch_log_defaults()
  expect_true(inherits(batch_defaults$created_at, "POSIXct"))
})

testthat::test_that("adaptive_v3_config normalizes NULL overrides", {
  config <- pairwiseLLM:::adaptive_v3_config(3L, NULL)
  expect_true(is.list(config))
  expect_equal(config$N, 3L)
})
