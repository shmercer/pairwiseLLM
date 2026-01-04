# Tests for uncovered branches in R/stability_metrics.R

test_that("5308-01 .stability_metrics validates topk", {
  prev <- tibble::tibble(ID = c("a", "b"), theta = c(0, 1))
  curr <- tibble::tibble(ID = c("a", "b"), theta = c(0, 1))

  expect_error(
    pairwiseLLM:::.stability_metrics(prev_theta_tbl = prev, curr_theta_tbl = curr, topk = 0),
    "`topk` must be a positive integer.",
    fixed = TRUE
  )
})

test_that("5308-02 .stability_metrics validates required columns", {
  prev <- tibble::tibble(ID = c("a", "b"), theta = c(0, 1))
  curr <- tibble::tibble(ID = c("a", "b"), x = c(0, 1))

  expect_error(
    pairwiseLLM:::.stability_metrics(prev_theta_tbl = prev, curr_theta_tbl = curr),
    "`curr_theta_tbl` must contain columns `ID` and `theta`.",
    fixed = TRUE
  )
})

test_that("5308-03 random tie-breaking is deterministic given seed", {
  prev <- tibble::tibble(ID = c("a", "b", "c"), theta = c(0, 0, 0))
  curr <- tibble::tibble(ID = c("a", "b", "c"), theta = c(1, 1, 1))

  out1 <- pairwiseLLM:::.stability_metrics(
    prev_theta_tbl = prev,
    curr_theta_tbl = curr,
    topk = 2,
    topk_ties = "random",
    seed = 202
  )

  # Change the global RNG state and confirm the same result is returned when
  # seed is supplied.
  set.seed(999)
  out2 <- pairwiseLLM:::.stability_metrics(
    prev_theta_tbl = prev,
    curr_theta_tbl = curr,
    topk = 2,
    topk_ties = "random",
    seed = 202
  )

  expect_equal(out1, out2)
})
