# Workstream E: user-function calling helper

testthat::test_that(".call_user_fun validates inputs", {
  testthat::expect_error(
    pairwiseLLM:::.call_user_fun(fun = "not a function", args = list()),
    "`fun` must be a function"
  )
})

testthat::test_that(".call_user_fun filters args when no ellipsis is present", {
  f <- function(x, y = 10) {
    x + y
  }

  out <- pairwiseLLM:::.call_user_fun(f, args = list(x = 2, y = 3, z = 99))
  testthat::expect_equal(out, 5)
})

testthat::test_that(".call_user_fun passes all args when ellipsis is present", {
  f <- function(x, ...) {
    dots <- list(...)
    x + sum(unlist(dots))
  }

  out <- pairwiseLLM:::.call_user_fun(f, args = list(x = 2, y = 3, z = 4))
  testthat::expect_equal(out, 9)
})

testthat::test_that(".call_user_fun ignores args for zero-arg functions", {
  f <- function() 42

  out <- pairwiseLLM:::.call_user_fun(f, args = list(x = 1, y = 2))
  testthat::expect_equal(out, 42)
})

testthat::test_that(".call_user_fun passes positional args when args are unnamed", {
  f <- function(x) x

  # When args are unnamed (names(args) is NULL), we pass them positionally.
  out <- pairwiseLLM:::.call_user_fun(f, args = list(100))
  testthat::expect_equal(out, 100)
})
