test_that(".call_user_fun validates args and handles special formals cases", {
  expect_error(
    pairwiseLLM:::.call_user_fun(mean, args = 1),
    "`args` must be a list",
    fixed = TRUE
  )

  expect_identical(
    pairwiseLLM:::.call_user_fun(`+`, args = list(1, 2)),
    3
  )

  no_args_fun <- function() 42
  expect_identical(
    pairwiseLLM:::.call_user_fun(no_args_fun, args = list()),
    42
  )
})
