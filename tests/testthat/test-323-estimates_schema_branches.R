test_that("estimates_schema helpers cover NULL, all-NA, and length mismatch branches", {
  # .rank_desc NULL branch
  expect_null(pairwiseLLM:::.rank_desc(NULL))

  # .rank_desc all-NA branch
  x <- c(NA_real_, NA_real_, NA_real_)
  expect_equal(pairwiseLLM:::.rank_desc(x), rep(NA_integer_, 3))

  # .make_estimates_tbl length mismatch in a named argument
  expect_error(
    pairwiseLLM:::.make_estimates_tbl(ids = c("A", "B"), theta_rc = c(0, 1, 2)),
    "match length(ids)"
  )

  # .make_estimates_tbl length mismatch in `...`
  expect_error(
    pairwiseLLM:::.make_estimates_tbl(ids = c("A", "B"), theta_rc = c(0, 1), extra_col = 1:3),
    "All `...` columns must match length(ids)"
  )
})
