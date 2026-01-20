testthat::test_that("refit_every_batches validates inputs and returns a positive cadence", {
  testthat::expect_error(pairwiseLLM:::refit_every_batches(CW = NA, batch_size = 1), "positive integer")
  testthat::expect_error(pairwiseLLM:::refit_every_batches(CW = 0, batch_size = 1), "positive integer")
  testthat::expect_error(pairwiseLLM:::refit_every_batches(CW = 1, batch_size = 0), "positive integer")

  testthat::expect_equal(pairwiseLLM:::refit_every_batches(CW = 1, batch_size = 10), 1L)
  testthat::expect_equal(pairwiseLLM:::refit_every_batches(CW = 10, batch_size = 3), 4L)
})

testthat::test_that("should_refit validates inputs and triggers at CW threshold", {
  testthat::expect_error(
    pairwiseLLM:::should_refit(comparisons_observed = -1, last_refit_at = 0, batch_size = 1, CW = 1),
    "non-negative integer"
  )
  testthat::expect_error(
    pairwiseLLM:::should_refit(comparisons_observed = 0, last_refit_at = -1, batch_size = 1, CW = 1),
    "non-negative integer"
  )
  testthat::expect_error(
    pairwiseLLM:::should_refit(comparisons_observed = 0, last_refit_at = 0, batch_size = 0, CW = 1),
    "positive integer"
  )
  testthat::expect_error(
    pairwiseLLM:::should_refit(comparisons_observed = 0, last_refit_at = 0, batch_size = 1, CW = 0),
    "positive integer"
  )

  testthat::expect_false(
    pairwiseLLM:::should_refit(comparisons_observed = 9, last_refit_at = 3, batch_size = 1, CW = 7)
  )
  testthat::expect_true(
    pairwiseLLM:::should_refit(comparisons_observed = 10, last_refit_at = 3, batch_size = 1, CW = 7)
  )
})
