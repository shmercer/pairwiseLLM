test_that("stability metrics sign-flip alignment yields small RMS when mirrored", {
  prev <- tibble::tibble(ID = c("A","B","C","D"), theta = c(2, 1, 0, -1))
  curr <- tibble::tibble(ID = c("A","B","C","D"), theta = c(-2, -1, 0, 1))

  m <- .stability_metrics(prev, curr, topk = 2L, topk_ties = "id")
  expect_true(is.finite(m$rms_theta_delta))
  expect_lt(m$rms_theta_delta, 1e-8)
  expect_equal(m$topk_overlap, 1)
})
