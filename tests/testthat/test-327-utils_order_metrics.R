testthat::test_that(".bt_order_metrics handles NULL, non-data.frame, and ordering", {
  testthat::expect_null(pairwiseLLM:::.bt_order_metrics(NULL))

  x <- list(a = 1)
  testthat::expect_identical(pairwiseLLM:::.bt_order_metrics(x), x)

  metrics <- data.frame(
    z = 1,
    stage = "final",
    batch_index = 2,
    a = 3
  )
  out <- pairwiseLLM:::.bt_order_metrics(metrics)
  # first known fields appear first, then sorted rest
  testthat::expect_identical(names(out), c("batch_index", "stage", "a", "z"))
})
