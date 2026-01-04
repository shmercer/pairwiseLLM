test_that("stability stop is gated by graph health", {
  dec <- .stop_decision(
    round = 3L,
    min_rounds = 2L,
    graph_healthy = FALSE,
    stability_reached = TRUE,
    precision_reached = FALSE
  )
  expect_false(dec$stop)
  expect_true(is.na(dec$reason))
})
