test_that("stability has priority over precision when both are reached and graph is healthy", {
  dec <- pairwiseLLM:::.stop_decision(
    round = 5L,
    min_rounds = 1L,
    graph_healthy = TRUE,
    stability_reached = TRUE,
    precision_reached = TRUE
  )

  expect_true(dec$stop)
  expect_identical(dec$reason, "stability_reached")
})
