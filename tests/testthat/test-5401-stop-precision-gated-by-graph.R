test_that("precision stop is gated by graph health", {
  dec <- pairwiseLLM:::.stop_decision(
    round = 5L,
    min_rounds = 1L,
    graph_healthy = FALSE,
    stability_reached = FALSE,
    precision_reached = TRUE
  )

  expect_false(dec$stop)
  expect_true(is.na(dec$reason))

  expect_identical(dec$details$stop_blocked_by, "graph_unhealthy")
  expect_identical(dec$details$stop_blocked_candidates, "precision_reached")
})
