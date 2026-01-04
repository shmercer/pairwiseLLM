test_that("precision stop is not gated by graph health", {
  dec <- pairwiseLLM:::.stop_decision(
    round = 5L,
    min_rounds = 1L,
    graph_healthy = FALSE,
    stability_reached = FALSE,
    precision_reached = TRUE
  )

  expect_true(dec$stop)
  expect_identical(dec$reason, "precision_reached")
  expect_true(is.na(dec$details$stop_blocked_by))
  expect_true(is.na(dec$details$stop_blocked_candidates))
})
