test_that("budget exhausted yields explicit reason", {
  dec <- .stop_decision(
    round = 3L,
    min_rounds = 2L,
    budget_exhausted = TRUE,
    graph_healthy = TRUE,
    stability_reached = TRUE,
    precision_reached = TRUE
  )
  expect_true(dec$stop)
  expect_equal(dec$reason, "pair_budget_exhausted")
})
