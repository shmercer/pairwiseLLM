test_that("max rounds reached yields explicit reason", {
  dec <- .stop_decision(
    round = 10L,
    min_rounds = 2L,
    max_rounds_reached = TRUE,
    graph_healthy = TRUE,
    stability_reached = FALSE,
    precision_reached = FALSE
  )
  expect_true(dec$stop)
  expect_equal(dec$reason, "max_rounds_reached")
})
