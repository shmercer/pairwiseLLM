test_that("no_new_pairs has priority and yields explicit reason", {
  dec <- .stop_decision(
    round = 3L,
    min_rounds = 2L,
    no_new_pairs = TRUE,
    graph_healthy = TRUE,
    stability_reached = TRUE,
    precision_reached = TRUE
  )
  expect_true(dec$stop)
  expect_equal(dec$reason, "no_new_pairs")
})
