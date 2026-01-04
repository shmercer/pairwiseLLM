test_that("min_rounds prevents early stop even if criteria met", {
  dec <- .stop_decision(
    round = 1L,
    min_rounds = 2L,
    no_new_pairs = FALSE,
    budget_exhausted = FALSE,
    max_rounds_reached = FALSE,
    graph_healthy = TRUE,
    stability_reached = TRUE,
    precision_reached = TRUE
  )
  expect_false(dec$stop)
  expect_true(is.na(dec$reason))
})
