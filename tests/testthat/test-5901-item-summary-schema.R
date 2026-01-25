testthat::test_that("item summary schema matches contract", {
  schema <- item_summary_schema()

  expected <- c(
    "ID",
    "deg",
    "posA_prop",
    "theta_mean",
    "theta_p2.5",
    "theta_p5",
    "theta_p50",
    "theta_p95",
    "theta_p97.5",
    "theta_sd",
    "rank_mean",
    "rank_p2.5",
    "rank_p5",
    "rank_p50",
    "rank_p95",
    "rank_p97.5",
    "rank_sd"
  )

  testthat::expect_identical(colnames(schema), expected)
  testthat::expect_true(is.character(schema$ID))
  testthat::expect_true(is.double(schema$theta_mean))
  testthat::expect_true(is.integer(schema$deg))
})

testthat::test_that("item summary builder emits one row per item", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- adaptive_state_new(samples, config = list(d1 = 2L))

  fit <- list(
    theta_draws = matrix(
      c(
        0.2, -0.1, -0.1,
        0.1, 0.0, -0.1,
        0.3, -0.2, -0.1,
        0.0, 0.1, -0.1
      ),
      nrow = 4,
      byrow = TRUE,
      dimnames = list(NULL, state$ids)
    )
  )

  summary <- build_item_summary(state, fit)

  testthat::expect_equal(nrow(summary), state$N)
  testthat::expect_identical(colnames(summary), colnames(item_summary_schema()))
  testthat::expect_true(all(summary$ID == state$ids))
  testthat::expect_true(is.double(summary$rank_mean))
  testthat::expect_true(is.integer(summary$deg))
})
