testthat::test_that("item summary schema matches contract", {
  schema <- item_summary_schema()

  expected <- c(
    "ID",
    "theta_mean",
    "theta_sd",
    "theta_ci90_lo",
    "theta_ci90_hi",
    "theta_ci95_lo",
    "theta_ci95_hi",
    "rank_mean",
    "rank_sd",
    "deg",
    "posA_prop"
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
