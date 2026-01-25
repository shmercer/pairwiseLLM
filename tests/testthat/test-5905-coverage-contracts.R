testthat::test_that("adaptive_v3_config handles list edge cases", {
  config <- pairwiseLLM:::adaptive_v3_config(3, list())
  testthat::expect_true(is.list(config))

  testthat::expect_error(
    pairwiseLLM:::adaptive_v3_config(3, 1),
    "`overrides` must be a list."
  )
})

testthat::test_that("reliability_EAP and gini_degree edge cases", {
  testthat::expect_true(is.na(pairwiseLLM:::compute_reliability_EAP(NULL)))
  bad_draws <- matrix(c(0, NA, 1, 2), nrow = 2)
  testthat::expect_true(is.na(pairwiseLLM:::compute_reliability_EAP(bad_draws)))
  flat_draws <- matrix(rep(1, 6), nrow = 3)
  testthat::expect_true(is.na(pairwiseLLM:::compute_reliability_EAP(flat_draws)))

  testthat::expect_true(is.na(pairwiseLLM:::compute_gini_degree(NULL)))
  testthat::expect_true(is.na(pairwiseLLM:::compute_gini_degree(c(NA_real_, NaN))))
  testthat::expect_equal(pairwiseLLM:::compute_gini_degree(5), 0)
})

testthat::test_that("item summary defaults can expand rows", {
  defaults <- pairwiseLLM:::.adaptive_item_summary_defaults(2L)
  testthat::expect_equal(nrow(defaults), 2L)
  testthat::expect_identical(colnames(defaults), colnames(pairwiseLLM:::item_summary_schema()))
})

testthat::test_that("round log builder uses epsilon summary and draws", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list())

  draws <- list(
    theta = matrix(
      c(
        0.1, 0.0, -0.1,
        0.2, -0.1, -0.1
      ),
      nrow = 2,
      byrow = TRUE
    ),
    epsilon = c(0.05, 0.07)
  )
  fit <- list(
    draws = draws,
    epsilon_summary = tibble::tibble(
      epsilon_mean = 0.06,
      epsilon_p2.5 = 0.04,
      epsilon_p5 = 0.05,
      epsilon_p50 = 0.06,
      epsilon_p95 = 0.07,
      epsilon_p97.5 = 0.08
    )
  )

  metrics <- list(
    theta_sd_median_S = 0.1,
    tau = 0.2,
    theta_sd_pass = TRUE,
    U0 = 0.5,
    U_pass = TRUE,
    diagnostics_pass = TRUE
  )
  stop_out <- list(stop_decision = FALSE, stop_reason = NA_character_)

  row <- pairwiseLLM:::build_round_log_row(
    state = state,
    fit = fit,
    metrics = metrics,
    stop_out = stop_out,
    config = state$config$v3
  )

  testthat::expect_equal(row$epsilon_mean, 0.06)
  testthat::expect_equal(row$epsilon_p5, 0.05)
  testthat::expect_equal(row$epsilon_p95, 0.07)
  testthat::expect_equal(row$epsilon_p2.5, 0.04)
  testthat::expect_equal(row$epsilon_p97.5, 0.08)
})

testthat::test_that("item summary builder handles missing draws and colnames", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))

  bad_fit <- list(draws = list(theta = matrix(0, nrow = 2, ncol = 2)))
  defaults <- pairwiseLLM:::build_item_summary(state, bad_fit)
  testthat::expect_equal(nrow(defaults), 0L)

  fit <- list(
    theta_draws = matrix(
      c(0.1, -0.1, 0.2, -0.2),
      nrow = 2,
      byrow = TRUE
    )
  )
  summary <- pairwiseLLM:::build_item_summary(state, fit)
  testthat::expect_true(all(summary$ID == state$ids))
})
