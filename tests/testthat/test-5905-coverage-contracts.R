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

testthat::test_that("round log builder uses epsilon summaries from fit contract", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list())

  theta_draws <- matrix(
    c(
      0.1, 0.0, -0.1,
      0.2, -0.1, -0.1
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(NULL, state$ids)
  )
  epsilon_draws <- c(0.05, 0.07)
  fit <- make_v3_fit_contract(
    state$ids,
    theta_draws = theta_draws,
    epsilon_draws = epsilon_draws
  )

  metrics <- list(
    theta_sd_eap = 0.1,
    reliability_EAP = 0.9,
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

  eps_probs <- stats::quantile(
    epsilon_draws,
    probs = c(0.025, 0.05, 0.95, 0.975),
    names = FALSE
  )
  testthat::expect_equal(row$epsilon_mean, mean(epsilon_draws))
  testthat::expect_equal(row$epsilon_p5, eps_probs[[2L]])
  testthat::expect_equal(row$epsilon_p95, eps_probs[[3L]])
  testthat::expect_equal(row$epsilon_p2.5, eps_probs[[1L]])
  testthat::expect_equal(row$epsilon_p97.5, eps_probs[[4L]])
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
