testthat::test_that("round_log row retains epsilon and beta percentile columns", {
  withr::local_seed(1)
  ids <- c("A", "B", "C")
  samples <- tibble::tibble(ID = ids, text = ids)
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(model_variant = "btl"))

  theta_draws <- matrix(
    c(0.2, -0.1, 0.0, 0.2, -0.1, 0.0),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(NULL, ids)
  )
  fit <- pairwiseLLM:::build_v3_fit_contract(
    theta_draws = theta_draws,
    epsilon_draws = NULL,
    beta_draws = NULL,
    model_variant = "btl"
  )

  row <- pairwiseLLM:::build_round_log_row(
    state = state,
    fit = fit,
    metrics = pairwiseLLM:::.adaptive_stop_metrics_defaults(),
    stop_out = list()
  )

  expected_cols <- c(
    "epsilon_p2.5", "epsilon_p5", "epsilon_p50", "epsilon_p95", "epsilon_p97.5",
    "beta_p2.5", "beta_p5", "beta_p50", "beta_p95", "beta_p97.5"
  )
  testthat::expect_true(all(expected_cols %in% names(row)))
  testthat::expect_true(all(is.na(unlist(row[expected_cols]))))
})
