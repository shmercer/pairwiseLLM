testthat::test_that("adaptive_v3_config handles override normalization paths", {
  cfg <- pairwiseLLM:::adaptive_v3_config(6, NULL)
  testthat::expect_true(is.list(cfg))
  testthat::expect_equal(cfg$N, 6L)

  testthat::expect_error(
    pairwiseLLM:::adaptive_v3_config(6, 1),
    "overrides"
  )
})

testthat::test_that("adaptive_v3_check_named_int enforces naming and ids", {
  ids <- c("A", "B")
  good <- stats::setNames(as.integer(c(1L, 2L)), ids)

  testthat::expect_silent(
    pairwiseLLM:::.adaptive_v3_check_named_int(good, "good", ids = ids)
  )

  testthat::expect_error(
    pairwiseLLM:::.adaptive_v3_check_named_int(c(1, 2), "bad"),
    "integer"
  )

  testthat::expect_error(
    pairwiseLLM:::.adaptive_v3_check_named_int(as.integer(c(1L, 2L)), "bad"),
    "named"
  )

  bad_ids <- stats::setNames(as.integer(c(1L, 2L)), c("A", "C"))
  testthat::expect_error(
    pairwiseLLM:::.adaptive_v3_check_named_int(bad_ids, "bad", ids = ids),
    "ids"
  )

  bad_neg <- stats::setNames(as.integer(c(-1L, 2L)), ids)
  testthat::expect_error(
    pairwiseLLM:::.adaptive_v3_check_named_int(bad_neg, "bad"),
    "non-negative"
  )
})

testthat::test_that("item summary defaults and round log row fill metrics", {
  defaults <- pairwiseLLM:::.adaptive_item_summary_defaults(n_rows = 2L)
  testthat::expect_equal(nrow(defaults), 2L)
  testthat::expect_true("theta_mean" %in% names(defaults))

  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list())

  fit <- make_v3_fit_contract(
    state$ids,
    theta_draws = matrix(
      c(0.1, -0.1, 0.2, -0.2),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, state$ids)
    ),
    epsilon_draws = c(0.1, 0.1)
  )
  metrics <- list(
    theta_sd_median_S = 0.2,
    tau = 0.3,
    theta_sd_pass = TRUE,
    U0 = 0.5,
    U_pass = TRUE,
    diagnostics_pass = TRUE
  )

  row <- pairwiseLLM:::build_round_log_row(
    state = state,
    fit = fit,
    metrics = metrics,
    stop_out = list(stop_decision = FALSE, stop_reason = NA_character_),
    config = state$config$v3
  )

  testthat::expect_equal(row$theta_sd_eap, metrics$theta_sd_median_S)
  testthat::expect_equal(row$epsilon_mean, 0.1)
})

testthat::test_that("build_item_summary supplies missing draw colnames", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)

  draws <- matrix(c(0.1, 0.2, -0.1, -0.2), nrow = 2, byrow = TRUE)
  fit <- list(theta_draws = draws)

  summary_tbl <- pairwiseLLM:::build_item_summary(state, fit = fit)
  testthat::expect_equal(summary_tbl$ID, state$ids)
  testthat::expect_true(all(is.finite(summary_tbl$theta_mean)))
})
