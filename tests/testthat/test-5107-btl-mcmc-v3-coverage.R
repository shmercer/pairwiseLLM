testthat::test_that("btl mcmc v3 validates bt_data payloads and ids", {
  bt_data <- list(
    A = c(1L, 2L),
    B = c(2L, 1L),
    Y = c(1L, 0L),
    N = 2L,
    item_id = c("A", "B")
  )
  out <- pairwiseLLM:::.btl_mcmc_v3_validate_bt_data(bt_data)
  expect_equal(out$N, 2L)
  expect_equal(out$item_id, c("A", "B"))

  results <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "C",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2026-01-02 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_v3_prepare_bt_data(results, ids = c("A", "B")),
    "contained in `ids`"
  )
})

testthat::test_that("btl mcmc v3 prepare catches unmatched indices after setdiff", {
  results <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "C",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2026-01-02 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test"
  )

  testthat::with_mocked_bindings(
    expect_error(
      pairwiseLLM:::.btl_mcmc_v3_prepare_bt_data(results, ids = c("A", "B")),
      "contains ids not present"
    ),
    setdiff = function(...) character(),
    .package = "pairwiseLLM"
  )
})

testthat::test_that("btl mcmc v3 unpacking flags malformed draws", {
  no_cols <- matrix(0.1, nrow = 1, ncol = 2)
  expect_error(
    pairwiseLLM:::.btl_mcmc_v3_unpack_draws(no_cols),
    "column names"
  )

  bad_theta <- list(
    theta = matrix("x", nrow = 1, ncol = 1),
    epsilon = 0.1
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_v3_unpack_draws(bad_theta),
    "theta"
  )

  bad_eps <- list(
    theta = matrix(0.1, nrow = 1, ncol = 1),
    epsilon = "bad"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_v3_unpack_draws(bad_eps),
    "epsilon"
  )
})

testthat::test_that("btl mcmc v3 diagnostics cover non-finite and missing summaries", {
  fit_nonfinite <- list(
    diagnostic_summary = function() tibble::tibble(num_divergent = c(Inf)),
    summary = function(variables = NULL) {
      tibble::tibble(
        rhat = c(NA_real_, NaN),
        ess_bulk = c(NA_real_, NaN),
        ess_tail = c(NA_real_, NaN)
      )
    }
  )
  out_nonfinite <- NULL
  testthat::expect_warning(
    {
      out_nonfinite <- pairwiseLLM:::.btl_mcmc_v3_collect_diagnostics(fit_nonfinite, model_variant = "btl_e_b")
    },
    "NAs introduced"
  )
  expect_true(is.na(out_nonfinite$divergences))
  expect_true(length(out_nonfinite$notes) > 0L)

  fit_missing <- list(
    diagnostic_summary = function() tibble::tibble(num_divergent = 0L),
    summary = function(variables = NULL) stop("summary missing")
  )
  out_missing <- pairwiseLLM:::.btl_mcmc_v3_collect_diagnostics(fit_missing, model_variant = "btl_e_b")
  expect_true(is.na(out_missing$max_rhat))
  expect_true(length(out_missing$notes) > 0L)
})
