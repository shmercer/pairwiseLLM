testthat::test_that("diagnostics_gate passes when thresholds are met", {
  config <- pairwiseLLM:::adaptive_v3_config(
    3L,
    list(
      max_rhat = 1.05,
      min_ess_bulk = 200,
      min_ess_bulk_near_stop = 500
    )
  )

  fit <- list(
    diagnostics = list(
      divergences = 0L,
      max_rhat = 1.01,
      min_ess_bulk = 300
    )
  )

  expect_true(pairwiseLLM:::diagnostics_gate(fit, config))
})

testthat::test_that("diagnostics_gate fails on individual criteria", {
  config <- pairwiseLLM:::adaptive_v3_config(
    3L,
    list(
      max_rhat = 1.02,
      min_ess_bulk = 200,
      min_ess_bulk_near_stop = 500
    )
  )

  base_fit <- list(
    diagnostics = list(
      divergences = 0L,
      max_rhat = 1.01,
      min_ess_bulk = 300
    )
  )

  bad_div <- base_fit
  bad_div$diagnostics$divergences <- 2L
  expect_false(pairwiseLLM:::diagnostics_gate(bad_div, config))

  bad_rhat <- base_fit
  bad_rhat$diagnostics$max_rhat <- 1.10
  expect_false(pairwiseLLM:::diagnostics_gate(bad_rhat, config))

  bad_ess <- base_fit
  bad_ess$diagnostics$min_ess_bulk <- 150
  expect_false(pairwiseLLM:::diagnostics_gate(bad_ess, config))
})

testthat::test_that("diagnostics_gate uses stricter ESS near stop", {
  config <- pairwiseLLM:::adaptive_v3_config(
    3L,
    list(
      min_ess_bulk = 300,
      min_ess_bulk_near_stop = 800
    )
  )

  fit <- list(
    diagnostics = list(
      divergences = 0L,
      max_rhat = 1.00,
      min_ess_bulk = 600
    )
  )

  expect_true(pairwiseLLM:::diagnostics_gate(fit, config, near_stop = FALSE))
  expect_false(pairwiseLLM:::diagnostics_gate(fit, config, near_stop = TRUE))
})

testthat::test_that("diagnostics_gate reads diagnostics fields directly from fit", {
  config <- pairwiseLLM:::adaptive_v3_config(
    3L,
    list(
      max_rhat = 1.05,
      min_ess_bulk = 200,
      min_ess_bulk_near_stop = 500
    )
  )

  fit <- list(divergences = 0L, max_rhat = 1.01, min_ess_bulk = 300)
  testthat::expect_true(pairwiseLLM:::diagnostics_gate(fit, config))
})

testthat::test_that("diagnostics_gate returns FALSE when diagnostics fields are missing", {
  config <- pairwiseLLM:::adaptive_v3_config(
    3L,
    list(
      max_rhat = 1.05,
      min_ess_bulk = 200,
      min_ess_bulk_near_stop = 500
    )
  )

  fit <- list(divergences = 0L, max_rhat = 1.01)
  testthat::expect_false(pairwiseLLM:::diagnostics_gate(fit, config))
})

testthat::test_that("diagnostics_gate returns FALSE when diagnostics list omits fields", {
  config <- pairwiseLLM:::adaptive_v3_config(
    3L,
    list(
      max_rhat = 1.05,
      min_ess_bulk = 200,
      min_ess_bulk_near_stop = 500
    )
  )

  fit <- list(diagnostics = list(divergences = 0L, max_rhat = 1.01))
  testthat::expect_false(pairwiseLLM:::diagnostics_gate(fit, config))
})
