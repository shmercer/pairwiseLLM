testthat::test_that("allocation_precision_ramp returns NULL when it cannot decide", {
  f <- allocation_precision_ramp(step = 0.2, max_within = 0.8)

  # Missing prev_metrics -> NULL
  state <- list(
    metrics = tibble::tibble(rel_se_p90 = 0.5),
    prev_metrics = NULL,
    within_batch_frac = 0.1,
    core_audit_frac = 0.1
  )
  testthat::expect_null(f(state))

  # Missing metric column -> NULL
  state$prev_metrics <- tibble::tibble(other = 0.7)
  testthat::expect_null(f(state))
})

testthat::test_that("allocation_precision_ramp increases within_batch_frac on improvement", {
  f <- allocation_precision_ramp(step = 0.2, max_within = 0.8, min_improve = 0.05)

  state <- list(
    metrics = tibble::tibble(rel_se_p90 = 0.40),
    prev_metrics = tibble::tibble(rel_se_p90 = 0.55),
    within_batch_frac = 0.10,
    core_audit_frac = 0.10
  )
  out <- f(state)
  testthat::expect_type(out, "list")
  testthat::expect_equal(out$within_batch_frac, 0.30)

  # Clamp at max_within
  state$within_batch_frac <- 0.75
  out2 <- f(state)
  testthat::expect_equal(out2$within_batch_frac, 0.80)

  # Not enough improvement -> NULL
  state$within_batch_frac <- 0.10
  state$metrics$rel_se_p90 <- 0.52
  state$prev_metrics$rel_se_p90 <- 0.55
  testthat::expect_null(f(state))
})

testthat::test_that("allocation_audit_on_drift adjusts core_audit_frac up and back toward baseline", {
  f <- allocation_audit_on_drift(
    drift_metric = "linking_max_abs_shift",
    drift_threshold = 0.20,
    step = 0.05,
    base_core_audit = 0.10,
    max_core_audit = 0.40
  )

  # High drift -> increase
  state <- list(
    metrics = tibble::tibble(linking_max_abs_shift = 0.35),
    within_batch_frac = 0.25,
    core_audit_frac = 0.10
  )
  out <- f(state)
  testthat::expect_equal(out$core_audit_frac, 0.15)

  # Low drift and above baseline -> decrease toward baseline
  state$core_audit_frac <- 0.20
  state$metrics$linking_max_abs_shift <- 0.05
  out2 <- f(state)
  testthat::expect_equal(out2$core_audit_frac, 0.15)

  # Low drift and already at baseline -> NULL
  state$core_audit_frac <- 0.10
  testthat::expect_null(f(state))
})

testthat::test_that("allocation policies return NULL for non-finite inputs and cover baseline-below adjustment", {
  # allocation_precision_ramp non-finite cur/prev -> NULL (covers early return)
  f <- allocation_precision_ramp(step = 0.2, max_within = 0.8)
  state <- list(
    metrics = tibble::tibble(rel_se_p90 = NA_real_),
    prev_metrics = tibble::tibble(rel_se_p90 = 0.8),
    within_batch_frac = 0.1,
    core_audit_frac = 0.1
  )
  testthat::expect_null(f(state))

  # non-finite within -> NULL (covers within check)
  state$metrics$rel_se_p90 <- 0.4
  state$prev_metrics$rel_se_p90 <- 0.6
  state$within_batch_frac <- NA_real_
  testthat::expect_null(f(state))

  # allocation_audit_on_drift missing metric -> NULL
  g <- allocation_audit_on_drift(drift_metric = "linking_max_abs_shift")
  state2 <- list(metrics = tibble::tibble(other = 0.1), core_audit_frac = 0.1)
  testthat::expect_null(g(state2))

  # non-finite drift -> NULL
  state3 <- list(metrics = tibble::tibble(linking_max_abs_shift = NA_real_), core_audit_frac = 0.1)
  testthat::expect_null(g(state3))

  # non-finite audit -> NULL
  state4 <- list(metrics = tibble::tibble(linking_max_abs_shift = 0.05), core_audit_frac = NA_real_)
  testthat::expect_null(g(state4))

  # low drift and audit below baseline -> increases toward baseline (covers audit < base branch)
  g2 <- allocation_audit_on_drift(
    drift_metric = "linking_max_abs_shift",
    drift_threshold = 0.2,
    step = 0.05,
    base_core_audit = 0.10,
    max_core_audit = 0.40
  )
  state5 <- list(metrics = tibble::tibble(linking_max_abs_shift = 0.01), core_audit_frac = 0.00)
  out <- g2(state5)
  testthat::expect_equal(out$core_audit_frac, 0.05)
})
