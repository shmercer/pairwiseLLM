test_that("bt_stop_metrics validates fit structure and required columns", {
  bad1 <- list()
  expect_error(bt_stop_metrics(bad1), "contain a `\\$theta`")

  bad2 <- list(theta = tibble::tibble(ID = "A", theta = 1))
  expect_error(bt_stop_metrics(bad2), "must contain columns")
})

test_that("bt_stop_metrics validates se_probs and fit_bounds", {
  fit <- list(
    theta = tibble::tibble(ID = "A", theta = 1, se = 0.1)
  )

  expect_error(bt_stop_metrics(fit, se_probs = "x"), "se_probs")
  expect_error(bt_stop_metrics(fit, se_probs = c(0.5, Inf)), "se_probs")

  expect_error(bt_stop_metrics(fit, fit_bounds = 1), "fit_bounds")
  expect_error(bt_stop_metrics(fit, fit_bounds = c(NA, 1.3)), "fit_bounds")
})

test_that("bt_stop_metrics computes SE summaries and relative SE metrics", {
  fit <- list(
    engine = "mock",
    reliability = 0.91,
    theta = tibble::tibble(
      ID = c("A", "B", "C", "D"),
      theta = c(0, 1, 2, 3),
      se = c(0.2, 0.3, 0.4, 0.5)
    )
  )

  m <- bt_stop_metrics(fit, se_probs = c(0.5, 0.9, 0.95))
  expect_s3_class(m, "tbl_df")
  expect_equal(nrow(m), 1L)

  expect_equal(m$engine, "mock")
  expect_equal(m$n_items, 4L)

  expect_true(is.finite(m$theta_sd))
  expect_true(m$theta_sd > 0)

  expect_equal(m$se_mean, mean(c(0.2, 0.3, 0.4, 0.5)))
  expect_equal(m$se_max, 0.5)

  expect_true(all(c("se_p50", "se_p90", "se_p95") %in% names(m)))
  expect_true(is.finite(m$rel_se_mean))
  expect_true(is.finite(m$rel_se_p90))
})

test_that("bt_stop_metrics sets relative metrics to NA when theta_sd <= 0", {
  fit <- list(
    engine = "mock",
    theta = tibble::tibble(
      ID = c("A", "B", "C"),
      theta = c(1, 1, 1),
      se = c(0.2, 0.2, 0.2)
    )
  )

  m <- bt_stop_metrics(fit)
  expect_equal(m$theta_sd, 0)
  expect_true(is.na(m$rel_se_mean))
  expect_true(is.na(m$rel_se_p90))
})

test_that("bt_stop_metrics incorporates diagnostics when available", {
  fit <- list(
    engine = "sirt",
    reliability = 0.92,
    theta = tibble::tibble(
      ID = c("A", "B", "C", "D"),
      theta = c(0, 1, 2, 3),
      se = c(0.2, 0.3, 0.4, 0.5)
    ),
    diagnostics = list(
      sepG = 3.2,
      item_fit = tibble::tibble(
        ID = c("A", "B", "C", "D"),
        infit = c(1.0, 1.2, 1.4, 0.8),
        outfit = c(1.0, 1.1, 1.6, 0.9)
      ),
      judge_fit = tibble::tibble(
        judge = c("mA", "mB"),
        infit = c(1.0, 1.5),
        outfit = c(1.0, 1.2)
      )
    )
  )

  m <- bt_stop_metrics(fit, fit_bounds = c(0.7, 1.3))

  expect_equal(m$sepG, 3.2)

  # Item C is misfitting (infit/outfit above upper)
  expect_equal(m$item_misfit_prop, 1 / 4)

  # Judge mB is misfitting (infit above upper)
  expect_equal(m$judge_misfit_prop, 1 / 2)
})

test_that("bt_should_stop validates inputs", {
  bad <- tibble::tibble(a = 1)
  expect_error(bt_should_stop(bad), "missing required column: reliability")

  m <- tibble::tibble(
    reliability = 0.9, sepG = 3, rel_se_p90 = 0.3,
    item_misfit_prop = 0, judge_misfit_prop = 0
  )
  expect_error(bt_should_stop(dplyr::bind_rows(m, m)), "one-row")
  expect_error(bt_should_stop(m, prev_metrics = dplyr::bind_rows(m, m)), "one-row")
})

test_that("bt_should_stop stops when precision target is met (and other criteria pass)", {
  m <- tibble::tibble(
    reliability = 0.92,
    sepG = 3.2,
    rel_se_p90 = 0.25,
    item_misfit_prop = 0.00,
    judge_misfit_prop = 0.00
  )

  res <- bt_should_stop(
    m,
    reliability_target = 0.90,
    sepG_target = 3.0,
    rel_se_p90_target = 0.30,
    rel_se_p90_min_improve = 0.01,
    max_item_misfit_prop = 0.05,
    max_judge_misfit_prop = 0.05
  )

  expect_true(res$stop)
  expect_s3_class(res$details, "tbl_df")
  expect_true(all(c("criterion", "value", "threshold", "pass") %in% names(res$details)))
})

test_that("bt_should_stop stops on stalled improvement when precision target not met", {
  prev <- tibble::tibble(
    reliability = 0.92, sepG = 3.2, rel_se_p90 = 0.41,
    item_misfit_prop = 0.00, judge_misfit_prop = 0.00
  )

  curr <- tibble::tibble(
    reliability = 0.92, sepG = 3.2, rel_se_p90 = 0.40,
    item_misfit_prop = 0.00, judge_misfit_prop = 0.00
  )

  # Improvement is ~2.44% (0.41->0.40), which is <= 3% threshold => "stalled"
  res <- bt_should_stop(
    curr,
    prev_metrics = prev,
    rel_se_p90_target = 0.30, # not met
    rel_se_p90_min_improve = 0.03 # stalled threshold
  )

  expect_true(res$stop)
  expect_true(is.finite(res$improve$rel_se_p90_improve_pct))
})

test_that("bt_should_stop does not stop when reliability criterion fails", {
  m <- tibble::tibble(
    reliability = 0.85,
    sepG = 3.2,
    rel_se_p90 = 0.25,
    item_misfit_prop = 0.00,
    judge_misfit_prop = 0.00
  )

  res <- bt_should_stop(m, reliability_target = 0.90, rel_se_p90_target = 0.30)
  expect_false(res$stop)
})

test_that("bt_should_stop treats NA thresholds as disabled", {
  m <- tibble::tibble(
    reliability = NA_real_,
    sepG = NA_real_,
    rel_se_p90 = 0.25,
    item_misfit_prop = NA_real_,
    judge_misfit_prop = NA_real_
  )

  res <- bt_should_stop(
    m,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_target = 0.30,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_
  )

  expect_true(res$stop) # precision target met; others disabled
})

test_that("bt_stop_metrics can compute precision summaries on a subset of IDs", {
  fit <- list(
    engine = "mock",
    theta = tibble::tibble(
      ID = c("A", "B", "C", "D"),
      theta = c(0, 1, 2, 3),
      se = c(0.2, 0.3, 0.4, 0.5)
    )
  )

  m <- bt_stop_metrics(fit, ids = c("A", "C"))
  expect_equal(m$n_items, 2L)
  expect_equal(m$n_total_items, 4L)
  expect_equal(m$se_mean, mean(c(0.2, 0.4)))
})

test_that("bt_stop_metrics can include core drift metrics when prev_fit and core_ids are provided", {
  fit_prev <- list(
    engine = "mock",
    theta = tibble::tibble(
      ID = c("A", "B", "C"),
      theta = c(0, 0.5, 2.5),
      se = c(0.2, 0.2, 0.2)
    )
  )
  fit_cur <- list(
    engine = "mock",
    theta = tibble::tibble(
      ID = c("A", "B", "C"),
      theta = c(0, 1, 2),
      se = c(0.2, 0.2, 0.2)
    )
  )

  m <- bt_stop_metrics(fit_cur, prev_fit = fit_prev, core_ids = c("A", "B", "C"))
  expect_true(all(c("core_n", "core_mean_abs_shift", "core_max_abs_shift") %in% names(m)))
  expect_equal(m$core_n, 3L)
  expect_equal(m$core_mean_abs_shift, (0 + 0.5 + 0.5) / 3)
  expect_equal(m$core_max_abs_shift, 0.5)
})

test_that("bt_should_stop can gate stopping on core drift thresholds", {
  m <- tibble::tibble(
    reliability = 0.92,
    sepG = 3.2,
    rel_se_p90 = 0.25,
    item_misfit_prop = 0.00,
    judge_misfit_prop = 0.00,
    core_theta_cor = 0.80,
    core_theta_spearman = 1.00,
    core_max_abs_shift = 0.60,
    core_p90_abs_shift = 0.50
  )

  res_fail <- bt_should_stop(
    m,
    rel_se_p90_target = 0.30,
    core_theta_cor_target = 0.90
  )
  expect_false(res_fail$stop)

  res_pass <- bt_should_stop(
    m,
    rel_se_p90_target = 0.30,
    core_theta_cor_target = 0.70,
    core_theta_spearman_target = 0.90,
    core_max_abs_shift_target = 0.70,
    core_p90_abs_shift_target = 0.60
  )
  expect_true(res_pass$stop)
})
