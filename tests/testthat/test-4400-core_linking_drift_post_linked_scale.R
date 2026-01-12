test_that("drift_post uses linked theta when available", {
  ids <- c("a", "b", "c")

  reference_fit <- list(
    theta = tibble::tibble(
      ID = ids,
      theta = c(0, 1, 2),
      se = c(0.1, 0.1, 0.1)
    )
  )

  fit <- list(
    theta = tibble::tibble(
      ID = ids,
      theta = c(10, 11, 12),
      se = c(0.2, 0.2, 0.2),
      theta_linked = c(0.05, 1.05, 2.05),
      se_linked = c(0.1, 0.1, 0.1)
    )
  )

  drift_pre <- pairwiseLLM:::bt_drift_metrics(fit, reference_fit, ids = ids, prefix = "linking_pre_")
  drift_post <- pairwiseLLM:::.compute_drift_on_theta(
    fit,
    reference_fit,
    ids = ids,
    prefix = "linking_post_",
    theta_col = "theta_linked",
    se_col = "se_linked"
  )

  expect_gt(drift_pre$linking_pre_mean_abs_shift, drift_post$linking_post_mean_abs_shift)
})


test_that("missing linked columns falls back to baseline drift", {
  ids <- c("a", "b", "c")

  reference_fit <- list(
    theta = tibble::tibble(
      ID = ids,
      theta = c(0, 1, 2),
      se = c(0.1, 0.1, 0.1)
    )
  )

  fit <- list(
    theta = tibble::tibble(
      ID = ids,
      theta = c(10, 11, 12),
      se = c(0.2, 0.2, 0.2)
    )
  )

  drift_baseline <- pairwiseLLM:::bt_drift_metrics(fit, reference_fit, ids = ids, prefix = "baseline_")
  drift_post <- pairwiseLLM:::.compute_drift_on_theta(
    fit,
    reference_fit,
    ids = ids,
    prefix = "baseline_",
    theta_col = "theta_linked",
    se_col = "se_linked"
  )

  expect_identical(drift_post, drift_baseline)
})


test_that("empty theta table path is schema-stable", {
  fit <- list(theta = tibble::tibble())
  reference_fit <- list(theta = tibble::tibble(ID = character(), theta = numeric(), se = numeric()))

  drift_post <- pairwiseLLM:::.compute_drift_on_theta(
    fit,
    reference_fit,
    ids = "a",
    prefix = "linking_post_",
    theta_col = "theta_linked",
    se_col = "se_linked"
  )

  expect_true(inherits(drift_post, "tbl_df"))
  expect_equal(nrow(drift_post), 1L)
  expect_true(all(c("linking_post_n", "linking_post_p90_abs_shift") %in% names(drift_post)))
  expect_equal(drift_post$linking_post_n, 0L)
})
