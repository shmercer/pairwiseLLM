testthat::test_that("bt_drift_metrics covers theta input normalization and validation", {
  # .as_theta_tibble: accept named numeric vector
  vec <- c(A = 1, B = 2)
  t_vec <- pairwiseLLM:::.as_theta_tibble(vec, arg_name = "cur")
  testthat::expect_s3_class(t_vec, "tbl_df")
  testthat::expect_setequal(t_vec$ID, c("A", "B"))

  # .as_theta_tibble: normalize common column aliases
  df_alias <- tibble::tibble(id = c("A", "B"), theta_linked = c(0.1, -0.2), se_linked = c(0.01, 0.02))
  t_alias <- pairwiseLLM:::.as_theta_tibble(df_alias, arg_name = "cur")
  testthat::expect_true(all(c("ID", "theta", "se") %in% names(t_alias)))

  # rownames as ID fallback
  df2 <- data.frame(theta = c(0, 1), se = c(0.1, 0.2))
  rownames(df2) <- c("X", "Y")
  t2 <- pairwiseLLM:::.as_theta_tibble(df2, arg_name = "cur")
  testthat::expect_identical(t2$ID, c("X", "Y"))

  # invalid fit objects
  testthat::expect_error(
    pairwiseLLM:::.as_theta_tibble(list(theta = NULL), arg_name = "cur"),
    "theta.*NULL"
  )

  # bt_drift_metrics validation errors
  baseline <- tibble::tibble(ID = c("A", "B"), theta = c(0, 1))
  current <- tibble::tibble(ID = c("A", "B"), theta = c(0.1, 1.2))

  testthat::expect_error(
    pairwiseLLM:::bt_drift_metrics(current = current, baseline = NULL),
    "baseline"
  )

  testthat::expect_error(
    pairwiseLLM:::bt_drift_metrics(current = current, baseline = baseline, prefix = NA_character_),
    "prefix"
  )

  testthat::expect_error(
    pairwiseLLM:::bt_drift_metrics(current = current, baseline = baseline, ids = c("A", NA)),
    "ids"
  )

  testthat::expect_error(
    pairwiseLLM:::bt_drift_metrics(current = current, baseline = baseline, abs_shift_probs = c(0.2, 1.2)),
    "abs_shift_probs"
  )

  testthat::expect_error(
    pairwiseLLM:::bt_drift_metrics(current = current, baseline = baseline, methods = 1),
    "methods"
  )
})
