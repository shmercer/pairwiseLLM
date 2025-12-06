test_that("summarize_bt_fit returns a tidy tibble with expected columns", {
  skip_if_not_installed("sirt")

  data("example_writing_pairs", package = "pairwiseLLM")
  bt <- build_bt_data(example_writing_pairs)

  fit <- fit_bt_model(bt, engine = "sirt")
  summary_tbl <- suppressWarnings(
    summarize_bt_fit(fit)
  )

  expect_s3_class(summary_tbl, "tbl_df")

  expected_cols <- c("ID", "theta", "se", "rank", "engine", "reliability")
  expect_true(all(expected_cols %in% names(summary_tbl)))

  expect_equal(nrow(summary_tbl), nrow(fit$theta))
  expect_setequal(summary_tbl$ID, fit$theta$ID)

  expect_type(summary_tbl$theta, "double")
  expect_type(summary_tbl$se, "double")
  expect_type(summary_tbl$rank, "integer")
  expect_type(summary_tbl$engine, "character")
  expect_type(summary_tbl$reliability, "double")

  expect_true(all(summary_tbl$engine == fit$engine))
  expect_true(all(summary_tbl$reliability == fit$reliability))

  finite_idx <- which(is.finite(summary_tbl$theta))
  finite_ranks <- summary_tbl$rank[finite_idx]
  expect_setequal(finite_ranks, seq_along(finite_ranks))
})

test_that("summarize_bt_fit works with BradleyTerry2 engine as well", {
  skip_if_not_installed("BradleyTerry2")

  data("example_writing_pairs", package = "pairwiseLLM")
  bt <- build_bt_data(example_writing_pairs)

  fit <- suppressWarnings(
    fit_bt_model(bt, engine = "BradleyTerry2")
  )
  summary_tbl <- suppressWarnings(
    summarize_bt_fit(fit)
  )

  expect_s3_class(summary_tbl, "tbl_df")
  expected_cols <- c("ID", "theta", "se", "rank", "engine", "reliability")
  expect_true(all(expected_cols %in% names(summary_tbl)))

  expect_equal(nrow(summary_tbl), nrow(fit$theta))
  expect_setequal(summary_tbl$ID, fit$theta$ID)

  expect_true(all(is.na(summary_tbl$reliability)))
  expect_true(all(summary_tbl$engine == "BradleyTerry2"))
})

test_that("summarize_bt_fit errors cleanly on malformed input", {
  bad_fit1 <- list() # no theta
  expect_error(
    summarize_bt_fit(bad_fit1)
  )

  bad_fit2 <- list(theta = tibble::tibble(ID = "A", theta = 0.1))
  expect_error(
    summarize_bt_fit(bad_fit2),
    "must contain columns: ID, theta, se",
    fixed = TRUE
  )
})
