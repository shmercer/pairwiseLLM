testthat::test_that("run accessors coerce data.frames and error on invalid inputs", {
  # Coercion: a base data.frame should be converted to a tibble
  run_df <- list(
    theta = data.frame(
      ID = c("A", "B"),
      theta = c(0.1, -0.1),
      stringsAsFactors = FALSE
    ),
    estimates = NULL,
    pairing_diagnostics = NULL
  )
  class(run_df) <- "pairwiseLLM_run"

  theta_tbl <- bt_get_theta(run_df)
  testthat::expect_s3_class(theta_tbl, "tbl_df")
  testthat::expect_identical(theta_tbl$ID, c("A", "B"))

  # Invalid run: must be list-like
  testthat::expect_error(
    bt_get_theta(1),
    "`run` must be a list-like run object.",
    fixed = TRUE
  )

  # Invalid field types should raise informative errors
  run_bad_theta <- list(theta = 1)
  class(run_bad_theta) <- "pairwiseLLM_run"
  testthat::expect_error(
    bt_get_theta(run_bad_theta),
    "`run$theta` must be a tibble/data.frame or NULL.",
    fixed = TRUE
  )

  run_bad_diag <- list(pairing_diagnostics = "oops")
  class(run_bad_diag) <- "pairwiseLLM_run"
  testthat::expect_error(
    bt_get_pairing_diagnostics(run_bad_diag),
    "`run$pairing_diagnostics` must be a tibble/data.frame or NULL.",
    fixed = TRUE
  )
})
