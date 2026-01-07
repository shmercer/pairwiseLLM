test_that("5514 run_summary uses last-resort judge fit details when summary extraction fails", {
  x <- list(
    run_type = "adaptive_core_linking",
    ids = c("A", "B"),
    results = tibble::tibble(
      ID1 = "A",
      ID2 = "B",
      better_id = "A",
      judge = "j1"
    ),
    final_fit = list(
      # mimic raw `sirt` output shape where fit_judges is top-level
      fit_judges = tibble::tibble(judge = "j1")
    )
  )

  out <- pairwiseLLM::run_summary(x)

  expect_true(is.list(out))
  expect_true(is.list(out$judge))
  expect_true(tibble::is_tibble(out$judge$fit$summary))
  expect_true(out$judge$fit$summary$has_judge_fit)

  # The minimal fit_judges table does not have infit/outfit columns, so it should be
  # returned via the last-resort tibble conversion path.
  expect_true(tibble::is_tibble(out$judge$fit$details))
  expect_true(all(c("judge") %in% names(out$judge$fit$details)))
})
