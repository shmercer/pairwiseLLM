test_that("319-01 summarize_bt_fit coerces non-numeric theta", {
  fit <- list(
    engine = "mock",
    theta = tibble::tibble(
      ID = c("a", "b"),
      theta = c("1.25", "-0.50"),
      se = c(0.1, 0.2)
    ),
    reliability = 0.5,
    diagnostics = list()
  )

  out <- pairwiseLLM::summarize_bt_fit(fit, verbose = FALSE)

  expect_true(is.numeric(out$theta))
  expect_equal(out$theta, c(1.25, -0.50))
})
