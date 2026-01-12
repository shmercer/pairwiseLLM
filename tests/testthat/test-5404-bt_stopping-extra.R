test_that("bt_stop_metrics covers defensive branches", {
  # Compatibility shim for fit$se as a data.frame
  fit <- list(
    theta = tibble::tibble(ID = c("A", "B"), theta = c(0, 1)),
    se = data.frame(ID = c("A", "B"), se = c(0.1, 0.2))
  )
  m <- pairwiseLLM::bt_stop_metrics(fit, stability_topk = 1L)
  expect_true(is.data.frame(m))

  # empty ids
  fit2 <- list(
    theta = tibble::tibble(ID = character(), theta = numeric()),
    se = tibble::tibble(ID = character(), se = numeric())
  )
  m2 <- pairwiseLLM::bt_stop_metrics(fit2)
  expect_equal(nrow(m2), 1L)

  # invalid stability_topk
  expect_error(pairwiseLLM::bt_stop_metrics(fit, stability_topk = 0L), "stability_topk")

  # fit$se missing required columns -> error
  fit_bad <- list(theta = fit$theta, se = tibble::tibble(foo = 1))
  expect_error(pairwiseLLM::bt_stop_metrics(fit_bad), "must contain")
})

test_that("bt_should_stop validates drift columns", {
  m <- tibble::tibble(
    reliability = 0.95,
    sepG = 3.1,
    item_misfit_prop = 0,
    judge_misfit_prop = 0,
    rel_se_p90 = 0.2
  )
  expect_error(pairwiseLLM::bt_should_stop(metrics = m, core_theta_cor_target = 0.90), "missing required column")
})
