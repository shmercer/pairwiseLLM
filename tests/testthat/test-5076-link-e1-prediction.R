test_that("E1 predicts arbitrary oriented pairs by posterior integration", {
  input <- do.call(prepare_link_input, link_e1_mixed_args())
  fit <- fit_link(input)
  pairs <- input$cross[, -6L]
  oracle <- link_e1_grid(input, pairs = pairs)
  predicted <- predict_link(fit, pairs)
  expect_equal(predicted, oracle$prediction, tolerance = 1e-7)
  expect_true(all(is.finite(predicted) & predicted > 0 & predicted < 1))
  expect_identical(predict_link(fit, pairs[FALSE, ]), numeric())
  expect_identical(predict_link(fit, pairs[8:1, ]), rev(predicted))
  # Neither the fitted rows nor the posterior mean restrict the prediction API.
  pairs$A_item[1] <- "b"
  novel <- link_e1_grid(input, pairs = pairs)
  expect_equal(predict_link(fit, pairs), novel$prediction, tolerance = 1e-7)
  args <- link_contract_args(edges = 1L)
  args$judge$beta <- 0
  input <- do.call(prepare_link_input, args)
  fit <- fit_link(input)
  pair <- input$cross[, -6L]
  reverse <- pair
  reverse[c("A_set", "A_item", "B_set", "B_item")] <- pair[c("B_set", "B_item", "A_set", "A_item")]
  p <- predict_link(fit, pair)
  expect_equal(predict_link(fit, reverse), 1 - p, tolerance = 1e-9)
  plug_in <- .link_probability(fit$items$theta_link_mean[1], fit$items$theta_link_mean[4], input$judge)
  expect_gt(abs(p - plug_in), .01)
  args$judge$beta <- 1
  biased <- fit_link(do.call(prepare_link_input, args))
  expect_gt(abs(predict_link(biased, reverse) + predict_link(biased, pair) - 1), .01)
})

test_that("E1 predicts from the prior and refines prediction integration when needed", {
  args <- link_contract_args()
  # A diffuse offset prior makes the predictive sigmoid narrow in prior units.
  args$control <- list(delta_prior = list(mean = 0, sd = 100))
  fit <- fit_link(do.call(prepare_link_input, args))
  pair <- link_contract_input(edges = 1L)$cross[, -6L]
  before <- fit$prediction$state
  p <- predict_link(fit, pair)
  reference <- integrate(function(x) {
    (.9 * plogis(-1.3 - 100 * x) + .05) * dnorm(x)
  }, -Inf, Inf, rel.tol = 1e-11)$value
  expect_equal(p, reference, tolerance = 1e-9)
  expect_identical(fit$prediction$state, before)
  args$judge$epsilon <- 1
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_equal(predict_link(fit, pair), .5, tolerance = 1e-12)
})

test_that("E1 repeatability, RDS and continuation preserve cumulative evidence", {
  input <- do.call(prepare_link_input, link_e1_mixed_args())
  first <- fit_link(input)
  second <- fit_link(input)
  expect_identical(first$offset, second$offset)
  expect_identical(first$items, second$items)
  expect_identical(first$prediction, second$prediction)
  expect_identical(first$diagnostics$quadrature, second$diagnostics$quadrature)
  path <- file.path(withr::local_tempdir(), "e1.rds")
  saveRDS(first, path)
  restored <- readRDS(path)
  expect_identical(restored, first)
  pairs <- input$cross[, -6L]
  expect_identical(predict_link(restored, pairs), predict_link(first, pairs))
  args <- link_e1_mixed_args()
  args$cross <- args$cross[1:3, ]
  prefix <- fit_link(do.call(prepare_link_input, args))
  continued <- fit_link(input, prefix)
  expect_identical(continued$offset, first$offset)
  expect_identical(continued$prediction, first$prediction)
  expect_identical(continued$provenance$counts$cross, 8L)
  args <- link_e1_mixed_args()
  args$control <- list(initial = c(delta = 1000))
  hinted <- fit_link(do.call(prepare_link_input, args))
  expect_identical(hinted$offset, first$offset)
  args$cross$y_A[1] <- 1L - args$cross$y_A[1]
  expect_error(fit_link(do.call(prepare_link_input, args), prefix), "unchanged old evidence")
})

test_that("prediction rejects corrupted state and reports its own integration failures", {
  fit <- fit_link(link_contract_input())
  pairs <- link_contract_input(edges = 1L)$cross[, -6L]
  bad <- fit
  bad$prediction$state$panels[[1]]$x[1] <- 0
  expect_error(predict_link(bad, pairs), "state was modified")
  bad <- fit
  bad$prediction$state$controls$subdivisions <- 1L
  expect_error(predict_link(bad, pairs), "controls do not match")
  bad <- fit
  bad$prediction$state$log_normalizer <- 100
  expect_error(predict_link(bad, pairs), "normalizer does not match")
  bad <- fit
  bad$prediction$state$method <- "plugin"
  expect_error(predict_link(bad, pairs), "serialized quadrature")
  local_mocked_bindings(.link_e1_quadrature = function(...) {
    .link_e1_fail("prediction_integration_failure", "forced prediction integration failure")
  }, .package = "pairwiseLLM")
  expect_error(predict_link(fit, pairs), class = "pairwiseLLM_e1_numerical_error")
})
