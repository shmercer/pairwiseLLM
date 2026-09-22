test_that("prediction integrates the full Gaussian contrast deterministically", {
  input <- link_e2_input()
  fit <- fit_link(input)
  pairs <- input$cross[, -6]
  p <- predict_link(fit, pairs)
  expect_identical(p, predict_link(fit, pairs))
  expect_identical(p[8:1], predict_link(fit, pairs[8:1, ]))
  expect_identical(predict_link(fit, pairs[FALSE, ]), numeric())
  # Independent finite-grid Gaussian expectation; no production predictor.
  grid <- seq(-10, 10, length.out = 100001)
  X <- pairwiseLLM:::.link_e2_surface(pairs, input)
  mu <- as.double(X %*% fit$continuation$mode) + input$judge$beta
  variance <- diag(X %*% fit$uncertainty$covariance %*% t(X))
  expected <- vapply(seq_along(mu), function(i) {
    p <- input$judge$epsilon / 2 + (1 - input$judge$epsilon) * plogis(mu[i] + sqrt(variance[i]) * grid)
    sum(p * dnorm(grid)) * (grid[2] - grid[1])
  }, numeric(1))
  expect_equal(p, expected, tolerance = 1e-8)
  plugin <- input$judge$epsilon / 2 + (1 - input$judge$epsilon) * plogis(mu)
  expect_gt(max(abs(p - plugin)), .001)
  expect_identical(fit$prediction$state$method, "gaussian_contrast_quadrature")
  path <- tempfile(fileext = ".rds")
  withr::defer(unlink(path))
  saveRDS(fit, path)
  expect_identical(predict_link(readRDS(path), pairs), p)
})

test_that("prediction covers unseen legal pairs and oriented bias", {
  args <- link_e2_args()
  args$judge$beta <- 0
  input <- do.call(prepare_link_input, args)
  fit <- fit_link(input)
  pairs <- data.frame(observation_id = c("test-forward", "test-reverse"),
    A_set = c("H", "S"), A_item = c("c", "c"), B_set = c("S", "H"), B_item = c("c", "c"))
  p <- predict_link(fit, pairs)
  expect_equal(sum(p), 1)
  args$judge$beta <- .8
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_gt(sum(predict_link(fit, pairs)), 1)
})

test_that("Gaussian integration handles broad, extreme, and degenerate contrasts", {
  control <- pairwiseLLM:::.link_e2_controls(list())
  integrate <- pairwiseLLM:::.link_e2_integrate
  expect_equal(integrate(2, 0, control), plogis(2))
  expect_equal(integrate(1e200, 1, control), 1)
  expect_equal(integrate(-1e200, 1, control), 0)
  expect_equal(integrate(0, 1e10, control), .5)
  expect_equal(integrate(2e4, 1e4, control), pnorm(2), tolerance = 1e-7)
})

test_that("continuation refits original bridges and cumulative evidence exactly once", {
  old <- fit_link(link_e2_input(4))
  input <- link_e2_input(8)
  next_fit <- fit_link(input, previous = old)
  fresh <- fit_link(input)
  expect_identical(next_fit$items, fresh$items)
  expect_identical(next_fit$uncertainty, fresh$uncertainty)
  expect_identical(next_fit$prediction, fresh$prediction)
  expect_identical(next_fit$provenance$counts$cross, 8L)
  expect_identical(next_fit$provenance$counts$phase_a_hub, 0L)
  args <- link_e2_args()
  args$control <- list(initial = setNames(rep(50, 5), colnames(input$item_transform)))
  hinted <- fit_link(do.call(prepare_link_input, args))
  expect_identical(hinted$items, fresh$items)
  expect_identical(hinted$uncertainty, fresh$uncertainty)
})
