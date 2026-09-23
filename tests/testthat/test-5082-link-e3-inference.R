test_that("E3 MAP and Laplace covariance match an independent single-use objective", {
  input <- link_e3_input()
  fit <- fit_link(input)
  oracle <- link_e3_objective(input)
  ref <- optim(rep(0, ncol(input$item_transform)), oracle, method = "BFGS",
    control = list(reltol = 1e-12, maxit = 2000))
  expect_true(fit$diagnostics$fit_valid)
  expect_equal(unname(fit$continuation$mode), ref$par, tolerance = 2e-5)
  expect_equal(fit$diagnostics$optimization$objective, unname(oracle(fit$continuation$mode)), tolerance = 1e-10)
  expect_equal(unname(fit$uncertainty$covariance), solve(optimHess(ref$par, oracle)), tolerance = 2e-5)
  expect_equal(fit$diagnostics$optimization$free_hessian %*% fit$uncertainty$covariance,
    diag(ncol(input$item_transform)), ignore_attr = TRUE, tolerance = 1e-9)
  expect_equal(fit$items$theta_link_upper, fit$items$theta_link_mean + qnorm(.975) * fit$items$theta_link_sd)
  expect_lte(fit$diagnostics$optimization$gradient_max, 1e-6)
})

test_that("E3 derivatives, count reconciliation, and Stan evidence agree", {
  input <- link_e3_input()
  kernel <- pairwiseLLM:::.link_e3_kernel(input)
  w <- c(.2, -.3, .4, -.2, .1)
  q <- as.double(kernel$mean + kernel$lower %*% w)
  oracle <- link_e3_objective(input)
  objective <- function(w) oracle(as.double(kernel$mean + kernel$lower %*% w))
  obj <- pairwiseLLM:::.link_gaussian_objective(w, kernel, TRUE)
  step <- diag(1e-5, length(w))
  gradient <- vapply(seq_along(w), function(j) (objective(w + step[, j]) - objective(w - step[, j])) / 2e-5, numeric(1))
  expect_equal(obj$value, oracle(q), tolerance = 1e-10)
  expect_equal(obj$gradient, gradient, tolerance = 1e-7)
  expect_equal(obj$hessian, optimHess(w, objective), tolerance = 2e-5)
  data <- pairwiseLLM:::.link_e3_stan_data(input)
  theta <- as.double(input$item_transform %*% q)
  expect_equal(obj$value, -sum(dnorm(q[-1], log = TRUE)) - dnorm(q[1], 0, 5, log = TRUE) -
    sum(dbinom(data$Y, 1, .88 * plogis(theta[data$A] - theta[data$B] + .17) + .06, log = TRUE)))
  expect_equal(data$M, input$counts$phase_a_hub + input$counts$phase_a_spoke + input$counts$cross)
  expect_identical(data$H_H, unname(input$basis$hub$H))
  expect_identical(data$H_S, unname(input$basis$spoke$H))
  # Removing any one block changes the objective by exactly its likelihood.
  for (block in c("hub", "spoke", "cross")) {
    args <- link_e3_args()
    removed <- if (block == "cross") args$cross else args$phase_a[[block]]$observations
    if (block == "cross") args$cross <- removed[FALSE, ] else args$phase_a[[block]]$observations <- removed[FALSE, ]
    reduced <- do.call(prepare_link_input, args)
    empty <- args
    empty$cross <- empty$cross[FALSE, ]
    for (k in c("hub", "spoke")) empty$phase_a[[k]]$observations <- empty$phase_a[[k]]$observations[FALSE, ]
    only <- empty
    if (block == "cross") only$cross <- removed else only$phase_a[[block]]$observations <- removed
    contribution <- link_e3_objective(do.call(prepare_link_input, only))(q) -
      link_e3_objective(do.call(prepare_link_input, empty))(q)
    expect_equal(oracle(q) - link_e3_objective(reduced)(q), contribution, tolerance = 1e-10)
  }
})

test_that("zero-edge E3 fits shapes but preserves the exact independent offset prior", {
  args <- link_e3_args(0L)
  args$control <- list(delta_prior = list(mean = 2, sd = 3))
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_true(fit$diagnostics$fit_valid)
  expect_equal(unlist(fit$offset[1:4]), c(2, 3, qnorm(c(.025, .975), 2, 3)), ignore_attr = TRUE)
  expect_identical(fit$offset$identification, "prior_only")
  expect_equal(fit$uncertainty$covariance[1, -1], rep(0, 4), ignore_attr = TRUE)
  expect_equal(fit$uncertainty$covariance[2:3, 4:5], matrix(0, 2, 2), ignore_attr = TRUE)
  expect_gt(max(abs(fit$items$theta_link_mean[1:3])), .01)
  for (k in c("hub", "spoke")) args$phase_a[[k]]$observations <- args$phase_a[[k]]$observations[FALSE, ]
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_equal(fit$items$theta_link_mean, c(0, 0, 0, 2, 2, 2))
  expect_equal(unname(fit$uncertainty$covariance), diag(c(9, 1, 1, 1, 1)))
})

test_that("singleton sets and epsilon boundaries retain honest uncertainty", {
  for (epsilon in c(0, 1)) {
    args <- link_e3_args()
    args$judge$epsilon <- epsilon
    fit <- fit_link(do.call(prepare_link_input, args))
    expect_true(fit$diagnostics$fit_valid)
    if (epsilon == 1) {
      expect_equal(fit$offset$delta_sd, 5)
      expect_identical(fit$offset$identification, "unidentified")
      expect_equal(predict_link(fit, args$cross[, -6]), rep(.5, nrow(args$cross)))
    }
  }
  for (singleton in list("hub", "spoke", c("hub", "spoke"))) {
    args <- link_e3_args(0)
    for (k in singleton) {
      args[[k]]$items <- data.frame(item_id = "one")
      args$phase_a[[k]]$observations <- args$phase_a[[k]]$observations[FALSE, ]
    }
    fit <- fit_link(do.call(prepare_link_input, args))
    expect_true(fit$diagnostics$fit_valid)
    expect_equal(fit$offset$delta_sd, 5)
  }
})
