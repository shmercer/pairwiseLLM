test_that("zero cross edges preserve independent full bridges and the offset prior", {
  input <- link_e2_input(0)
  fit <- fit_link(input)
  oracle <- link_e2_oracle(input)
  expect_true(fit$diagnostics$fit_valid)
  expect_identical(fit$offset$identification, "prior_only")
  expect_equal(fit$continuation$mode, oracle$prior_mean, ignore_attr = TRUE)
  expect_equal(fit$uncertainty$covariance, oracle$prior_covariance, ignore_attr = TRUE)
  expect_equal(fit$offset$delta_sd, 5)
  expect_equal(fit$items$theta_link_mean, as.double(input$item_transform %*% oracle$prior_mean))
  args <- link_e2_args(0)
  args$control <- list(delta_prior = list(mean = 2, sd = 3))
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_equal(unlist(fit$offset[1:4]), c(2, 3, qnorm(c(.025, .975), 2, 3)), ignore_attr = TRUE)
})

test_that("E2 matches an independent Gaussian/Bernoulli objective and Laplace calculation", {
  input <- link_e2_input()
  oracle <- link_e2_oracle(input)
  fit <- fit_link(input)
  expect_true(fit$diagnostics$fit_valid)
  expect_equal(fit$continuation$mode, oracle$mode, tolerance = 2e-5, ignore_attr = TRUE)
  expect_equal(fit$diagnostics$optimization$objective, oracle$objective(fit$continuation$mode), tolerance = 1e-10)
  expect_equal(fit$uncertainty$covariance, oracle$covariance, tolerance = 2e-5, ignore_attr = TRUE)
  V <- input$item_transform %*% fit$uncertainty$covariance %*% t(input$item_transform)
  expect_equal(fit$items$theta_link_sd, sqrt(diag(V)))
  expect_equal(fit$items$theta_link_upper - fit$items$theta_link_mean,
    qnorm(.975) * sqrt(diag(V)))
  expect_equal(fit$diagnostics$uncertainty_scope, "joint_shapes_and_offset")
  expect_gt(max(abs(fit$uncertainty$covariance[1, -1])), .01)
  expect_identical(fit$provenance$hashes, input$hashes)
  expect_identical(fit$provenance$counts, input$counts)
  expect_lte(fit$diagnostics$optimization$gradient_max, 1e-6)
})

test_that("analytic derivatives match independent numerical differentiation", {
  input <- link_e2_input()
  bridges <- lapply(c("hub", "spoke"), function(k) {
    pairwiseLLM:::.link_e2_bridge(input$phase_a[[k]]$value, input$basis[[k]])
  })
  names(bridges) <- c("hub", "spoke")
  kernel <- pairwiseLLM:::.link_e2_kernel(input, bridges)
  for (epsilon in c(0, .15, 1)) {
    kernel$epsilon <- epsilon
    w <- c(.1, -.3, .2, .4, -.1)
    f <- function(w) pairwiseLLM:::.link_e2_objective(w, kernel)$value
    obj <- pairwiseLLM:::.link_e2_objective(w, kernel, TRUE)
    steps <- diag(1e-5, length(w))
    numerical <- vapply(seq_along(w), function(j) (f(w + steps[, j]) - f(w - steps[, j])) / 2e-5, numeric(1))
    expect_equal(obj$gradient, numerical, tolerance = 1e-7)
    expect_equal(obj$hessian, optimHess(w, f), tolerance = 1e-5)
    likelihood <- pairwiseLLM:::.link_e2_likelihood(c(-1e300, -1000, 0, 1000, 1e300), epsilon)
    expect_true(all(is.finite(unlist(likelihood))))
  }
})

test_that("both signs of bridge correlation affect the objective", {
  input <- link_e2_input(0)
  b <- lapply(c("hub", "spoke"), function(k) {
    pairwiseLLM:::.link_e2_bridge(input$phase_a[[k]]$value, input$basis[[k]])
  })
  names(b) <- c("hub", "spoke")
  point <- c(1, 1, -1, 1, .5)
  values <- vapply(c(-.6, 0, .6), function(rho) {
    covariance <- matrix(c(1, rho, rho, 1), 2)
    b$hub$lower <- t(chol(covariance))
    kernel <- pairwiseLLM:::.link_e2_kernel(input, b)
    w <- solve(kernel$lower, point - kernel$mean)
    pairwiseLLM:::.link_e2_objective(w, kernel)$value
  }, numeric(1))
  expect_gt(abs(values[1] - values[2]), .1)
  expect_gt(abs(values[3] - values[2]), .1)
  expect_gt(abs(values[1] - values[3]), .1)
})

test_that("item permutations, relabeling, and evidence permutations preserve E2", {
  args <- link_e2_args()
  base <- fit_link(do.call(prepare_link_input, args))
  args$hub$items <- args$hub$items[3:1, , drop = FALSE]
  args$phase_a$hub$draws <- args$phase_a$hub$draws[, 3:1]
  args$cross <- args$cross[8:1, ]
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_equal(fit$items, base$items)
  rename <- c(a = "z", b = "x", c = "y")
  for (k in c("hub", "spoke")) {
    args[[k]]$items$item_id <- unname(rename[args[[k]]$items$item_id])
    colnames(args$phase_a[[k]]$draws) <- unname(rename[colnames(args$phase_a[[k]]$draws)])
  }
  args$cross$A_item <- unname(rename[args$cross$A_item])
  args$cross$B_item <- unname(rename[args$cross$B_item])
  fit <- fit_link(do.call(prepare_link_input, args))
  order <- c(3, 1, 2, 6, 4, 5)
  expect_equal(fit$items$theta_link_mean[order], base$items$theta_link_mean, tolerance = 2e-6)
  expect_equal(fit$items$theta_link_sd[order], base$items$theta_link_sd, tolerance = 2e-6)
  expect_equal(fit$offset, base$offset, tolerance = 2e-6)
})

test_that("sign and presentation reversal respect the frozen positional judge", {
  args <- link_e2_args()
  base <- fit_link(do.call(prepare_link_input, args))
  args$phase_a <- lapply(args$phase_a, function(x) list(draws = -x$draws))
  args$judge$beta <- -args$judge$beta
  args$cross$y_A <- 1L - args$cross$y_A
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_equal(fit$items$theta_link_mean, -base$items$theta_link_mean, tolerance = 2e-6)
  expect_equal(fit$uncertainty$covariance, base$uncertainty$covariance, tolerance = 2e-6)
  expect_equal(fit$offset$delta_mean, -base$offset$delta_mean, tolerance = 2e-6)
  args <- link_e2_args()
  x <- args$cross
  args$cross$A_set <- x$B_set
  args$cross$B_set <- x$A_set
  args$cross$A_item <- x$B_item
  args$cross$B_item <- x$A_item
  args$cross$y_A <- 1L - x$y_A
  args$judge$beta <- -args$judge$beta
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_equal(fit$items, base$items, tolerance = 2e-6)
})

test_that("epsilon one and singleton sets retain honest uncertainty", {
  args <- link_e2_args()
  args$judge$epsilon <- 1
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_true(fit$diagnostics$fit_valid)
  expect_equal(fit$offset$delta_sd, 5)
  expect_identical(fit$offset$identification, "unidentified")
  expect_equal(predict_link(fit, args$cross[, -6]), rep(.5, 8))
  for (edges in c(0L, 1L)) {
    args <- link_contract_args("gaussian_posterior_bridge", edges)
    args$hub$items <- args$spoke$items <- data.frame(item_id = "one")
    args$phase_a <- list(hub = list(draws = matrix(c(1, 2), 2, dimnames = list(NULL, "one"))),
      spoke = list(draws = matrix(c(3, 4), 2, dimnames = list(NULL, "one"))))
    args$cross$A_item <- args$cross$B_item <- rep("one", edges)
    fit <- fit_link(do.call(prepare_link_input, args))
    expect_true(fit$diagnostics$fit_valid)
    expect_equal(fit$diagnostics$n_parameters, 1L)
    expect_equal(fit$items$theta_link_sd[1], 0)
    expect_equal(fit$items$theta_link_sd[2], fit$offset$delta_sd)
  }
})
