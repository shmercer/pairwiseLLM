test_that("E1 zero edges retain the exact prior and conditional covariance", {
  for (prior in list(list(mean = 0, sd = 5), list(mean = 2, sd = 3))) {
    args <- link_contract_args()
    args$control <- list(delta_prior = prior)
    input <- do.call(prepare_link_input, args)
    fit <- fit_link(input)
    expect_true(fit$diagnostics$fit_valid)
    expect_identical(fit$offset$delta_mean, prior$mean)
    expect_identical(fit$offset$delta_sd, prior$sd)
    expect_equal(unlist(fit$offset[c("delta_lower", "delta_upper")]),
      qnorm(c(.025, .975), prior$mean, prior$sd), ignore_attr = TRUE)
    expect_identical(fit$offset$identification, "prior_only")
    expect_equal(fit$diagnostics$quadrature$log_normalizer, 0, tolerance = 1e-12)
    V <- fit$uncertainty$item_transform %*% fit$uncertainty$covariance %*% t(fit$uncertainty$item_transform)
    expect_equal(V[3:4, 3:4], matrix(prior$sd^2, 2L, 2L))
    expect_true(all(V[1:2, ] == 0))
    expect_identical(fit$diagnostics$uncertainty_scope, "offset_only_conditional_on_fixed_shapes")
    expect_identical(fit$diagnostics$n_parameters, 1L)
    expect_identical(fit$diagnostics$hessian_pd, NA)
    expect_identical(fit$diagnostics$quadrature$domain, c(-Inf, Inf))
  }
})

test_that("E1 agrees with independent dense integration and preserves shapes", {
  for (args in list(link_contract_args(edges = 1L), link_e1_mixed_args())) {
    input <- do.call(prepare_link_input, args)
    oracle <- link_e1_grid(input)
    fit <- fit_link(input)
    expect_true(fit$diagnostics$fit_valid)
    expect_equal(fit$offset$delta_mean, oracle$mean, tolerance = 1e-7)
    expect_equal(fit$offset$delta_sd, oracle$sd, tolerance = 1e-7)
    expect_equal(c(fit$offset$delta_lower, fit$offset$delta_upper), oracle$interval, tolerance = 2e-6)
    expect_equal(fit$diagnostics$quadrature$log_normalizer, oracle$log_normalizer, tolerance = 1e-7)
    expect_equal(fit$items$theta_link_mean[1:2], unname(input$phase_a$hub$value))
    expect_equal(fit$items$theta_link_mean[3:4] - fit$offset$delta_mean, unname(input$phase_a$spoke$value))
    expect_equal(fit$items$theta_link_sd, c(0, 0, rep(fit$offset$delta_sd, 2)))
    expect_equal(fit$items$theta_link_lower, c(unname(input$phase_a$hub$value),
      unname(input$phase_a$spoke$value) + fit$offset$delta_lower))
    expect_identical(fit$provenance$counts$phase_a_hub, 0L)
    expect_identical(fit$provenance$counts$phase_a_spoke, 0L)
  }
})

test_that("E1 obeys sign, presentation, and permutation invariance", {
  args <- link_e1_mixed_args()
  args$control <- list(delta_prior = list(mean = 1.2, sd = 3))
  fit <- fit_link(do.call(prepare_link_input, args))
  sign <- args
  sign$phase_a <- lapply(sign$phase_a, function(x) list(points = -x$points))
  sign$judge$beta <- -sign$judge$beta
  sign$control$delta_prior$mean <- -sign$control$delta_prior$mean
  sign$cross$y_A <- 1L - sign$cross$y_A
  flipped <- fit_link(do.call(prepare_link_input, sign))
  expect_equal(flipped$items$theta_link_mean, -fit$items$theta_link_mean, tolerance = 1e-8)
  expect_equal(flipped$offset$delta_sd, fit$offset$delta_sd, tolerance = 1e-8)
  expect_equal(flipped$offset$delta_lower, -fit$offset$delta_upper, tolerance = 1e-7)
  reverse <- args
  reverse$cross[c("A_set", "A_item", "B_set", "B_item")] <- args$cross[c("B_set", "B_item", "A_set", "A_item")]
  reverse$cross$y_A <- 1L - args$cross$y_A
  reverse$judge$beta <- -args$judge$beta
  reversed <- fit_link(do.call(prepare_link_input, reverse))
  expect_equal(reversed$offset, fit$offset, tolerance = 1e-8)
  permuted <- args
  permuted$cross <- args$cross[8:1, ]
  permuted$hub$items <- args$hub$items[2:1, , drop = FALSE]
  permuted$phase_a$spoke$points <- rev(args$phase_a$spoke$points)
  reordered <- fit_link(do.call(prepare_link_input, permuted))
  expect_identical(reordered$offset, fit$offset)
  expect_identical(reordered$items, fit$items)
  expect_false(identical(reordered$provenance$hashes$cross, fit$provenance$hashes$cross))
})

test_that("posterior mass far into a prior tail agrees with the grid oracle", {
  args <- link_contract_args(edges = 2000L)
  args$judge$epsilon <- args$judge$beta <- 0
  args$phase_a$hub$points <- c(a = 40, b = -40)
  args$phase_a$spoke$points <- c(a = 0, b = 0)
  args$cross$y_A[] <- 0L
  input <- do.call(prepare_link_input, args)
  fit <- fit_link(input)
  oracle <- link_e1_grid(input, limits = c(-60, 80))
  expect_true(fit$diagnostics$fit_valid)
  expect_gt(fit$offset$delta_mean, 40)
  expect_equal(fit$offset$delta_mean, oracle$mean, tolerance = 1e-7)
  expect_equal(fit$offset$delta_sd, oracle$sd, tolerance = 1e-7)
  expect_equal(c(fit$offset$delta_lower, fit$offset$delta_upper), oracle$interval, tolerance = 2e-6)
})

test_that("legitimate repeated judgments update the likelihood once per row", {
  args <- link_contract_args(edges = 10L)
  input <- do.call(prepare_link_input, args)
  fit <- fit_link(input)
  oracle <- link_e1_grid(input)
  single <- fit_link(link_contract_input(edges = 1L))
  expect_equal(fit$offset$delta_mean, oracle$mean, tolerance = 1e-7)
  expect_lt(fit$offset$delta_mean, single$offset$delta_mean)
  expect_identical(fit$provenance$counts$cross, 10L)
  args$cross$observation_id[2] <- args$cross$observation_id[1]
  expect_error(do.call(prepare_link_input, args), "unique")
})

test_that("separated lapse-mixture modes are integrated rather than optimized away", {
  args <- link_contract_args(edges = 16L)
  args$phase_a$hub$points <- c(a = -12, b = 12)
  args$phase_a$spoke$points <- c(a = 0, b = 0)
  args$cross$A_item <- rep(c("a", "b"), each = 8L)
  args$cross$y_A <- rep(c(1L, 0L), each = 8L)
  args$judge$epsilon <- .1
  args$judge$beta <- 0
  input <- do.call(prepare_link_input, args)
  fit <- fit_link(input)
  oracle <- link_e1_grid(input)
  expect_true(fit$diagnostics$fit_valid)
  expect_equal(fit$offset$delta_mean, 0, tolerance = 1e-8)
  expect_equal(fit$offset$delta_sd, oracle$sd, tolerance = 1e-7)
  expect_equal(c(fit$offset$delta_lower, fit$offset$delta_upper), oracle$interval, tolerance = 2e-6)
  expect_gt(fit$offset$delta_sd, 10)
})

test_that("extreme evidence, lapse endpoints and singleton sets are supported", {
  args <- link_contract_args(edges = 2000L)
  args$hub$items <- args$spoke$items <- data.frame(item_id = "one")
  args$phase_a <- list(hub = list(points = c(one = 100)), spoke = list(points = c(one = -100)))
  args$cross$A_item <- args$cross$B_item <- "one"
  args$judge <- list(beta = 0, epsilon = 0, model_variant = "btl", link = "logit", source = "fixture")
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_true(fit$diagnostics$fit_valid)
  expect_lt(fit$offset$delta_mean, -7)
  expect_equal(fit$items$theta_link_mean, c(0, fit$offset$delta_mean))
  args$cross$y_A <- rep(c(0L, 1L), 1000L)
  balanced <- fit_link(do.call(prepare_link_input, args))
  expect_true(balanced$diagnostics$fit_valid)
  expect_equal(balanced$offset$delta_mean, 0, tolerance = 1e-10)
  expect_equal(balanced$offset$delta_sd, sqrt(4 / 2000), tolerance = 1e-3)
  args$judge$epsilon <- 1
  args$judge$model_variant <- "btl_e"
  lapse <- fit_link(do.call(prepare_link_input, args))
  expect_identical(lapse$offset$delta_mean, 0)
  expect_identical(lapse$offset$delta_sd, 5)
  expect_identical(lapse$offset$identification, "unidentified")
  expect_equal(lapse$diagnostics$quadrature$log_normalizer, -2000 * log(2))
})
