# Explicit opt-in: real optional CmdStan, synthetic data only, no providers.
test_that("real Stan E3 matches the R model and well-behaved Laplace posterior", {
  skip_if(Sys.getenv("PAIRWISELLM_TEST_E3_STAN") != "true", "set PAIRWISELLM_TEST_E3_STAN=true for real E3 Stan parity")
  skip_if_not_installed("cmdstanr")
  skip_if_not_installed("posterior")
  pairwiseLLM:::.btl_mcmc_require_cmdstanr()
  args <- link_e3_args(symmetric = TRUE)
  map <- fit_link(do.call(prepare_link_input, args))
  args$control <- list(estimator = list(engine = "mcmc", cmdstan = list(
    chains = 4L, parallel_chains = 2L, iter_warmup = 1000L, iter_sampling = 2000L,
    adapt_delta = .95, max_treedepth = 15L, seed = 278L, output_dir = withr::local_tempdir())))
  input <- do.call(prepare_link_input, args)
  sampled <- pairwiseLLM:::.link_e3_sample(pairwiseLLM:::.link_e3_stan_data(input),
    pairwiseLLM:::.link_e3_controls(input$control$estimator))
  local_mocked_bindings(.link_e3_sample = function(...) sampled, .package = "pairwiseLLM")
  fit <- fit_link(input)
  expect_true(fit$diagnostics$fit_valid, info = paste(fit$diagnostics$sampler$audit_gate$failed, collapse = ", "))
  expect_identical(fit$provenance$hashes[c("phase_a_hub", "phase_a_spoke", "cross", "judge")],
    map$provenance$hashes[c("phase_a_hub", "phase_a_spoke", "cross", "judge")])
  expect_identical(fit$uncertainty$basis, map$uncertainty$basis)
  expect_identical(colnames(fit$prediction$state$free_draws), colnames(input$item_transform))
  expect_equal(fit$items$theta_link_mean, map$items$theta_link_mean, tolerance = .025)
  expect_equal(fit$offset$delta_mean, map$offset$delta_mean, tolerance = .025)
  expect_true(all(fit$items$theta_link_sd / map$items$theta_link_sd > .9))
  expect_true(all(fit$items$theta_link_sd / map$items$theta_link_sd < 1.1))
  expect_lt(abs(fit$offset$delta_sd / map$offset$delta_sd - 1), .1)
  # Stan lp__ uses unnormalized densities. Compare differences, including all
  # three evidence blocks, to avoid depending on omitted Normal constants.
  free <- pairwiseLLM:::.link_e3_free_draws(sampled$fit, input)
  lp <- sampled$fit$draws(variables = "lp__", format = "draws_array")
  oracle <- link_e3_objective(input)
  at <- c(1L, 27L, 101L)
  r_logp <- vapply(at, function(i) -oracle(free[i, 1, ]), numeric(1))
  s_logp <- as.double(lp[at, 1, 1])
  expect_equal(r_logp - r_logp[1], s_logp - s_logp[1], tolerance = 2e-4)
  pairs <- args$cross[1:3, -6]
  expect_equal(predict_link(fit, pairs), predict_link(map, pairs), tolerance = .02)
  expect_equal(fit$items$theta_link_sd,
    apply(fit$prediction$state$free_draws %*% t(input$item_transform), 2, sd))
})

test_that("real Stan supports zero-edge and zero-dimensional shape blocks", {
  skip_if(Sys.getenv("PAIRWISELLM_TEST_E3_STAN") != "true", "set PAIRWISELLM_TEST_E3_STAN=true for real E3 Stan parity")
  skip_if_not_installed("cmdstanr")
  skip_if_not_installed("posterior")
  args <- link_e3_args(0L, symmetric = TRUE)
  args$hub$items <- data.frame(item_id = "one")
  args$phase_a$hub$observations <- args$phase_a$hub$observations[FALSE, ]
  args$control <- list(estimator = list(engine = "mcmc", cmdstan = list(
    chains = 4L, parallel_chains = 2L, iter_warmup = 1000L, iter_sampling = 2000L,
    adapt_delta = .95, max_treedepth = 15L, seed = 279L, output_dir = withr::local_tempdir())))
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_true(fit$diagnostics$fit_valid)
  expect_identical(fit$offset$identification, "prior_only")
  expect_equal(unlist(fit$offset[1:4]), c(0, 5, qnorm(c(.025, .975), 0, 5)), ignore_attr = TRUE)
  expect_equal(unname(fit$uncertainty$covariance[1, -1]), c(0, 0))
  expect_equal(fit$items$theta_link_sd[1], 0)
  expect_gt(sd(fit$prediction$state$free_draws[, 1]), 4.5)
})

test_that("real asymmetric E3 MCMC respects sign, presentation, and item relabeling", {
  skip_if(Sys.getenv("PAIRWISELLM_TEST_E3_STAN") != "true", "set PAIRWISELLM_TEST_E3_STAN=true for real E3 Stan parity")
  skip_if_not_installed("cmdstanr")
  skip_if_not_installed("posterior")
  args <- link_e3_args()
  args$control <- list(estimator = list(engine = "mcmc", cmdstan = list(
    chains = 4L, parallel_chains = 2L, iter_warmup = 1000L, iter_sampling = 2000L,
    adapt_delta = .95, max_treedepth = 15L, seed = 280L, output_dir = withr::local_tempdir())))
  sampler <- pairwiseLLM:::.link_e3_sample
  last_sampled <- NULL
  local_mocked_bindings(.link_e3_sample = function(...) {
    last_sampled <<- sampler(...)
    last_sampled
  }, .package = "pairwiseLLM")
  base_input <- do.call(prepare_link_input, args)
  base <- fit_link(base_input)
  expect_true(base$diagnostics$fit_valid)
  free <- pairwiseLLM:::.link_e3_free_draws(last_sampled$fit, base_input)
  lp <- last_sampled$fit$draws(variables = "lp__", format = "draws_array")
  oracle <- link_e3_objective(base_input)
  at <- c(1L, 37L, 91L)
  r_logp <- vapply(at, function(i) -oracle(free[i, 1, ]), numeric(1))
  s_logp <- as.double(lp[at, 1, 1])
  expect_true(all(abs((r_logp - r_logp[1]) - (s_logp - s_logp[1])) < 5e-4))
  for (change in c("sign", "presentation", "relabel")) {
    modified <- args
    rename <- c(a = "z", b = "x", c = "y")
    transform <- function(x) {
      if (change == "relabel") {
        x$A_item <- unname(rename[x$A_item])
        x$B_item <- unname(rename[x$B_item])
        return(x[nrow(x):1, ])
      }
      if (change == "presentation") {
        old <- x
        x$A_set <- old$B_set
        x$B_set <- old$A_set
        x$A_item <- old$B_item
        x$B_item <- old$A_item
      }
      x$y_A <- 1L - x$y_A
      x
    }
    if (change != "relabel") modified$judge$beta <- -args$judge$beta
    for (k in c("hub", "spoke")) {
      modified$phase_a[[k]]$observations <- transform(args$phase_a[[k]]$observations)
      if (change == "relabel") modified[[k]]$items$item_id <- unname(rename[args[[k]]$items$item_id])
    }
    modified$cross <- transform(args$cross)
    fit <- fit_link(do.call(prepare_link_input, modified))
    expect_true(fit$diagnostics$fit_valid, info = change)
    sign <- if (change == "sign") -1 else 1
    order <- if (change == "relabel") c(3, 1, 2, 6, 4, 5) else 1:6
    # Fixed absolute tolerances exceed the Monte Carlo errors of these 8,000-
    # draw, five-parameter fixtures; they do not assume Laplace accuracy here.
    expect_true(all(abs(fit$items$theta_link_mean[order] - sign * base$items$theta_link_mean) <= .06))
    expect_lte(abs(fit$offset$delta_mean - sign * base$offset$delta_mean), .06)
    expect_true(all(abs(fit$items$theta_link_sd[order] / base$items$theta_link_sd - 1) < .1))
  }
})

test_that("real singleton Stan fits support both lapse boundaries", {
  skip_if(Sys.getenv("PAIRWISELLM_TEST_E3_STAN") != "true", "set PAIRWISELLM_TEST_E3_STAN=true for real E3 Stan parity")
  skip_if_not_installed("cmdstanr")
  skip_if_not_installed("posterior")
  args <- link_e3_args()
  for (k in c("hub", "spoke")) {
    args[[k]]$items <- data.frame(item_id = "one")
    args$phase_a[[k]]$observations <- args$phase_a[[k]]$observations[FALSE, ]
  }
  args$cross$A_item <- args$cross$B_item <- "one"
  args$control <- list(estimator = list(engine = "mcmc", cmdstan = list(
    chains = 4L, parallel_chains = 2L, iter_warmup = 1000L, iter_sampling = 2000L,
    adapt_delta = .95, max_treedepth = 15L, seed = 281L, output_dir = withr::local_tempdir())))
  for (epsilon in c(0, 1)) {
    args$judge$epsilon <- epsilon
    input <- do.call(prepare_link_input, args)
    fit <- fit_link(input)
    expect_true(fit$diagnostics$fit_valid)
    expect_identical(colnames(fit$prediction$state$free_draws), "delta")
    expect_equal(fit$items$theta_link_sd[1], 0)
    expect_equal(fit$items$theta_link_sd[2], fit$offset$delta_sd)
    if (epsilon == 1) {
      expect_identical(fit$offset$identification, "unidentified")
      expect_equal(fit$offset$delta_sd, 5)
    } else {
      expect_identical(fit$offset$identification, "cross_set")
      expect_lt(fit$offset$delta_sd, 2)
    }
  }
})
