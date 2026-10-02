test_that("bootstrap bias correction reduces bias against an exact binomial oracle", {
  n <- 8L
  wins <- 0:n
  contrast <- log((wins + 1) / (n - wins + 1))
  expected <- vapply(plogis(contrast), function(p) sum(dbinom(wins, n, p) * contrast), numeric(1))
  exact_corrected <- 2 * contrast - expected
  weights <- dbinom(wins, n, plogis(1))
  raw_bias <- sum(weights * contrast) - 1
  oracle_bias <- sum(weights * exact_corrected) - 1
  corrected <- numeric(n + 1L)
  for (k in wins) {
    fit <- bootstrap_fit(k)
    expect_equal(diff(rev(fit$theta$theta)), contrast[k + 1L], tolerance = 1e-10)
    out <- bootstrap_bt_model(fit, "fixed", 512, 70304 + k)
    corrected[k + 1L] <- diff(rev(out$theta$theta_corrected))
    expect_lt(abs(2 * out$theta$bootstrap_mean[1] - expected[k + 1L]),
      6 * 2 * out$theta$mcse_bias[1] + 1e-10)
  }
  expect_equal(raw_bias, -0.1518532, tolerance = 1e-7)
  expect_lt(abs(oracle_bias), .01)
  expect_lt(abs(sum(weights * corrected) - 1), abs(raw_bias) / 2)
})

test_that("seed derivation is index based and stable at integer boundaries", {
  for (seed in c(0L, 1L, .Machine$integer.max)) {
    seeds <- lapply(c(1L, 2L, .Machine$integer.max), function(i) pairwiseLLM:::.bt_bootstrap_seeds(seed, i))
    expect_true(all(vapply(seeds, is.integer, logical(1))))
    expect_true(all(unlist(seeds) >= 1))
    expect_true(all(unlist(seeds) <= .Machine$integer.max))
    expect_length(unique(unlist(seeds)), 12L)
    expect_identical(seeds, lapply(c(1L, 2L, .Machine$integer.max),
      function(i) pairwiseLLM:::.bt_bootstrap_seeds(seed, i)))
  }
  expect_equal(unname(pairwiseLLM:::.bt_bootstrap_seeds(304L, 1L)),
    c(304011324L, 304011425L, 304011526L, 304011627L))
})
