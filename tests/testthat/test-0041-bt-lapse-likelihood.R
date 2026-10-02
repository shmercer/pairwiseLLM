test_that("lapse probabilities and log likelihood match independent ordered formulas", {
  case <- lapse_case()
  kernel <- case$kernel
  par <- c(head(case$theta, -1L) - tail(case$theta, 1L), case$beta, case$epsilon)
  s <- pairwiseLLM:::.bt_lapse_surface(par, kernel)
  d <- kernel$counts
  p <- (1 - case$epsilon) * plogis(case$theta[d$first] - case$theta[d$second] + case$beta) + case$epsilon / 2
  expect_equal(s$probabilities, p, tolerance = 1e-14)
  expect_equal(s$value, -sum(d$wins * log(p) + d$losses * log1p(-p)), tolerance = 1e-12)
  expect_equal(s$gradient, rep(0, length(par)), tolerance = 1e-9)
  for (epsilon in c(0, 0.001, 0.2, 1)) {
    eta <- c(-1000, -5, 0, 5, 1000)
    expect_equal(pairwiseLLM:::.link_log_likelihood(eta, epsilon)$logp,
                 pairwiseLLM:::.link_e1_log_probability(eta, epsilon), tolerance = 1e-14)
    expect_true(all(is.finite(pairwiseLLM:::.link_e1_log_probability(eta, epsilon))))
  }
  stan <- readLines(system.file("stan", "btl_e_b.stan", package = "pairwiseLLM"))
  expect_true(any(grepl("theta[A[m]] - theta[B[m]] + beta", stan, fixed = TRUE)))
  expect_true(any(grepl("(1 - epsilon) * inv_logit(d) + epsilon * 0.5", stan, fixed = TRUE)))
})

test_that("complete natural and logit lapse derivatives agree with finite differences", {
  kernel <- lapse_case(seed = 30501)$kernel
  for (epsilon in c(0.01, 0.2, 0.7)) for (logit in c(FALSE, TRUE)) {
    par <- c(-2, 0.3, 1.7, -0.25, if (logit) qlogis(epsilon) else epsilon)
    surface <- if (logit) {
      function(x) pairwiseLLM:::.bt_lapse_objective(x, kernel)
    } else {
      function(x) pairwiseLLM:::.bt_lapse_surface(x, kernel)
    }
    h <- 1e-6
    steps <- diag(length(par)) * h
    gradient <- vapply(seq_along(par), function(j) {
      (surface(par + steps[, j])$value - surface(par - steps[, j])$value) / (2 * h)
    }, numeric(1))
    H <- vapply(seq_along(par), function(j) {
      (surface(par + steps[, j])$gradient - surface(par - steps[, j])$gradient) / (2 * h)
    }, numeric(length(par)))
    expect_equal(surface(par)$gradient, gradient, tolerance = 1e-7)
    expect_equal(unname(surface(par)$hessian), H, tolerance = 1e-7)
  }
})

test_that("ordered aggregation preserves orientation and uses the established centered map", {
  case <- lapse_case(seed = 30501)
  data <- lapse_binary_data(case)
  kernel <- pairwiseLLM:::.bt_lapse_design(data)
  expect_equal(kernel$counts, case$kernel$counts)
  expect_equal(kernel$transform, pairwiseLLM:::.bt_binary_design(data)$transform)
  expect_equal(nrow(kernel$counts), 12L)
  par <- c(-4, -2, -1, 0.3, 0.2)
  p <- (1 - par[5]) * plogis(kernel$transform[match(data$object1, kernel$ids), ] %*% par[1:3] -
                              kernel$transform[match(data$object2, kernel$ids), ] %*% par[1:3] + par[4]) + par[5] / 2
  expect_equal(pairwiseLLM:::.bt_lapse_surface(par, kernel)$value,
                 -sum(dbinom(data$result, 1, p, log = TRUE)), tolerance = 1e-12)
})

test_that("natural zero-boundary derivatives agree with one-sided differences", {
  kernel <- lapse_case(epsilon = 0, seed = 30501)$kernel
  par <- c(-4, -2.5, -1, 0.3, 0)
  surface <- function(x) pairwiseLLM:::.bt_lapse_surface(x, kernel)
  h <- 1e-6
  step <- c(rep(0, 4), h)
  s <- surface(par)
  score <- (-3 * s$value + 4 * surface(par + step)$value - surface(par + 2 * step)$value) / (2 * h)
  curvature <- (-3 * s$gradient + 4 * surface(par + step)$gradient -
                  surface(par + 2 * step)$gradient) / (2 * h)
  expect_equal(tail(s$gradient, 1L), score, tolerance = 1e-7)
  expect_equal(unname(s$hessian[, 5]), curvature, tolerance = 1e-7)
})

test_that("expected information is a symmetric weighted Gram matrix even at zero cross terms", {
  case <- lapse_case(8L, 0.3, 0.2, "cycle_chords")
  par <- c(head(case$theta, -1L) - tail(case$theta, 1L), case$beta, case$epsilon)
  kernel <- case$kernel
  eta <- as.vector(kernel$X %*% head(par, -1L))
  q <- plogis(eta)
  p <- (1 - case$epsilon) * q + case$epsilon / 2
  derivative <- cbind(kernel$X * ((1 - case$epsilon) * q * (1 - q)), 0.5 - q)
  oracle <- crossprod(derivative, derivative * ((kernel$counts$wins + kernel$counts$losses) / (p * (1 - p))))
  information <- pairwiseLLM:::.bt_lapse_information(par, kernel)
  expect_identical(information, t(information))
  expect_equal(unname(information), unname(oracle), tolerance = 1e-12)
  expect_true(pairwiseLLM:::.bt_alpha_matrix(information)$positive_definite)
})
