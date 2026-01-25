testthat::test_that("summarize_draws computes epsilon percentiles from posterior draws", {
  draws <- list(
    theta = matrix(
      c(0.1, 0.2, -0.1,
        0.2, 0.1, -0.2,
        0.0, 0.3, -0.1),
      nrow = 3,
      byrow = TRUE
    ),
    epsilon = c(0.05, 0.1, 0.2, 0.3)
  )
  colnames(draws$theta) <- c("A", "B", "C")

  out <- pairwiseLLM:::summarize_draws(draws)
  eps_probs <- stats::quantile(draws$epsilon, probs = c(0.025, 0.05, 0.5, 0.95, 0.975), names = FALSE)

  testthat::expect_equal(out$epsilon_summary$epsilon_mean, mean(draws$epsilon))
  testthat::expect_equal(out$epsilon_summary$epsilon_p2.5, eps_probs[[1L]])
  testthat::expect_equal(out$epsilon_summary$epsilon_p5, eps_probs[[2L]])
  testthat::expect_equal(out$epsilon_summary$epsilon_p50, eps_probs[[3L]])
  testthat::expect_equal(out$epsilon_summary$epsilon_p95, eps_probs[[4L]])
  testthat::expect_equal(out$epsilon_summary$epsilon_p97.5, eps_probs[[5L]])
  testthat::expect_false(isTRUE(all.equal(out$epsilon_summary$epsilon_mean, 2 / 22)))
})

local_rebind_namespace <- function(ns, name, value) {
  env <- asNamespace(ns)
  has_old <- exists(name, envir = env, inherits = FALSE)
  old <- if (has_old) get(name, envir = env, inherits = FALSE) else NULL
  locked <- if (has_old) bindingIsLocked(name, env) else FALSE
  if (locked) {
    unlockBinding(name, env)
  }
  assign(name, value, envir = env)
  if (locked) {
    lockBinding(name, env)
  }
  function() {
    if (locked) {
      unlockBinding(name, env)
    }
    if (has_old) {
      assign(name, old, envir = env)
    } else if (exists(name, envir = env, inherits = FALSE)) {
      rm(list = name, envir = env)
    }
    if (locked) {
      lockBinding(name, env)
    }
  }
}

testthat::test_that("round log uses epsilon percentiles and leaves b fields NA when absent", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list())

  epsilon_draws <- c(0.08, 0.1, 0.12, 0.14)
  eps_probs <- stats::quantile(
    epsilon_draws,
    probs = c(0.025, 0.05, 0.5, 0.95, 0.975),
    names = FALSE
  )
  fit <- list(
    theta_draws = matrix(
      c(0.1, 0.0, -0.1,
        0.2, -0.1, -0.1),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, state$ids)
    ),
    epsilon_summary = tibble::tibble(
      epsilon_mean = mean(epsilon_draws),
      epsilon_p2.5 = eps_probs[[1L]],
      epsilon_p5 = eps_probs[[2L]],
      epsilon_p50 = eps_probs[[3L]],
      epsilon_p95 = eps_probs[[4L]],
      epsilon_p97.5 = eps_probs[[5L]]
    )
  )
  metrics <- list(
    theta_sd_median_S = 0.1,
    tau = 0.2,
    theta_sd_pass = TRUE,
    U0 = 0.5,
    U_pass = TRUE,
    diagnostics_pass = TRUE
  )

  row <- pairwiseLLM:::build_round_log_row(
    state = state,
    fit = fit,
    metrics = metrics,
    stop_out = list(stop_decision = FALSE, stop_reason = NA_character_),
    config = state$config$v3
  )

  testthat::expect_equal(row$epsilon_mean, mean(epsilon_draws))
  testthat::expect_equal(row$epsilon_p50, eps_probs[[3L]])
  b_vals <- c(row$b_mean, row$b_p2.5, row$b_p5, row$b_p50, row$b_p95, row$b_p97.5)
  testthat::expect_true(all(is.na(b_vals)))
})

testthat::test_that("refit updates state epsilon mean from fit contract", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list())

  epsilon_draws <- c(0.08, 0.12, 0.14)
  eps_probs <- stats::quantile(
    epsilon_draws,
    probs = c(0.025, 0.05, 0.5, 0.95, 0.975),
    names = FALSE
  )
  theta_draws <- matrix(
    c(0.1, -0.1, 0.2, -0.2),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(NULL, state$ids)
  )
  mcmc_fit <- list(
    draws = list(theta = theta_draws, epsilon = epsilon_draws),
    theta_summary = tibble::tibble(item_id = state$ids, theta_mean = c(0.1, 0.2)),
    epsilon_summary = tibble::tibble(
      epsilon_mean = mean(epsilon_draws),
      epsilon_p2.5 = eps_probs[[1L]],
      epsilon_p5 = eps_probs[[2L]],
      epsilon_p50 = eps_probs[[3L]],
      epsilon_p95 = eps_probs[[4L]],
      epsilon_p97.5 = eps_probs[[5L]]
    ),
    diagnostics = list(max_rhat = 1.01),
    mcmc_config_used = list(chains = 2L)
  )

  restore <- local_rebind_namespace(
    "pairwiseLLM",
    ".fit_bayes_btl_mcmc_adaptive",
    function(...) mcmc_fit
  )
  on.exit(restore(), add = TRUE)
  out <- pairwiseLLM:::.adaptive_get_refit_fit(
    state = state,
    adaptive = list(refit_B = 1L),
    batch_size = 1L,
    seed = 1,
    allow_refit = TRUE
  )

  testthat::expect_equal(out$state$posterior$epsilon_mean, mean(epsilon_draws))
})
