local_rebind_namespace <- function(ns, name, value) {
  env <- asNamespace(ns)
  old <- get(name, envir = env, inherits = FALSE)
  locked <- bindingIsLocked(name, env)
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
    assign(name, old, envir = env)
    if (locked) {
      lockBinding(name, env)
    }
  }
}

testthat::test_that("adaptive epsilon mean uses v3 contract validation", {
  ids <- c("a", "b")
  theta_draws <- matrix(c(0.1, 0.2, 0.2, 0.1), nrow = 2, ncol = 2)
  colnames(theta_draws) <- ids

  fit <- make_v3_fit_contract(
    ids,
    theta_draws = theta_draws,
    epsilon_draws = c(0.2, 0.2)
  )
  state <- list(ids = ids, config = list(v3 = list()))

  eps <- pairwiseLLM:::.adaptive_epsilon_mean_from_state(state, fit)
  testthat::expect_equal(eps, 0.2)
})

testthat::test_that("compute_pair_stats_from_draws rejects non-finite stats", {
  draws <- matrix(c(0.1, 0.2, 0.2, 0.1), nrow = 2, ncol = 2)
  colnames(draws) <- c("a", "b")
  candidates <- tibble::tibble(i_id = "a", j_id = "b")

  restore <- local_rebind_namespace("stats", "var", function(...) NA_real_)
  on.exit(restore(), add = TRUE)

  testthat::expect_error(
    pairwiseLLM:::compute_pair_stats_from_draws(draws, candidates),
    "Pair statistics"
  )
})

testthat::test_that("utility_delta_var_p rejects non-finite utilities", {
  restore <- local_rebind_namespace("stats", "plogis", function(...) NA_real_)
  on.exit(restore(), add = TRUE)

  testthat::expect_error(
    pairwiseLLM:::utility_delta_var_p(mean_d = 0.1, var_d = 0.2, epsilon_mean = 0.1),
    "Utility values must be finite"
  )
})

testthat::test_that("compute_pair_utility validates utility outputs", {
  draws <- matrix(c(0.1, 0.2, 0.2, 0.1), nrow = 2, ncol = 2)
  colnames(draws) <- c("a", "b")
  candidates <- tibble::tibble(i_id = "a", j_id = "b")

  restore <- local_rebind_namespace(
    "pairwiseLLM",
    "utility_delta_var_p",
    function(...) c(-1)
  )
  on.exit(restore(), add = TRUE)

  testthat::expect_error(
    pairwiseLLM:::compute_pair_utility(draws, candidates, epsilon_mean = 0.2),
    "non-negative"
  )
})

testthat::test_that("apply_degree_penalty validates utility table and ids", {
  samples <- tibble::tibble(ID = c("a", "b"), text = c("x", "y"))
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  testthat::expect_error(
    pairwiseLLM:::apply_degree_penalty(list(), state),
    "data frame"
  )

  bad_type <- tibble::tibble(i_id = "a", j_id = "b", utility_raw = "oops")
  testthat::expect_error(
    pairwiseLLM:::apply_degree_penalty(bad_type, state),
    "utility_raw"
  )

  bad_ids <- tibble::tibble(i_id = "a", j_id = "c", utility_raw = 0.1)
  testthat::expect_error(
    pairwiseLLM:::apply_degree_penalty(bad_ids, state),
    "state\\$ids"
  )
})

testthat::test_that("compute_stop_metrics validates v3 fits and utilities", {
  samples <- tibble::tibble(ID = c("a", "b", "c"), text = c("x", "y", "z"))
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$posterior$diagnostics_pass <- TRUE

  v3_config <- pairwiseLLM:::adaptive_v3_config(state$N)

  theta_draws <- matrix(
    c(0.1, 0.2, 0.3, 0.4, 0.2, 0.1),
    nrow = 2,
    ncol = 3
  )
  colnames(theta_draws) <- state$ids
  fit <- make_v3_fit_contract(
    state$ids,
    theta_draws = theta_draws,
    diagnostics = list(divergences = 0, max_rhat = 1.0, min_ess_bulk = 500)
  )

  utilities <- tibble::tibble(i_id = "a", j_id = "b", utility_raw = 0.01)
  metrics <- pairwiseLLM:::compute_stop_metrics(
    state = state,
    fit = fit,
    candidates_with_utility = utilities,
    config = v3_config
  )

  testthat::expect_true(is.list(metrics))
  testthat::expect_true(is.finite(metrics$U_top_median))
})

testthat::test_that("should_stop rejects non-integer M1_target", {
  samples <- tibble::tibble(ID = c("a", "b"), text = c("x", "y"))
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$M1_target <- 1.5

  metrics <- list(
    hard_cap_reached = FALSE,
    U0 = NA_real_,
    diagnostics_pass = FALSE,
    theta_sd_pass = FALSE,
    U_pass = FALSE
  )
  v3_config <- pairwiseLLM:::adaptive_v3_config(state$N)

  testthat::expect_error(
    pairwiseLLM:::should_stop(metrics, state, v3_config),
    "M1_target"
  )
})
