testthat::test_that("stop metrics cover subset selection edge branches", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 12L)
  )
  state$posterior$diagnostics_pass <- TRUE

  draws <- matrix(
    c(
      2, 1, 0, -1,
      2.1, 0.9, 0.1, -1.1
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(NULL, state$ids)
  )
  fit <- list(
    theta_draws = draws,
    theta_mean = stats::setNames(colMeans(draws), state$ids)
  )

  config_small <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(S_subset = 1L, K_top = 1L, U_abs = 0.5)
  )
  metrics_small <- pairwiseLLM:::compute_stop_metrics(
    state,
    fit,
    tibble::tibble(utility = 0.1),
    config_small
  )
  testthat::expect_true(is.finite(metrics_small$theta_sd_median_S))

  testthat::with_mocked_bindings(
    {
      config_abort <- pairwiseLLM:::adaptive_v3_config(
        state$N,
        list(S_subset = 3L, K_top = 1L, U_abs = 0.5)
      )
      testthat::expect_error(
        pairwiseLLM:::compute_stop_metrics(
          state,
          fit,
          tibble::tibble(utility = 0.1),
          config_abort
        ),
        "Stopping subset size exceeded"
      )
    },
    floor = function(x) 2,
    .package = "base"
  )

  testthat::with_mocked_bindings(
    {
      config_zero <- pairwiseLLM:::adaptive_v3_config(
        state$N,
        list(S_subset = 2L, K_top = 1L, U_abs = 0.5)
      )
      testthat::expect_error(
        pairwiseLLM:::compute_stop_metrics(
          state,
          fit,
          tibble::tibble(utility = 0.1),
          config_zero
        ),
        "Stopping subset selection failed"
      )
    },
    floor = function(x) 1,
    .package = "base"
  )
})

testthat::test_that("stop metrics abort when subset ids miss target count", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 12L)
  )
  state$posterior$diagnostics_pass <- TRUE
  draws <- matrix(
    c(
      2, 1, 0, -1,
      2.1, 0.9, 0.1, -1.1
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(NULL, state$ids)
  )
  fit <- list(
    theta_draws = draws,
    theta_mean = stats::setNames(colMeans(draws), state$ids)
  )
  config_bad <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(S_subset = 3L, K_top = 1L, U_abs = 0.5)
  )

  base_unique <- base::unique
  testthat::with_mocked_bindings(
    {
      testthat::expect_error(
        pairwiseLLM:::compute_stop_metrics(
          state,
          fit,
          tibble::tibble(utility = 0.1),
          config_bad
        ),
        "Stopping subset selection failed"
      )
    },
    unique = function(x, ...) {
      if (length(x) == 3L) {
        return(x[1:2])
      }
      base_unique(x, ...)
    },
    .package = "base"
  )
})

testthat::test_that("stop metric config checks reject invalid min_new_pairs_for_check", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 4L)
  )
  state$posterior$diagnostics_pass <- TRUE
  draws <- matrix(c(0.1, -0.1, 0.2, -0.2), nrow = 2, byrow = TRUE)
  colnames(draws) <- state$ids
  fit <- list(
    theta_draws = draws,
    theta_mean = stats::setNames(colMeans(draws), state$ids)
  )
  config_bad <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(S_subset = 1L, K_top = 1L, U_abs = 0.5)
  )
  config_bad$min_new_pairs_for_check <- 0L

  testthat::expect_error(
    pairwiseLLM:::compute_stop_metrics(
      state,
      fit,
      tibble::tibble(utility = 0.1),
      config_bad
    ),
    "min_new_pairs_for_check"
  )
})

testthat::test_that("should_stop covers stopped mode and validation errors", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 4L)
  )
  config_v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list())
  state$mode <- "stopped"
  state$stop_reason <- "v3_converged"

  out <- pairwiseLLM:::should_stop(
    metrics = list(hard_cap_reached = FALSE, U0 = 0.1),
    state = state,
    config = config_v3
  )
  testthat::expect_true(out$stop_decision)

  state_active <- state
  state_active$mode <- "adaptive"

  state_bad <- state_active
  state_bad$mode <- "adaptive"
  state_bad$M1_target <- 1.5
  testthat::with_mocked_bindings(
    {
      testthat::expect_error(
        pairwiseLLM:::should_stop(
          metrics = list(hard_cap_reached = FALSE, U0 = 0.1, refit_performed = TRUE),
          state = state_bad,
          config = config_v3
        ),
        "M1_target"
      )
    },
    validate_state = function(state) invisible(state),
    .package = "pairwiseLLM"
  )

  config_bad <- config_v3
  config_bad$min_new_pairs_for_check <- 0L
  testthat::expect_error(
    pairwiseLLM:::should_stop(
      metrics = list(hard_cap_reached = FALSE, U0 = 0.1, refit_performed = TRUE),
      state = state_active,
      config = config_bad
    ),
    "min_new_pairs_for_check"
  )
})
