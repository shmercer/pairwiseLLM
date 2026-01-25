testthat::test_that("adaptive_run stopping checks cover fit/dx/empty candidates branches", {
  withr::local_seed(123)

  make_state_with_pair <- function(state) {
    A_id <- state$ids[[1]]
    B_id <- state$ids[[2]]
    unordered_key <- make_unordered_key(A_id, B_id)
    ordered_key <- make_ordered_key(A_id, B_id)
    pair_uid <- paste0(unordered_key, "#1")
    created_at <- as.POSIXct("2024-01-01", tz = "UTC")

    state$history_pairs <- tibble::tibble(
      pair_uid = pair_uid,
      unordered_key = unordered_key,
      ordered_key = ordered_key,
      A_id = A_id,
      B_id = B_id,
      A_text = state$texts[[A_id]],
      B_text = state$texts[[B_id]],
      phase = "phase2",
      iter = 0L,
      created_at = created_at
    )
    state$history_results <- tibble::tibble(
      pair_uid = pair_uid,
      unordered_key = unordered_key,
      ordered_key = ordered_key,
      A_id = A_id,
      B_id = B_id,
      better_id = A_id,
      winner_pos = 1L,
      phase = "phase2",
      iter = 0L,
      received_at = created_at,
      backend = "test",
      model = "test"
    )
    state$comparisons_scheduled <- 1L
    state$comparisons_observed <- 1L
    state
  }

  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 6L)
  )
  state$config$v3 <- adaptive_v3_config(state$N)
  state$phase <- "phase2"
  state$config$CW <- 1L
  state <- make_state_with_pair(state)

  out_no_draws <- testthat::with_mocked_bindings(
    .adaptive_run_stopping_checks(state, adaptive = list(), seed = 1),
    .adaptive_get_refit_fit = function(state, adaptive, batch_size, seed, allow_refit = TRUE) {
      list(state = state, fit = list(theta_mean = stats::setNames(rep(0, state$N), state$ids)))
    }
  )
  testthat::expect_false(identical(out_no_draws$state$mode, "stopped"))

  fit_with_diagnostics <- make_v3_fit_contract(
    state$ids,
    theta_draws = matrix(0, nrow = 2, ncol = state$N, dimnames = list(NULL, state$ids)),
    diagnostics = list(divergences = 0, max_rhat = 1.0, min_ess_bulk = 1000)
  )

  testthat::expect_silent(
    testthat::with_mocked_bindings(
      .adaptive_run_stopping_checks(state, adaptive = list(), seed = 1),
      .adaptive_get_refit_fit = function(state, adaptive, batch_size, seed, allow_refit = TRUE) {
        list(state = state, fit = fit_with_diagnostics)
      },
      generate_candidates = function(...) tibble::tibble(i = character(), j = character())
    )
  )
})

testthat::test_that("compute_stop_metrics validates inputs and should_stop validates metrics", {
  withr::local_seed(123)

  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 6L)
  )
  config_v3 <- adaptive_v3_config(state$N)
  state$config$v3 <- config_v3
  base_fit <- make_v3_fit_contract(state$ids)

  testthat::expect_error(
    compute_stop_metrics(state, fit = list(), candidates_with_utility = tibble::tibble(), config = config_v3),
    "theta_draws"
  )
  testthat::expect_error(
    compute_stop_metrics(
      state,
      fit = within(base_fit, theta_draws <- "bad"),
      candidates_with_utility = tibble::tibble(),
      config = config_v3
    ),
    "numeric matrix"
  )

  one_draw <- matrix(0, nrow = 1, ncol = state$N, dimnames = list(NULL, state$ids))
  testthat::expect_error(
    compute_stop_metrics(
      state,
      fit = within(base_fit, theta_draws <- one_draw),
      candidates_with_utility = tibble::tibble(),
      config = config_v3
    ),
    "at least two draws"
  )

  config_bad_lag <- config_v3
  config_bad_lag$stability_lag <- 0L
  testthat::expect_error(
    compute_stop_metrics(
      state,
      fit = base_fit,
      candidates_with_utility = tibble::tibble(),
      config = config_bad_lag
    ),
    "stability_lag"
  )

  testthat::expect_error(
    compute_stop_metrics(
      state,
      fit = base_fit,
      candidates_with_utility = list(),
      config = config_v3
    ),
    "data frame"
  )

  state$posterior$diagnostics_pass <- "bad"
  testthat::expect_error(
    suppressWarnings(compute_stop_metrics(
      state,
      fit = base_fit,
      candidates_with_utility = tibble::tibble(utility = 0.1),
      config = config_v3
    )),
    "diagnostics_pass"
  )

  testthat::expect_error(
    should_stop(metrics = "bad", state = state, config = config_v3),
    "metrics"
  )
})

