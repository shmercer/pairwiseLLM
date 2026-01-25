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
    state$last_refit_at <- 0L
    state$new_since_refit <- 1L
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

testthat::test_that("compute_stop_metrics validates history and stability inputs", {
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
  fit <- make_v3_fit_contract(state$ids)

  bad_mean <- fit
  bad_mean$theta_mean[[1L]] <- NA_real_
  testthat::expect_error(
    compute_stop_metrics(
      state,
      fit = bad_mean,
      candidates_with_utility = tibble::tibble(),
      config = config_v3
    ),
    "theta_mean"
  )

  state_bad_history <- state
  state_bad_history$posterior$theta_mean_history <- "bad"
  testthat::expect_error(
    compute_stop_metrics(
      state_bad_history,
      fit = fit,
      candidates_with_utility = tibble::tibble(),
      config = config_v3
    ),
    "theta_mean_history"
  )

  config_bad_refits <- config_v3
  config_bad_refits$min_refits_for_stability <- 0L
  testthat::expect_error(
    compute_stop_metrics(
      state,
      fit = fit,
      candidates_with_utility = tibble::tibble(),
      config = config_bad_refits
    ),
    "min_refits_for_stability"
  )

  state$posterior$theta_mean_history <- list(stats::setNames(rep(0.1, state$N), state$ids))
  config_rank_bad <- config_v3
  config_rank_bad$min_refits_for_stability <- 1L
  config_rank_bad$stability_lag <- 1L
  config_rank_bad$rank_spearman_min <- NA_real_
  testthat::expect_error(
    compute_stop_metrics(
      state,
      fit = fit,
      candidates_with_utility = tibble::tibble(),
      config = config_rank_bad
    ),
    "rank_spearman_min"
  )

  lag_bad_names <- stats::setNames(rep(0.1, state$N), c("X", "Y", "Z"))
  state$posterior$theta_mean_history <- list(lag_bad_names)
  config_lag <- config_v3
  config_lag$min_refits_for_stability <- 1L
  config_lag$stability_lag <- 1L
  testthat::expect_error(
    compute_stop_metrics(
      state,
      fit = fit,
      candidates_with_utility = tibble::tibble(),
      config = config_lag
    ),
    "named over `state\\$ids`"
  )

  lag_bad_values <- stats::setNames(c(NA_real_, 0.1, 0.2), state$ids)
  state$posterior$theta_mean_history <- list(lag_bad_values)
  testthat::expect_error(
    compute_stop_metrics(
      state,
      fit = fit,
      candidates_with_utility = tibble::tibble(),
      config = config_lag
    ),
    "Lagged theta history must be finite"
  )
})

testthat::test_that("adaptive_update_theta_history requires fit summaries and list history", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- adaptive_state_new(samples, config = list(d1 = 2L))

  testthat::expect_error(
    pairwiseLLM:::.adaptive_update_theta_history(state, fit = list(theta_mean = c(0, 1))),
    "theta_mean"
  )

  fit <- make_v3_fit_contract(state$ids)
  state$posterior$theta_mean_history <- "bad"
  testthat::expect_error(
    pairwiseLLM:::.adaptive_update_theta_history(state, fit = fit),
    "theta_mean_history"
  )
})

testthat::test_that("should_stop validates state fields and config thresholds", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- adaptive_state_new(samples, config = list(d1 = 2L, M1_target = 1L, budget_max = 4L))
  config_v3 <- adaptive_v3_config(state$N)

  state$mode <- "stopped"
  out <- should_stop(list(), state, config_v3)
  testthat::expect_true(isTRUE(out$stop_decision))

  state$mode <- "adaptive"
  state$M1_target <- "bad"
  testthat::expect_error(
    should_stop(list(refit_performed = TRUE), state, config_v3),
    "M1_target"
  )

  state$M1_target <- 1L
  state$comparisons_observed <- 1L
  state$comparisons_scheduled <- 1L
  state$last_refit_at <- 0L
  state$new_since_refit <- 1L
  unordered_key <- make_unordered_key(state$ids[[1L]], state$ids[[2L]])
  ordered_key <- make_ordered_key(state$ids[[1L]], state$ids[[2L]])
  pair_uid <- pair_uid_from_state(state, unordered_key)
  state$history_pairs <- tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = state$ids[[1L]],
    B_id = state$ids[[2L]],
    A_text = state$texts[[state$ids[[1L]]]],
    B_text = state$texts[[state$ids[[2L]]]],
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2024-01-01", tz = "UTC")
  )
  state$history_results <- tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = state$ids[[1L]],
    B_id = state$ids[[2L]],
    better_id = state$ids[[1L]],
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2024-01-01", tz = "UTC"),
    backend = "test",
    model = "test"
  )
  state$posterior$theta_mean_history <- "bad"
  testthat::expect_error(
    should_stop(
      metrics = list(refit_performed = TRUE, diagnostics_pass = TRUE),
      state = state,
      config = config_v3
    ),
    "theta_mean_history"
  )

  state$posterior$theta_mean_history <- list(stats::setNames(c(0.1, 0.2), state$ids))
  config_bad_min <- config_v3
  config_bad_min$min_refits_for_stability <- 0L
  testthat::expect_error(
    should_stop(
      metrics = list(refit_performed = TRUE, diagnostics_pass = TRUE),
      state = state,
      config = config_bad_min
    ),
    "min_refits_for_stability"
  )

  config_bad_eap <- config_v3
  config_bad_eap$eap_reliability_min <- NA_real_
  testthat::expect_error(
    should_stop(
      metrics = list(
        refit_performed = TRUE,
        diagnostics_pass = TRUE,
        reliability_EAP = 0.9,
        rho_theta_lag = 0.9,
        delta_sd_theta_lag = 0.05,
        rank_stability_pass = TRUE
      ),
      state = state,
      config = config_bad_eap
    ),
    "eap_reliability_min"
  )

  config_bad_theta_sd <- config_v3
  config_bad_theta_sd$theta_sd_rel_change_max <- NA_real_
  testthat::expect_error(
    should_stop(
      metrics = list(
        refit_performed = TRUE,
        diagnostics_pass = TRUE,
        reliability_EAP = 0.9,
        rho_theta_lag = 0.9,
        delta_sd_theta_lag = 0.05,
        rank_stability_pass = TRUE
      ),
      state = state,
      config = config_bad_theta_sd
    ),
    "theta_sd_rel_change_max"
  )
})

