testthat::test_that("no mid-batch refit occurs before batch completion", {
  withr::local_seed(123)

  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- adaptive_state_new(
    samples = samples,
    config = list(d1 = 2L, M1_target = 2L, budget_max = 6L)
  )
  state$config$v3 <- adaptive_v3_config(state$N, list(refit_B = 3L))
  state$fit <- list(
    theta_mean = stats::setNames(rep(0, state$N), state$ids),
    theta_draws = matrix(0, nrow = 2L, ncol = state$N, dimnames = list(NULL, state$ids)),
    epsilon_mean = 0.1,
    diagnostics = list()
  )

  make_result <- function(state, A_id, B_id, better_id) {
    unordered_key <- make_unordered_key(A_id, B_id)
    ordered_key <- make_ordered_key(A_id, B_id)
    pair_uid <- pair_uid_from_state(state, unordered_key)
    winner_pos <- if (better_id == A_id) 1L else 2L
    tibble::tibble(
      pair_uid = pair_uid,
      unordered_key = unordered_key,
      ordered_key = ordered_key,
      A_id = A_id,
      B_id = B_id,
      better_id = better_id,
      winner_pos = as.integer(winner_pos),
      phase = "phase2",
      iter = 1L,
      received_at = as.POSIXct("2024-01-01", tz = "UTC"),
      backend = "mock",
      model = "mock"
    )
  }

  calls <- rlang::env(refits = 0L)
  make_mcmc_fit <- function(ids) {
    theta_draws <- matrix(0, nrow = 2L, ncol = length(ids), dimnames = list(NULL, ids))
    list(
      draws = list(theta = theta_draws),
      theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
      epsilon_summary = tibble::tibble(epsilon_mean = 0.1),
      diagnostics = list()
    )
  }
  mock_fit <- function(bt_data, config, seed = NULL) {
    calls$refits <- calls$refits + 1L
    force(config)
    force(seed)
    ids <- bt_data$item_id %||% state$ids
    make_mcmc_fit(ids)
  }

  testthat::with_mocked_bindings(
    .fit_bayes_btl_mcmc_adaptive = mock_fit,
    {
      ingest <- .adaptive_ingest_results_incremental(
        state,
        make_result(state, "A", "B", "A")
      )
      state <- ingest$state
      ingest <- .adaptive_ingest_results_incremental(
        state,
        make_result(state, "A", "C", "C")
      )
      state <- ingest$state
      ingest <- .adaptive_ingest_results_incremental(
        state,
        make_result(state, "A", "D", "A")
      )
      state <- ingest$state

      out <- .adaptive_get_refit_fit(
        state,
        adaptive = list(),
        batch_size = 3L,
        seed = 1L,
        allow_refit = FALSE
      )
      state <- out$state
      testthat::expect_equal(calls$refits, 0L)
      testthat::expect_equal(state$new_since_refit, 3L)

      out <- .adaptive_get_refit_fit(
        state,
        adaptive = list(),
        batch_size = 3L,
        seed = 1L,
        allow_refit = TRUE
      )
      state <- out$state
      testthat::expect_equal(calls$refits, 1L)
      testthat::expect_equal(state$new_since_refit, 0L)
    }
  )
})
