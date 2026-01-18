testthat::test_that("refit cadence follows refit_B threshold", {
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
  state$fast_fit <- list(
    theta_mean = stats::setNames(rep(0, state$N), state$ids),
    theta_draws = matrix(0, nrow = 1L, ncol = state$N, dimnames = list(NULL, state$ids)),
    diagnostics = NULL
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
  mock_fit <- function(results, ids, n_draws, seed = NULL) {
    calls$refits <- calls$refits + 1L
    list(
      theta_mean = stats::setNames(rep(0, length(ids)), ids),
      theta_draws = matrix(0, nrow = 1L, ncol = length(ids), dimnames = list(NULL, ids)),
      diagnostics = NULL
    )
  }

  testthat::with_mocked_bindings(
    fit_bayes_btl_fast = mock_fit,
    {
      ingest <- .adaptive_ingest_results_incremental(
        state,
        make_result(state, "A", "B", "A")
      )
      state <- ingest$state
      out <- .adaptive_get_refit_fit(
        state,
        adaptive = list(n_draws_fast = 10L),
        batch_size = 1L,
        seed = 1L
      )
      state <- out$state
      testthat::expect_equal(calls$refits, 0L)

      ingest <- .adaptive_ingest_results_incremental(
        state,
        make_result(state, "A", "C", "C")
      )
      state <- ingest$state
      out <- .adaptive_get_refit_fit(
        state,
        adaptive = list(n_draws_fast = 10L),
        batch_size = 1L,
        seed = 1L
      )
      state <- out$state
      testthat::expect_equal(calls$refits, 0L)

      ingest <- .adaptive_ingest_results_incremental(
        state,
        make_result(state, "A", "D", "A")
      )
      state <- ingest$state
      out <- .adaptive_get_refit_fit(
        state,
        adaptive = list(n_draws_fast = 10L),
        batch_size = 1L,
        seed = 1L
      )
      state <- out$state

      testthat::expect_equal(calls$refits, 1L)
      testthat::expect_equal(state$new_since_refit, 0L)
      testthat::expect_equal(state$last_refit_at, state$comparisons_observed)
    }
  )
})
