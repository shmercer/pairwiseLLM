testthat::test_that("resume preserves refit state and timing", {
  withr::local_seed(123)

  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )

  make_state <- function() {
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
    state
  }

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
  make_pair_row <- function(state, A_id, B_id) {
    unordered_key <- make_unordered_key(A_id, B_id)
    ordered_key <- make_ordered_key(A_id, B_id)
    pair_uid <- pair_uid_from_state(state, unordered_key)
    tibble::tibble(
      pair_uid = pair_uid,
      unordered_key = unordered_key,
      ordered_key = ordered_key,
      A_id = A_id,
      B_id = B_id,
      A_text = state$texts[[A_id]],
      B_text = state$texts[[B_id]],
      phase = "phase2",
      iter = 1L,
      created_at = as.POSIXct("2024-01-01", tz = "UTC")
    )
  }

  mock_fit <- function(results, ids, n_draws, seed = NULL) {
    list(
      theta_mean = stats::setNames(rep(0, length(ids)), ids),
      theta_draws = matrix(0, nrow = 1L, ncol = length(ids), dimnames = list(NULL, ids)),
      diagnostics = NULL
    )
  }

  temp_dir <- withr::local_tempdir()
  state_path <- file.path(temp_dir, "adaptive_state.rds")

  calls_resume <- rlang::env(refits = 0L)
  mock_fit_resume <- function(results, ids, n_draws, seed = NULL) {
    calls_resume$refits <- calls_resume$refits + 1L
    mock_fit(results, ids, n_draws, seed = seed)
  }

  testthat::with_mocked_bindings(
    fit_bayes_btl_fast = mock_fit_resume,
    {
      state <- make_state()
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
      out <- .adaptive_get_refit_fit(
        state,
        adaptive = list(n_draws_fast = 10L),
        batch_size = 1L,
        seed = 1L
      )
      state <- out$state
      testthat::expect_equal(calls_resume$refits, 0L)

      state$history_pairs <- dplyr::bind_rows(
        make_pair_row(state, "A", "B"),
        make_pair_row(state, "A", "C")
      )
      state$comparisons_scheduled <- nrow(state$history_pairs)
      adaptive_state_save(state, state_path)
      state_loaded <- adaptive_state_load(state_path)
      testthat::expect_equal(state_loaded$new_since_refit, 2L)

      ingest <- .adaptive_ingest_results_incremental(
        state_loaded,
        make_result(state_loaded, "A", "D", "A")
      )
      state_loaded <- ingest$state
      out <- .adaptive_get_refit_fit(
        state_loaded,
        adaptive = list(n_draws_fast = 10L),
        batch_size = 1L,
        seed = 1L
      )
      state_loaded <- out$state

      testthat::expect_equal(calls_resume$refits, 1L)
      testthat::expect_equal(state_loaded$new_since_refit, 0L)
      testthat::expect_equal(state_loaded$last_refit_at, state_loaded$comparisons_observed)
    }
  )

  calls_continuous <- rlang::env(refits = 0L)
  mock_fit_continuous <- function(results, ids, n_draws, seed = NULL) {
    calls_continuous$refits <- calls_continuous$refits + 1L
    mock_fit(results, ids, n_draws, seed = seed)
  }

  testthat::with_mocked_bindings(
    fit_bayes_btl_fast = mock_fit_continuous,
    {
      state <- make_state()
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
      out <- .adaptive_get_refit_fit(
        state,
        adaptive = list(n_draws_fast = 10L),
        batch_size = 1L,
        seed = 1L
      )
      state <- out$state
      testthat::expect_equal(calls_continuous$refits, 0L)

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

      testthat::expect_equal(calls_continuous$refits, 1L)
      testthat::expect_equal(state$new_since_refit, 0L)
    }
  )
})
