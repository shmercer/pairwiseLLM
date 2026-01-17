testthat::test_that("diagnostics failures trigger repair mode and exploration-only batches", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$phase <- "phase2"
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N)
  adaptive <- list(bins = 2L, mix_struct = 0.7, within_adj_split = 0.5, exploration_frac = 0.1)

  called <- new.env(parent = emptyenv())
  called$repair <- FALSE

  out <- NULL
  testthat::expect_warning(
    {
      out <- testthat::with_mocked_bindings(
        pairwiseLLM:::.adaptive_schedule_next_pairs(state, 1L, adaptive, seed = 1),
        .adaptive_get_refit_fit = function(state, adaptive, batch_size, seed) {
          list(
            state = state,
            fit = list(
              theta_mean = stats::setNames(c(0, 0, 0), state$ids),
              theta_draws = matrix(0, nrow = 2, ncol = 3, dimnames = list(NULL, state$ids)),
              diagnostics = list(divergences = 1L, max_rhat = 1.5, min_ess_bulk = 10)
            )
          )
        },
        diagnostics_gate_v3 = function(...) FALSE,
        phase1_generate_pairs = function(state, n_pairs, mix_struct, within_adj_split, bins, seed) {
          called$repair <- TRUE
          list(state = state, pairs = pairwiseLLM:::.adaptive_empty_pairs_tbl())
        },
        compute_ranking_from_theta_mean = function(...) testthat::fail("Unexpected adaptive ranking call."),
        select_window_size = function(...) testthat::fail("Unexpected adaptive window call."),
        build_candidate_pairs = function(...) testthat::fail("Unexpected adaptive candidate call."),
        select_pairs_from_candidates = function(...) testthat::fail("Unexpected adaptive selection call.")
      )
    },
    "Diagnostics gate failed; entering repair mode"
  )

  expect_true(called$repair)
  expect_equal(out$state$mode, "repair")
  expect_equal(out$state$repair_attempts, 1L)

  called$repair <- FALSE
  out2 <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_schedule_next_pairs(out$state, 1L, adaptive, seed = 1),
    .adaptive_get_refit_fit = function(state, adaptive, batch_size, seed) {
      list(
        state = state,
        fit = list(
          theta_mean = stats::setNames(c(0, 0, 0), state$ids),
          theta_draws = matrix(0, nrow = 2, ncol = 3, dimnames = list(NULL, state$ids)),
          diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 500)
        )
      )
    },
    diagnostics_gate_v3 = function(...) TRUE,
    compute_ranking_from_theta_mean = function(theta_mean, state) state$ids,
    select_window_size = function(N, phase, near_stop) 1L,
    build_candidate_pairs = function(...) tibble::tibble(
      i_id = character(),
      j_id = character(),
      unordered_key = character()
    ),
    phase1_generate_pairs = function(state, n_pairs, mix_struct, within_adj_split, bins, seed) {
      called$repair <- TRUE
      list(state = state, pairs = pairwiseLLM:::.adaptive_empty_pairs_tbl())
    }
  )

  expect_false(called$repair)
  expect_equal(out2$state$mode, "adaptive")
  expect_equal(out2$state$repair_attempts, 0L)
})
