testthat::test_that("repair mode stops when diagnostics keep failing", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$phase <- "phase2"
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list(repair_max_cycles = 1L))
  state$repair_attempts <- 1L
  adaptive <- list(bins = 2L, mix_struct = 0.7, within_adj_split = 0.5)

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
    phase1_generate_pairs = function(...) testthat::fail("Repair batch should not be scheduled.")
  )

  expect_equal(out$state$mode, "stopped")
  expect_equal(out$state$stop_reason, "diagnostics_failed")
  expect_equal(nrow(out$pairs), 0L)
})
