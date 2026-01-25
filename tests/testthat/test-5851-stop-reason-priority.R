testthat::test_that("candidate starvation sets stop_reason and blocks v3_converged", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  config <- pairwiseLLM:::adaptive_v3_config(state$N)

  out <- pairwiseLLM:::.adaptive_append_batch_log(
    state = state,
    iter = 1L,
    phase = "phase2",
    mode = "adaptive",
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    batch_size_target = 2L,
    selection = tibble::tibble(),
    candidate_stats = list(n_candidates_generated = 0L, n_candidates_after_filters = 0L),
    candidate_starved = TRUE,
    fallback_stage = "starved",
    W_used = config$W,
    config = config,
    exploration_only = FALSE,
    utilities = tibble::tibble(),
    iter_exit_path = "no_pairs_selected"
  )

  testthat::expect_identical(out$stop_reason, "candidate_starvation")
  testthat::expect_false(identical(out$stop_reason, "v3_converged"))
})

testthat::test_that("diagnostics failure takes precedence over v3_converged", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  state$config$v3 <- config
  state$repair_attempts <- config$repair_max_cycles

  fit <- make_v3_fit_contract(
    state$ids,
    theta_draws = matrix(seq_len(2L * state$N), nrow = 2L, ncol = state$N),
    diagnostics = list(divergences = 1L, max_rhat = 1.10, min_ess_bulk = 10)
  )

  out <- NULL
  testthat::expect_warning(
    {
      out <- pairwiseLLM:::.adaptive_apply_diagnostics_gate(
        state = state,
        fit = fit,
        config = config,
        near_stop = FALSE,
        refit_performed = TRUE
      )
    },
    "Diagnostics gate failed"
  )

  testthat::expect_identical(out$state$stop_reason, "diagnostics_failed")
  testthat::expect_false(identical(out$state$stop_reason, "v3_converged"))
})
