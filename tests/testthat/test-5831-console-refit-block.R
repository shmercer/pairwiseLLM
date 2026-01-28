testthat::test_that("refit progress block includes required labels", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(progress = TRUE, progress_every_refit = 1L, progress_level = "refit")
  )
  state$phase <- "phase2"
  state$batch_log <- tibble::tibble(iter = 3L, phase = "phase2")

  round_row <- tibble::tibble(
    round_id = 1L,
    iter_at_refit = 1L,
    total_pairs = 3L,
    new_pairs = 2L,
    scheduled_pairs = 10L,
    completed_pairs = 8L,
    backlog_unjudged = 2L,
    divergences = 0L,
    max_rhat = 1.01,
    min_ess_bulk = 1200,
    epsilon_mean = 0.09,
    reliability_EAP = 0.87,
    eap_pass = FALSE,
    diagnostics_pass = TRUE,
    theta_sd_eap = 0.48,
    rho_theta_lag = 0.99,
    theta_corr_pass = TRUE,
    delta_sd_theta_lag = 0.01,
    rho_rank_lag = 0.99,
    rho_rank_pass = TRUE,
    rank_stability_pass = TRUE,
    lag_eligible = TRUE,
    stop_decision = FALSE,
    stop_reason = NA_character_,
    n_unique_pairs_seen = 3L
  )

  out <- capture.output({
    pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row)
  })

  testthat::expect_true(any(grepl("GATES", out)))
  testthat::expect_true(any(grepl("LAG", out)))
  testthat::expect_true(any(grepl("STOP", out)))
  testthat::expect_true(any(grepl("diagnostics_pass", out)))
  testthat::expect_true(any(grepl("eap_pass", out)))
})

testthat::test_that("refit progress output omits removed labels", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(progress = TRUE, progress_every_refit = 1L, progress_level = "refit")
  )
  state$batch_log <- tibble::tibble(iter = 1L, phase = "phase1")

  round_row <- tibble::tibble(
    round_id = 1L,
    iter_at_refit = 1L,
    total_pairs = 3L,
    new_pairs = 1L,
    diagnostics_pass = TRUE,
    divergences = 0L,
    max_rhat = 1.01,
    min_ess_bulk = 1200,
    reliability_EAP = 0.9,
    eap_pass = TRUE,
    lag_eligible = FALSE,
    stop_decision = FALSE,
    stop_reason = NA_character_
  )

  out <- capture.output({
    pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row)
  })
  text <- paste(out, collapse = "\n")

  testthat::expect_false(grepl("stop_passes", text))
  testthat::expect_false(grepl("stop_eligible", text))
  testthat::expect_false(grepl("item_summary", text))
})

testthat::test_that("refit progress maps converged stop reason to all_gates_passed", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(progress = TRUE, progress_every_refit = 1L, progress_level = "refit")
  )
  state$batch_log <- tibble::tibble(iter = 2L, phase = "phase1")

  round_row <- tibble::tibble(
    round_id = 1L,
    iter_at_refit = 2L,
    total_pairs = 3L,
    new_pairs = 1L,
    diagnostics_pass = TRUE,
    divergences = 0L,
    max_rhat = 1.01,
    min_ess_bulk = 1200,
    reliability_EAP = 0.9,
    eap_pass = TRUE,
    lag_eligible = TRUE,
    stop_decision = TRUE,
    stop_reason = "v3_converged"
  )

  out <- capture.output({
    pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row)
  })
  text <- paste(out, collapse = "\n")

  testthat::expect_true(grepl("stop_reason   : all_gates_passed", text, fixed = TRUE))
})
