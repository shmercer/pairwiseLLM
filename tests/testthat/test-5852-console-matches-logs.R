testthat::test_that("iteration console line matches batch_log values", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  config$progress <- TRUE
  config$progress_every_iter <- 1L
  state$config$v3 <- config

  state$comparisons_scheduled <- 2L
  state$comparisons_observed <- 1L
  state$log_counters$comparisons_observed <- 0L
  state$log_counters$failed_attempts <- 0L

  selection <- tibble::tibble(
    utility = c(0.2, 0.1),
    is_explore = c(TRUE, FALSE)
  )
  utilities <- tibble::tibble(utility = c(0.05, 0.1, 0.15))

  output <- capture.output(
    state <- pairwiseLLM:::.adaptive_append_batch_log(
      state = state,
      iter = 1L,
      phase = "phase2",
      mode = "adaptive",
      created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      batch_size_target = 3L,
      selection = selection,
      candidate_stats = list(n_candidates_generated = 3L, n_candidates_after_filters = 3L),
      candidate_starved = TRUE,
      fallback_stage = "starved",
      fallback_used = "expand_2x",
      W_used = config$W,
      config = config,
      exploration_only = FALSE,
      utilities = utilities,
      iter_exit_path = NULL
    )
  )

  line1 <- output[[length(output) - 1L]]
  line2 <- output[[length(output)]]
  batch_row <- state$batch_log[nrow(state$batch_log), , drop = FALSE]

  testthat::expect_true(grepl("scheduled 2", line1))
  testthat::expect_true(grepl("completed 1", line1))
  testthat::expect_true(grepl("backlog 1", line1))
  testthat::expect_true(grepl("unique 0", line1))
  testthat::expect_true(grepl("failed 0", line1))
  testthat::expect_true(grepl("\\+sel 2", line2))
  testthat::expect_true(grepl("\\+done 1", line2))
  testthat::expect_true(grepl("cand 3", line2))
  testthat::expect_true(grepl("mode adaptive", line2))
  testthat::expect_true(grepl("safe FALSE", line2))
  testthat::expect_identical(batch_row$candidate_starved[[1L]], TRUE)
})

testthat::test_that("refit console block matches round_log gate values", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$phase <- "phase2"
  state$posterior$diagnostics_pass <- TRUE

  config <- pairwiseLLM:::adaptive_v3_config(state$N)
  config$progress <- TRUE
  config$progress_every_refit <- 1L
  config$progress_level <- "refit"
  state$config$v3 <- config

  theta_draws <- matrix(0, nrow = 2L, ncol = state$N)
  colnames(theta_draws) <- state$ids
  fit <- make_v3_fit_contract(
    state$ids,
    theta_draws = theta_draws,
    diagnostics = list(divergences = 0L, max_rhat = 1.0, min_ess_bulk = 500)
  )

  utilities <- tibble::tibble(utility = c(0.001, 0.002, 0.0015))
  metrics <- pairwiseLLM:::compute_stop_metrics(
    state = state,
    fit = fit,
    candidates_with_utility = utilities,
    config = config
  )
  state$posterior$stop_metrics <- metrics
  round_row <- pairwiseLLM:::build_round_log_row(
    state = state,
    fit = fit,
    metrics = metrics,
    stop_out = list(stop_decision = FALSE, stop_reason = NA_character_),
    config = config
  )
  state$batch_log <- tibble::tibble(iter = state$iter, phase = state$phase)

  output <- capture.output(
    pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row, config)
  )
  text <- paste(output, collapse = "\n")
  expected_diag <- paste0(
    "diagnostics_pass : ",
    ifelse(round_row$diagnostics_pass, "TRUE", "FALSE")
  )
  expected_eap <- paste0(
    "eap_pass         : ",
    pairwiseLLM:::.adaptive_progress_value(round_row$eap_pass)
  )
  expected_stop <- "stop_decision : FALSE"

  testthat::expect_true(grepl(expected_diag, text, fixed = TRUE))
  testthat::expect_true(grepl(expected_eap, text, fixed = TRUE))
  testthat::expect_true(grepl(expected_stop, text, fixed = TRUE))
})
