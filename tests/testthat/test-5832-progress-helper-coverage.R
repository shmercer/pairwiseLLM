testthat::test_that("adaptive progress helpers cover value formatting and gating", {
  expect_equal(pairwiseLLM:::.adaptive_progress_level(list(progress_level = NA_character_)), "refit")
  expect_equal(pairwiseLLM:::.adaptive_progress_level(list(progress_level = "basic")), "basic")

  expect_equal(pairwiseLLM:::.adaptive_progress_value(NULL), "NA")
  expect_equal(pairwiseLLM:::.adaptive_progress_value(NA_real_), "NA")
  expect_equal(pairwiseLLM:::.adaptive_progress_value(TRUE), "TRUE")
  expect_equal(pairwiseLLM:::.adaptive_progress_value(2), "2")
  expect_equal(pairwiseLLM:::.adaptive_progress_value_with_note(2, note = NULL), "2")
  expect_equal(pairwiseLLM:::.adaptive_progress_value_with_note(NULL, note = "missing"), "NA (missing)")

  numeric_out <- pairwiseLLM:::.adaptive_progress_value(0.1234, digits = 2)
  expect_true(grepl("0.12", numeric_out))
  expect_equal(pairwiseLLM:::.adaptive_progress_value("alpha"), "alpha")

  expect_false(pairwiseLLM:::.adaptive_progress_should_iter(list(progress = FALSE), 1L))
  expect_false(pairwiseLLM:::.adaptive_progress_should_iter(
    list(progress = TRUE, progress_every_iter = 0L),
    1L
  ))
  expect_false(pairwiseLLM:::.adaptive_progress_should_iter(
    list(progress = TRUE, progress_every_iter = 1L),
    NA_integer_
  ))
  expect_true(pairwiseLLM:::.adaptive_progress_should_iter(
    list(progress = TRUE, progress_every_iter = 2L),
    4L
  ))

  expect_false(pairwiseLLM:::.adaptive_progress_should_refit(list(progress = FALSE), 1L))
  expect_false(pairwiseLLM:::.adaptive_progress_should_refit(
    list(progress = TRUE, progress_every_refit = 0L),
    1L
  ))
  expect_false(pairwiseLLM:::.adaptive_progress_should_refit(
    list(progress = TRUE, progress_every_refit = 1L),
    NA_integer_
  ))
  expect_true(pairwiseLLM:::.adaptive_progress_should_refit(
    list(progress = TRUE, progress_every_refit = 2L),
    4L
  ))

  expect_equal(pairwiseLLM:::.adaptive_progress_effective_cores(NA, 6L), 6L)
  expect_equal(pairwiseLLM:::.adaptive_progress_effective_cores(NA, NA), 1L)
})

testthat::test_that("adaptive progress formatting includes short batch details", {
  batch_row <- tibble::tibble(
    phase = "phase2",
    iter = 3L,
    n_pairs_selected = 1L,
    batch_size_target = 2L,
    n_pairs_completed = 0L,
    candidate_starved = TRUE,
    fallback_used = "expand_2x",
    reason_short_batch = "dup_gate_exhausted"
  )

  line <- pairwiseLLM:::.adaptive_progress_format_iter_line(batch_row)
  expect_true(grepl("phase2", line))
  expect_true(grepl("iter=3", line))
  expect_true(grepl("starved=TRUE", line))
  expect_true(grepl("fallback=expand_2x", line))
  expect_true(grepl("reason=dup_gate_exhausted", line))
})

testthat::test_that("adaptive progress formatting rejects non-data frames", {
  testthat::expect_error(
    pairwiseLLM:::.adaptive_progress_format_iter_line("bad"),
    "data frame"
  )
})

testthat::test_that("adaptive progress formatting omits starved and reason when absent", {
  batch_row <- tibble::tibble(
    phase = "phase1",
    iter = 1L,
    n_pairs_selected = 2L,
    batch_size_target = 2L,
    n_pairs_completed = 1L,
    candidate_starved = NA,
    fallback_used = "base_window",
    reason_short_batch = NA_character_
  )

  line <- pairwiseLLM:::.adaptive_progress_format_iter_line(batch_row)
  expect_false(grepl("starved=", line))
  expect_false(grepl("fallback=", line))
  expect_false(grepl("reason=", line))
})

testthat::test_that("adaptive progress refit block formats diagnostics and stability", {
  config <- list(
    progress_level = "full",
    eap_reliability_min = 0.9,
    stability_lag = 2L,
    theta_corr_min = 0.9,
    theta_sd_rel_change_max = 0.1,
    rank_spearman_min = 0.9
  )
  state <- list(
    phase = "phase2",
    iter = 5L,
    batch_log = tibble::tibble(iter = 5L, phase = "phase2")
  )
  round_row <- tibble::tibble(
    round_id = 2L,
    iter_at_refit = 5L,
    mode = "adaptive",
    total_pairs = 6L,
    new_pairs = 2L,
    scheduled_pairs = 10L,
    completed_pairs = 8L,
    backlog_unjudged = 2L,
    divergences = 0L,
    max_rhat = 1.01,
    min_ess_bulk = 900,
    epsilon_mean = 0.09,
    reliability_EAP = 0.87,
    eap_pass = FALSE,
    diagnostics_pass = TRUE,
    theta_sd_eap = 0.48,
    rho_theta_lag = 0.99,
    theta_corr_pass = TRUE,
    delta_sd_theta_lag = 0.01,
    delta_sd_theta_pass = TRUE,
    rho_rank_lag = 0.99,
    rho_rank_pass = TRUE,
    rank_stability_pass = TRUE,
    lag_eligible = TRUE,
    stop_decision = FALSE,
    stop_reason = NA_character_,
    n_unique_pairs_seen = 3L
  )

  lines <- pairwiseLLM:::.adaptive_progress_format_refit_block(round_row, config, state = state)
  expect_true(any(grepl("GATES", lines)))
  expect_true(any(grepl("LAG", lines)))
  expect_true(any(grepl("STOP", lines)))
  expect_true(any(grepl("diagnostics_pass", lines)))
  expect_true(any(grepl("eap_pass", lines)))

  expect_error(
    pairwiseLLM:::.adaptive_progress_format_refit_block(1, config),
    "round_row"
  )
})

testthat::test_that("adaptive progress refit block marks stability as not eligible when NA", {
  config <- list(progress_level = "refit", stability_lag = 2L)
  state <- list(
    phase = "phase1",
    iter = 2L,
    batch_log = tibble::tibble(iter = 2L, phase = "phase1")
  )
  round_row <- tibble::tibble(
    round_id = 1L,
    iter_at_refit = 2L,
    mode = "adaptive",
    total_pairs = 3L,
    new_pairs = 1L,
    scheduled_pairs = 4L,
    completed_pairs = 2L,
    backlog_unjudged = 2L,
    reliability_EAP = 0.9,
    diagnostics_pass = TRUE,
    theta_sd_eap = 0.12,
    rho_theta_lag = NA_real_,
    delta_sd_theta_lag = NA_real_,
    rho_rank_lag = NA_real_,
    rank_stability_pass = NA,
    lag_eligible = FALSE,
    stop_decision = FALSE,
    stop_reason = NA_character_
  )

  lines <- pairwiseLLM:::.adaptive_progress_format_refit_block(round_row, config, state = state)
  expect_true(any(grepl("LAG \\(eligible=FALSE", lines)))
  expect_true(any(grepl("rho_theta=NA", lines)))
})

testthat::test_that("adaptive progress emitters respect cadence and level", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)

  state$config$v3 <- list(progress = FALSE)
  expect_false(isTRUE(pairwiseLLM:::.adaptive_progress_emit_iter(state)))

  state$config$v3 <- list(progress = TRUE, progress_every_iter = 2L)
  state$batch_log <- tibble::tibble()
  expect_false(isTRUE(pairwiseLLM:::.adaptive_progress_emit_iter(state)))

  state$batch_log <- tibble::tibble(
    phase = "phase1",
    iter = 1L,
    n_pairs_selected = 1L,
    batch_size_target = 2L,
    n_pairs_completed = 1L,
    candidate_starved = FALSE,
    reason_short_batch = NA_character_
  )
  expect_false(isTRUE(pairwiseLLM:::.adaptive_progress_emit_iter(state)))

  state$batch_log$iter <- 2L
  iter_out <- capture.output({
    iter_result <- pairwiseLLM:::.adaptive_progress_emit_iter(state)
  })
  expect_true(isTRUE(iter_result))
  expect_true(any(grepl("selected=", iter_out)))

  round_row <- tibble::tibble(
    round_id = 1L,
    iter_at_refit = 2L,
    total_pairs = 3L,
    new_pairs = 1L,
    divergences = 0L,
    max_rhat = 1.01,
    min_ess_bulk = 900,
    epsilon_mean = 0.09,
    reliability_EAP = 0.87,
    eap_pass = FALSE,
    diagnostics_pass = TRUE,
    theta_sd_eap = 0.48,
    rho_theta_lag = NA_real_,
    delta_sd_theta_lag = NA_real_,
    rho_rank_lag = NA_real_,
    rank_stability_pass = NA,
    lag_eligible = FALSE,
    stop_decision = FALSE,
    stop_reason = NA_character_
  )

  state$config$v3 <- list(progress = FALSE, progress_level = "refit", progress_every_refit = 1L)
  expect_false(isTRUE(pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row)))

  state$config$v3 <- list(progress = TRUE, progress_level = "basic", progress_every_refit = 1L)
  expect_false(isTRUE(pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row)))

  state$config$v3 <- list(progress = TRUE, progress_level = "refit", progress_every_refit = 2L)
  expect_false(isTRUE(pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row)))

  state$config$v3 <- list(progress = TRUE, progress_level = "refit", progress_every_refit = 1L)
  refit_out <- capture.output({
    refit_result <- pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row)
  })
  expect_true(isTRUE(refit_result))
  expect_true(any(grepl("GATES", refit_out)))
})

testthat::test_that("adaptive progress emitters reject non-state inputs", {
  testthat::expect_error(
    pairwiseLLM:::.adaptive_progress_emit_iter(list()),
    "adaptive_state"
  )
  testthat::expect_error(
    pairwiseLLM:::.adaptive_progress_emit_refit(list(), tibble::tibble()),
    "adaptive_state"
  )
})

testthat::test_that("adaptive progress emitters print when configured", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$phase <- "phase2"

  state$config$v3 <- list(progress = TRUE, progress_every_iter = 1L)
  state$batch_log <- tibble::tibble(
    phase = "phase2",
    iter = 1L,
    n_pairs_selected = 1L,
    batch_size_target = 1L,
    n_pairs_completed = 1L,
    candidate_starved = FALSE,
    reason_short_batch = NA_character_
  )
  iter_out <- capture.output({
    iter_result <- pairwiseLLM:::.adaptive_progress_emit_iter(state)
  })
  expect_true(isTRUE(iter_result))
  expect_true(any(grepl("selected=", iter_out)))

  round_row <- pairwiseLLM:::.adaptive_round_log_defaults()
  round_row$round_id <- 1L
  round_row$iter_at_refit <- 1L
  round_row$total_pairs <- 3L
  round_row$new_pairs <- 1L
  round_row$divergences <- 0L
  round_row$max_rhat <- 1.01
  round_row$min_ess_bulk <- 900
  round_row$epsilon_mean <- 0.09
  round_row$reliability_EAP <- 0.87
  round_row$diagnostics_pass <- TRUE
  round_row$theta_sd_eap <- 0.48
  round_row$rho_theta_lag <- 0.99
  round_row$delta_sd_theta_lag <- 0.01
  round_row$rho_rank_lag <- 0.99
  round_row$rank_stability_pass <- TRUE
  round_row$lag_eligible <- TRUE
  round_row$stop_decision <- FALSE
  round_row$stop_reason <- NA_character_

  state$config$v3 <- list(progress = TRUE, progress_level = "refit", progress_every_refit = 1L)
  refit_out <- capture.output({
    refit_result <- pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row)
  })
  expect_true(isTRUE(refit_result))
  expect_true(any(grepl("STOP", refit_out)))
})
