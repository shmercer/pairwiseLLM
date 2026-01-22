testthat::test_that("adaptive progress helpers cover value formatting and gating", {
  expect_equal(pairwiseLLM:::.adaptive_progress_level(list(progress_level = NA_character_)), "refit")
  expect_equal(pairwiseLLM:::.adaptive_progress_level(list(progress_level = "basic")), "basic")

  expect_equal(pairwiseLLM:::.adaptive_progress_value(NULL), "NA")
  expect_equal(pairwiseLLM:::.adaptive_progress_value(NA_real_), "NA")
  expect_equal(pairwiseLLM:::.adaptive_progress_value(TRUE), "TRUE")
  expect_equal(pairwiseLLM:::.adaptive_progress_value(2), "2")

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
})

testthat::test_that("adaptive progress formatting includes short batch details", {
  batch_row <- tibble::tibble(
    phase = "phase2",
    iter = 3L,
    n_pairs_selected = 1L,
    batch_size_target = 2L,
    n_pairs_completed = 0L,
    candidate_starved = TRUE,
    reason_short_batch = "dup_gate_exhausted"
  )

  line <- pairwiseLLM:::.adaptive_progress_format_iter_line(batch_row)
  expect_true(grepl("phase2", line))
  expect_true(grepl("iter=3", line))
  expect_true(grepl("starved=TRUE", line))
  expect_true(grepl("reason=dup_gate_exhausted", line))
})

testthat::test_that("adaptive progress refit block formats diagnostics and stability", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$phase <- "phase2"
  state$checks_passed_in_row <- 1L

  config <- list(progress_level = "full", checks_passed_target = 2L)
  round_row <- tibble::tibble(
    round_id = 2L,
    iter_at_refit = 5L,
    divergences = 0L,
    max_rhat = 1.01,
    min_ess_bulk = 900,
    epsilon_mean = 0.09,
    reliability_EAP = 0.87,
    diagnostics_pass = TRUE,
    theta_sd_median = 0.48,
    tau = 0.80,
    theta_sd_pass = TRUE,
    U0 = 0.0012,
    U_abs = 0.0024,
    U_pass = TRUE,
    rank_stability_pass = TRUE,
    frac_weak_adj = 0.03,
    min_adj_prob = 0.72,
    n_unique_pairs_seen = 3L,
    hard_cap_threshold = 12L,
    hard_cap_reached = FALSE,
    mcmc_chains = 2L,
    mcmc_parallel_chains = 2L,
    mcmc_cores_detected_physical = 4L,
    mcmc_cores_detected_logical = 8L,
    mcmc_core_fraction = 0.6
  )

  lines <- pairwiseLLM:::.adaptive_progress_format_refit_block(round_row, state, config)
  expect_true(any(grepl("Stability:", lines)))
  expect_true(any(grepl("Hard cap:", lines)))

  expect_error(
    pairwiseLLM:::.adaptive_progress_format_refit_block(1, state, config),
    "round_row"
  )
  expect_error(
    pairwiseLLM:::.adaptive_progress_format_refit_block(tibble::tibble(), list(), config),
    "adaptive_state"
  )
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
    invisible(pairwiseLLM:::.adaptive_progress_emit_iter(state))
  })
  expect_true(any(grepl("selected=", iter_out)))

  round_row <- tibble::tibble(
    round_id = 1L,
    iter_at_refit = 2L,
    divergences = 0L,
    max_rhat = 1.01,
    min_ess_bulk = 900,
    epsilon_mean = 0.09,
    reliability_EAP = 0.87,
    diagnostics_pass = TRUE,
    theta_sd_median = 0.48,
    tau = 0.80,
    theta_sd_pass = TRUE,
    U0 = 0.0012,
    U_abs = 0.0024,
    U_pass = TRUE,
    rank_stability_pass = NA,
    frac_weak_adj = NA_real_,
    min_adj_prob = NA_real_,
    mcmc_chains = 2L,
    mcmc_parallel_chains = 2L,
    mcmc_cores_detected_physical = 4L,
    mcmc_cores_detected_logical = 8L,
    mcmc_core_fraction = 0.6
  )

  state$config$v3 <- list(progress = FALSE, progress_level = "refit", progress_every_refit = 1L)
  expect_false(isTRUE(pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row)))

  state$config$v3 <- list(progress = TRUE, progress_level = "basic", progress_every_refit = 1L)
  expect_false(isTRUE(pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row)))

  state$config$v3 <- list(progress = TRUE, progress_level = "refit", progress_every_refit = 2L)
  expect_false(isTRUE(pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row)))

  state$config$v3 <- list(progress = TRUE, progress_level = "refit", progress_every_refit = 1L)
  refit_out <- capture.output({
    invisible(pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row))
  })
  expect_true(any(grepl("rel_EAP", refit_out)))
})
