testthat::test_that("diagnostics_gate validates inputs", {
  config <- pairwiseLLM:::adaptive_v3_config(3L)

  expect_error(pairwiseLLM:::diagnostics_gate(1, config), "`fit`")
  expect_error(pairwiseLLM:::diagnostics_gate(list(), config), "fit\\$diagnostics")

  fit <- list(
    diagnostics = list(
      divergences = 0L,
      max_rhat = 1,
      min_ess_bulk = 1000
    )
  )
  expect_error(
    pairwiseLLM:::diagnostics_gate(fit, config, near_stop = NA),
    "near_stop"
  )
})

testthat::test_that("adaptive_run helpers cover missing columns and empty keys", {
  expect_error(
    pairwiseLLM:::.adaptive_pairs_to_submit_tbl(tibble::tibble(A_id = "A")),
    "missing required columns"
  )

  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)

  out <- pairwiseLLM:::.adaptive_results_seen_set(state, c(NA_character_, ""))
  expect_equal(length(out$results_seen), 0L)

  state$history_results <- tibble::tibble(
    pair_uid = c(NA_character_, ""),
    unordered_key = c("A:B", "A:B"),
    ordered_key = c("A:B", "B:A"),
    A_id = c("A", "B"),
    B_id = c("B", "A"),
    better_id = c("A", "B"),
    winner_pos = c(1L, 2L),
    phase = c("phase1", "phase1"),
    iter = c(0L, 0L),
    received_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    backend = c("openai", "openai"),
    model = c("gpt-test", "gpt-test")
  )
  synced <- pairwiseLLM:::.adaptive_state_sync_results_seen(state)
  expect_equal(length(synced$results_seen), 0L)
})

testthat::test_that("diagnostics gate integration covers pass/fail branches", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  config <- pairwiseLLM:::adaptive_v3_config(state$N)

  pass_out <- pairwiseLLM:::.adaptive_apply_diagnostics_gate(
    state,
    fit = list(diagnostics = NULL),
    config = config,
    near_stop = FALSE
  )
  expect_true(pass_out$diagnostics_pass)

  state$mode <- "repair"
  state$repair_attempts <- 2L
  fit <- list(
    diagnostics = list(
      divergences = 0L,
      max_rhat = 1,
      min_ess_bulk = config$min_ess_bulk
    )
  )
  pass_reset <- pairwiseLLM:::.adaptive_apply_diagnostics_gate(
    state,
    fit = fit,
    config = config,
    near_stop = FALSE
  )
  expect_equal(pass_reset$state$mode, "adaptive")
  expect_equal(pass_reset$state$repair_attempts, 0L)
  state <- pass_reset$state

  config_warn <- config
  config_warn$repair_max_cycles <- 1L
  fail_fit <- list(
    diagnostics = list(
      divergences = 1L,
      max_rhat = 2,
      min_ess_bulk = 1
    )
  )
  fail_out <- NULL
  testthat::expect_warning(
    {
      fail_out <- pairwiseLLM:::.adaptive_apply_diagnostics_gate(
        state,
        fit = fail_fit,
        config = config_warn,
        near_stop = FALSE
      )
    },
    "entering repair mode"
  )
  expect_equal(fail_out$state$mode, "repair")
})

testthat::test_that("repair scheduling handles target bounds and duplicates", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$phase <- "phase2"
  state$unordered_count[["A:B"]] <- 1L
  adaptive <- list(bins = 2L, mix_struct = 0.7, within_adj_split = 0.5)
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N)

  expect_error(
    pairwiseLLM:::.adaptive_schedule_repair_pairs(state, -1L, adaptive, seed = 1),
    "target_pairs"
  )

  empty_out <- pairwiseLLM:::.adaptive_schedule_repair_pairs(state, 0L, adaptive, seed = 1)
  expect_equal(nrow(empty_out$pairs), 0L)

  out <- NULL
  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_schedule_repair_pairs(state, 1L, adaptive, seed = 1),
    .adaptive_get_refit_fit = function(state, adaptive, batch_size, seed) {
      list(
        state = state,
        fit = list(
          theta_mean = stats::setNames(c(0, 0), state$ids),
          theta_draws = matrix(0, nrow = 2, ncol = 2, dimnames = list(NULL, state$ids)),
          diagnostics = NULL
        )
      )
    },
    generate_candidates = function(...) {
      tibble::tibble(i = "A", j = "B")
    },
    compute_pair_utility = function(...) {
      tibble::tibble(
        i_id = "A",
        j_id = "B",
        unordered_key = "A:B",
        utility = 0.2,
        utility_raw = 0.2,
        p_mean = 0.5
      )
    },
    apply_degree_penalty = function(utilities, state) utilities,
    select_batch = function(state, candidates_with_utility, config, seed = NULL, exploration_only = FALSE) {
      tibble::tibble(
        i_id = "A",
        j_id = "B",
        unordered_key = "A:B",
        utility = 0.2,
        utility_raw = 0.2,
        p_mean = 0.5,
        A_id = "A",
        B_id = "B"
      )
    },
    .package = "pairwiseLLM"
  )

  expect_equal(out$state$mode, "repair")
  expect_equal(out$state$iter, 1L)
  expect_equal(out$pairs$phase, "phase2")
  expect_equal(out$pairs$iter, 1L)
  expect_equal(out$state$history_pairs$phase[[nrow(out$state$history_pairs)]], "phase2")
  expect_equal(out$state$history_pairs$iter[[nrow(out$state$history_pairs)]], 1L)
})

testthat::test_that("schedule_next_pairs covers stopped mode and near-stop phase shift", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  adaptive <- list(exploration_frac = 0.1)

  state$mode <- "stopped"
  stopped <- pairwiseLLM:::.adaptive_schedule_next_pairs(state, 1L, adaptive, seed = 1)
  expect_equal(nrow(stopped$pairs), 0L)

  state$mode <- "adaptive"
  state$phase <- "phase2"
  state$budget_max <- 10L
  state$comparisons_scheduled <- 0L
  state$U0 <- NA_real_
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N)

  captured <- new.env(parent = emptyenv())
  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_schedule_next_pairs(state, 1L, adaptive, seed = 1, near_stop = TRUE),
    .adaptive_get_refit_fit = function(state, adaptive, batch_size, seed) {
      list(
        state = state,
        fit = list(
          theta_mean = stats::setNames(c(0, 0), state$ids),
          theta_draws = matrix(0, nrow = 2, ncol = 2, dimnames = list(NULL, state$ids)),
          diagnostics = NULL
        )
      )
    },
    generate_candidates = function(...) {
      tibble::tibble(i = "A", j = "B")
    },
    compute_pair_utility = function(...) {
      tibble::tibble(
        i_id = "A",
        j_id = "B",
        unordered_key = "A:B",
        utility = 0.2,
        utility_raw = 0.2
      )
    },
    apply_degree_penalty = function(utilities, state) utilities,
    select_batch = function(state, candidates_with_utility, config, seed = NULL, exploration_only = FALSE) {
      captured$U0 <- state$U0
      tibble::tibble(
        i_id = character(),
        j_id = character(),
        unordered_key = character(),
        utility = double(),
        utility_raw = double(),
        p_mean = double(),
        A_id = character(),
        B_id = character()
      )
    },
    .package = "pairwiseLLM"
  )

  expect_equal(out$state$phase, "phase3")
  expect_equal(captured$U0, 0.2)
})

testthat::test_that("next_action respects stop_reason and resume seeds repair fields", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$mode <- "stopped"
  state$stop_reason <- "diagnostics_failed"
  out <- pairwiseLLM:::.adaptive_next_action(state, scheduled_pairs = 1L)
  expect_equal(out$reason, "diagnostics_failed")

  state$repair_attempts <- 0L
  state$stop_reason <- NA_character_
  state$config$backend <- "openai"
  state$config$model <- "gpt-test"
  state$config$trait_name <- "quality"
  state$config$trait_description <- "Which is better?"
  state$config$prompt_template <- "template"
  state$config$submission <- list()

  resume_out <- testthat::with_mocked_bindings(
    adaptive_rank_resume(
      state = state,
      mode = "live",
      submission_info = list(
        backend = "openai",
        model = "gpt-test",
        trait_name = "quality",
        trait_description = "Which is better?",
        prompt_template = "template"
      )
    ),
    .adaptive_run_stopping_checks = function(state, adaptive, seed) {
      state$mode <- "stopped"
      state$stop_reason <- "diagnostics_failed"
      list(state = state)
    },
    .package = "pairwiseLLM"
  )

  expect_equal(resume_out$state$repair_attempts, 0L)
  expect_equal(resume_out$state$stop_reason, "diagnostics_failed")
  expect_equal(resume_out$next_action$reason, "diagnostics_failed")
})

testthat::test_that("replacement loop defaults batch size and phase when missing", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  adaptive <- list(max_refill_rounds = 1L)
  captured <- new.env(parent = emptyenv())

  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_run_replacements_live(
      state = state,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      prompt_template = "template",
      backend = "openai",
      adaptive = adaptive,
      submission = list(),
      missing = 1L,
      seed = 1,
      replacement_phase = NULL,
      base_batch_size = NA_integer_
    ),
    .adaptive_schedule_replacement_pairs = function(state, target_pairs, adaptive, seed, replacement_phase) {
      captured$phase <- replacement_phase
      list(state = state, pairs = pairwiseLLM:::.adaptive_empty_pairs_tbl())
    },
    .package = "pairwiseLLM"
  )

  expect_equal(captured$phase, "phase1")
  expect_equal(length(out$submissions), 0L)
})

testthat::test_that("repair schedule handles empty utilities and selection", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$phase <- "phase2"
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N)
  adaptive <- list()

  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::.adaptive_schedule_repair_pairs(state, 1L, adaptive, seed = 1),
    .adaptive_get_refit_fit = function(state, adaptive, batch_size, seed) {
      list(
        state = state,
        fit = list(
          theta_mean = stats::setNames(c(0, 0), state$ids),
          theta_draws = matrix(0, nrow = 2, ncol = 2, dimnames = list(NULL, state$ids)),
          diagnostics = NULL
        )
      )
    },
    generate_candidates = function(...) tibble::tibble(),
    select_batch = function(...) pairwiseLLM:::.adaptive_empty_pairs_tbl(),
    .package = "pairwiseLLM"
  )

  expect_equal(nrow(out$pairs), 0L)
  expect_equal(out$state$mode, "repair")
  expect_equal(out$state$iter, 1L)
})

testthat::test_that("adaptive_rank_resume seeds repair defaults when NULL", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$repair_attempts <- NULL
  state$stop_reason <- NULL
  state$config$backend <- "openai"
  state$config$model <- "gpt-test"
  state$config$trait_name <- "quality"
  state$config$trait_description <- "Which is better?"
  state$config$prompt_template <- "template"
  state$config$submission <- list()

  out <- testthat::with_mocked_bindings(
    adaptive_rank_resume(
      state = state,
      mode = "live",
      submission_info = list(
        backend = "openai",
        model = "gpt-test",
        trait_name = "quality",
        trait_description = "Which is better?",
        prompt_template = "template"
      )
    ),
    validate_state = function(state) invisible(state),
    .adaptive_run_stopping_checks = function(state, adaptive, seed) {
      state$mode <- "stopped"
      list(state = state)
    },
    .package = "pairwiseLLM"
  )

  expect_equal(out$state$repair_attempts, 0L)
  expect_true(is.na(out$state$stop_reason))
})
