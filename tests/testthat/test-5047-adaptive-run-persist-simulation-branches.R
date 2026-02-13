test_that("adaptive round helpers cover start-next, commit, and starvation transitions", {
  state <- pairwiseLLM::adaptive_rank_start(make_test_items(5), seed = 1L)
  state$warm_start_done <- TRUE
  state$round$staged_active <- TRUE
  state$round$stage_index <- 1L
  state$round$stage_order <- pairwiseLLM:::.adaptive_stage_order()
  state$round$stage_quotas <- as.list(stats::setNames(rep.int(1L, 4L), state$round$stage_order))
  state$round$stage_committed <- as.list(stats::setNames(rep.int(0L, 4L), state$round$stage_order))

  step_row <- tibble::tibble(
    round_stage = "anchor_link",
    A = 1L,
    B = 2L,
    star_override_used = TRUE
  )
  committed <- pairwiseLLM:::.adaptive_round_commit(state, step_row)
  expect_true(is.list(committed$round))
  committed_total <- committed$round$committed_total
  if (is.null(committed_total)) {
    committed_total <- 0L
  }
  expect_gte(committed_total, 1L)

  advanced <- pairwiseLLM:::.adaptive_round_start_next(committed)
  round_id <- advanced$round$round_id
  if (is.null(round_id)) {
    round_id <- 0L
  }
  expect_true(round_id >= 2L)
  expect_true(is.list(advanced$refit_meta$last_completed_round_summary))

  starve <- pairwiseLLM:::.adaptive_round_starvation(
    committed,
    tibble::tibble(round_stage = "anchor_link")
  )
  expect_true(is.list(starve$state))
  expect_true(is.logical(starve$exhausted))

  bad_stage <- pairwiseLLM:::.adaptive_round_starvation(
    committed,
    tibble::tibble(round_stage = NA_character_)
  )
  expect_true(isTRUE(bad_stage$exhausted))

  warm_committed <- pairwiseLLM:::.adaptive_round_commit_warm_start(state)
  expect_true(is.list(warm_committed$round))

  no_round <- state
  no_round$round <- NULL
  no_round$warm_start_done <- FALSE
  activated <- pairwiseLLM:::.adaptive_round_activate_if_ready(no_round)
  expect_true(is.list(activated$round))
})

test_that("adaptive_rank_start and adaptive_rank_run_live cover additional input and starvation branches", {
  expect_error(
    pairwiseLLM::adaptive_rank_start(make_test_items(3), seed = 1L, bogus = 1L),
    "Only `now_fn`"
  )

  state <- pairwiseLLM::adaptive_rank_start(make_test_items(3), seed = 1L)
  state$warm_start_done <- FALSE

  out <- testthat::with_mocked_bindings(
    run_one_step = function(st, judge, ...) {
      row <- list(
        step_id = as.integer(nrow(st$step_log) + 1L),
        timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
        status = "starved",
        candidate_starved = TRUE,
        round_stage = "warm_start"
      )
      st$step_log <- pairwiseLLM:::append_step_log(st$step_log, row)
      st
    },
    .package = "pairwiseLLM",
    {
      pairwiseLLM::adaptive_rank_run_live(
        state = state,
        judge = make_deterministic_judge("invalid"),
        n_steps = 1L,
        progress = "none"
      )
    }
  )
  expect_true(isTRUE(out$meta$stop_decision))
  expect_identical(out$meta$stop_reason, "candidate_starvation")

  state2 <- pairwiseLLM::adaptive_rank_start(make_test_items(4), seed = 1L)
  state2$warm_start_done <- TRUE
  state2$round$staged_active <- TRUE
  state2$round$stage_index <- 1L

  out2 <- testthat::with_mocked_bindings(
    run_one_step = function(st, judge, ...) {
      row <- list(
        step_id = as.integer(nrow(st$step_log) + 1L),
        timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
        status = "starved",
        candidate_starved = TRUE,
        round_stage = "anchor_link"
      )
      st$step_log <- pairwiseLLM:::append_step_log(st$step_log, row)
      st
    },
    maybe_refit_btl = function(state, config, fit_fn) {
      list(state = state, refit_performed = FALSE, config = config, refit_context = list())
    },
    .package = "pairwiseLLM",
    {
      pairwiseLLM::adaptive_rank_run_live(
        state = state2,
        judge = make_deterministic_judge("invalid"),
        n_steps = 1L,
        progress = "none"
      )
    }
  )
  expect_true(inherits(out2, "adaptive_state"))
})

test_that("persist validator checks metadata and btl-fit load branch", {
  state <- pairwiseLLM::adaptive_rank_start(make_test_items(3), seed = 1L)
  state$btl_fit <- list(example = 1L)
  session_dir <- withr::local_tempdir()
  pairwiseLLM::save_adaptive_session(state, session_dir, overwrite = TRUE)

  loaded <- pairwiseLLM::load_adaptive_session(session_dir)
  expect_true(is.list(loaded$btl_fit))

  meta_path <- file.path(session_dir, "metadata.rds")
  meta <- readRDS(meta_path)
  meta$schema_version <- NA_character_
  saveRDS(meta, meta_path)
  expect_error(pairwiseLLM::validate_session_dir(session_dir), "schema_version")

  pairwiseLLM::save_adaptive_session(state, session_dir, overwrite = TRUE)
  meta <- readRDS(meta_path)
  meta$n_items <- "three"
  saveRDS(meta, meta_path)
  expect_error(pairwiseLLM::validate_session_dir(session_dir), "n_items")

  pairwiseLLM::save_adaptive_session(state, session_dir, overwrite = TRUE)
  step_path <- file.path(session_dir, "step_log.rds")
  step <- readRDS(step_path)
  if (nrow(step) == 0L) {
    step <- pairwiseLLM:::append_step_log(
      step,
      list(
        step_id = 1L,
        timestamp = as.POSIXct("2026-01-01", tz = "UTC"),
        pair_id = 1L,
        A = 99L,
        B = 98L,
        Y = 1L
      )
    )
  } else {
    step$A[[1L]] <- 99L
    step$B[[1L]] <- 98L
  }
  saveRDS(step, step_path)
  expect_error(pairwiseLLM::load_adaptive_session(session_dir), "invalid item indices")

  state2 <- pairwiseLLM::adaptive_rank_start(make_test_items(3), seed = 1L)
  state2$config$persist_item_log <- TRUE
  state2$item_log <- list(tibble::tibble(
    refit_id = 1L,
    item_id = state2$item_ids,
    theta_mean = as.double(c(0, 0, 0)),
    `theta_p2.5` = as.double(c(0, 0, 0)),
    `theta_p5` = as.double(c(0, 0, 0)),
    `theta_p50` = as.double(c(0, 0, 0)),
    `theta_p95` = as.double(c(0, 0, 0)),
    `theta_p97.5` = as.double(c(0, 0, 0)),
    theta_sd = as.double(c(1, 1, 1)),
    rank_mean = as.double(c(1, 2, 3)),
    degree = as.integer(c(0, 0, 0)),
    pos_count_A = as.integer(c(0, 0, 0)),
    pos_count_B = as.integer(c(0, 0, 0))
  ))
  session_dir2 <- withr::local_tempdir()
  pairwiseLLM::save_adaptive_session(state2, session_dir2, overwrite = TRUE)
  expect_no_error(pairwiseLLM::validate_session_dir(session_dir2))
})

test_that("simulation harness helper validations cover error and empty branches", {
  expect_error(pairwiseLLM:::.adaptive_simulation_validate_seed(NA, "run_seed"), "single non-missing integer")
  expect_error(pairwiseLLM:::.adaptive_simulation_default_items(1L), "integer >= 2")

  expect_error(
    pairwiseLLM:::.adaptive_simulation_judge(tibble::tibble(item_id = "1"), judge_seed = 1L),
    "must contain `item_id` and `quality_score`"
  )

  items <- pairwiseLLM:::.adaptive_simulation_default_items(3L)
  judge <- pairwiseLLM:::.adaptive_simulation_judge(items, judge_seed = 1L, invalid_steps = 1L)
  st <- pairwiseLLM::adaptive_rank_start(items, seed = 1L)
  j1 <- judge(items[1, ], items[2, ], st)
  expect_false(isTRUE(j1$is_valid))
  expect_identical(j1$invalid_reason, "invalid_fixture_step")

  bad_items <- tibble::tibble(item_id = c("1", "2"), quality_score = c(NA_real_, NA_real_))
  judge_bad <- pairwiseLLM:::.adaptive_simulation_judge(bad_items, judge_seed = 2L)
  j2 <- judge_bad(bad_items[1, ], bad_items[2, ], st)
  expect_false(isTRUE(j2$is_valid))
  expect_identical(j2$invalid_reason, "missing_fixture_score")

  empty_quota <- pairwiseLLM:::.adaptive_stage_quota_summary(tibble::tibble())
  expect_identical(nrow(empty_quota), 0L)

  blank_step <- tibble::tibble(
    round_stage = character(),
    pair_id = integer(),
    i = integer(),
    j = integer()
  )
  expect_true(pairwiseLLM:::.adaptive_warm_start_connectivity(blank_step, item_ids = "A"))
  expect_false(pairwiseLLM:::.adaptive_warm_start_connectivity(blank_step, item_ids = c("A", "B")))

  expect_error(
    pairwiseLLM:::.adaptive_simulation_run(
      scenario = "baseline",
      run_seed = 1L,
      judge_seed = 1L,
      n_steps = 0L
    ),
    "integer >= 1"
  )
})

test_that("adaptive_rank_run_live covers progress event, persistence writes, and refit-stop save branches", {
  state <- pairwiseLLM::adaptive_rank_start(make_test_items(4), seed = 1L)
  state$warm_start_done <- TRUE
  state$round$staged_active <- TRUE
  state$config$session_dir <- tempfile("session-")
  state$config$persist_item_log <- TRUE

  tracker <- new.env(parent = emptyenv())
  tracker$cli_called <- 0L
  tracker$save_called <- 0L
  tracker$write_called <- 0L

  out <- testthat::with_mocked_bindings(
    run_one_step = function(st, judge, ...) {
      row <- list(
        step_id = as.integer(nrow(st$step_log) + 1L),
        timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
        status = "ok",
        candidate_starved = FALSE,
        round_stage = "anchor_link",
        A = 1L,
        B = 2L,
        Y = 1L
      )
      st$step_log <- pairwiseLLM:::append_step_log(st$step_log, row)
      st
    },
    adaptive_progress_step_event = function(step_row, cfg) "event",
    maybe_refit_btl = function(state, config, fit_fn) {
      list(state = state, refit_performed = TRUE, config = config, refit_context = list())
    },
    compute_stop_metrics = function(state, config) pairwiseLLM:::.adaptive_stop_metrics_defaults(),
    .adaptive_maybe_enter_phase3 = function(state, metrics, config) state,
    should_stop = function(metrics, config) FALSE,
    .adaptive_round_log_row = function(state, metrics, stop_decision, stop_reason, refit_context, config) {
      rr <- pairwiseLLM:::round_log_schema()[1, , drop = FALSE]
      rr$refit_id <- 1L
      rr
    },
    append_round_log = function(round_log, row) round_log,
    .adaptive_build_item_log_refit = function(state, refit_id) {
      tibble::tibble(
        refit_id = 1L,
        item_id = as.character(state$item_ids[[1L]]),
        theta_mean = 0,
        `theta_p2.5` = 0,
        `theta_p5` = 0,
        `theta_p50` = 0,
        `theta_p95` = 0,
        `theta_p97.5` = 0,
        theta_sd = 1,
        rank_mean = 1,
        degree = 0L,
        pos_count_A = 0L,
        pos_count_B = 0L
      )
    },
    .adaptive_append_item_log = function(state, item_log_tbl) {
      state$item_log <- list(item_log_tbl)
      state
    },
    .adaptive_session_paths = function(session_dir) {
      list(item_log_dir = file.path(tempdir(), "item-log"))
    },
    .adaptive_write_item_log_files = function(item_log, item_log_dir) {
      tracker$write_called <- tracker$write_called + 1L
      invisible(NULL)
    },
    adaptive_progress_refit_block = function(round_row, cfg) character(),
    save_adaptive_session = function(state, session_dir, overwrite = TRUE) {
      tracker$save_called <- tracker$save_called + 1L
      invisible(state)
    },
    .package = "pairwiseLLM",
    {
      testthat::with_mocked_bindings(
        cli_inform = function(...) {
          tracker$cli_called <- tracker$cli_called + 1L
          invisible(NULL)
        },
        .package = "cli",
        {
          pairwiseLLM::adaptive_rank_run_live(
            state = state,
            judge = make_deterministic_judge("i_wins"),
            n_steps = 1L,
            progress = "all"
          )
        }
      )
    }
  )
  expect_true(inherits(out, "adaptive_state"))
  expect_gte(tracker$cli_called, 1L)
  expect_gte(tracker$write_called, 1L)
  expect_gte(tracker$save_called, 1L)

  state_starve <- pairwiseLLM::adaptive_rank_start(make_test_items(4), seed = 1L)
  state_starve$warm_start_done <- TRUE
  state_starve$round$staged_active <- TRUE
  state_starve$config$session_dir <- tempfile("session-starve-")
  tracker2 <- new.env(parent = emptyenv())
  tracker2$save_called <- 0L

  out_starve <- testthat::with_mocked_bindings(
    run_one_step = function(st, judge, ...) {
      row <- list(
        step_id = as.integer(nrow(st$step_log) + 1L),
        timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
        status = "starved",
        candidate_starved = TRUE,
        round_stage = "anchor_link"
      )
      st$step_log <- pairwiseLLM:::append_step_log(st$step_log, row)
      st
    },
    .adaptive_round_starvation = function(state, step_row) list(state = state, exhausted = TRUE),
    save_adaptive_session = function(state, session_dir, overwrite = TRUE) {
      tracker2$save_called <- tracker2$save_called + 1L
      invisible(state)
    },
    .package = "pairwiseLLM",
    {
      pairwiseLLM::adaptive_rank_run_live(
        state = state_starve,
        judge = make_deterministic_judge("invalid"),
        n_steps = 1L,
        progress = "none"
      )
    }
  )
  expect_true(isTRUE(out_starve$meta$stop_decision))
  expect_identical(out_starve$meta$stop_reason, "candidate_starvation")
  expect_gte(tracker2$save_called, 1L)

  state_stop <- pairwiseLLM::adaptive_rank_start(make_test_items(4), seed = 1L)
  state_stop$warm_start_done <- TRUE
  state_stop$round$staged_active <- TRUE
  state_stop$config$session_dir <- tempfile("session-stop-")
  tracker3 <- new.env(parent = emptyenv())
  tracker3$save_called <- 0L

  out_stop <- testthat::with_mocked_bindings(
    run_one_step = function(st, judge, ...) {
      row <- list(
        step_id = as.integer(nrow(st$step_log) + 1L),
        timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
        status = "ok",
        candidate_starved = FALSE,
        round_stage = "anchor_link",
        A = 1L,
        B = 2L,
        Y = 1L
      )
      st$step_log <- pairwiseLLM:::append_step_log(st$step_log, row)
      st
    },
    maybe_refit_btl = function(state, config, fit_fn) {
      list(state = state, refit_performed = TRUE, config = config, refit_context = list())
    },
    compute_stop_metrics = function(state, config) pairwiseLLM:::.adaptive_stop_metrics_defaults(),
    .adaptive_maybe_enter_phase3 = function(state, metrics, config) state,
    should_stop = function(metrics, config) TRUE,
    .adaptive_round_log_row = function(state, metrics, stop_decision, stop_reason, refit_context, config) {
      rr <- pairwiseLLM:::round_log_schema()[1, , drop = FALSE]
      rr$refit_id <- 1L
      rr
    },
    append_round_log = function(round_log, row) round_log,
    .adaptive_build_item_log_refit = function(state, refit_id) pairwiseLLM:::.adaptive_item_log_defaults(n_rows = 0L),
    .adaptive_append_item_log = function(state, item_log_tbl) state,
    save_adaptive_session = function(state, session_dir, overwrite = TRUE) {
      tracker3$save_called <- tracker3$save_called + 1L
      invisible(state)
    },
    .package = "pairwiseLLM",
    {
      pairwiseLLM::adaptive_rank_run_live(
        state = state_stop,
        judge = make_deterministic_judge("i_wins"),
        n_steps = 1L,
        progress = "none"
      )
    }
  )
  expect_true(isTRUE(out_stop$meta$stop_decision))
  expect_identical(out_stop$meta$stop_reason, "btl_converged")
  expect_gte(tracker3$save_called, 1L)
})

test_that("adaptive round helper early-return branches are covered directly", {
  state <- pairwiseLLM::adaptive_rank_start(make_test_items(3), seed = 1L)
  state$round <- NULL
  state$warm_start_done <- FALSE
  act <- pairwiseLLM:::.adaptive_round_activate_if_ready(state)
  expect_true(is.list(act$round))

  state2 <- pairwiseLLM::adaptive_rank_start(make_test_items(3), seed = 1L)
  state2$warm_start_done <- TRUE
  state2$round$round_committed <- state2$round$round_pairs_target
  act2 <- pairwiseLLM:::.adaptive_round_activate_if_ready(state2)
  expect_true(is.list(act2$round))

  bad_adv <- pairwiseLLM:::.adaptive_round_advance_stage(
    list(round = list(stage_index = 99L, stage_order = c("anchor_link"), stage_shortfalls = list(anchor_link = 0L))),
    shortfall = 1L
  )
  expect_identical(bad_adv$round$stage_index, 99L)

  no_stage <- pairwiseLLM:::.adaptive_round_commit(
    list(round = list(staged_active = TRUE, stage_order = c("anchor_link"), stage_index = 1L)),
    tibble::tibble(round_stage = "bad", A = 1L, B = 2L)
  )
  expect_true(is.list(no_stage$round))

  invalid_idx_state <- pairwiseLLM::adaptive_rank_start(make_test_items(3), seed = 1L)
  invalid_idx_state$warm_start_done <- TRUE
  invalid_idx_state$round$staged_active <- TRUE
  invalid_idx_state$round$stage_order <- pairwiseLLM:::.adaptive_stage_order()
  invalid_idx_state$round$stage_quotas <- as.list(stats::setNames(rep.int(2L, 4L), invalid_idx_state$round$stage_order))
  invalid_idx_state$round$stage_committed <- as.list(stats::setNames(rep.int(0L, 4L), invalid_idx_state$round$stage_order))
  step_invalid_idx <- tibble::tibble(round_stage = "anchor_link", A = 99L, B = 98L)
  out_invalid_idx <- pairwiseLLM:::.adaptive_round_commit(invalid_idx_state, step_invalid_idx)
  expect_true(is.list(out_invalid_idx$round))

  warm_no_round <- pairwiseLLM:::.adaptive_round_commit_warm_start(list(round = NULL))
  expect_null(warm_no_round$round)

  starve_no_round <- pairwiseLLM:::.adaptive_round_starvation(list(round = NULL), tibble::tibble(round_stage = "anchor_link"))
  expect_true(isTRUE(starve_no_round$exhausted))
})

test_that("adaptive rank start and warm-start starvation save branch validations", {
  expect_error(
    pairwiseLLM::adaptive_rank_start(make_test_items(3), seed = 1L, session_dir = c("a", "b")),
    "single string"
  )
  expect_error(
    pairwiseLLM::adaptive_rank_start(make_test_items(3), seed = 1L, persist_item_log = NA),
    "must be TRUE or FALSE"
  )

  state <- pairwiseLLM::adaptive_rank_start(make_test_items(3), seed = 1L)
  state$warm_start_done <- FALSE
  state$config$session_dir <- tempfile("session-warm-starve-")
  tracker <- new.env(parent = emptyenv())
  tracker$save_called <- 0L

  out <- testthat::with_mocked_bindings(
    run_one_step = function(st, judge, ...) {
      row <- list(
        step_id = as.integer(nrow(st$step_log) + 1L),
        timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
        status = "starved",
        candidate_starved = TRUE,
        round_stage = "warm_start"
      )
      st$step_log <- pairwiseLLM:::append_step_log(st$step_log, row)
      st
    },
    save_adaptive_session = function(state, session_dir, overwrite = TRUE) {
      tracker$save_called <- tracker$save_called + 1L
      invisible(state)
    },
    .package = "pairwiseLLM",
    {
      pairwiseLLM::adaptive_rank_run_live(
        state = state,
        judge = make_deterministic_judge("invalid"),
        n_steps = 1L,
        progress = "none"
      )
    }
  )
  expect_true(isTRUE(out$meta$stop_decision))
  expect_identical(out$meta$stop_reason, "candidate_starvation")
  expect_gte(tracker$save_called, 1L)
})

test_that("phase B stage exhaustion persists across round rollover within refit window", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22")
  )
  state <- pairwiseLLM::adaptive_rank_start(
    items,
    seed = 31L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$round$staged_active <- TRUE
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("ready", "ready"),
      validation_message = c("ready", "ready"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(),
    ready_for_phase_b = TRUE,
    phase = "phase_b"
  )
  state$controller$current_link_spoke_id <- 2L

  starve <- pairwiseLLM:::.adaptive_round_starvation(
    state,
    tibble::tibble(round_stage = "anchor_link", link_spoke_id = 2L)
  )
  expect_false(isTRUE(starve$exhausted))

  rolled <- pairwiseLLM:::.adaptive_round_start_next(starve$state)
  refit_id <- pairwiseLLM:::.adaptive_link_refit_window_id(rolled)
  stage_quotas <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = as.integer(rolled$round$round_id),
    n_items = as.integer(rolled$n_items),
    controller = utils::modifyList(rolled$controller, list(current_link_spoke_id = 2L))
  )
  progress <- pairwiseLLM:::.adaptive_link_stage_progress(
    state = rolled,
    spoke_id = 2L,
    stage_quotas = stage_quotas,
    stage_order = pairwiseLLM:::.adaptive_stage_order(),
    refit_id = refit_id
  )

  expect_identical(progress$stage_committed[["anchor_link"]], progress$stage_quotas[["anchor_link"]])
  expect_identical(progress$active_stage, "long_link")
})
