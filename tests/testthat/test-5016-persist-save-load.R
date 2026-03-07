test_that("save_adaptive_session and load_adaptive_session round-trip adaptive artifacts", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  judge <- make_deterministic_judge("i_wins")

  withr::local_seed(1)
  state <- adaptive_rank_run_live(state, judge, n_steps = 2L, progress = "none")

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  expect_true(file.exists(file.path(session_dir, "state.rds")))
  expect_true(file.exists(file.path(session_dir, "step_log.rds")))
  expect_true(file.exists(file.path(session_dir, "round_log.rds")))
  expect_true(file.exists(file.path(session_dir, "metadata.rds")))

  reloaded <- load_adaptive_session(session_dir)
  expect_equal(reloaded$item_ids, state$item_ids)
  expect_equal(nrow(reloaded$step_log), nrow(state$step_log))
  expect_equal(reloaded$meta$schema_version, "adaptive-session")

  file.remove(file.path(session_dir, "state.rds"))
  expect_error(
    load_adaptive_session(session_dir),
    "missing required artifacts"
  )
})

test_that("load_adaptive_session rejects malformed schema metadata", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  meta_path <- file.path(session_dir, "metadata.rds")
  metadata <- readRDS(meta_path)
  metadata$schema_version <- ""
  saveRDS(metadata, meta_path)

  expect_error(
    load_adaptive_session(session_dir),
    "schema_version"
  )
})

test_that("save_adaptive_session overwrite removes stale optional artifacts", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  state$btl_fit <- list(theta = rep(0, 4))
  state$config$persist_item_log <- TRUE
  state$item_log <- list(tibble::tibble(step_id = 1L))

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  expect_true(file.exists(file.path(session_dir, "btl_fit.rds")))
  expect_true(dir.exists(file.path(session_dir, "item_log")))

  state_overwrite <- state
  state_overwrite$btl_fit <- NULL
  state_overwrite$config$persist_item_log <- FALSE

  save_adaptive_session(state_overwrite, session_dir, overwrite = TRUE)

  expect_false(file.exists(file.path(session_dir, "btl_fit.rds")))
  expect_false(dir.exists(file.path(session_dir, "item_log")))

  reloaded <- load_adaptive_session(session_dir)
  expect_null(reloaded$btl_fit)
  expect_false(isTRUE(reloaded$config$persist_item_log))
})

test_that("load_adaptive_session rejects step rows with partial item indices", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  judge <- make_deterministic_judge("i_wins")

  withr::local_seed(1)
  state <- adaptive_rank_run_live(state, judge, n_steps = 1L, progress = "none")

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  step_path <- file.path(session_dir, "step_log.rds")
  step_log <- readRDS(step_path)
  step_log$A[[1]] <- NA_integer_
  saveRDS(step_log, step_path)

  expect_error(
    load_adaptive_session(session_dir),
    "incomplete item indices"
  )
})

test_that("validate_session_dir rejects step_log schema drift (missing/extra/order)", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)

  session_missing <- withr::local_tempdir()
  save_adaptive_session(state, session_missing)
  step_path <- file.path(session_missing, "step_log.rds")
  step_log <- readRDS(step_path)
  step_log$explore_rate_used <- NULL
  saveRDS(step_log, step_path)
  expect_error(
    validate_session_dir(session_missing),
    "missing required columns"
  )

  session_extra <- withr::local_tempdir()
  save_adaptive_session(state, session_extra)
  step_path <- file.path(session_extra, "step_log.rds")
  step_log <- readRDS(step_path)
  step_log$unexpected_col <- 1L
  saveRDS(step_log, step_path)
  expect_error(
    validate_session_dir(session_extra),
    "unexpected columns"
  )

  session_order <- withr::local_tempdir()
  save_adaptive_session(state, session_order)
  step_path <- file.path(session_order, "step_log.rds")
  step_log <- readRDS(step_path)
  reordered <- c(names(step_log)[2L], names(step_log)[1L], names(step_log)[-c(1L, 2L)])
  step_log <- step_log[, reordered, drop = FALSE]
  saveRDS(step_log, step_path)
  expect_error(
    validate_session_dir(session_order),
    "column order does not match canonical schema"
  )
})

test_that("validate_session_dir rejects round_log schema drift for quota fields", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)
  round_path <- file.path(session_dir, "round_log.rds")
  round_log <- readRDS(round_path)
  round_log$long_quota_raw <- NULL
  saveRDS(round_log, round_path)

  expect_error(
    validate_session_dir(session_dir),
    "missing required columns"
  )
})

test_that("load_adaptive_session backfills legacy round_log post-stop columns", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  judge <- make_deterministic_judge("i_wins")

  withr::local_seed(1)
  state <- adaptive_rank_run_live(state, judge, n_steps = 2L, progress = "none")

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  round_path <- file.path(session_dir, "round_log.rds")
  round_log <- readRDS(round_path)
  round_log$max_pairs_after_stop <- NULL
  round_log$pairs_committed_after_stop <- NULL
  saveRDS(round_log, round_path)

  restored <- load_adaptive_session(session_dir)
  expect_true(all(c("max_pairs_after_stop", "pairs_committed_after_stop") %in% names(restored$round_log)))
  expect_true(is.integer(restored$round_log$max_pairs_after_stop))
  expect_true(is.integer(restored$round_log$pairs_committed_after_stop))
  expect_true(all(restored$round_log$max_pairs_after_stop == 0L))
  expect_true(all(restored$round_log$pairs_committed_after_stop == 0L))
})

test_that("load_adaptive_session accepts persisted item logs with current schema", {
  items <- make_test_items(6)
  state <- adaptive_rank_start(items, persist_item_log = TRUE)
  judge <- make_deterministic_judge("i_wins")
  stub <- make_deterministic_fit_fn(state$item_ids)

  withr::local_seed(1)
  state <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 4L,
    fit_fn = stub$fit_fn,
    btl_config = list(refit_pairs_target = 2L),
    progress = "none"
  )
  expect_gte(length(state$item_log), 1L)

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  restored <- load_adaptive_session(session_dir)
  expect_true(isTRUE(restored$config$persist_item_log))
  expect_gte(length(restored$item_log), 1L)
  expect_equal(
    names(adaptive_item_log(restored, refit_id = 1L)),
    pairwiseLLM:::.adaptive_item_log_columns()
  )
})

test_that("validate_session_dir accepts legacy item log schema for resume", {
  items <- make_test_items(6)
  state <- adaptive_rank_start(items, persist_item_log = TRUE)
  judge <- make_deterministic_judge("i_wins")
  stub <- make_deterministic_fit_fn(state$item_ids)

  withr::local_seed(1)
  state <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 4L,
    fit_fn = stub$fit_fn,
    btl_config = list(refit_pairs_target = 2L),
    progress = "none"
  )

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  legacy_cols <- c(
    "refit_id",
    "item_id",
    "theta_mean",
    "theta_p2.5",
    "theta_p5",
    "theta_p50",
    "theta_p95",
    "theta_p97.5",
    "theta_sd",
    "rank_mean",
    "degree",
    "pos_count_A",
    "pos_count_B"
  )
  item_path <- file.path(session_dir, "item_log", "refit_0001.rds")
  item_new <- readRDS(item_path)
  legacy_item <- tibble::tibble(
    refit_id = as.integer(item_new$refit_id),
    item_id = as.character(item_new$item_id),
    theta_mean = as.double(item_new$theta_raw_eap),
    `theta_p2.5` = as.double(item_new$`theta_raw_p2.5`),
    `theta_p5` = as.double(item_new$`theta_raw_p5`),
    `theta_p50` = as.double(item_new$`theta_raw_p50`),
    `theta_p95` = as.double(item_new$`theta_raw_p95`),
    `theta_p97.5` = as.double(item_new$`theta_raw_p97.5`),
    theta_sd = as.double(item_new$theta_raw_sd),
    rank_mean = as.double(item_new$rank_raw),
    degree = as.integer(item_new$degree),
    pos_count_A = as.integer(item_new$pos_count_A),
    pos_count_B = as.integer(item_new$pos_count_B)
  )
  legacy_item <- legacy_item[, legacy_cols, drop = FALSE]
  saveRDS(legacy_item, item_path)

  expect_silent(validate_session_dir(session_dir))
})

test_that("save/load preserves planned probe panels and realized probe bookkeeping", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22")
  )
  state <- adaptive_rank_start(
    items,
    seed = 52L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("ready", "ready"),
      validation_message = c("ok", "ok"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(
      `1` = list(items = tibble::tibble(
        global_item_id = c("gh1", "gh2"),
        theta_raw_mean = c(0.2, -0.2),
        theta_raw_sd = c(0.1, 0.1),
        rank_mu_raw = c(1, 2)
      )),
      `2` = list(items = tibble::tibble(
        global_item_id = c("gs21", "gs22"),
        theta_raw_mean = c(0.1, -0.1),
        theta_raw_sd = c(0.1, 0.1),
        rank_mu_raw = c(1, 2)
      ))
    ),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L),
    set_stop_pass_by_set = list(`1` = TRUE, `2` = TRUE),
    phase = "phase_b",
    ready_spokes = 2L,
    active_phase_a_set = NA_integer_,
    phase_b_started_at_step = 1L
  )
  state <- pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins"))

  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)
  restored <- load_adaptive_session(session_dir)

  expect_equal(
    restored$linking$probe$panels_by_spoke[["2"]]$pair_key,
    state$linking$probe$panels_by_spoke[["2"]]$pair_key
  )
  expect_equal(
    restored$linking$probe$realized_edges$pair_key,
    state$linking$probe$realized_edges$pair_key
  )
})
