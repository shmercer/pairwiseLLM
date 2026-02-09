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
