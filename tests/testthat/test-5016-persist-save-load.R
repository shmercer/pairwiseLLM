test_that("save_adaptive_session and load_adaptive_session round-trip v2 artifacts", {
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
  expect_equal(reloaded$meta$schema_version, "v2-0")

  file.remove(file.path(session_dir, "state.rds"))
  expect_error(
    load_adaptive_session(session_dir),
    "missing required artifacts"
  )
})

test_that("load_adaptive_session rejects unsupported schema versions", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  meta_path <- file.path(session_dir, "metadata.rds")
  metadata <- readRDS(meta_path)
  metadata$schema_version <- "v1-0"
  saveRDS(metadata, meta_path)

  expect_error(
    load_adaptive_session(session_dir),
    "Unsupported adaptive session schema version"
  )
})
