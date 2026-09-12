test_that("saved adaptive sessions reject malformed objects and missing artifacts", {
  state <- task07_start(task07_fixture(), strategy = "random")
  session <- withr::local_tempdir()
  save_adaptive_session(state, session)
  saveRDS(list(), file.path(session, "state.rds"))
  expect_error(validate_session_dir(session), "adaptive_state")
  save_adaptive_session(state, session, overwrite = TRUE)
  unlink(file.path(session, "metadata.rds"))
  expect_error(validate_session_dir(session), "missing required artifacts")
  save_adaptive_session(state, session, overwrite = TRUE)
  unlink(file.path(session, "link_stage_log.rds"))
  loaded <- load_adaptive_session(session)
  expect_identical(loaded$link_stage_log, pairwiseLLM:::new_link_stage_log())
  expect_identical(loaded$trueskill_state, state$trueskill_state)
  expect_identical(loaded$controller$pairing_strategy, "random")
})

test_that("state validation rejects malformed persisted identity and Phase A surfaces", {
  state <- task07_start(task07_fixture())
  bad <- state
  bad$global_item_ids <- 1:8
  expect_error(pairwiseLLM:::validate_state(bad), "global_item_ids")
  bad <- state
  bad$linking <- "bad"
  expect_error(pairwiseLLM:::validate_state(bad), "linking.*list")
  bad <- state
  bad$linking$hub_id <- NA_integer_
  expect_error(pairwiseLLM:::validate_state(bad), "hub_id")
  bad <- state
  bad$history_pairs <- list()
  expect_error(pairwiseLLM:::validate_state(bad), "history_pairs")
  for (field in c("set_status", "artifacts", "phase", "phase_b_started_at_step")) {
    bad <- state
    bad$linking$phase_a[[field]] <- switch(field, set_status = list(),
      artifacts = 1L, phase = "unknown", phase_b_started_at_step = c(1L, 2L))
    expect_error(pairwiseLLM:::validate_state(bad), field)
  }
  bad <- state
  bad$linking$phase_a <- "bad"
  expect_error(pairwiseLLM:::validate_state(bad), "phase_a.*list")
})

test_that("item log rewrites trim only obsolete refit artifacts", {
  directory <- file.path(withr::local_tempdir(), "item_log")
  logs <- list(tibble::tibble(item_id = "a", theta = 1), tibble::tibble(item_id = "a", theta = 2))
  write <- pairwiseLLM:::.adaptive_write_item_log_files
  write(logs, directory)
  extra <- file.path(directory, "unrelated.rds")
  saveRDS("keep", extra)
  write(logs[1L], directory, trim_stale = TRUE)
  expect_true(file.exists(file.path(directory, "refit_0001.rds")))
  expect_false(file.exists(file.path(directory, "refit_0002.rds")))
  expect_identical(readRDS(extra), "keep")
  write(list(tibble::tibble(item_id = "a", theta = 99)), directory, overwrite_existing = FALSE)
  expect_identical(readRDS(file.path(directory, "refit_0001.rds")), logs[[1L]])
  write(list(), directory, trim_stale = TRUE)
  expect_false(dir.exists(directory))
})

test_that("stop boundary backfill counts committed evidence and refuses further replay at the cap", {
  f <- task07_fixture(n = 4L)
  state <- task07_run(f, strategy = "random", n_steps = 5L)$state
  state$meta$stop_boundary_step_id <- 3L
  state$meta$stop_boundary_refit_id <- 1L
  state$meta$pairs_committed_after_stop <- -1L
  state$controller$max_pairs_after_stop <- 2L
  fixed <- pairwiseLLM:::.adaptive_stop_boundary_bootstrap(state)
  expect_identical(fixed$meta$pairs_committed_after_stop, 2L)
  state$step_log$pair_id[5L] <- NA_integer_
  expect_identical(pairwiseLLM:::.adaptive_stop_boundary_bootstrap(state)$meta$pairs_committed_after_stop, 1L)
  state$step_log <- pairwiseLLM:::new_step_log()
  expect_identical(pairwiseLLM:::.adaptive_stop_boundary_bootstrap(state)$meta$pairs_committed_after_stop, 0L)
  session <- withr::local_tempdir()
  save_adaptive_session(fixed, session)
  resumed <- load_adaptive_session(session)
  actual <- adaptive_rank_run_live(resumed, function(...) stop("No evidence may be requested"),
    n_steps = 1L, progress = "none")
  expect_identical(actual$meta$stop_reason, "max_pairs_after_stop_exhausted")
  expect_identical(actual$history_pairs, resumed$history_pairs)
  expect_identical(actual$trueskill_state, resumed$trueskill_state)
  expect_identical(actual$step_log, resumed$step_log)
})

test_that("refit reconciliation rejects corrupt step boundaries without consuming evidence", {
  f <- task07_fixture(n = 4L)
  fit <- make_test_btl_fit(f$ids, draws = outer(seq_len(10) * 0.005, 1:4, "+"))
  state <- adaptive_rank_run_live(task07_start(f, strategy = "random"),
    make_adaptive_judge_replay(f$outcomes, f$ids), n_steps = 3L,
    fit_fn = function(...) fit, btl_config = list(refit_pairs_target = 3L), progress = "none")
  for (boundary in c(-1L, 4L)) {
    log <- state$round_log
    log$step_id_at_refit <- boundary
    expect_error(pairwiseLLM:::.adaptive_resume_reconcile_refit_meta(state, state$step_log, log),
      "out of range")
  }
  fixed <- pairwiseLLM:::.adaptive_resume_reconcile_refit_meta(state, state$step_log, state$round_log)
  expect_identical(fixed$refit_meta$last_refit_M_done, 3L)
  expect_identical(fixed$history_pairs[c("A_id", "B_id")], state$history_pairs[c("A_id", "B_id")])
  expect_false(any(fixed$history_pairs[["is_probe_step"]] %in% TRUE))
  expect_identical(fixed$trueskill_state, state$trueskill_state)
})
