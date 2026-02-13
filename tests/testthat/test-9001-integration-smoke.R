test_that("adaptive integration smoke: deterministic run, resume, and invariants", {
  withr::local_seed(123)

  items <- make_test_items(8)
  judge <- make_deterministic_judge("i_wins")
  session_dir <- withr::local_tempdir()

  # Start and run a small deterministic segment.
  state0 <- adaptive_rank_start(
    items = items,
    seed = 123L,
    session_dir = session_dir
  )
  state1 <- adaptive_rank_run_live(
    state = state0,
    judge = judge,
    n_steps = 6L,
    session_dir = session_dir,
    progress = "none"
  )

  # Shape + step-count assertions.
  expect_true(tibble::is_tibble(state1$step_log))
  expect_true(all(c("step_id", "status", "i", "j", "pair_id") %in% names(state1$step_log)))
  expect_equal(nrow(state1$step_log), 6L)

  ok_rows <- state1$step_log[state1$step_log$status == "ok", , drop = FALSE]
  if (nrow(ok_rows) > 0L) {
    expect_true(all(!is.na(ok_rows$i)))
    expect_true(all(!is.na(ok_rows$j)))
    expect_true(all(ok_rows$i != ok_rows$j))
  }

  # History consistency for committed (ok) steps.
  history <- state1$history_pairs
  expect_true(all(history$A_id != history$B_id))
  if (nrow(ok_rows) > 0L) {
    ids <- as.character(state1$item_ids)
    ok_pairs <- vapply(
      seq_len(nrow(ok_rows)),
      function(idx) {
        a <- ids[[ok_rows$A[[idx]]]]
        b <- ids[[ok_rows$B[[idx]]]]
        paste(sort(c(a, b)), collapse = "|")
      },
      character(1)
    )
    hist_pairs <- vapply(
      seq_len(nrow(history)),
      function(idx) paste(sort(c(history$A_id[[idx]], history$B_id[[idx]])), collapse = "|"),
      character(1)
    )
    expect_true(all(ok_pairs %in% hist_pairs))
    # In this smoke path (warm-start only), committed unordered pairs are unique.
    expect_equal(length(unique(ok_pairs)), length(ok_pairs))
  }

  # Persist explicitly, then resume and continue.
  save_adaptive_session(state1, session_dir = session_dir, overwrite = TRUE)
  resumed <- adaptive_rank_resume(session_dir = session_dir)
  prev_log <- resumed$step_log

  state2 <- adaptive_rank_run_live(
    state = resumed,
    judge = judge,
    n_steps = 2L,
    session_dir = session_dir,
    progress = "none"
  )

  expect_equal(nrow(state2$step_log), nrow(prev_log) + 2L)
  expect_equal(state2$step_log[seq_len(nrow(prev_log)), , drop = FALSE], prev_log)

  # Determinism check: same seed + setup yields same selected pair sequence.
  stateA <- adaptive_rank_start(items = items, seed = 777L)
  stateB <- adaptive_rank_start(items = items, seed = 777L)

  withr::local_seed(777)
  runA <- adaptive_rank_run_live(stateA, judge, n_steps = 6L, progress = "none")
  withr::local_seed(777)
  runB <- adaptive_rank_run_live(stateB, judge, n_steps = 6L, progress = "none")

  seqA <- runA$step_log[, c("i", "j")]
  seqB <- runB$step_log[, c("i", "j")]
  expect_equal(seqA, seqB)
})
