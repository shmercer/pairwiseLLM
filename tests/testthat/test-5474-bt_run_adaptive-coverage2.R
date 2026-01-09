test_that("bt_run_adaptive: resume-stage bookkeeping and bootstrap helper branches", {
  # ---- Build a minimal checkpoint payload to exercise resume stage logic ----
  td <- withr::local_tempdir()

  ids <- c("A", "B")
  samples <- tibble::tibble(ID = ids, text = c("a", "b"))

  # Prior rounds: last stage is stage1_rc so the resume logic should set
  # stage counters and keep stage 1.
  rounds_prev <- tibble::tibble(
    round_index = 1L,
    stage = "stage1_rc",
    conn_streak = 2L,
    stage2_rounds = 0L
  )

  chk <- list(
    run_type = "adaptive",
    ids = ids,
    results = tibble::tibble(ID1 = character(), ID2 = character(), better_id = character()),
    rounds = rounds_prev,
    state = tibble::tibble(),
    next_round = 2L,
    completed = FALSE
  )
  pairwiseLLM:::.bt_write_checkpoint(td, chk, basename = "run_state", overwrite = TRUE)

  # Resume should execute the hybrid stage bookkeeping block (uncovered lines
  # around 903-923), even if we stop early before doing any rounds.
  # Use stage1_max_rounds = 0 so the loop terminates deterministically.
  out <- pairwiseLLM::bt_run_adaptive(
    samples = samples,
    judge_fun = function(pairs) tibble::tibble(ID1 = character(), ID2 = character(), better_id = character()),
    fit_engine_running = "hybrid",
    resume_from = td,
    checkpoint_dir = td,
    stage1_max_rounds = 1L,
    init_round_size = 0L,
    max_rounds = 0L,
    final_refit = FALSE,
    return_diagnostics = FALSE
  )
  expect_true(inherits(out, "pairwiseLLM_run"))

  # ---- Cover the stage2_min_rounds guard (line 535) ----
  expect_error(
    pairwiseLLM::bt_run_adaptive(
      samples = samples,
      judge_fun = function(pairs) tibble::tibble(ID1 = character(), ID2 = character(), better_id = character()),
      stage2_min_rounds = -1L,
      init_round_size = 0L,
      max_rounds = 0L,
      final_refit = FALSE,
      return_diagnostics = FALSE
    ),
    "stage2_min_rounds",
    fixed = TRUE
  )

  # ---- Capture and exercise the internal .bootstrap_pairs helper ----
  # We deliberately trigger a checkpoint read error (nonexistent dir) so we can
  # grab the bt_run_adaptive frame without unwinding.
  captured <- NULL
  captured_add_key <- NULL
  expect_error(
    withCallingHandlers(
      pairwiseLLM::bt_run_adaptive(
        samples = samples,
        judge_fun = function(pairs) tibble::tibble(ID1 = character(), ID2 = character(), better_id = character()),
        resume_from = file.path(td, "does-not-exist"),
        checkpoint_dir = file.path(td, "does-not-exist"),
        init_round_size = 0L,
        max_rounds = 0L,
        final_refit = FALSE,
        return_diagnostics = FALSE
      ),
      error = function(e) {
        # Find the bt_run_adaptive frame and extract the inner function.
        frames <- sys.frames()
        idx <- which(vapply(frames, function(f) exists(".bootstrap_pairs", envir = f, inherits = FALSE), logical(1)))
        if (length(idx) > 0L) {
          f <- frames[[idx[[1]]]]
          captured <<- get(".bootstrap_pairs", envir = f, inherits = FALSE)
          if (exists(".add_pair_key_direction", envir = f, inherits = FALSE)) {
            captured_add_key <<- get(".add_pair_key_direction", envir = f, inherits = FALSE)
          }
        }
      }
    ),
    "No checkpoint found",
    fixed = TRUE
  )
  expect_true(is.function(captured))
  expect_true(is.function(captured_add_key))

  # .add_pair_key_direction should no-op when required columns are missing
  # (covers line 632).
  no_key <- captured_add_key(tibble::tibble(x = 1))
  expect_identical(no_key, tibble::tibble(x = 1))

  # 1) invalid n_pairs (covers line 647)
  expect_error(captured(n_pairs = -1L, existing_pairs = tibble::tibble(), seed = NULL), "n_pairs", fixed = TRUE)

  # 2) early-return empty when n_pairs == 0 (covers line 651)
  empty <- captured(n_pairs = 0L, existing_pairs = tibble::tibble(), seed = NULL)
  expect_identical(names(empty), c("ID1", "text1", "ID2", "text2"))
  expect_equal(nrow(empty), 0L)

  # 3) nontrivial path with existing pairs and position balancing metadata
  existing <- tibble::tibble(ID1 = "A", ID2 = "B")
  # ensure stable ordering and stable key generation
  res <- captured(n_pairs = 1L, existing_pairs = existing, seed = NULL)
  expect_equal(nrow(res), 1L)
  expect_true(all(c("ID1", "ID2", "text1", "text2") %in% names(res)))
})


test_that("bt_run_adaptive: planned_repeat_pairs planned_keys fallback", {
  # This covers the branch where planned_repeat_pairs lacks `pair_key` and the
  # runner derives it from ID1/ID2 (lines 1292-1293).
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))

  # Mock bt_adaptive_round to return a pairs tibble with a planned_repeat_pairs
  # attribute that has ID1/ID2 but no pair_key.
  fake_pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")
  attr(fake_pairs, "planned_repeat_pairs") <- tibble::tibble(ID1 = "A", ID2 = "B")

  testthat::local_mocked_bindings(
    bt_adaptive_round = function(...) {
      list(
        pairs = fake_pairs,
        fit = NULL,
        metrics = list(stop = FALSE),
        stage = "stage2",
        stop_reason = NA_character_,
        pairing_diagnostics = tibble::tibble()
      )
    },
    .env = asNamespace("pairwiseLLM")
  )

  # Fit/build helpers aren't needed because we stop immediately.
  out <- pairwiseLLM::bt_run_adaptive(
    samples = samples,
    judge_fun = function(pairs) tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
    init_round_size = 0L,
    max_rounds = 1L,
    min_rounds = 0L,
    final_refit = FALSE,
    return_diagnostics = FALSE
  )
  expect_true(inherits(out, "pairwiseLLM_run"))
})