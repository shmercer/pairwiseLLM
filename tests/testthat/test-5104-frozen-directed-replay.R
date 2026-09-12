replay_matrix_5104 <- function(ids = letters[1:4]) {
  out <- expand.grid(A_id = ids, B_id = ids, stringsAsFactors = FALSE)
  out <- out[out$A_id != out$B_id, ]
  out$Y <- as.integer(seq_len(nrow(out)) %% 3L == 0L)
  tibble::as_tibble(out)
}

replay_item_5104 <- function(id) tibble::tibble(item_id = id)

test_that("complete directed matrices retain exact rows, binary values, and metadata", {
  for (n in c(2L, 4L, 12L)) {
    ids <- letters[seq_len(n)]
    outcomes <- replay_matrix_5104(ids)
    outcomes$request_id <- paste0("request-", seq_len(nrow(outcomes)))
    outcomes$panel_id <- "synthetic"
    expect_identical(validate_adaptive_replay(outcomes, rev(ids)), outcomes)
    expect_identical(nrow(outcomes), n * (n - 1L))
    for (y in list(as.logical(outcomes$Y), as.double(outcomes$Y), as.character(outcomes$Y))) {
      converted <- outcomes
      converted$Y <- y
      expect_identical(validate_adaptive_replay(converted, ids), outcomes)
    }
  }
})

test_that("matrix validation rejects malformed input without dropping rows", {
  ids <- letters[1:4]
  outcomes <- replay_matrix_5104(ids)
  for (bad in list(NULL, 1:4, character(), "a", c("a", "a"), c("a", NA),
    c("a", ""), c("a", " "), matrix(ids), factor(ids))) {
    expect_error(validate_adaptive_replay(outcomes, bad), "item_ids")
  }
  for (bad in list(NULL, 1, NA, logical(), c(TRUE, FALSE), matrix(TRUE))) {
    expect_error(validate_adaptive_replay(outcomes, ids, complete = bad), "complete")
    expect_error(make_adaptive_judge_replay(outcomes, ids, strict_use = bad), "strict_use")
  }
  duplicate_names <- as.data.frame(outcomes)
  names(duplicate_names) <- c("A_id", "B_id", "B_id")
  for (bad in list(NULL, as.list(outcomes), outcomes[, 1:2], duplicate_names)) {
    expect_error(validate_adaptive_replay(bad, ids), "data frame.*A_id, B_id, Y")
  }
  for (column in c("A_id", "B_id")) {
    for (value in list(NA_character_, "", " ", 1, factor("a"))) {
      bad <- outcomes
      bad[[column]] <- rep(value, nrow(bad))
      expect_error(validate_adaptive_replay(bad, ids), "character IDs")
    }
    bad <- outcomes
    bad[[column]][[1L]] <- "foreign"
    expect_error(validate_adaptive_replay(bad, ids), "active panel.*row 1.*foreign")
  }
  bad <- outcomes
  bad$B_id[[1L]] <- bad$A_id[[1L]]
  expect_error(validate_adaptive_replay(bad, ids), "Self-pair.*row 1")
  for (value in list(NA, NaN, Inf, -1, 2, 0.1, "yes", "0.0", " 1", "TRUE")) {
    bad <- outcomes
    bad$Y[[1L]] <- value
    expect_error(validate_adaptive_replay(bad, ids), "Y must be exactly.*row 1")
  }
  for (value in list(factor(outcomes$Y), as.list(outcomes$Y), as.complex(outcomes$Y),
    matrix(outcomes$Y), as.Date(outcomes$Y, origin = "2000-01-01"))) {
    bad <- outcomes
    bad$Y <- value
    expect_error(validate_adaptive_replay(bad, ids), "binary.*vector")
  }
  expect_error(validate_adaptive_replay(rbind(outcomes, outcomes[1, ]), ids),
    "Duplicate ordered key.*row 13")
  # Correct row count alone is insufficient.
  expect_error(validate_adaptive_replay(rbind(outcomes[-1, ], outcomes[2, ]), ids),
    "Duplicate ordered key")
  expect_error(validate_adaptive_replay(outcomes[-1, ], ids),
    "expected 12 rows; found 11.*Missing ordered key: \\(b, a\\)")
  expect_error(validate_adaptive_replay(outcomes[0, ], ids), "found 0.*Missing ordered key")
  expect_identical(validate_adaptive_replay(outcomes[0, ], ids, complete = FALSE), outcomes[0, ])
})

test_that("replay uses only the requested orientation and rejects exact reuse", {
  outcomes <- tibble::tibble(A_id = c("a", "b"), B_id = c("b", "a"), Y = c(0L, 1L))
  a <- replay_item_5104("a")
  b <- replay_item_5104("b")
  judge <- make_adaptive_judge_replay(outcomes, c("a", "b"))
  expect_identical(judge(a, b)$Y, 0L)
  expect_identical(judge(b, a)$Y, 1L)
  expect_error(judge(a, b), "already used.*\\(a, b\\)")
  expect_error(judge(b, a), "already used.*\\(b, a\\)")
  # Equal values in opposite orientations prove there is no complement operation.
  outcomes$Y[] <- 1L
  judge <- make_adaptive_judge_replay(outcomes, c("a", "b"))
  expect_identical(judge(a, b)$Y, 1L)
  expect_identical(judge(b, a)$Y, 1L)
  for (bad in list(NULL, "a", tibble::tibble(x = "a"), rbind(a, b))) {
    expect_error(judge(bad, b), "one-row data frame")
    expect_error(judge(a, bad), "one-row data frame")
  }
  expect_error(judge(replay_item_5104(1), b), "character IDs")
  for (bad in list(list(), adaptive_rank_start(c("a", "c")),
    adaptive_rank_start(c("a", "b", "c")))) {
    expect_error(judge(a, b, bad), "state item IDs.*exactly")
  }
  expect_error(judge(a, replay_item_5104("foreign")), "No frozen outcome.*foreign")
  expect_error(judge(a, a), "No frozen outcome.*\\(a, a\\)")
  partial <- make_adaptive_judge_replay(outcomes[1, ], c("a", "b"), complete = FALSE)
  expect_error(partial(b, a), "No frozen outcome.*\\(b, a\\)")
  expect_identical(partial(a, b)$Y, 1L)
  empty <- make_adaptive_judge_replay(outcomes[0, ], c("a", "b"), complete = FALSE)
  expect_error(empty(a, b), "No frozen outcome")
  loose <- make_adaptive_judge_replay(outcomes, c("a", "b"), strict_use = FALSE)
  expect_identical(loose(a, b), loose(a, b))
})

test_that("replay keys cannot collide on delimiters and caller mutation does not change outcomes", {
  ids <- c("a:b", "c", "a", "b:c", "é")
  outcomes <- replay_matrix_5104(ids)
  expected <- outcomes
  judge <- make_adaptive_judge_replay(outcomes, ids)
  outcomes$Y[] <- 1L - outcomes$Y
  ids[] <- "changed"
  for (k in seq_len(nrow(expected))) {
    result <- judge(replay_item_5104(expected$A_id[[k]]), replay_item_5104(expected$B_id[[k]]))
    expect_identical(result$Y, expected$Y[[k]])
    expect_true(pairwiseLLM:::validate_judge_result(result,
      expected$A_id[[k]], expected$B_id[[k]])$is_valid)
  }
})

test_that("replay neither draws randomness nor calls the network or simulation judge", {
  withr::local_seed(711)
  before <- .Random.seed
  testthat::local_mocked_bindings(req_perform = function(...) stop("network called"),
    .package = "httr2")
  testthat::local_mocked_bindings(.adaptive_simulation_judge = function(...) stop("simulation called"),
    .package = "pairwiseLLM")
  ids <- letters[1:4]
  judge <- make_adaptive_judge_replay(replay_matrix_5104(ids), ids, strict_use = FALSE)
  a <- replay_item_5104("a")
  b <- replay_item_5104("b")
  state <- adaptive_rank_start(ids)
  result <- judge(a, b, state)
  state$step_log <- state$step_log[rep(NA_integer_, 10L), ]
  expect_identical(judge(a, b, state, step_id = 987L), result)
  expect_identical(.Random.seed, before)
  expect_identical(result$judge_backend, "replay")
})

test_that("study ceiling is validated and preserves the historical relaxed third observation", {
  for (bad in list(1, 4, 2.5, Inf, NA, "2", TRUE, numeric(), c(2, 3), matrix(2))) {
    expect_error(adaptive_rank_start(letters[1:4],
      adaptive_config = list(dup_max_obs_relaxed = bad)), "dup_max_obs_relaxed")
  }
  expect_identical(adaptive_rank_start(letters[1:4])$controller$dup_max_obs_relaxed, 3L)
  expect_identical(adaptive_rank_start(letters[1:4],
    adaptive_config = list(dup_max_obs_relaxed = NULL))$controller$dup_max_obs_relaxed, 3L)
  items <- make_test_items(2)
  history <- tibble::tibble(A_id = c("1", "2"), B_id = c("2", "1"))
  state <- make_test_state(items, make_test_trueskill_state(items), history)
  normal <- pairwiseLLM:::select_next_pair(state, candidates = tibble::tibble(i = "1", j = "2"))
  expect_false(normal$candidate_starved)
  expect_identical(c(normal$A, normal$B), c(1L, 2L))
  explicit <- pairwiseLLM:::.adaptive_apply_controller_config(state, list(dup_max_obs_relaxed = 3L))
  expect_identical(pairwiseLLM:::select_next_pair(explicit,
    candidates = tibble::tibble(i = "1", j = "2")), normal)
  study <- pairwiseLLM:::.adaptive_apply_controller_config(state, list(dup_max_obs_relaxed = 2L))
  selected <- pairwiseLLM:::select_next_pair(study, candidates = tibble::tibble(i = "1", j = "2"))
  expect_true(selected$candidate_starved)
  expect_true(is.na(selected$A))
  study$controller$dup_max_obs_relaxed <- NULL
  expect_identical(pairwiseLLM:::.adaptive_controller_resolve(study)$dup_max_obs_relaxed, 3L)
})

test_that("live replay is deterministic, reverses repeats, and stops before a third observation", {
  withr::local_seed(817)
  before <- .Random.seed
  for (strategy in c("hybrid", "random", "trueskill_p50", "trueskill_pollitt")) {
    for (n in c(2L, 4L)) {
      ids <- letters[seq_len(n)]
      outcomes <- replay_matrix_5104(ids)
      run <- function() {
        state <- adaptive_rank_start(ids, seed = 71L,
          now_fn = function() as.POSIXct("2026-09-11", tz = "UTC"),
          adaptive_config = list(pairing_strategy = strategy, dup_max_obs_relaxed = 2L))
        adaptive_rank_run_live(state, make_adaptive_judge_replay(outcomes, ids),
          n_steps = 30L, btl_config = list(refit_pairs_target = 5000L), progress = "none")
      }
      first <- run()
      second <- run()
      expect_identical(first$history_pairs, second$history_pairs)
      expect_identical(first$step_log, second$step_log)
      expect_identical(first$trueskill_state, second$trueskill_state)
      history <- first$step_log[first$step_log$status == "ok", c("A_id", "B_id", "Y")]
      expect_gt(nrow(history), n - 1L)
      expect_lte(nrow(history), n * (n - 1L))
      expect_identical(anyDuplicated(history[, c("A_id", "B_id")]), 0L)
      for (k in seq_len(nrow(history))) {
        row <- which(outcomes$A_id == history$A_id[[k]] & outcomes$B_id == history$B_id[[k]])
        expect_identical(history$Y[[k]], outcomes$Y[[row]])
      }
      counts <- table(pairwiseLLM:::make_unordered_key(history$A_id, history$B_id))
      expect_true(all(counts <= 2L))
      expect_true(all(first$step_log$judge_backend[first$step_log$status == "ok"] == "replay"))
      if (n == 2L) {
        expect_identical(nrow(history), 2L)
        expect_identical(history$A_id, rev(history$B_id))
      }
    }
  }
  expect_identical(.Random.seed, before)
})

test_that("replay bootstrap evidence is identical across all warm modes", {
  ids <- letters[1:4]
  outcomes <- replay_matrix_5104(ids)
  prior <- make_warm_start_prior(stats::setNames(c(-1, -0.2, 0.2, 1), ids))
  reference <- NULL
  for (mode in c("cold", "btl_only", "trueskill_only", "both")) {
    state <- adaptive_rank_start(ids, seed = 56L, warm_start_mode = mode,
      warm_start_prior = if (mode == "cold") NULL else prior,
      adaptive_config = list(dup_max_obs_relaxed = 2L))
    state <- adaptive_rank_run_live(state, make_adaptive_judge_replay(outcomes, ids),
      n_steps = 3L, progress = "none")
    evidence <- state$step_log[, c("A_id", "B_id", "Y")]
    if (is.null(reference)) reference <- evidence
    expect_identical(evidence, reference)
  }
})

test_that("Phase A respects the study ceiling while Phase B keeps its existing limit", {
  items <- tibble::tibble(item_id = 1:4, set_id = c(1L, 1L, 2L, 2L))
  for (ceiling in c(2L, 3L)) {
    for (n_prior in 1:2) {
      history <- tibble::tibble(A_id = c("1", "2")[seq_len(n_prior)],
        B_id = c("2", "1")[seq_len(n_prior)])
      state <- make_test_state(items, make_test_trueskill_state(items), history)
      state$controller$run_mode <- "link_one_spoke"
      state$controller$dup_max_obs_relaxed <- ceiling
      config <- pairwiseLLM:::adaptive_defaults(4L)
      config$dup_max_obs_relaxed <- ceiling
      for (phase in c("phase_a", "phase_b")) {
        state$linking$phase_a$phase <- phase
        hs <- pairwiseLLM:::.adaptive_history_state_resolve(state)
        result <- pairwiseLLM:::.adaptive_select_stage(
          stage = list(name = "relaxed", dup_policy = "relaxed"),
          state = state, config = config, controller = state$controller,
          generation_stage = "warm_start", round = state$round, history_state = hs,
          counts = pairwiseLLM:::.adaptive_history_state_counts(hs, state$item_ids),
          step_id = 1L, seed_base = 1L, candidates = tibble::tibble(i = "1", j = "2"))
        expected <- as.integer(phase == "phase_a" && n_prior < ceiling)
        expect_identical(nrow(result$selected), expected)
      }
    }
  }
})

test_that("a fresh R session resumes replay and does not create an RNG state", {
  session <- withr::local_tempdir()
  result_path <- file.path(session, "child-result.rds")
  config_path <- file.path(session, "child-config.rds")
  ids <- letters[1:4]
  outcomes <- replay_matrix_5104(ids)
  state <- adaptive_rank_start(ids, seed = 41L,
    adaptive_config = list(pairing_strategy = "trueskill_pollitt", dup_max_obs_relaxed = 2L))
  judge <- make_adaptive_judge_replay(outcomes, ids)
  state <- adaptive_rank_run_live(state, judge, n_steps = 4L, progress = "none")
  save_adaptive_session(state, session)
  expected <- adaptive_rank_run_live(state, judge, n_steps = 2L, progress = "none")
  dev_path <- if (pkgload::is_dev_package("pairwiseLLM")) {
    getNamespaceInfo("pairwiseLLM", "path")
  } else {
    NULL
  }
  saveRDS(list(dev_path = dev_path, libpaths = .libPaths(), session = session,
    result_path = result_path, outcomes = outcomes, ids = ids), config_path)
  code <- paste(
    "cfg <- readRDS(commandArgs(TRUE)[[1]])",
    ".libPaths(cfg$libpaths)",
    "if (!is.null(cfg$dev_path)) pkgload::load_all(cfg$dev_path, quiet=TRUE) else library(pairwiseLLM)",
    "s <- pairwiseLLM::load_adaptive_session(cfg$session)",
    "if (exists('.Random.seed', .GlobalEnv)) rm('.Random.seed', envir=.GlobalEnv)",
    "j <- pairwiseLLM::make_adaptive_judge_replay(cfg$outcomes, cfg$ids)",
    "err <- tryCatch(j(data.frame(item_id=s$history_pairs$A_id[1]),",
    "data.frame(item_id=s$history_pairs$B_id[1]), s), error=conditionMessage)",
    "stopifnot(is.character(err), grepl('already used', err))",
    "s <- pairwiseLLM::adaptive_rank_run_live(s, j, n_steps=2L, progress='none')",
    "stopifnot(!exists('.Random.seed', .GlobalEnv))",
    "saveRDS(s, cfg$result_path)", sep = "\n")
  output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", shQuote(code), shQuote(config_path)), stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if ((!is.null(status) && status != 0L) || !file.exists(result_path)) {
    stop("Fresh replay process did not produce a result (status ",
      if (is.null(status)) 0L else status, "):\n", paste(output, collapse = "\n"))
  }
  actual <- readRDS(result_path)
  fields <- c("A_id", "B_id", "Y", "pairing_strategy", "target_distance")
  expect_identical(actual$step_log[, fields], expected$step_log[, fields])
  expect_identical(actual$controller$dup_max_obs_relaxed, 2L)
  expect_identical(actual$history_state, expected$history_state)
  expect_identical(actual$trueskill_state, expected$trueskill_state)
})

test_that("a recreated judge uses committed history and a saved study ceiling on resume", {
  ids <- letters[1:4]
  outcomes <- replay_matrix_5104(ids)
  for (strategy in c("hybrid", "random")) {
    state <- adaptive_rank_start(ids, seed = 8L,
      adaptive_config = list(pairing_strategy = strategy, dup_max_obs_relaxed = 2L))
    judge <- make_adaptive_judge_replay(outcomes, ids)
    state <- adaptive_rank_run_live(state, judge, n_steps = 4L,
      btl_config = list(refit_pairs_target = 5000L), progress = "none")
    path <- withr::local_tempdir()
    save_adaptive_session(state, path)
    resumed <- load_adaptive_session(path)
    expect_identical(resumed$controller$dup_max_obs_relaxed, 2L)
    fresh_judge <- make_adaptive_judge_replay(outcomes, ids)
    a <- replay_item_5104(state$history_pairs$A_id[[1L]])
    b <- replay_item_5104(state$history_pairs$B_id[[1L]])
    expect_error(fresh_judge(a, b, resumed), "already used")
    loose <- make_adaptive_judge_replay(outcomes, ids, strict_use = FALSE)
    expect_true(loose(a, b, resumed)$is_valid)
    continued <- adaptive_rank_run_live(state, judge, n_steps = 3L, progress = "none")
    resumed <- adaptive_rank_run_live(resumed, fresh_judge, n_steps = 3L, progress = "none")
    expect_identical(resumed$step_log[, c("A_id", "B_id", "Y")],
      continued$step_log[, c("A_id", "B_id", "Y")])
    expect_identical(resumed$trueskill_state, continued$trueskill_state)
    expect_identical(resumed$warm_start_idx, continued$warm_start_idx)
  }
})
