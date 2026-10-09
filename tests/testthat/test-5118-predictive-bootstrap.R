bootstrap_fixture_5118 <- function() {
  f <- reservoir_fixture()
  f$prior <- make_warm_start_prior(stats::setNames(c(-1, -0.6, -0.2, 0.2, 0.6, 1), f$ids),
    prior_sd = stats::setNames(c(0.2, 0.7, 0.3, 0.8, 0.4, 0.6), f$ids))
  f
}

bootstrap_args_5118 <- function(f) {
  clock <- function() as.POSIXct("2026-10-09", tz = "UTC")
  environment(clock) <- baseenv()
  list(items = f$ids, seed = 87L, now_fn = clock, replay_reservoir = f$reservoir,
    warm_start_prior = f$prior, warm_start_mode = "both",
    warm_start_trueskill = "predictive_distribution", bootstrap_policy = "predictive_connected",
    adaptive_config = list(pairing_strategy = "trueskill_pollitt"))
}

bootstrap_start_5118 <- function(f = bootstrap_fixture_5118(), ...) {
  do.call(adaptive_rank_start, utils::modifyList(bootstrap_args_5118(f), list(...), keep.null = TRUE))
}

test_that("the five arms isolate destinations, distribution, and graph policy", {
  withr::local_seed(316L)
  rng <- .Random.seed
  f <- bootstrap_fixture_5118()
  modes <- c("cold", "btl_only", "both", "trueskill_only", "both")
  states <- lapply(seq_along(modes), function(i) {
    bootstrap_start_5118(f, warm_start_mode = modes[[i]],
      warm_start_prior = if (i == 1L) NULL else f$prior,
      warm_start_trueskill = if (i <= 2L) NULL else "predictive_distribution",
      bootstrap_policy = if (i <= 3L) "shuffled_connected" else "predictive_connected")
  })
  expect_identical(states[[1]]$warm_start_pairs, states[[2]]$warm_start_pairs)
  expect_identical(states[[1]]$warm_start_pairs, states[[3]]$warm_start_pairs)
  expect_identical(states[[4]]$warm_start_pairs, states[[5]]$warm_start_pairs)
  expect_false(identical(states[[3]]$warm_start_pairs, states[[5]]$warm_start_pairs))
  expect_identical(states[[1]]$trueskill_state, states[[2]]$trueskill_state)
  expect_identical(states[[3]]$trueskill_state, states[[4]]$trueskill_state)
  expect_identical(states[[3]]$trueskill_state, states[[5]]$trueskill_state)
  for (i in seq_along(states)) {
    state <- states[[i]]
    expect_identical(pairwiseLLM:::.warm_start_btl_prior_for_state(state),
      if (i %in% c(2L, 3L, 5L)) f$prior else NULL)
    expect_identical(nrow(state$warm_start_pairs), length(f$ids) - 1L)
    finished <- reservoir_run(state, f, 7L)
    expect_identical(nrow(finished$history_pairs), 7L)
    expect_identical(sum(finished$step_log$round_stage == "warm_start"), 5L)
    expect_identical(finished$step_log$round_stage[6:7], rep("direct_pairing", 2L))
    expect_identical(finished$step_log$pairing_strategy[6:7], rep("trueskill_pollitt", 2L))
    expect_identical(finished$warm_start_idx, 6L)
    expect_true(finished$warm_start_done)
    expect_identical(finished$warm_start_pairs, state$warm_start_pairs)
    expect_reservoir_evidence(finished, f)
    expect_no_error(pairwiseLLM:::.adaptive_reservoir_validate_state(finished))
  }
  expect_identical(.Random.seed, rng)
})

test_that("predictive queues are built once from selectable endpoints without Y", {
  f <- bootstrap_fixture_5118()
  original <- pairwiseLLM:::.adaptive_predictive_tree
  calls <- 0L
  testthat::local_mocked_bindings(.adaptive_predictive_tree = function(item_ids, edges,
      initial_prediction, seed, policy) {
    calls <<- calls + 1L
    expect_identical(edges, f$reservoir$manifest$edges)
    expect_identical(names(edges), c("A_id", "B_id"))
    original(item_ids, edges, initial_prediction, seed, policy)
  }, .package = "pairwiseLLM")
  initial <- bootstrap_start_5118(f)
  expect_identical(calls, 1L)
  changed <- f
  changed$outcomes$Y <- 1L - f$outcomes$Y
  changed$reservoir <- make_adaptive_replay_reservoir(changed$outcomes, changed$ids)
  other <- bootstrap_start_5118(changed)
  expect_identical(initial$warm_start_pairs, other$warm_start_pairs)
  expect_identical(initial$bootstrap, other$bootstrap)
  expect_false(identical(initial$meta$replay_reservoir_digest, other$meta$replay_reservoir_digest))
  expect_identical(calls, 2L)
  full <- reservoir_run(initial, f, 7L)
  path <- withr::local_tempdir()
  save_adaptive_session(full, path)
  expect_no_error(validate_session_dir(path))
  restored <- adaptive_rank_resume(path)
  expect_identical(calls, 2L)
  expect_identical(restored$bootstrap, initial$bootstrap)
  expect_identical(restored$warm_start_pairs, initial$warm_start_pairs)
  # Held-out and reversal observations never enter this manifest or the builder.
  keys <- pairwiseLLM:::.adaptive_reservoir_ordered_key(
    initial$warm_start_pairs$i_id, initial$warm_start_pairs$j_id)
  expect_true(all(keys %in% pairwiseLLM:::.adaptive_reservoir_ordered_key(f$outcomes$A_id, f$outcomes$B_id)))
  expect_false(any(keys %in% pairwiseLLM:::.adaptive_reservoir_ordered_key(f$outcomes$B_id, f$outcomes$A_id)))
})

test_that("invalid attempts retain predictive bootstrap and Pollitt decisions", {
  f <- bootstrap_fixture_5118()
  initial <- bootstrap_start_5118(f)
  judge <- make_adaptive_judge_replay(f$reservoir)
  invalid <- function(...) list(is_valid = FALSE, invalid_reason = "retry")
  attributes(invalid) <- attributes(judge)
  for (before in list(initial, reservoir_run(initial, f, 5L))) {
    bad <- adaptive_rank_run_live(before, invalid, progress = "none")
    for (field in c("warm_start_pairs", "warm_start_idx", "warm_start_done", "bootstrap",
      "trueskill_state", "history_pairs")) expect_identical(bad[[field]], before[[field]])
    retried <- reservoir_run(bad, f, 1L)
    expected <- reservoir_run(before, f, 1L)
    fields <- c("A_id", "B_id", "Y", "pairing_strategy", "target_distance")
    expect_identical(tail(retried$step_log[fields], 1L), tail(expected$step_log[fields], 1L))
    expect_identical(retried$history_pairs, expected$history_pairs)
    expect_identical(retried$trueskill_state, expected$trueskill_state)
    expect_identical(nrow(retried$history_pairs), nrow(before$history_pairs) + 1L)
    expect_identical(nrow(retried$step_log), nrow(before$step_log) + 2L)
    expect_reservoir_evidence(retried, f)
  }
})

test_that("invalid predictive configurations fail before model loading or judging", {
  f <- bootstrap_fixture_5118()
  testthat::local_mocked_bindings(.warm_start_prior_resolve_model = function(...) stop("model loaded"),
    .package = "pairwiseLLM")
  for (bad in list(NULL, "", NA_character_, 1, TRUE, character(), matrix("predictive_connected"),
    c("predictive_connected", "shuffled_connected"))) {
    expect_error(bootstrap_start_5118(f, bootstrap_policy = bad), "bootstrap_policy")
  }
  for (args in list(list(replay_reservoir = NULL), list(warm_start_trueskill = NULL),
    list(adaptive_config = list(pairing_strategy = "random")),
    list(adaptive_config = list(pairing_strategy = "hybrid")), list(adaptive_config = NULL))) {
    expect_error(do.call(bootstrap_start_5118, c(list(f = f, warm_start_prior = NULL,
      warm_start_model = "unused"), args)), "Predictive bootstrap requires")
  }
  expect_error(bootstrap_start_5118(f, warm_start_prior = NULL), "requires.*warm_start")
  expect_error(bootstrap_start_5118(f, warm_start_mode = "btl_only"), "requires warm_start_mode")
  expect_error(bootstrap_start_5118(f, items = data.frame(item_id = f$ids, set_id = rep(1:2, each = 3)),
    adaptive_config = list(run_mode = "link_one_spoke", link_estimation_mode = "fixed_shape_offset",
      pairing_strategy = "hybrid")), "within_set")
  state <- bootstrap_start_5118(f)
  named <- bootstrap_start_5118(f,
    warm_start_trueskill = c(mapping = "predictive_distribution"),
    bootstrap_policy = c(graph = "predictive_connected"))
  expect_identical(named$bootstrap, state$bootstrap)
  expect_error(adaptive_rank_run_live(state, make_adaptive_judge_replay(f$reservoir),
    adaptive_config = list(pairing_strategy = "hybrid"), progress = "none"), "Predictive bootstrap requires")
  expect_error(adaptive_rank_run_live(state, function(...) stop("judge called"), progress = "none"),
    "judge identity mismatch")
})

test_that("predictive frozen records reject mutations before replacing a saved session", {
  f <- bootstrap_fixture_5118()
  state <- reservoir_run(bootstrap_start_5118(f), f, 2L)
  path <- withr::local_tempdir()
  save_adaptive_session(state, path)
  original <- tools::md5sum(list.files(path, full.names = TRUE, recursive = TRUE))
  reject <- function(bad, pattern = "bootstrap|Bootstrap|mapping|prior|reservoir|TrueSkill") {
    expect_error(save_adaptive_session(bad, path, overwrite = TRUE), pattern)
    expect_identical(tools::md5sum(names(original)), original)
  }
  for (field in names(state$bootstrap)) {
    bad <- state
    bad$bootstrap[[field]] <- NULL
    reject(bad)
  }
  for (field in c("bootstrap_policy", "bootstrap_policy_version", "bootstrap_digest")) {
    bad <- state
    bad$meta[[field]] <- NULL
    reject(bad)
  }
  bad <- state
  bad$meta$bootstrap_policy <- "shuffled_connected"
  reject(bad)
  bad <- state
  bad$meta$bootstrap_policy_version <- 2L
  reject(bad)
  bad <- state
  bad$meta$seed <- state$meta$seed + 1L
  reject(bad)
  bad <- state
  bad$item_index <- rev(bad$item_index)
  reject(bad)
  bad <- state
  bad$items$item_id <- rev(bad$items$item_id)
  reject(bad)
  bad <- state
  bad$warm_start_pairs <- bad$warm_start_pairs[5:1, ]
  reject(bad)
  bad <- state
  bad$warm_start_pairs$i_id[1] <- "foreign"
  reject(bad)
  bad <- state
  bad$warm_start_idx <- 1L
  reject(bad)
  bad <- state
  bad$warm_start_done <- TRUE
  reject(bad)
  bad <- state
  bad$meta$bootstrap_digest <- paste(rep("0", 64), collapse = "")
  reject(bad)
  bad <- state
  bad$replay_reservoir$edges$A_id[1] <- "foreign"
  reject(bad, "IDs|endpoints|integrity|edges")
  bad <- state
  bad$predictive_prior <- make_warm_start_prior(stats::setNames(1:6, f$ids), prior_sd = 0.4)
  bad$meta$predictive_prior_digest <- bad$predictive_prior$digest
  reject(bad)
})

test_that("bootstrap validation checks structure independently of the checksum", {
  state <- bootstrap_start_5118()
  resign <- function(bad) {
    bad$bootstrap$digest <- pairwiseLLM:::.adaptive_bootstrap_hash(bad$bootstrap, bad$warm_start_pairs)
    bad$meta$bootstrap_digest <- bad$bootstrap$digest
    bad
  }
  for (field in c("seed", "item_ids", "predictive_prior_digest", "trueskill_mapping",
    "initial_prediction", "manifest_digest", "tree_policy")) {
    bad <- state
    bad$bootstrap[field] <- list("changed")
    expect_error(pairwiseLLM:::.adaptive_bootstrap_validate(resign(bad)), "frozen inputs")
  }
  for (version in list(2L, NULL)) {
    bad <- state
    bad$bootstrap$format_version <- version
    expect_error(pairwiseLLM:::.adaptive_bootstrap_validate(resign(bad)), "integrity")
  }
  queues <- list(state$warm_start_pairs[-1, ], as.data.frame(state$warm_start_pairs),
    state$warm_start_pairs, state$warm_start_pairs, state$warm_start_pairs, state$warm_start_pairs)
  queues[[3]]$i_id[1] <- queues[[3]]$j_id[1]
  queues[[4]]$i_id[1] <- NA_character_
  queues[[5]][2, ] <- queues[[5]][1, ]
  queues[[6]][1, ] <- queues[[6]][1, 2:1]
  for (queue in queues) {
    bad <- state
    bad$warm_start_pairs <- queue
    expect_error(pairwiseLLM:::.adaptive_bootstrap_validate(resign(bad)), "queue|edges")
  }
  # Five allowed edges with an a-b-c-d cycle cannot connect all six items.
  bad <- state
  bad$warm_start_pairs <- tibble::tibble(i_id = c("b", "c", "d", "d", "e"),
    j_id = c("a", "b", "c", "a", "d"))
  expect_error(pairwiseLLM:::.adaptive_bootstrap_validate(resign(bad)), "connected tree")
  bad <- state
  bad$warm_start_pairs$i_id[1] <- "foreign"
  bad$replay_reservoir$edges <- rbind(bad$replay_reservoir$edges,
    data.frame(A_id = "foreign", B_id = bad$warm_start_pairs$j_id[1]))
  expect_error(pairwiseLLM:::.adaptive_bootstrap_validate(resign(bad)), "foreign item IDs")
  bad <- state
  bad$meta$bootstrap_policy <- bad$meta$bootstrap_policy_version <- bad$meta$bootstrap_digest <- NULL
  expect_error(pairwiseLLM:::.adaptive_bootstrap_validate(bad), "metadata is missing")
})

test_that("disk bootstrap identities cannot be removed, changed, or downgraded", {
  f <- bootstrap_fixture_5118()
  state <- reservoir_run(bootstrap_start_5118(f), f, 2L)
  path <- withr::local_tempdir()
  save_adaptive_session(state, path)
  metadata <- readRDS(file.path(path, "metadata.rds"))
  expect_identical(validate_session_dir(path), metadata)
  expect_identical(metadata$bootstrap_digest, state$bootstrap$digest)
  for (field in c("bootstrap_policy", "bootstrap_policy_version", "bootstrap_digest")) {
    for (value in list(NULL, "changed")) {
      bad <- metadata
      bad[[field]] <- value
      saveRDS(bad, file.path(path, "metadata.rds"))
      expect_error(validate_session_dir(path), "bootstrap integrity")
      expect_error(adaptive_rank_resume(path), "bootstrap integrity")
    }
  }
  saveRDS(metadata, file.path(path, "metadata.rds"))
  bad <- state
  bad$bootstrap <- NULL
  bad$meta$bootstrap_policy <- bad$meta$bootstrap_policy_version <- bad$meta$bootstrap_digest <- NULL
  saveRDS(bad, file.path(path, "state.rds"))
  expect_error(adaptive_rank_resume(path), "bootstrap integrity")
  saveRDS(state, file.path(path, "state.rds"))
  log <- state$step_log
  log$A_id[1] <- "foreign"
  saveRDS(log, file.path(path, "step_log.rds"))
  expect_error(validate_session_dir(path), "log and history integrity")
  expect_error(adaptive_rank_resume(path), "log and history integrity")
})

test_that("wrapper forwards bootstrap options and preserves omitted options on resume", {
  f <- bootstrap_fixture_5118()
  data <- data.frame(item_id = f$ids, text = "Synthetic item")
  path <- withr::local_tempdir()
  args <- bootstrap_args_5118(f)
  args$items <- args$now_fn <- NULL
  out <- do.call(adaptive_rank, c(list(data = data, session_dir = path, n_steps = 1L,
    judge = make_adaptive_judge_replay(f$reservoir), progress = "none"), args))
  expect_identical(out$state$meta$bootstrap_policy, "predictive_connected")
  call <- list(data = data, session_dir = path, judge = make_adaptive_judge_replay(f$reservoir),
    n_steps = 1L, progress = "none")
  resumed <- do.call(adaptive_rank, call)
  expect_identical(resumed$state$bootstrap, out$state$bootstrap)
  expect_identical(nrow(resumed$state$history_pairs), 2L)
  expect_error(do.call(adaptive_rank, c(call, list(bootstrap_policy = "shuffled_connected"))),
    "Cannot change.*bootstrap_policy")
  expect_error(do.call(adaptive_rank, c(call, list(seed = 88L))), "Cannot change.*seed")
  expect_error(do.call(adaptive_rank, c(call, list(warm_start_trueskill = "predictive_distribution"))),
    "Omit all warm-start")
  expect_no_error(do.call(adaptive_rank, c(call, list(bootstrap_policy = "predictive_connected", seed = 87L))))
  expect_error(adaptive_rank_resume(path, bootstrap_policy = "shuffled_connected"), "must be empty")
})

test_that("legacy queues and default summaries retain their behavior", {
  f <- bootstrap_fixture_5118()
  for (reservoir in list(NULL, f$reservoir)) {
    state <- adaptive_rank_start(f$ids, seed = 87L, replay_reservoir = reservoir,
      now_fn = bootstrap_args_5118(f)$now_fn)
    legacy <- state
    legacy$meta$bootstrap_policy <- legacy$meta$bootstrap_policy_version <- NULL
    judge <- if (is.null(reservoir)) make_deterministic_judge("i_wins") else make_adaptive_judge_replay(reservoir)
    a <- adaptive_rank_run_live(state, judge, n_steps = 2L, progress = "none")
    b <- adaptive_rank_run_live(legacy, judge, n_steps = 2L, progress = "none")
    for (field in c("warm_start_pairs", "warm_start_idx", "warm_start_done", "trueskill_state",
      "step_log", "round_log", "history_pairs")) expect_identical(a[[field]], b[[field]])
    expect_identical(summarize_adaptive(a), summarize_adaptive(b))
    expect_identical(names(summarize_adaptive(a)), c("n_items", "steps_attempted", "committed_pairs",
      "n_refits", "last_stop_decision", "last_stop_reason"))
    path <- withr::local_tempdir()
    save_adaptive_session(b, path)
    # Simulate artifacts written before bootstrap policy metadata existed.
    saved <- readRDS(file.path(path, "state.rds"))
    saved$meta$bootstrap_policy <- saved$meta$bootstrap_policy_version <- NULL
    metadata <- readRDS(file.path(path, "metadata.rds"))
    metadata$bootstrap_policy <- metadata$bootstrap_policy_version <- metadata$bootstrap_digest <- NULL
    saveRDS(saved, file.path(path, "state.rds"))
    saveRDS(metadata, file.path(path, "metadata.rds"))
    restored <- adaptive_rank_resume(path)
    expect_identical(restored$warm_start_pairs, b$warm_start_pairs)
    expect_identical(restored$trueskill_state, b$trueskill_state)
    expect_identical(restored$warm_start_idx, b$warm_start_idx)
    audit <- summarize_adaptive(restored, include_bootstrap = TRUE)$bootstrap[[1L]]
    expect_identical(audit$policy, "shuffled_connected")
    expect_identical(audit$version, 1L)
    expect_true(is.na(audit$digest))
    expect_no_error(save_adaptive_session(restored, path, overwrite = TRUE))
    expect_identical(adaptive_rank_resume(path)$meta$bootstrap_policy, "shuffled_connected")
  }
  state <- bootstrap_start_5118(f)
  audit <- summarize_adaptive(state, include_bootstrap = TRUE)$bootstrap[[1L]]
  expect_identical(audit$digest, state$bootstrap$digest)
  expect_identical(audit$diagnostics, state$bootstrap$diagnostics)
  for (bad in list(NA, 1, logical(), c(TRUE, FALSE))) {
    expect_error(summarize_adaptive(state, include_bootstrap = bad), "include_bootstrap")
  }
})

test_that("synthetic schedule replication preserves predictive initialization independently of selector seeds", {
  f <- bootstrap_fixture_5118()
  state <- bootstrap_start_5118(f)
  out <- bootstrap_adaptive(state, n_rep = 2L, budget = 7L,
    btl_config = list(refit_pairs_target = 100L), keep = "full")
  expect_identical(out$n_success, 2L)
  expect_length(unique(out$replicates$selector), 2L)
  for (artifact in out$artifacts) {
    expect_identical(artifact$state$bootstrap, state$bootstrap)
    expect_identical(artifact$state$warm_start_pairs, state$warm_start_pairs)
    expect_identical(artifact$state$meta$initialization_seed, state$meta$seed)
    expect_no_error(pairwiseLLM:::.adaptive_reservoir_validate_state(artifact$state))
  }
})

test_that("fresh-process resume uses frozen bootstrap without model or graph initialization", {
  withr::local_seed(316L)
  rng <- .Random.seed
  f <- bootstrap_fixture_5118()
  root <- withr::local_tempdir()
  cases <- list()
  for (mode in c("trueskill_only", "both")) {
    for (steps in c(0L, 2L, 5L, 6L)) {
      state <- bootstrap_start_5118(f, warm_start_mode = mode)
      if (steps > 0L) state <- reservoir_run(state, f, steps)
      if (steps == 2L) {
        invalid <- function(...) list(is_valid = FALSE, invalid_reason = "retry after resume")
        attributes(invalid) <- attributes(make_adaptive_judge_replay(f$reservoir))
        state <- adaptive_rank_run_live(state, invalid, progress = "none")
      }
      path <- file.path(root, paste(mode, steps, sep = "-"))
      save_adaptive_session(state, path)
      cases[[length(cases) + 1L]] <- list(path = path, before = state, after = reservoir_run(state, f, 1L))
    }
  }
  config <- file.path(root, "config.rds")
  result <- file.path(root, "result.rds")
  dev_path <- if (pkgload::is_dev_package("pairwiseLLM")) getNamespaceInfo("pairwiseLLM", "path") else NULL
  saveRDS(list(dev_path = dev_path, libpaths = .libPaths(), reservoir = f$reservoir,
    sessions = lapply(cases, `[[`, "path"), result = result), config)
  code <- paste(
    "cfg <- readRDS(commandArgs(TRUE)[[1]])",
    ".libPaths(cfg$libpaths)",
    "if (!is.null(cfg$dev_path)) pkgload::load_all(cfg$dev_path,quiet=TRUE) else library(pairwiseLLM)",
    "testthat::test_that('resume has no initialization side effects', {",
    "fail <- function(...) stop('forbidden initialization')",
    "testthat::local_mocked_bindings(.warm_start_adaptive_init=fail,",
    "extract_warm_start_features=fail, make_warm_start_prior=fail, adaptive_rank_start=fail,",
    "predict.pairwiseLLM_warm_model=fail, .warm_start_prior_resolve_model=fail,",
    ".adaptive_predictive_tree=fail, .adaptive_reservoir_bootstrap=fail, .package='pairwiseLLM')",
    "stopifnot(!exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE))",
    "judge <- pairwiseLLM::make_adaptive_judge_replay(cfg$reservoir)",
    "out <- lapply(cfg$sessions, function(path) {",
    "s <- pairwiseLLM::adaptive_rank_resume(path)",
    "list(before=s, after=pairwiseLLM::adaptive_rank_run_live(s,judge,n_steps=1L,progress='none')) })",
    "stopifnot(!exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE))",
    "saveRDS(out,cfg$result)",
    "testthat::expect_length(out,length(cfg$sessions))", "})", sep = "\n")
  output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", shQuote(code), shQuote(config)), stdout = TRUE, stderr = TRUE))
  expect_true(is.null(attr(output, "status")), info = paste(output, collapse = "\n"))
  expect_true(file.exists(result))
  actual <- readRDS(result)
  for (i in seq_along(cases)) {
    for (part in c("before", "after")) {
      for (field in c("bootstrap", "warm_start_pairs", "warm_start_idx", "warm_start_done",
        "predictive_prior", "trueskill_state", "step_log", "round_log")) {
        expect_identical(actual[[i]][[part]][[field]], cases[[i]][[part]][[field]])
      }
      # Legacy resume adds a canonical probe flag even to an empty history.
      expect_identical(actual[[i]][[part]]$history_pairs[c("A_id", "B_id")],
        cases[[i]][[part]]$history_pairs[c("A_id", "B_id")])
      expect_false(any(actual[[i]][[part]]$history_pairs$is_probe_step))
    }
  }
  expect_identical(.Random.seed, rng)
})
