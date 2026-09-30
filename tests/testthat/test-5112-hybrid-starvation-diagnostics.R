starvation_state_5112 <- function(observations = 2L, limit = 3L) {
  items <- make_test_items(4L)
  pairs <- t(utils::combn(as.character(items$item_id), 2L))
  history <- tibble::tibble(A_id = rep(pairs[, 1L], observations),
    B_id = rep(pairs[, 2L], observations))
  state <- make_test_state(items,
    make_test_trueskill_state(items, mu = c(-30, -10, 10, 30), sigma = rep(1, 4)), history)
  state$controller$dup_max_obs_relaxed <- limit
  state$meta$now_fn <- function() as.POSIXct("2026-09-30", tz = "UTC")
  environment(state$meta$now_fn) <- baseenv()
  state
}

starvation_run_5112 <- function(state) {
  adaptive_rank_run_live(state, function(...) stop("exhaustion must not call the judge"),
    n_steps = 20L, btl_config = list(refit_pairs_target = 5000L), progress = "none")
}

test_that("hybrid records duplicate exhaustion while arithmetic capacity remains", {
  state <- starvation_state_5112()
  withr::local_seed(298)
  rng <- .Random.seed
  out <- starvation_run_5112(state)
  expect_identical(out$meta$stop_reason, "candidate_starvation")
  report <- summarize_adaptive(out, include_starvation = TRUE)$starvation_diagnostic[[1L]]
  expect_identical(report$remaining_capacity_upper_bound, 6)
  expect_identical(report$max_observations_per_pair, 3L)
  expect_identical(report$classification, "duplicate_policy_exhausted")
  expect_identical(report$admissibility_scope, "examined_pools")
  expect_identical(report$scope$item_ids, state$item_ids)
  expect_identical(report$committed_pairs, 12L)
  expect_identical(report$step_id, as.integer(nrow(out$step_log)))
  expect_setequal(report$attempts$round_stage, pairwiseLLM:::.adaptive_stage_order())
  expect_true(all(report$attempts$n_candidates_after_hard_filters > 0L))
  expect_true(all(report$attempts$n_admissible_candidates == 0L))
  expect_true(all(report$attempts$n_candidates_after_duplicates == 0L))
  expect_true(all(!report$attempts$bounded))
  expect_true(all(report$attempts$duplicate_policy[report$attempts$fallback == "global_safe"] == "default"))
  expect_true(all(report$attempts$duplicate_policy[report$attempts$fallback == "dup_relax"] == "relaxed"))
  expect_identical(out$history_pairs, state$history_pairs)
  expect_identical(.Random.seed, rng)
  expect_output(print(out), "repeat rules excluded.*upper bound: 6")
})

test_that("capacity uses each pair's ceiling, including committed bootstrap history", {
  for (limit in 2:3) {
    out <- starvation_run_5112(starvation_state_5112(limit, limit))
    report <- summarize_adaptive(out, TRUE)$starvation_diagnostic[[1L]]
    expect_identical(report$classification, "pair_capacity_exhausted")
    expect_identical(report$remaining_capacity_upper_bound, 0)
    expect_output(print(out), "all allowed pair observations have been used")
  }
  state <- adaptive_rank_start(letters[1:4], seed = 298)
  out <- adaptive_rank_run_live(state, make_deterministic_judge(), n_steps = 3L, progress = "none")
  scope <- pairwiseLLM:::.adaptive_starvation_scope(out)
  capacity <- pairwiseLLM:::.adaptive_starvation_capacity(out, scope)
  expect_identical(capacity$remaining_capacity_upper_bound, 15)
  # Historical observations beyond a pair's ceiling cannot consume other pairs' slots.
  state <- starvation_state_5112()
  state$history_pairs <- state$history_pairs[rep(1L, 20L), ]
  expect_identical(pairwiseLLM:::.adaptive_starvation_capacity(state,
    pairwiseLLM:::.adaptive_starvation_scope(state))$remaining_capacity_upper_bound, 15)
})

test_that("diagnostic collection preserves recovery, third observations and exact outputs", {
  fixture <- readRDS(testthat::test_path("fixtures", "selector-296", "baseline.rds"))
  for (state in c(fixture$states, lapply(fixture$successful, `[[`, "state"))) {
    env <- new.env(parent = emptyenv())
    expect_identical(pairwiseLLM:::select_next_pair(state, 1L, diagnostic_env = env),
      pairwiseLLM:::select_next_pair(state, 1L))
  }
  env <- new.env(parent = emptyenv())
  selected <- pairwiseLLM:::select_next_pair(fixture$states$relaxed, 1L, diagnostic_env = env)
  expect_false(selected$candidate_starved)
  expect_identical(selected$fallback_used, "dup_relax")
  expect_gt(env$attempts$n_admissible_candidates[env$attempts$fallback == "dup_relax"], 0L)
  expect_identical(env$attempts$n_admissible_candidates[env$attempts$fallback == "global_safe"], 0L)
  expect_identical(pairwiseLLM:::.adaptive_starvation_class(env$attempts, 10), "selection_inconsistency")
  expect_identical(pairwiseLLM:::.adaptive_starvation_class(env$attempts, 0), "selection_inconsistency")
})

test_that("diagnostics distinguish exposure, hard gates and star caps", {
  state <- starvation_state_5112(0L)
  state$round$staged_active <- TRUE
  state$round$stage_order <- pairwiseLLM:::.adaptive_stage_order()
  state$round$stage_index <- 4L
  state <- pairwiseLLM:::.adaptive_refresh_round_anchors(state)
  # With no repeat allowance, every endpoint is already used this round.
  state$round$per_round_item_uses <- stats::setNames(rep(1L, 4L), state$item_ids)
  state$round$repeat_in_round_budget <- 0L
  env <- new.env(parent = emptyenv())
  expect_true(pairwiseLLM:::select_next_pair(state, diagnostic_env = env)$candidate_starved)
  expect_true(all(env$attempts$exhaustion_filter == "exposure"))
  expect_true(all(env$attempts$n_candidates_before_exposure_filters > 0L))
  expect_identical(pairwiseLLM:::.adaptive_starvation_class(env$attempts, 18), "exposure_star_cap_exhausted")
  state$round$per_round_item_uses[] <- 0L
  # Force actual recent-degree pressure while leaving unused pairs among items 1--4.
  state$history_pairs <- tibble::tibble(A_id = rep(c("1", "3"), 20L), B_id = rep(c("2", "4"), 20L))
  state$history_state <- pairwiseLLM:::.adaptive_history_state_rebuild(state$history_pairs, state$item_ids)
  env <- new.env(parent = emptyenv())
  expect_true(pairwiseLLM:::select_next_pair(state, diagnostic_env = env)$candidate_starved)
  expect_true(any(env$attempts$exhaustion_filter == "star_caps"))
  expect_identical(pairwiseLLM:::.adaptive_starvation_class(env$attempts, 12), "exposure_star_cap_exhausted")
  state$controller$global_identified <- TRUE
  state$round$stage_index <- 2L
  env <- new.env(parent = emptyenv())
  expect_true(pairwiseLLM:::select_next_pair(state, diagnostic_env = env)$candidate_starved)
  expect_true(any(env$attempts$exhaustion_filter == "hard"))
  expect_identical(pairwiseLLM:::.adaptive_starvation_class(env$attempts, 12), "other_filter_exhausted")
})

test_that("bounded evidence and mixed causes do not imply exhaustive admissibility", {
  state <- starvation_state_5112()
  candidates <- pairwiseLLM:::generate_stage_candidates_from_state(state, "local_link", "global_safe",
    C_max = 1L, seed = 298L)
  env <- new.env(parent = emptyenv())
  pairwiseLLM:::select_next_pair(state, candidates = candidates, diagnostic_env = env)
  expect_true(env$attempts$bounded[[1L]])
  expect_gt(env$attempts$n_candidates_legal_domain_total[[1L]], 1)
  attempts <- env$attempts
  attempts$exhaustion_filter[[1L]] <- "star_caps"
  expect_identical(pairwiseLLM:::.adaptive_starvation_class(attempts, 6), "mixed_filter_exhaustion")
  attempts$exhaustion_filter[[1L]] <- "unknown"
  expect_identical(pairwiseLLM:::.adaptive_starvation_class(attempts, 6), "unknown")
  attempts$exhaustion_filter[[1L]] <- NA_character_
  expect_identical(pairwiseLLM:::.adaptive_starvation_class(attempts, 6), "unknown")
  expect_identical(pairwiseLLM:::.adaptive_starvation_class(attempts[0, ], 6), "unknown")
})

test_that("terminal evidence is optional and never leaks into a later selection", {
  out <- starvation_run_5112(starvation_state_5112())
  expected <- c("n_items", "steps_attempted", "committed_pairs", "n_refits", "last_stop_decision", "last_stop_reason")
  expect_identical(names(summarize_adaptive(out)), expected)
  expect_identical(summarize_adaptive(out, TRUE)[expected], summarize_adaptive(out))
  for (bad in list(NULL, NA, 1, c(TRUE, FALSE))) {
    expect_error(summarize_adaptive(out, bad), "include_starvation")
  }
  old <- out
  old$meta$starvation_diagnostic <- NULL
  expect_null(summarize_adaptive(old, TRUE)$starvation_diagnostic[[1L]])
  expect_false(any(grepl("pair availability", capture.output(print(old)))))
  for (change in c("step", "history", "stop", "reason", "scope", "starved")) {
    stale <- out
    if (change == "step") stale$meta$starvation_diagnostic$step_id <- 0L
    if (change == "history") stale$meta$starvation_diagnostic$committed_pairs <- 0L
    if (change == "stop") stale$meta$stop_decision <- FALSE
    if (change == "reason") stale$meta$stop_reason <- "btl_converged"
    if (change == "scope") stale$meta$starvation_diagnostic$scope$item_ids <- "foreign"
    if (change == "starved") stale$step_log$candidate_starved[] <- FALSE
    expect_null(summarize_adaptive(stale, TRUE)$starvation_diagnostic[[1L]], info = change)
  }
  reset <- starvation_state_5112(0L)
  reset$meta$starvation_diagnostic <- out$meta$starvation_diagnostic
  reset <- pairwiseLLM:::run_one_step(reset, make_deterministic_judge())
  expect_null(reset$meta$starvation_diagnostic)
  expect_identical(tail(reset$step_log$status, 1L), "ok")
})

test_that("saved terminal diagnostics survive persistence and old sessions stay readable", {
  # Real committed logs make this a valid persisted session, including bootstrap.
  state <- adaptive_rank_start(letters[1:4], seed = 298,
    adaptive_config = list(dup_max_obs_relaxed = 2L))
  state <- adaptive_rank_run_live(state, make_deterministic_judge(), n_steps = 100L,
    btl_config = list(refit_pairs_target = 5000L), progress = "none")
  report <- summarize_adaptive(state, TRUE)$starvation_diagnostic[[1L]]
  expect_false(is.null(report))
  path <- withr::local_tempdir()
  save_adaptive_session(state, path)
  restored <- load_adaptive_session(path)
  expect_identical(summarize_adaptive(restored, TRUE), summarize_adaptive(state, TRUE))
  expect_identical(restored$step_log, state$step_log)
  expect_identical(restored$meta$schema_version, state$meta$schema_version)
  old <- load_adaptive_session(testthat::test_path("fixtures", "selector-296", "session"))
  expect_null(summarize_adaptive(old, TRUE)$starvation_diagnostic[[1L]])
})

test_that("reservoir and active-set bounds respect their actual domains", {
  f <- reservoir_fixture()
  state <- reservoir_start(f, "hybrid")
  state <- reservoir_run(state, f)
  report <- summarize_adaptive(state, TRUE)$starvation_diagnostic[[1L]]
  expect_identical(report$capacity_domain, "replay_reservoir")
  expect_identical(report$max_observations_per_pair, 1L)
  expect_identical(report$remaining_capacity_upper_bound,
    as.double(nrow(f$outcomes) - nrow(state$history_pairs)))
  expect_reservoir_evidence(state, f)
  linked <- adaptive_rank_start(tibble::tibble(item_id = letters[1:4], set_id = c(1L, 1L, 2L, 2L)),
    adaptive_config = list(run_mode = "link_one_spoke", link_estimation_mode = "fixed_shape_offset"))
  scope <- pairwiseLLM:::.adaptive_starvation_scope(linked)
  expect_identical(scope$set_id, 1L)
  expect_identical(scope$item_ids, as.character(linked$items$item_id[linked$items$set_id == 1L]))
  pairs <- scope$item_ids[1:2]
  linked$history_pairs <- tibble::tibble(A_id = c(pairs[[1L]], "foreign"), B_id = c(pairs[[2L]], "other"))
  expect_identical(pairwiseLLM:::.adaptive_starvation_capacity(linked, scope)$remaining_capacity_upper_bound,
    3 * choose(length(scope$item_ids), 2) - 1)
})

test_that("Phase A unresolved-set termination retains scoped diagnostic evidence", {
  state <- adaptive_rank_start(tibble::tibble(item_id = letters[1:8], set_id = rep(1:2, each = 4L)),
    seed = 298L, adaptive_config = list(run_mode = "link_one_spoke",
      link_estimation_mode = "fixed_shape_offset", dup_max_obs_relaxed = 2L))
  pairs <- t(utils::combn(letters[1:4], 2L))
  state$history_pairs <- tibble::tibble(A_id = rep(pairs[, 1L], 2L), B_id = rep(pairs[, 2L], 2L))
  state$history_state <- pairwiseLLM:::.adaptive_history_state_rebuild(state$history_pairs, state$item_ids)
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  state$linking$phase_a$warm_start_scope_set <- 1L
  out <- starvation_run_5112(state)
  expect_identical(out$meta$stop_reason, "phase_a_set_unresolved")
  expect_identical(out$linking$phase_a$set_status$status[[1L]], "failed")
  report <- summarize_adaptive(out, TRUE)$starvation_diagnostic[[1L]]
  expect_identical(report$scope, list(item_ids = letters[1:4], set_id = 1L))
  expect_identical(report$remaining_capacity_upper_bound, 0)
  expect_identical(report$classification, "pair_capacity_exhausted")
  expect_setequal(report$attempts$round_stage, pairwiseLLM:::.adaptive_stage_order())
  expect_output(print(out), "all allowed pair observations have been used")
  # An absent active domain cannot establish zero remaining capacity.
  capacity <- pairwiseLLM:::.adaptive_starvation_capacity(out, list(item_ids = character()))
  expect_true(is.na(capacity$remaining_capacity_upper_bound))
  expect_identical(capacity$capacity_domain, "unavailable")
})

test_that("diagnostic attempts from a different history, scope or step are not accumulated", {
  out <- starvation_run_5112(starvation_state_5112())
  env <- new.env(parent = emptyenv())
  step_id <- as.integer(nrow(out$step_log) + 1L)
  selection <- pairwiseLLM:::select_next_pair(starvation_state_5112(), step_id, diagnostic_env = env)
  for (change in c("step", "history", "scope")) {
    stale <- out
    if (change == "step") stale$meta$starvation_diagnostic$step_id <- 0L
    if (change == "history") stale$meta$starvation_diagnostic$committed_pairs <- 0L
    if (change == "scope") stale$meta$starvation_diagnostic$scope$item_ids <- "foreign"
    recorded <- pairwiseLLM:::.adaptive_record_starvation(stale, selection, env$attempts, step_id)
    expect_identical(recorded$meta$starvation_diagnostic$attempts, env$attempts, info = change)
  }
})
