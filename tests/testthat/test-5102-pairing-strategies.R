strategy_state_5102 <- function(strategy = "trueskill_p50", ids = letters[1:4]) {
  state <- pairwiseLLM:::new_adaptive_state(ids,
    now_fn = function() as.POSIXct("2026-09-10", tz = "UTC"))
  state <- pairwiseLLM:::.adaptive_apply_controller_config(state,
    list(pairing_strategy = strategy))
  pairwiseLLM:::.adaptive_round_activate_if_ready(state)
}

strategy_focal_fixture_5102 <- function(strategy) {
  state <- strategy_state_5102(strategy)
  # The triangle leaves a uniquely least-exposed, without imposing a partner rule.
  state$history_pairs <- tibble::tibble(A_id = c("b", "c", "d"), B_id = c("c", "d", "b"))
  denom <- sqrt(2 * (25 / 6)^2 + 2 * (25 / 3)^2)
  state$trueskill_state$items$mu <- c(25, 25 + 0.1, 25 - denom * qnorm(1 / 3), 45)
  state
}

test_that("strategy validation is exact and historical default stays hybrid", {
  expect_identical(pairwiseLLM:::.adaptive_pairing_strategy(list()), "hybrid")
  expect_identical(adaptive_rank_start(letters[1:3])$controller$pairing_strategy, "hybrid")
  for (strategy in c("hybrid", "random", "trueskill_p50", "trueskill_pollitt")) {
    state <- adaptive_rank_start(letters[1:3], adaptive_config = list(pairing_strategy = strategy))
    expect_identical(state$controller$pairing_strategy, strategy)
  }
  for (bad in list("", " ", NA_character_, character(), c("hybrid", "random"),
    1, TRUE, "Random", matrix("random"))) {
    expect_error(adaptive_rank_start(letters[1:3],
      adaptive_config = list(pairing_strategy = bad)), "pairing_strategy")
  }
  for (mode in c("link_one_spoke", "link_multi_spoke")) {
    for (strategy in c("random", "trueskill_p50", "trueskill_pollitt")) {
      expect_error(adaptive_rank_start(letters[1:3],
        adaptive_config = list(run_mode = mode, pairing_strategy = strategy)), "requires.*within_set")
    }
  }
  state <- strategy_state_5102("random")
  state$controller$run_mode <- "link_one_spoke"
  expect_error(pairwiseLLM:::select_next_pair(state), "requires.*within_set")
  expect_error(pairwiseLLM:::select_next_pair(strategy_state_5102(), step_id = NA_integer_), "step_id")
  state <- strategy_state_5102("random")
  state <- pairwiseLLM:::.adaptive_apply_controller_config(state, list(boundary_k = 3L))
  expect_identical(state$controller$pairing_strategy, "random")
  state <- pairwiseLLM:::.adaptive_apply_controller_config(state, list(pairing_strategy = "hybrid"))
  expect_identical(state$round$stage_order, pairwiseLLM:::.adaptive_stage_order())
  expect_gt(length(state$round$stage_quotas), 0L)
})

test_that("probability strategies choose exact mathematical targets without BTL", {
  for (strategy in c("trueskill_p50", "trueskill_pollitt")) {
    state <- strategy_focal_fixture_5102(strategy)
    state$btl_fit <- list(unusable_for_selection = TRUE)
    selected <- pairwiseLLM:::select_next_pair(state)
    expected_partner <- if (strategy == "trueskill_p50") "b" else "c"
    expect_identical(state$item_ids[[selected$i]], "a")
    expect_identical(state$item_ids[[selected$j]], expected_partner)
    expected_p <- pnorm((25 - state$trueskill_state$items$mu[[selected$j]]) /
      sqrt(2 * (25 / 6)^2 + 2 * (25 / 3)^2))
    expect_equal(selected$p_ij, if (selected$A == selected$i) expected_p else 1 - expected_p)
    expected_distance <- if (strategy == "trueskill_p50") {
      abs(expected_p - 0.5)
    } else {
      min(abs(expected_p - 1 / 3), abs(expected_p - 2 / 3))
    }
    expect_equal(selected$target_distance, expected_distance, tolerance = 1e-14)
    expect_identical(selected$round_stage, "direct_pairing")
    expect_identical(selected$pair_type, "direct_pairing")
    expect_identical(selected$pairing_strategy, strategy)
    expect_identical(selected$run_mode, "within_set")
    expect_false(selected$is_explore_step)
    expect_true(is.na(selected$stage_quota))
    expect_identical(selected$n_candidates_scored, 3L)
    expect_identical(selected$deg_i, 0L)

    state$trueskill_state$items$mu[2:3] <- 27
    expect_identical(pairwiseLLM:::select_next_pair(state)$j, 2L)
    state$trueskill_state$items <- state$trueskill_state$items[4:1, ]
    expect_identical(pairwiseLLM:::select_next_pair(state)$j, 2L)
  }
  p <- c(0, 1 / 3, 0.5, 2 / 3, 1)
  expect_equal(pairwiseLLM:::.adaptive_pairing_target_distance(p, "trueskill_pollitt"),
    c(1 / 3, 0, 1 / 6, 0, 1 / 3))
  expect_equal(pairwiseLLM:::.adaptive_pairing_target_distance(p, "trueskill_pollitt"),
    pairwiseLLM:::.adaptive_pairing_target_distance(1 - p, "trueskill_pollitt"))
})

test_that("random uses seeded uniform legal partners independently of TrueSkill", {
  withr::local_seed(192)
  rng <- .Random.seed
  state <- strategy_focal_fixture_5102("random")
  choices <- integer()
  for (seed in 1:12) {
    state$meta$seed <- as.integer(seed)
    selected <- pairwiseLLM:::select_next_pair(state)
    expected_seed <- pairwiseLLM:::.adaptive_stage_seed(seed, 4L, 1L, offset = 302L)
    expected <- withr::with_seed(expected_seed, sample.int(3L, 1L)) + 1L
    expect_identical(selected$j, expected)
    expect_true(is.na(selected$target_distance))
    expect_identical(selected$n_candidates_scored, 0L)
    changed <- state
    changed$trueskill_state$items$mu <- c(100, -100, 20, 99)
    changed$trueskill_state$items$sigma <- c(1, 40, 2, 20)
    changed$controller$global_identified <- TRUE
    other <- pairwiseLLM:::select_next_pair(changed)
    expect_identical(other[c("i", "j", "A", "B")], selected[c("i", "j", "A", "B")])
    expect_identical(pairwiseLLM:::select_next_pair(state), selected)
    choices <- c(choices, selected$j)
  }
  expect_setequal(choices, 2:4)
  expect_identical(.Random.seed, rng)
})

test_that("focal ties are shared across policies and track committed minimum degree", {
  withr::local_seed(94)
  rng <- .Random.seed
  for (strategy in c("random", "trueskill_p50", "trueskill_pollitt")) {
    state <- strategy_state_5102(strategy, letters[1:6])
    for (k in 1:12) {
      degrees <- pairwiseLLM:::.adaptive_pair_counts(state$history_pairs, state$item_ids)$deg
      focal <- pairwiseLLM:::select_next_pair(state)$i
      expect_identical(unname(degrees[[focal]]), as.integer(min(degrees)))
      for (alternative in c("random", "trueskill_p50", "trueskill_pollitt")) {
        other <- state
        other$controller$pairing_strategy <- alternative
        expect_identical(pairwiseLLM:::select_next_pair(other)$i, focal)
      }
      state <- adaptive_rank_run_live(state, make_deterministic_judge("i_wins"),
        n_steps = 1L, progress = "none")
      expect_identical(tail(state$step_log$status, 1), "ok")
    }
    degrees <- pairwiseLLM:::.adaptive_pair_counts(state$history_pairs, state$item_ids)$deg
    expect_gte(min(degrees), 2L)
    expect_length(state$round$stage_quotas, 0L)
    expect_length(state$round$stage_committed, 0L)
    expect_length(state$round$stage_order, 0L)
    expect_identical(state$round$round_id, 2L)
    expect_identical(state$round$round_committed, 2L)
    expect_identical(state$round$committed_total, 12L)
    expect_length(state$round$anchor_ids, 0L)
  }
  expect_identical(.Random.seed, rng)
})

test_that("all strategies retain bootstrap evidence across warm modes and invalid retries", {
  ids <- letters[1:4]
  prior <- make_warm_start_prior(stats::setNames(c(-1, -0.2, 0.2, 1), ids))
  reference <- NULL
  for (mode in c("cold", "btl_only", "trueskill_only", "both")) {
    for (strategy in c("hybrid", "random", "trueskill_p50", "trueskill_pollitt")) {
      state <- adaptive_rank_start(ids, seed = 71, warm_start_mode = mode,
        warm_start_prior = if (mode == "cold") NULL else prior,
        adaptive_config = list(pairing_strategy = strategy))
      if (strategy != "hybrid") {
        expect_identical(pairwiseLLM:::select_next_pair(state)$round_stage, "warm_start")
        expect_identical(pairwiseLLM:::.adaptive_round_active_stage(state), "warm_start")
      }
      before <- state
      state <- adaptive_rank_run_live(state, make_deterministic_judge("invalid"),
        n_steps = 1L, progress = "none")
      expect_identical(state$history_state, before$history_state)
      expect_identical(state$warm_start_idx, 1L)
      expect_identical(state$round, before$round)
      state <- adaptive_rank_run_live(state, make_deterministic_judge("i_wins"),
        n_steps = 3L, progress = "none")
      evidence <- state$step_log[, c("A_id", "B_id", "Y", "round_stage")]
      if (is.null(reference)) reference <- evidence
      expect_identical(evidence, reference)
      expect_true(state$warm_start_done)
      expect_length(unique(state$step_log$unordered_key[-1]), 3L)
    }
  }
})

test_that("direct resume in a fresh R process preserves the next committed step", {
  session <- withr::local_tempdir()
  result <- tempfile(tmpdir = session, fileext = ".rds")
  state <- adaptive_rank_start(letters[1:5], seed = 413,
    adaptive_config = list(pairing_strategy = "trueskill_pollitt"))
  state <- adaptive_rank_run_live(state, make_deterministic_judge("i_wins"),
    n_steps = 6L, progress = "none")
  save_adaptive_session(state, session, overwrite = TRUE)
  expected <- adaptive_rank_run_live(state, make_deterministic_judge("i_wins"),
    n_steps = 1L, progress = "none")
  # covr runs tests beneath an installed package, which also has DESCRIPTION and R/.
  # Only a namespace actually loaded by pkgload should be loaded from source again.
  dev_path <- if (pkgload::is_dev_package("pairwiseLLM")) {
    getNamespaceInfo("pairwiseLLM", "path")
  } else {
    NULL
  }
  config_path <- tempfile(tmpdir = session, fileext = ".rds")
  saveRDS(list(dev_path = dev_path, session = session, result = result,
    libpaths = .libPaths()), config_path)
  code <- paste(
    "cfg <- readRDS(commandArgs(TRUE)[[1]])",
    ".libPaths(cfg$libpaths)",
    "if (!is.null(cfg$dev_path)) {",
    "pkgload::load_all(cfg$dev_path, quiet = TRUE)",
    "} else library(pairwiseLLM)",
    "s <- pairwiseLLM::load_adaptive_session(cfg$session)",
    "s <- pairwiseLLM::adaptive_rank_run_live(s, function(...) list(is_valid=TRUE, Y=1L),",
    "n_steps=1L, progress='none')",
    "saveRDS(s, cfg$result)", sep = "\n")
  output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", shQuote(code), shQuote(config_path)),
    stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if ((!is.null(status) && status != 0L) || !file.exists(result)) {
    stop("Fresh R resume process did not produce a result (status ",
      if (is.null(status)) 0L else status, "):\n", paste(output, collapse = "\n"))
  }
  actual <- readRDS(result)
  # Resume rebuilds canonical A/B history; non-probe markers can be absent until append.
  expect_identical(actual$history_pairs[, c("A_id", "B_id")],
    expected$history_pairs[, c("A_id", "B_id")])
  expect_false(any(actual$history_pairs$is_probe_step %in% TRUE))
  expect_identical(actual$history_state, expected$history_state)
  expect_identical(actual$trueskill_state, expected$trueskill_state)
  expect_identical(actual$round, expected$round)
  fields <- c("A_id", "B_id", "Y", "round_stage", "pairing_strategy", "target_distance")
  expect_identical(actual$step_log[, fields], expected$step_log[, fields])
})

test_that("direct repeats reverse, hard limits starve, and invalid judgments retry exactly", {
  for (strategy in c("random", "trueskill_p50", "trueskill_pollitt")) {
    state <- strategy_state_5102(strategy, c("a", "b"))
    state <- adaptive_rank_run_live(state, make_deterministic_judge("i_wins"),
      n_steps = 1L, progress = "none")
    first <- state$step_log[1, ]
    pending <- pairwiseLLM:::select_next_pair(state)
    invalid <- adaptive_rank_run_live(state, make_deterministic_judge("invalid"),
      n_steps = 1L, progress = "none")
    for (field in c("history_pairs", "history_state", "trueskill_state", "round", "refit_meta")) {
      expect_identical(invalid[[field]], state[[field]])
    }
    expect_identical(pairwiseLLM:::select_next_pair(invalid), pending)
    retried <- adaptive_rank_run_live(invalid, make_deterministic_judge("i_wins"),
      n_steps = 1L, progress = "none")
    second <- tail(retried$step_log, 1L)
    expect_identical(second$A_id, first$B_id)
    expect_identical(second$B_id, first$A_id)
    expect_identical(second$ordered_key, invalid$step_log$ordered_key[2])
    exhausted <- adaptive_rank_run_live(retried, function(...) stop("No legal pair"),
      n_steps = 5L, progress = "none")
    expect_identical(exhausted$meta$stop_reason, "candidate_starvation")
    expect_identical(exhausted$history_pairs, retried$history_pairs)
    expect_identical(exhausted$round, retried$round)
    expect_identical(tail(exhausted$step_log$starvation_reason, 1), "filtered_by_duplicates")
  }
})

test_that("direct hard filters require repeat orientation and canonical balance", {
  state <- strategy_state_5102("random", c("a", "b"))
  cache <- pairwiseLLM:::.adaptive_history_state_resolve(state, state$item_ids)
  counts <- pairwiseLLM:::.adaptive_history_state_counts(cache, state$item_ids)
  counts$pair_count <- c(`a:b` = 1L)
  out <- pairwiseLLM:::.adaptive_select_direct(state, "random", cache, counts,
    pairwiseLLM:::adaptive_defaults(2))
  expect_true(out$candidate_starved)
  expect_identical(out$starvation_reason, "filtered_by_hard_filters")
  expect_identical(out$n_candidates_after_hard_filters, 0L)

  counts$pair_count <- integer()
  counts$posA <- c(a = 3L, b = 0L)
  counts$posB <- c(a = 0L, b = 3L)
  out <- pairwiseLLM:::.adaptive_select_direct(state, "random", cache, counts,
    pairwiseLLM:::adaptive_defaults(2))
  expect_identical(out$A, 2L)
  expect_identical(out$B, 1L)
  counts$posA <- c(a = 0L, b = 3L)
  counts$posB <- c(a = 3L, b = 0L)
  out <- pairwiseLLM:::.adaptive_select_direct(state, "random", cache, counts,
    pairwiseLLM:::adaptive_defaults(2))
  expect_identical(out$A, 1L)
})

test_that("explicit hybrid is identical to the existing staged path", {
  withr::local_seed(450)
  rng_before <- .Random.seed
  implicit <- adaptive_rank_start(letters[1:6], seed = 62)
  explicit <- adaptive_rank_start(letters[1:6], seed = 62,
    adaptive_config = list(pairing_strategy = "hybrid"))
  testthat::local_mocked_bindings(
    .adaptive_select_direct = function(...) stop("Hybrid must not call direct selector"),
    .package = "pairwiseLLM")
  implicit <- adaptive_rank_run_live(implicit, make_deterministic_judge("i_wins"),
    n_steps = 8L, progress = "none")
  expect_identical(.Random.seed, rng_before)
  explicit <- adaptive_rank_run_live(explicit, make_deterministic_judge("i_wins"),
    n_steps = 8L, progress = "none")
  expect_identical(.Random.seed, rng_before)
  expect_identical(explicit$history_pairs, implicit$history_pairs)
  expect_identical(explicit$trueskill_state, implicit$trueskill_state)
  expect_identical(explicit$round, implicit$round)
  fields <- setdiff(names(explicit$step_log), "timestamp")
  expect_identical(explicit$step_log[, fields], implicit$step_log[, fields])
  expect_false(any(explicit$step_log$round_stage == "direct_pairing"))
})

test_that("direct live refits count committed evidence and retain stop-boundary continuation", {
  state <- adaptive_rank_start(letters[1:6],
    adaptive_config = list(pairing_strategy = "random", max_pairs_after_stop = 3L))
  stub <- make_deterministic_fit_fn(state$item_ids)
  state <- adaptive_rank_run_live(state, make_deterministic_judge("i_wins"),
    n_steps = 9L, fit_fn = stub$fit_fn, btl_config = list(refit_pairs_target = 3L), progress = "none")
  expect_equal(stub$get_calls(), 3L)
  expect_equal(state$round_log$total_pairs_done, c(3L, 6L, 9L))
  expect_equal(state$round_log$new_pairs_since_last_refit, rep(3L, 3L))
  state$meta$stop_boundary_step_id <- 9L
  state$meta$stop_boundary_refit_id <- 3L
  state$meta$pairs_committed_after_stop <- 0L
  state$meta$stop_decision <- FALSE
  state$meta$stop_reason <- "btl_converged"
  out <- adaptive_rank_run_live(state, make_deterministic_judge("i_wins"),
    n_steps = 10L, fit_fn = stub$fit_fn, progress = "none")
  expect_identical(out$meta$stop_boundary_step_id, 9L)
  expect_identical(out$meta$pairs_committed_after_stop, 3L)
  expect_identical(out$meta$stop_reason, "max_pairs_after_stop_exhausted")
})

test_that("the public wrapper forwards direct pairing into post-bootstrap steps", {
  samples <- data.frame(ID = letters[1:4], text = paste("item", letters[1:4]))
  out <- adaptive_rank(samples, judge = make_deterministic_judge("i_wins"),
    n_steps = 5L, adaptive_config = list(pairing_strategy = "trueskill_pollitt"), progress = "none")
  expect_identical(out$state$controller$pairing_strategy, "trueskill_pollitt")
  expect_identical(out$state$step_log$round_stage,
    c(rep("warm_start", 3L), rep("direct_pairing", 2L)))
  expect_true(all(is.finite(out$state$step_log$target_distance[4:5])))
})

test_that("strategy logs backfill before strict validation and sessions resume exact choices", {
  legacy <- pairwiseLLM:::new_step_log()
  legacy$pairing_strategy <- legacy$target_distance <- NULL
  aligned <- pairwiseLLM:::.adaptive_align_log_schema_for_resume(legacy,
    pairwiseLLM:::schema_step_log, "step_log", fill_missing = FALSE)
  expect_identical(aligned, pairwiseLLM:::new_step_log())
  for (strategy in c("random", "trueskill_p50", "trueskill_pollitt")) {
    state <- adaptive_rank_start(letters[1:5], seed = 97,
      adaptive_config = list(pairing_strategy = strategy))
    state <- adaptive_rank_run_live(state, make_deterministic_judge("i_wins"),
      n_steps = 6L, progress = "none")
    session <- withr::local_tempdir()
    save_adaptive_session(state, session, overwrite = TRUE)
    expect_silent(validate_session_dir(session))
    resumed <- load_adaptive_session(session)
    expect_identical(resumed$controller$pairing_strategy, strategy)
    expect_identical(resumed$step_log$pairing_strategy, rep(strategy, 6L))
    expect_identical(pairwiseLLM:::select_next_pair(resumed), pairwiseLLM:::select_next_pair(state))
    expect_identical(resumed$round, state$round)
    legacy <- state$step_log
    legacy$pairing_strategy <- legacy$target_distance <- NULL
    aligned <- pairwiseLLM:::.adaptive_align_log_schema_for_resume(legacy,
      pairwiseLLM:::schema_step_log, "step_log", fill_missing = FALSE)
    expect_identical(aligned$pairing_strategy, rep("hybrid", nrow(legacy)))
    expect_true(all(is.na(aligned$target_distance)))
  }
})
