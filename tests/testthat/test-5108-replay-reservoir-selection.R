test_that("every strategy uses frozen single observations deterministically", {
  withr::local_seed(812)
  rng <- .Random.seed
  f <- reservoir_fixture()
  bootstrap <- NULL
  for (strategy in c("random", "trueskill_p50", "trueskill_pollitt", "hybrid")) {
    for (mode in c("cold", "btl_only", "trueskill_only", "both")) {
      initial <- reservoir_start(f, strategy, mode)
      first <- reservoir_run(initial, f)
      second <- reservoir_run(initial, f)
      expect_reservoir_evidence(first, f)
      expect_gt(nrow(first$history_pairs), length(f$ids) - 1L)
      expect_identical(first$history_pairs, second$history_pairs)
      expect_identical(first$step_log, second$step_log)
      expect_identical(first$trueskill_state, second$trueskill_state)
      expect_no_error(pairwiseLLM:::.adaptive_reservoir_validate_state(first))
      tree <- first$step_log[seq_len(length(f$ids) - 1L), c("A_id", "B_id", "Y")]
      if (is.null(bootstrap)) bootstrap <- tree
      expect_identical(tree, bootstrap)
      expect_true(tail(first$step_log$starvation_reason, 1L) %in%
        c("reservoir_exhausted", "reservoir_constraints_exhausted"))
    }
  }
  expect_identical(.Random.seed, rng)
})

test_that("direct policies ignore exhausted low-degree items and absent preferred edges", {
  f <- reservoir_fixture()
  for (strategy in c("random", "trueskill_p50", "trueskill_pollitt")) {
    state <- reservoir_start(f, strategy)
    state$warm_start_done <- TRUE
    # Leave only d->a unused. Items with lower degrees are no longer selectable.
    used <- f$outcomes[!(f$outcomes$A_id == "d" & f$outcomes$B_id == "a"), ]
    state$history_pairs <- used[, c("A_id", "B_id")]
    selected <- pairwiseLLM:::select_next_pair(state)
    expect_false(selected$candidate_starved)
    expect_identical(state$item_ids[c(selected$A, selected$B)], c("d", "a"))
    expect_identical(selected$n_candidates_after_duplicates, 1L)
    state$history_pairs <- f$outcomes[, c("A_id", "B_id")]
    selected <- pairwiseLLM:::select_next_pair(state)
    expect_true(selected$candidate_starved)
    expect_identical(selected$starvation_reason, "reservoir_exhausted")
  }
})

test_that("stage domains contain only unused allowed edges before candidate caps", {
  withr::local_seed(113)
  f <- reservoir_fixture()
  state <- reservoir_start(f, "hybrid")
  state <- pairwiseLLM:::.adaptive_refresh_round_anchors(state)
  for (stage in pairwiseLLM:::.adaptive_stage_order()) {
    full <- pairwiseLLM:::generate_stage_candidates_from_state(state, stage, "global_safe",
      C_max = 100L, seed = 3L)
    capped <- pairwiseLLM:::generate_stage_candidates_from_state(state, stage, "global_safe",
      C_max = 1L, seed = 3L)
    allowed <- pairwiseLLM:::.adaptive_reservoir_key(f$outcomes$A_id, f$outcomes$B_id)
    expect_true(all(pairwiseLLM:::.adaptive_reservoir_key(full$i, full$j) %in% allowed))
    expect_lte(nrow(capped), 1L)
    expect_identical(attr(capped, "candidate_filter_counts")$n_candidates_legal_domain_total, nrow(full))
    if (nrow(capped) > 0L) {
      state_used <- state
      state_used$history_pairs <- tibble::tibble(A_id = capped$i, B_id = capped$j)
      remaining <- pairwiseLLM:::generate_stage_candidates_from_state(state_used, stage, "global_safe",
        C_max = 100L, seed = 3L)
      expect_false(any(pairwiseLLM:::.adaptive_reservoir_key(remaining$i, remaining$j) %in%
        pairwiseLLM:::.adaptive_reservoir_key(capped$i, capped$j)))
    }
  }
})

test_that("reservoir transactions retry discarded lookups and guard forged commits", {
  f <- reservoir_fixture()
  initial <- reservoir_start(f)
  judge <- make_adaptive_judge_replay(f$reservoir)
  first <- adaptive_rank_run_live(initial, judge, n_steps = 1L, progress = "none")
  retry <- adaptive_rank_run_live(initial, judge, n_steps = 1L, progress = "none")
  expect_identical(first$step_log, retry$step_log)
  selected <- pairwiseLLM:::.adaptive_warm_start_selection(initial, 1L)
  a <- initial$item_ids[[selected$A]]
  b <- initial$item_ids[[selected$B]]
  expect_error(pairwiseLLM:::apply_step_update(first, list(is_valid = TRUE, A_id = a, B_id = b)),
    "already committed")
  expect_error(pairwiseLLM:::apply_step_update(initial, list(is_valid = TRUE, A_id = b, B_id = a)),
    "frozen observed orientation")
  expect_error(adaptive_rank_run_live(initial, make_deterministic_judge("i_wins"), progress = "none"),
    "judge identity mismatch")
  bad <- first
  bad$step_log$Y[1] <- 1L - bad$step_log$Y[1]
  expect_error(reservoir_run(bad, f), "outcomes differ")
  bad <- first
  bad$step_log$A_id[1] <- "foreign"
  expect_error(reservoir_run(bad, f), "log and history integrity")
  attempt <- 0L
  interrupted <- function(A, B, state, ...) {
    attempt <<- attempt + 1L
    if (attempt == 1L) return(list(is_valid = FALSE, invalid_reason = "interrupted"))
    judge(A, B, state, ...)
  }
  attributes(interrupted) <- attributes(judge)
  retried <- adaptive_rank_run_live(initial, interrupted, n_steps = 2L, progress = "none")
  expect_identical(retried$step_log$status, c("invalid", "ok"))
  expect_identical(retried$step_log$A_id, rep(a, 2L))
  expect_identical(retried$step_log$B_id, rep(b, 2L))
  expect_identical(nrow(retried$history_pairs), 1L)
  expect_identical(retried$warm_start_idx, 2L)
})

test_that("hybrid searches the full legal domain when a capped sample fails exposure", {
  f <- reservoir_fixture()
  state <- reservoir_start(f, "hybrid")
  state$warm_start_done <- TRUE
  state <- pairwiseLLM:::.adaptive_round_activate_if_ready(state)
  state <- pairwiseLLM:::.adaptive_refresh_round_anchors(state)
  state$round$repeat_in_round_budget <- 0L
  seed <- pairwiseLLM:::.adaptive_stage_seed(state$meta$seed, 1L, 1L, offset = 11L)
  stage <- pairwiseLLM:::.adaptive_round_active_stage(state)
  capped <- pairwiseLLM:::generate_stage_candidates_from_state(state, stage, "base", 1L, seed)
  full <- pairwiseLLM:::generate_stage_candidates_from_state(state, stage, "base", 100L, seed)
  expect_gt(nrow(full), 1L)
  # Block the sampled non-anchor endpoint; other anchor edges stay available.
  blocked <- setdiff(c(capped$i, capped$j), state$round$anchor_ids)[[1L]]
  state$round$per_round_item_uses[[blocked]] <- 1L
  original <- pairwiseLLM:::adaptive_defaults
  testthat::local_mocked_bindings(adaptive_defaults = function(n_items) {
    defaults <- original(n_items)
    defaults$C_max <- 1L
    defaults
  }, .package = "pairwiseLLM")
  selected <- pairwiseLLM:::select_next_pair(state, step_id = 1L)
  expect_false(selected$candidate_starved)
  expect_identical(selected$fallback_used, "base")
  expect_gt(selected$n_candidates_generated, 1L)
  expect_false(blocked %in% state$item_ids[c(selected$i, selected$j)])
})

test_that("hybrid quota and exploration ignore exhausted low-degree vertices", {
  ids <- letters[1:6]
  clique <- t(utils::combn(ids[1:5], 2L))
  edges <- tibble::tibble(A_id = c(clique[, 1], "a"), B_id = c(clique[, 2], "f"), Y = 1L)
  reservoir <- make_adaptive_replay_reservoir(edges, ids)
  state <- adaptive_rank_start(ids, replay_reservoir = reservoir)
  state$warm_start_done <- TRUE
  state <- pairwiseLLM:::.adaptive_round_activate_if_ready(state)
  state$round$stage_index <- match("local_link", state$round$stage_order)
  state$history_pairs <- edges[!(edges$A_id == "b" & edges$B_id == "c"), c("A_id", "B_id")]
  original <- pairwiseLLM:::adaptive_defaults
  for (quota in c(0, 1)) {
    testthat::with_mocked_bindings({
      # f is globally least exposed but has no unused incident edge.
      # Exhausted vertices cannot trigger an impossible quota override either.
      selected <- pairwiseLLM:::select_next_pair(state)
      expect_false(selected$candidate_starved)
      expect_identical(state$item_ids[c(selected$A, selected$B)], c("b", "c"))
      expect_true(selected$is_explore_step)
    }, adaptive_defaults = function(n_items) {
      defaults <- original(n_items)
      defaults$quota_eps <- quota
      defaults$explore_rate <- 1
      defaults$explore_resample_max <- 1L
      defaults
    }, .package = "pairwiseLLM")
  }
  # Only f-a has been consumed; f is exhausted, and b-c remains among legal
  # edges incident to zero-degree vertices. Quota selection cannot choose f.
  state$history_pairs <- edges[edges$B_id == "f", c("A_id", "B_id")]
  testthat::with_mocked_bindings({
    selected <- pairwiseLLM:::select_next_pair(state)
    expect_false(selected$candidate_starved)
    expect_identical(selected$explore_reason, "coverage_quota_override")
    expect_false("f" %in% state$item_ids[c(selected$i, selected$j)])
  }, adaptive_defaults = function(n_items) {
    defaults <- original(n_items)
    defaults$quota_eps <- 1
    defaults
  }, .package = "pairwiseLLM")
})
