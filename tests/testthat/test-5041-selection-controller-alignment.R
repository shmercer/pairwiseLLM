test_that("round quotas taper long links and reallocate after identifiability", {
  q_pre <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = 5L,
    n_items = 100L,
    controller = list(global_identified = FALSE)
  )
  q_post <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = 5L,
    n_items = 100L,
    controller = list(global_identified = TRUE)
  )

  meta_pre <- attr(q_pre, "quota_meta")
  meta_post <- attr(q_post, "quota_meta")
  target <- pairwiseLLM:::adaptive_defaults(100L)$round_pairs_target

  expect_equal(sum(q_pre), target)
  expect_equal(sum(q_post), target)
  expect_equal(q_post[["long_link"]], meta_post$long_quota_effective)
  expect_equal(meta_post$long_quota_removed, meta_post$long_quota_raw - meta_post$long_quota_effective)
  expect_true(q_post[["long_link"]] <= q_pre[["long_link"]])
  expect_equal(q_post[["mid_link"]] - q_pre[["mid_link"]], meta_post$realloc_to_mid)
  expect_equal(q_post[["local_link"]] - q_pre[["local_link"]], meta_post$realloc_to_local)
  expect_true(meta_post$long_quota_removed >= 0L)
  expect_equal(meta_pre$long_quota_removed, 0L)
})

test_that("identifiability state is recomputed from reliability and rank correlation", {
  items <- make_test_items(4)
  trueskill_state <- make_test_trueskill_state(items, mu = c(8, 6, 4, 2))
  state <- make_test_state(items, trueskill_state)
  draws <- rbind(
    c(1.00, 0.80, 0.60, 0.40),
    c(0.98, 0.78, 0.58, 0.38),
    c(1.02, 0.82, 0.62, 0.42)
  )
  colnames(draws) <- as.character(items$item_id)
  state$btl_fit <- make_test_btl_fit(state$item_ids, draws = draws)

  hi <- pairwiseLLM:::.adaptive_update_identifiability_state(
    state,
    config = list(
      global_identified_reliability_min = 0.10,
      global_identified_rank_corr_min = 0.80
    )
  )
  lo <- pairwiseLLM:::.adaptive_update_identifiability_state(
    state,
    config = list(
      global_identified_reliability_min = 1.01,
      global_identified_rank_corr_min = 1.01
    )
  )

  expect_true(isTRUE(hi$controller$global_identified))
  expect_false(isTRUE(lo$controller$global_identified))
  expect_equal(hi$controller$global_identified_reliability_min, 0.10)
  expect_equal(hi$controller$global_identified_rank_corr_min, 0.80)
})

test_that("long-link gate uses trueskill before accepted posterior availability", {
  items <- make_test_items(2)
  trueskill_state <- make_test_trueskill_state(items, mu = c(25, 25))
  state <- make_test_state(items, trueskill_state)
  state$round$staged_active <- TRUE
  state$round$stage_index <- 2L
  state$controller <- pairwiseLLM:::.adaptive_controller_defaults(length(state$item_ids))
  state$controller$global_identified <- TRUE
  state$controller$p_long_low <- 0.45
  state$controller$p_long_high <- 0.55

  cand <- tibble::tibble(i = "1", j = "2")
  out <- testthat::with_mocked_bindings(
    score_candidates_u0 = function(candidates, trueskill_state) {
      candidates$p <- 0.99
      candidates$u0 <- 0.99 * 0.01
      candidates
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L, candidates = cand),
    .package = "pairwiseLLM"
  )

  expect_true(out$candidate_starved)
  expect_identical(out$long_gate_pass, FALSE)
  expect_identical(out$long_gate_reason, "trueskill_extreme")
})

test_that("long-link gate reason reflects selected fallback attempt", {
  items <- make_test_items(4)
  trueskill_state <- make_test_trueskill_state(items, mu = c(25, 25, 25, 25))
  state <- make_test_state(items, trueskill_state)
  state$round$staged_active <- TRUE
  state$round$stage_index <- 2L
  state$controller <- pairwiseLLM:::.adaptive_controller_defaults(length(state$item_ids))
  state$controller$global_identified <- TRUE
  state$controller$p_long_low <- 0.45
  state$controller$p_long_high <- 0.55

  calls <- 0L
  out <- testthat::with_mocked_bindings(
    generate_stage_candidates_from_state = function(state, stage_name, fallback_name, C_max, seed) {
      calls <<- calls + 1L
      ids <- as.character(state$item_ids)
      if (calls == 1L) {
        return(tibble::tibble(i = ids[[1L]], j = ids[[2L]]))
      }
      tibble::tibble(i = ids[[3L]], j = ids[[4L]])
    },
    score_candidates_u0 = function(candidates, trueskill_state) {
      candidates$p <- ifelse(candidates$i == "1" & candidates$j == "2", 0.99, 0.50)
      candidates$u0 <- candidates$p * (1 - candidates$p)
      candidates
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L),
    .package = "pairwiseLLM"
  )

  expect_false(isTRUE(out$candidate_starved))
  expect_identical(out$fallback_used, "expand_locality")
  expect_identical(out$long_gate_pass, TRUE)
  expect_identical(out$long_gate_reason, "trueskill_inside_gate")
})

test_that("within-set long-link gate ignores a contradictory accepted posterior", {
  items <- make_test_items(2)
  trueskill_state <- make_test_trueskill_state(items, mu = c(25, 25))
  state <- make_test_state(items, trueskill_state)
  state$round$staged_active <- TRUE
  state$round$stage_index <- 2L
  state$controller <- pairwiseLLM:::.adaptive_controller_defaults(length(state$item_ids))
  state$controller$global_identified <- TRUE
  state$controller$p_long_low <- 0.45
  state$controller$p_long_high <- 0.55
  state$btl_fit <- make_test_btl_fit(
    state$item_ids,
    draws = rbind(
      c(0.01, -0.01),
      c(0.02, -0.02),
      c(-0.01, 0.01),
      c(-0.02, 0.02)
    )
  )
  state$round_log <- tibble::tibble(
    refit_id = 1L,
    diagnostics_pass = TRUE,
    phase_scope = "global",
    phase_scope_set_id = NA_integer_
  )

  out <- testthat::with_mocked_bindings(
    score_candidates_u0 = function(candidates, trueskill_state) {
      candidates$p <- 0.99
      candidates$u0 <- 0.99 * 0.01
      candidates
    },
    .adaptive_long_link_gate_posterior_prob_vec = function(state, i_id, j_id, block_size = 2048L) {
      rlang::abort("within-set gate consulted BTL posterior")
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L, candidates = tibble::tibble(i = "1", j = "2")),
    .package = "pairwiseLLM"
  )

  expect_true(out$candidate_starved)
  expect_identical(out$long_gate_pass, FALSE)
  expect_identical(out$long_gate_reason, "trueskill_extreme")
})

test_that("within-set long-link gate accepts trueskill-inside despite posterior extreme", {
  items <- make_test_items(2)
  trueskill_state <- make_test_trueskill_state(items, mu = c(25, 25))
  state <- make_test_state(items, trueskill_state)
  state$round$staged_active <- TRUE
  state$round$stage_index <- 2L
  state$controller <- pairwiseLLM:::.adaptive_controller_defaults(length(state$item_ids))
  state$controller$global_identified <- TRUE
  state$controller$p_long_low <- 0.45
  state$controller$p_long_high <- 0.55
  state$btl_fit <- make_test_btl_fit(
    state$item_ids,
    draws = rbind(
      c(3, -3),
      c(3.1, -3.1),
      c(2.9, -2.9)
    )
  )
  state$round_log <- tibble::tibble(
    refit_id = 1L,
    diagnostics_pass = TRUE,
    phase_scope = "global",
    phase_scope_set_id = NA_integer_
  )

  out <- testthat::with_mocked_bindings(
    score_candidates_u0 = function(candidates, trueskill_state) {
      candidates$p <- 0.50
      candidates$u0 <- 0.25
      candidates
    },
    .adaptive_long_link_gate_posterior_prob_vec = function(state, i_id, j_id, block_size = 2048L) {
      rlang::abort("within-set gate consulted BTL posterior")
    },
    pairwiseLLM:::select_next_pair(state, step_id = 1L, candidates = tibble::tibble(i = "1", j = "2")),
    .package = "pairwiseLLM"
  )

  expect_false(out$candidate_starved)
  expect_identical(out$long_gate_pass, TRUE)
  expect_identical(out$long_gate_reason, "trueskill_inside_gate")
})

test_that("Phase A uses inclusive trueskill bounds and Phase B keeps its posterior gate", {
  items <- tibble::tibble(item_id = 1:4, set_id = c(1L, 1L, 2L, 2L))
  trueskill_state <- make_test_trueskill_state(items, mu = rep(25, 4L))
  state <- make_test_state(items, trueskill_state)
  state$round$staged_active <- TRUE
  state$round$stage_index <- 2L
  state$controller <- pairwiseLLM:::.adaptive_controller_defaults(length(state$item_ids))
  state$controller$run_mode <- "link_one_spoke"
  state$controller$hub_id <- 1L
  state$controller$global_identified <- TRUE
  state$controller$p_long_low <- 0.45
  state$controller$p_long_high <- 0.55
  state$linking$phase_a$phase <- "phase_a"
  config <- pairwiseLLM:::adaptive_defaults(length(state$item_ids))
  counts <- pairwiseLLM:::.adaptive_pair_counts(
    pairwiseLLM:::.adaptive_history_tbl(state),
    state$item_ids
  )
  stage <- list(name = "base", dup_policy = "default")
  candidates <- tibble::tibble(
    i = c("1", "1", "2", "3"),
    j = c("2", "3", "4", "4")
  )

  phase_a <- testthat::with_mocked_bindings(
    score_candidates_u0 = function(candidates, trueskill_state) {
      candidates$p <- c(0.45, 0.55, 0.44, 0.56)
      candidates$u0 <- candidates$p * (1 - candidates$p)
      candidates
    },
    .adaptive_long_link_gate_posterior_prob_vec = function(...) {
      rlang::abort("Phase A gate consulted BTL posterior")
    },
    pairwiseLLM:::.adaptive_select_stage(
      stage = stage,
      state = state,
      config = config,
      controller = state$controller,
      generation_stage = "long_link",
      round = state$round,
      history_state = pairwiseLLM:::.adaptive_history_state_resolve(state),
      counts = counts,
      step_id = 1L,
      seed_base = 1L,
      candidates = candidates
    ),
    .package = "pairwiseLLM"
  )

  expect_identical(phase_a$long_gate_pass, TRUE)
  expect_identical(phase_a$long_gate_reason, "trueskill_inside_gate")
  expect_equal(sort(phase_a$selected$p), c(0.45, 0.55))

  state$linking$phase_a$phase <- "phase_b"
  phase_b <- testthat::with_mocked_bindings(
    score_candidates_u0 = function(candidates, trueskill_state) {
      candidates$p <- 0.99
      candidates$u0 <- candidates$p * (1 - candidates$p)
      candidates
    },
    .adaptive_long_link_gate_has_posterior = function(state) TRUE,
    .adaptive_long_link_gate_posterior_prob_vec = function(state, i_id, j_id, block_size = 2048L) {
      rep_len(0.50, length(i_id))
    },
    pairwiseLLM:::.adaptive_select_stage(
      stage = stage,
      state = state,
      config = config,
      controller = state$controller,
      generation_stage = "long_link",
      round = state$round,
      history_state = pairwiseLLM:::.adaptive_history_state_resolve(state),
      counts = counts,
      step_id = 1L,
      seed_base = 1L,
      candidates = candidates[1L, , drop = FALSE]
    ),
    .package = "pairwiseLLM"
  )

  expect_identical(phase_b$long_gate_pass, TRUE)
  expect_identical(phase_b$long_gate_reason, "posterior_inside_gate")
  expect_equal(phase_b$selected$p, 0.99)
})

test_that("the preserved Phase B long gate falls back when posterior evidence is unusable", {
  items <- tibble::tibble(item_id = letters[1:4], set_id = c(1L, 1L, 2L, 2L))
  for (accepted in c(FALSE, TRUE)) {
    for (extreme in c(FALSE, TRUE)) {
      ts <- make_test_trueskill_state(items, mu = c(if (extreme) 100 else 25, 25, 25, 25))
      state <- make_test_state(items, ts)
      state$controller$run_mode <- "link_one_spoke"
      state$controller$global_identified <- TRUE
      state$controller$p_long_low <- 0.45
      state$controller$p_long_high <- 0.55
      state$linking$phase_a$phase <- "phase_b"
      if (accepted) {
        # Accepted draws lack the requested IDs: the existing gate must fall back.
        state$btl_fit <- make_test_btl_fit(c("other_a", "other_b"))
        state$round_log <- tibble::tibble(diagnostics_pass = TRUE)
      }
      history <- pairwiseLLM:::.adaptive_history_state_resolve(state)
      out <- pairwiseLLM:::.adaptive_select_stage(
        stage = list(name = "base", dup_policy = "default"), state = state,
        config = pairwiseLLM:::adaptive_defaults(4L), controller = state$controller,
        generation_stage = "long_link", round = state$round, history_state = history,
        counts = pairwiseLLM:::.adaptive_history_state_counts(history, state$item_ids),
        step_id = 1L, seed_base = 71L, candidates = tibble::tibble(i = "a", j = "c"))
      expect_identical(out$long_gate_pass, !extreme)
      expected <- if (extreme) {
        "posterior_unavailable_fallback_trueskill_extreme"
      } else {
        "posterior_unavailable_fallback"
      }
      expect_identical(out$long_gate_reason, expected)
    }
  }
})

test_that("explore_rate_used applies identifiability taper", {
  items <- make_test_items(6)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)
  state$round$staged_active <- TRUE
  state$round$stage_index <- 4L
  state$controller <- pairwiseLLM:::.adaptive_controller_defaults(length(state$item_ids))
  state$controller$global_identified <- TRUE

  cand <- tibble::tibble(i = "1", j = "2")
  out <- pairwiseLLM:::select_next_pair(state, step_id = 1L, candidates = cand)
  defaults <- pairwiseLLM:::adaptive_defaults(length(state$item_ids))

  expect_equal(out$explore_rate_used, defaults$explore_rate * defaults$explore_taper_mult)
})

test_that("local stage logs boundary priority mode after identifiability", {
  items <- make_test_items(12)
  trueskill_state <- make_test_trueskill_state(items, mu = seq(12, 1))
  state <- make_test_state(items, trueskill_state)
  state$round$staged_active <- TRUE
  state$round$stage_index <- 4L
  state$controller <- pairwiseLLM:::.adaptive_controller_defaults(length(state$item_ids))
  state$controller$global_identified <- TRUE
  defaults <- pairwiseLLM:::adaptive_defaults(length(state$item_ids))
  defaults$quota_eps <- 0
  defaults$explore_rate <- 0

  cand <- tibble::tibble(i = c("1", "11"), j = c("2", "12"))
  out <- testthat::with_mocked_bindings(
    adaptive_defaults = function(N) defaults,
    pairwiseLLM:::select_next_pair(state, step_id = 1L, candidates = cand),
    .package = "pairwiseLLM"
  )

  expect_identical(out$local_priority_mode, "boundary")
})

test_that("star-cap override is bounded per round", {
  items <- make_test_items(3)
  trueskill_state <- make_test_trueskill_state(items)
  history <- tibble::tibble(
    A_id = c(rep("1", 17), rep("2", 17)),
    B_id = c(rep("3", 17), rep("3", 17))
  )
  state <- make_test_state(items, trueskill_state, history = history)
  state$round$staged_active <- TRUE
  state$round$stage_index <- 4L
  state$round$star_override_budget_per_round <- 1L
  state$round$star_override_used <- 0L
  state$controller <- pairwiseLLM:::.adaptive_controller_defaults(length(state$item_ids))
  state$controller$global_identified <- TRUE

  cand <- tibble::tibble(i = "1", j = "2")
  out_1 <- pairwiseLLM:::select_next_pair(state, step_id = 1L, candidates = cand)

  state$round$star_override_used <- 1L
  out_2 <- pairwiseLLM:::select_next_pair(state, step_id = 2L, candidates = cand)

  expect_true(isTRUE(out_1$star_override_used))
  expect_identical(out_1$star_override_reason, "near_tie_override")
  expect_true(out_2$candidate_starved)
  expect_true(is.na(out_2$star_override_used))
  expect_identical(out_2$star_override_reason, "budget_exhausted")
})
