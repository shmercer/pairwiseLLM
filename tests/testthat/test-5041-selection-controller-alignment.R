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

test_that("long-link posterior gate activates only after identifiability", {
  items <- make_test_items(2)
  trueskill_state <- make_test_trueskill_state(items, mu = c(25, 25))
  state <- make_test_state(items, trueskill_state)
  state$round$staged_active <- TRUE
  state$round$stage_index <- 2L
  state$controller <- pairwiseLLM:::.adaptive_controller_defaults(length(state$item_ids))
  state$controller$global_identified <- TRUE
  draws <- matrix(c(1, -1, 1, -1, 1, -1), nrow = 3, byrow = TRUE)
  colnames(draws) <- state$item_ids
  state$btl_fit <- make_test_btl_fit(state$item_ids, draws = draws)

  cand <- tibble::tibble(i = "1", j = "2")
  out <- pairwiseLLM:::select_next_pair(state, step_id = 1L, candidates = cand)

  expect_true(out$candidate_starved)
  expect_identical(out$long_gate_pass, FALSE)
  expect_identical(out$long_gate_reason, "posterior_extreme")
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

  cand <- tibble::tibble(i = c("1", "11"), j = c("2", "12"))
  out <- pairwiseLLM:::select_next_pair(state, step_id = 1L, candidates = cand)

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
