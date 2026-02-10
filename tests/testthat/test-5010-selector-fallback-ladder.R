test_that("selector falls back when base window has no candidates", {
  items <- make_test_items(4)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)

  empty_candidates <- tibble::tibble(i = integer(), j = integer())
  out <- pairwiseLLM:::select_next_pair(state, candidates = empty_candidates)

  expect_false(out$candidate_starved)
  expect_equal(out$fallback_used, "expand_locality")
  expect_equal(out$fallback_path, "base>expand_locality")
  expect_false(is.na(out$i))
  expect_false(is.na(out$j))
})

test_that("local fallback stages preserve local semantics and allow same-stratum pairs", {
  items <- make_test_items(12)
  trueskill_state <- make_test_trueskill_state(items, mu = seq(12, 1))
  state <- make_test_state(items, trueskill_state)
  state$round$staged_active <- TRUE
  state <- pairwiseLLM:::.adaptive_refresh_round_anchors(state)

  local_expand <- pairwiseLLM:::generate_stage_candidates_from_state(
    state, "local_link", "expand_locality", C_max = 5000L, seed = 1L
  )
  local_global <- pairwiseLLM:::generate_stage_candidates_from_state(
    state, "local_link", "global_safe", C_max = 5000L, seed = 1L
  )

  defaults <- pairwiseLLM:::adaptive_defaults(nrow(items))
  proxy <- pairwiseLLM:::.adaptive_rank_proxy(state)
  strata <- pairwiseLLM:::.adaptive_assign_strata(proxy$scores, defaults)
  dist_expand <- abs(
    as.integer(strata$stratum_map[local_expand$i]) - as.integer(strata$stratum_map[local_expand$j])
  )
  dist_global <- abs(
    as.integer(strata$stratum_map[local_global$i]) - as.integer(strata$stratum_map[local_global$j])
  )

  expect_true(nrow(local_expand) > 0L)
  expect_true(nrow(local_global) >= nrow(local_expand))
  expect_true(all(dist_expand <= defaults$local_expand_max_dist))
  expect_true(any(dist_global == 0L))
})
