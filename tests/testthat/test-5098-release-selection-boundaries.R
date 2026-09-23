test_that("an all-top-band ranking assigns every item exactly once", {
  scores <- c(a = 3, b = 2, c = 1)
  out <- .adaptive_assign_strata(scores, list(top_band_pct = 1, top_band_bins = 3L, k_base = 2L))
  expect_identical(out$stratum_map, c(a = 1L, b = 2L, c = 3L))
  expect_identical(out$top_band_ids, names(scores))
  expect_false(anyDuplicated(names(out$stratum_map)) > 0L)
})

test_that("retained candidate ordering is stable under ties and coverage priority", {
  cand <- tibble::tibble(i = c("b", "a", "c"), j = c("d", "d", "d"), u0 = c(.2, .2, NA_real_))
  order <- function(x, ...) .adaptive_linking_selection_order(x, utility_mode = "pairing_trueskill_u0", ...)
  expect_identical(order(cand), c(2L, 1L, 3L))
  expect_identical(order(cand[FALSE, ]), integer())
  cand$coverage_priority <- c(1L, 0L, 1L)
  expect_identical(order(cand), c(1L, 3L))
  cand$u0[] <- NA_real_
  expect_error(order(cand, stage_name = "local_link", spoke_id = 2L), "non-finite")
  cand$u0 <- NULL
  expect_error(order(cand, stage_name = "local_link", spoke_id = 2L), "unavailable")
})

test_that("empty probe cells preserve typed panel fields", {
  empty <- .adaptive_link_probe_empty_pair_rows()
  expect_equal(.adaptive_link_probe_materialize_cell_pairs("h", "s", character(), 0L), empty)
  expect_equal(.adaptive_link_probe_sample_cell_pairs("h", "s", character(), 0L), empty)
  expect_equal(.adaptive_link_probe_sample_cell_pairs("h", "s", make_unordered_key("h", "s"),
    1L, random = FALSE, materialize_limit = 0L), empty)
  panel <- .adaptive_link_probe_add_panel_fields(empty, 1L, 2L, 1L, 1L)
  expect_identical(panel$spoke_id, integer())
  expect_identical(panel$pair_key, character())
  expect_error(.adaptive_link_require_phase_a_theta_map(list(), 2L, "theta_raw_mean", "s", "fixture"),
    "Phase A")
})

test_that("score fallback and endpoint roles preserve identities", {
  state <- task09_link_state()
  state$btl_fit$theta_mean[c("a", "b")] <- c(3, 4)
  expect_equal(.adaptive_link_safe_theta_map(state, 1L), c(a = -1, b = 1))
  expect_equal(.adaptive_link_safe_theta_map(state, 1L, TRUE), c(a = 3, b = 4))
  missing <- state
  missing$btl_fit <- NULL
  expect_equal(.adaptive_link_safe_theta_map(missing, 1L, TRUE), c(a = -1, b = 1))
  state$linking$phase_a$artifacts <- list()
  expect_equal(.adaptive_link_safe_theta_map(state, 1L), c(a = 3, b = 4))
  cand <- tibble::tibble(i = c("a", "c", "x"), j = c("c", "a", "a"))
  roles <- .adaptive_link_candidate_endpoint_roles(cand, c(a = 1L, c = 2L), 1L, 2L)
  expect_identical(roles$hub_item, rep("a", 3L))
  expect_identical(roles$spoke_item, c("c", "c", NA_character_))
  expect_identical(roles$i_set, c(1L, 2L, NA_integer_))
})

test_that("rank-one information algebra rejects incompatible dimensions", {
  f <- .adaptive_link_d_opt_rank1_gain_transform
  expect_equal(f(list(ok = TRUE, inv = matrix(2)), .5, "shift_only", 1, 0), log(2))
  expect_true(is.na(f(list(ok = TRUE, inv = diag(2)), .5, "shift_only", 1, 0)))
  expect_true(is.na(.adaptive_link_d_opt_rank1_gain_diag(list(ok = FALSE), .5, 1L)))
  expect_true(is.na(.adaptive_link_d_opt_gain_diag_state(-1, .5, 1L)))
  expect_identical(.adaptive_link_d_opt_state_key(3L, 2L), "3::2")
})

test_that("history cache migration supplies empty recent degrees", {
  cache <- .adaptive_history_state_upgrade(list(recent_pairs = tibble::tibble()), c("a", "b"))
  expect_identical(cache$recent_deg, c(a = 0L, b = 0L))
  expect_identical(.adaptive_history_state_recent_deg(cache, c("a", "b"), 1L), c(a = 0L, b = 0L))
})

test_that("retained stage context and coverage metadata use the selected spoke", {
  state <- task10_link_state()
  defaults <- adaptive_defaults(state$n_items)
  expect_identical(.adaptive_round_active_stage(state), "local_link")
  expect_false(.adaptive_link_phase_b_window_exhausted(state))
  ctx <- .adaptive_select_link_stage_context_build(state, state$controller, state$round,
    TRUE, list(`2` = list(B_spoke_refit_budget = 0L)), "local_link", state$item_ids, 2L)
  expect_true(ctx$generation_stage %in% c(.adaptive_stage_order(), "pooled_backfill"))
  expect_gte(ctx$stage_quota, 0L)
  expect_gte(ctx$budget_remaining_actual, 0L)
  missing <- .adaptive_link_selection_coverage_meta(state, state$controller, NA_integer_, defaults)
  expect_true(is.na(missing$source))
  coverage <- .adaptive_link_selection_coverage_meta(state, state$controller, 2L, defaults)
  expect_true(is.character(coverage$source))
  expect_gte(coverage$bins_used, 1L)
  expect_equal(nrow(.adaptive_link_candidate_pool(state, state$controller, NA_integer_)), 0L)
  pool <- .adaptive_link_candidate_pool(state, state$controller, 2L, include_utility = FALSE)
  expect_true(all(pool$link_stage %in% .adaptive_stage_order()))
})

test_that("refit summary caches can be rebuilt and reconciled with canonical history", {
  state <- task09_link_state()
  for (i in 1:2) state$step_log <- append_step_log(state$step_log,
    list(step_id = i, pair_id = i, A = 1L, B = 3L, Y = 1L,
      is_cross_set = TRUE, link_spoke_id = 2L, run_mode = "link_multi_spoke",
      link_stage = "anchor_link", is_probe_step = FALSE))
  rid <- .adaptive_link_refit_window_id(state)
  state <- .adaptive_link_refit_summary_ensure_current_entries(state, c(2L, NA_integer_), rid)
  expected <- .adaptive_link_refit_summary_current(state, rid, 2L, reconcile = TRUE)
  expect_identical(expected$n_pairs_cross_set_done, 2L)
  expect_identical(expected$n_unique_cross_pairs_seen, 1L)
  expect_identical(.adaptive_link_refit_summary_ensure_current_entries(state, 2L, rid), state)
  expect_identical(.adaptive_link_refit_summary_ensure_current_entries(state), state)
  rebuilt <- .adaptive_link_refit_summary_rebuild_current(state, rid, 2L)
  expect_equal(.adaptive_link_refit_summary_current(rebuilt, rid, 2L), expected)
  later <- .adaptive_link_refit_summary_from_step_log(state, rid, 2L, list(last_refit_step = 2L))
  expect_identical(later$n_pairs_cross_set_done, 2L)
  expect_identical(later$n_cross_edges_total_since_last_refit, 0L)
  altered <- expected
  altered$n_pairs_cross_set_done <- 3L
  altered$n_pairs_cross_set_active_done <- 3L
  expect_error(.adaptive_link_refit_summary_compare(altered, expected, rid, 2L), "does not match canonical")
  altered <- expected
  altered$stage_realized <- rev(altered$stage_realized)
  names(altered$stage_realized) <- names(expected$stage_realized)
  expect_error(.adaptive_link_refit_summary_compare(altered, expected, rid, 2L), "stage-realized")
})

test_that("pooled Phase A judge fitting forwards the inference contract", {
  state <- task09_link_state()
  fit <- state$btl_fit
  received <- NULL
  local_mocked_bindings(fit_bayes_btl_mcmc = function(...) {
    received <<- list(...)
    list(fit = fit)
  }, .package = "pairwiseLLM")
  out <- .adaptive_phase_a_pooled_judge_fit_default(tibble::tibble(), state$item_ids,
    "btl_e_b", list(chains = 2L), list(source = "synthetic"))
  expect_identical(received$inference_contract, list(source = "synthetic"))
  expect_identical(received$cmdstan$chains, 2L)
  expect_true(is.list(out))
  args <- list(model_variant = "btl_e_b", required_sets = 1:2,
    evidence_hash_by_set = c(`1` = "h", `2` = "s"), phase_b_started_at_step = 3L, created_at_step = 2L)
  expect_error(do.call(.adaptive_phase_a_pooled_judge_state_from_fit, c(list(fit = list()), args)), "beta_mean")
  expect_error(do.call(.adaptive_phase_a_pooled_judge_state_from_fit,
    c(list(fit = list(beta_mean = 0)), args)), "epsilon_mean")
})
