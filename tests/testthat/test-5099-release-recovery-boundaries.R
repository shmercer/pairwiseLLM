test_that("Phase A caches rebuild malformed counts and ignore incomplete evidence", {
  state <- task09_link_state()
  expected <- .adaptive_phase_a_committed_pairs_rebuild(state)
  state$refit_meta$phase_a_committed_pairs_by_set <- "corrupt"
  expect_equal(.adaptive_phase_a_committed_pairs_resolve(state), expected)
  expect_error(.adaptive_phase_a_committed_pairs_resolve(state, validate_existing = TRUE), "cache")
  state$linking$phase_a$within_set_evidence_by_set <- "corrupt"
  expect_identical(.adaptive_phase_a_runtime_evidence_cache(state), list())
  state$step_log <- tibble::tibble(step_id = 1L)
  expect_identical(.adaptive_phase_a_within_set_evidence_from_state(state, 1L),
    .adaptive_phase_a_empty_within_set_evidence())
  f <- function(row, y = 1L) .adaptive_phase_a_within_set_evidence_update("corrupt", state, row, "a", "b", y)
  expect_identical(f(tibble::tibble()), list())
  row <- tibble::tibble(set_i = 1L, set_j = 2L, pair_id = 1L, step_id = 1L)
  expect_identical(f(row), list())
  row$set_j <- 1L
  expect_identical(f(row, NA_integer_), list())
  row$pair_id <- 0L
  expect_identical(f(row), list())
  ctl <- state$controller
  ctl$judge_param_mode <- "per_set"
  expect_identical(.adaptive_phase_a_ensure_pooled_judge_state(state, controller = ctl), state)
})

test_that("pooled judge evidence cannot silently omit a multi-item set", {
  state <- task09_link_state()
  artifacts <- state$linking$phase_a$artifacts
  art <- artifacts[["1"]]
  art$phase_a_within_set_evidence <- .adaptive_phase_a_empty_within_set_evidence()
  art$phase_a_within_set_evidence_hash <- NULL
  art$n_pairs_committed <- 0L
  artifacts[["1"]] <- art
  expect_error(.adaptive_phase_a_pooled_judge_results(state, artifacts, 1:3, state$controller),
    "non-empty within-set evidence")
})

test_that("concurrent refit floors include probes without losing minimum active effort", {
  state <- task09_link_state()
  ctl <- state$controller
  ctl$multi_spoke_mode <- "concurrent"
  ctl$link_refit_pairs_per_spoke_rule <- "fixed"
  ctl$probe_active_floor_enabled <- FALSE
  ctl$probe_pairs_per_refit_per_spoke <- 4L
  phase <- list(phase = "phase_b", active_spokes = 2:3)
  f <- function(controller = ctl, context = phase) {
    .adaptive_phase_b_refit_pairs_target_floor(state, controller, context)
  }
  expect_identical(f(), 8L)
  expect_identical(f(context = list(phase = "phase_a", active_spokes = 2:3)), 0L)
  expect_identical(f(context = list(phase = "phase_b", active_spokes = 2L)), 0L)
  ctl$probe_active_floor_enabled <- TRUE
  ctl$probe_active_floor_min <- 1L
  ctl$probe_active_floor_frac <- .5
  expect_identical(f(), 16L)
  ctl$probe_active_floor_frac <- 2
  expect_error(f(), "must be in")
})

test_that("global score diagnostics require usable hub shapes and prior probe history", {
  state <- task09_link_state()
  expect_equal(.adaptive_link_delta_sd_max_derived(state, 1L, .2),
    .2 * sd(state$btl_fit$theta_mean[c("a", "b")]))
  expect_true(is.na(.adaptive_link_delta_sd_max_derived(state, 99L, .2)))
  state$btl_fit$theta_mean <- NULL
  state$btl_fit$btl_posterior_draws <- NULL
  expect_error(.adaptive_link_delta_sd_max_derived(state, 1L, .2), "numeric matrix")
  f <- .adaptive_link_probe_prior_realized_max
  expect_true(is.na(f(NULL, 2L, 1L, 3L)))
  rows <- tibble::tibble(spoke_id = c(2L, 2L, 2L, 3L), link_epoch_id = 1L,
    refit_id = c(1L, 2L, 3L, 1L), probe_edges_realized = c(2L, 4L, 8L, 99L))
  expect_identical(f(rows, 2L, 1L, 3L), 4L)
  expect_true(is.na(f(rows, 2L, 2L, 3L)))
  rows$probe_edges_realized[] <- NA_integer_
  expect_true(is.na(f(rows, 2L, 1L, 3L)))
})

test_that("cached concurrent budget status updates from actual committed counts", {
  state <- task09_link_state()
  ctl <- state$controller
  ctl$multi_spoke_mode <- "concurrent"
  ctl$link_budget_refit_id <- .adaptive_link_refit_window_id(state)
  ctl$link_budget_map <- list(`2` = list(B_spoke_refit_budget = 3L,
    concurrent_target_pairs = 3L, concurrent_floor_pairs = 0L),
    `3` = list(B_spoke_refit_budget = 0L, concurrent_target_pairs = 0L, concurrent_floor_pairs = 0L))
  out <- .adaptive_link_budget_map_for_refit(state, ctl, 2:3)
  expect_false(out[["2"]]$concurrent_target_met)
  expect_true(out[["2"]]$concurrent_floor_met)
  expect_true(out[["3"]]$concurrent_target_met)
  ctl$multi_spoke_mode <- "independent"
  ctl$link_budget_map <- ctl$link_budget_map["2"]
  out <- .adaptive_link_budget_map_for_refit(state, ctl, 2:3)
  expect_identical(out[["3"]]$B_spoke_refit_budget, 0L)
  expect_identical(out[["2"]], ctl$link_budget_map[["2"]])
})

test_that("quota arithmetic preserves budgets when supplied capacity changes", {
  # Unit-test allocation against a synthetic capacity snapshot. This does not
  # exercise or validate the gated adaptive selector that would produce it.
  state <- task09_link_state()
  stages <- .adaptive_stage_order()
  quotas <- stats::setNames(c(5L, 1L, 1L, 1L), stages)
  snapshot <- list(feasible_counts = stats::setNames(c(1L, 2L, 3L, 4L), stages),
    feasible_utility_mass = stats::setNames(c(0, 0, 1, 2), stages))
  local_mocked_bindings(.adaptive_link_stage_feasibility_snapshot = function(...) snapshot,
    .package = "pairwiseLLM")
  allocate <- function(q = quotas, spoke = 2L) {
    .adaptive_link_adjust_stage_quotas_for_feasibility(state, state$controller, spoke, q, stages)
  }
  out <- allocate()
  expect_equal(sum(out), sum(quotas))
  expect_true(all(out <= snapshot$feasible_counts))
  expect_identical(attr(out, "quota_meta")$feasibility_budget_released, 4L)
  expect_identical(attr(out, "quota_meta")$feasibility_reallocation_rule, "pooled_utility_backfill")
  attr(quotas, "quota_meta") <- list(linking_identified = TRUE)
  expect_equal(sum(allocate()), 8L)
  snapshot$feasible_counts[] <- 1L
  expect_identical(as.integer(allocate()), rep(1L, 4L))
  snapshot$feasible_counts[] <- 10L
  expect_false(attr(allocate(), "quota_meta")$feasibility_reallocation_used)
  expect_identical(as.integer(allocate(quotas * 0L)), rep(0L, 4L))
  expect_identical(as.integer(allocate(spoke = NA_integer_)), as.integer(quotas))
})

test_that("coverage metadata falls back to raw Phase A ranks when routing data are unavailable", {
  state <- task09_link_state()
  defaults <- adaptive_defaults(state$n_items)
  # The input-cache boundary may be unavailable in a historical session. Leave
  # prediction and selection gates untouched while testing metadata recovery.
  local_mocked_bindings(.adaptive_link_refit_local_inputs = function(...) stop("cache unavailable"),
    .package = "pairwiseLLM")
  f <- function(s, spoke = 2L) .adaptive_link_selection_coverage_meta(s, s$controller, spoke, defaults)
  out <- f(state)
  expect_true(is.character(out$source))
  expect_identical(out$bins_used, 1L)
  state$linking$phase_a$artifacts[["2"]]$items$rank_mu_raw <- 1:2
  expect_identical(f(state)$source, "phase_a_rank_mu_raw")
  state$step_log <- tibble::tibble(pair_id = 1:12, is_cross_set = TRUE, link_spoke_id = 2L)
  expect_identical(f(state)$source, "linking_global_score")
  expect_true(is.na(f(state, 99L)$source))
})

test_that("checkpoint and local memo boundaries reject incomplete requests", {
  expect_error(.adaptive_normalize_checkpoint_every_steps(NULL, allow_null = FALSE), "positive integer")
  state <- task09_link_state()
  state$step_log <- tibble::tibble(step_id = NA_integer_)
  expect_identical(.adaptive_link_refit_local_step_id(state), 0L)
  expect_silent(.adaptive_link_refit_local_memo_prune(NULL))
})

test_that("probe recovery guards handle missing blockers, budgets and panels", {
  surface <- function(codes) {
    tibble::tibble(
    link_lag_eligible = TRUE, link_min_refit_eligible = TRUE, link_diagnostics_pass = TRUE,
    reliability_link_global = .95, link_stop_reliability_min_used = .9,
    probe_brier = .1, probe_brier_max_used = .19,
    probe_pred_rmse_lagged = .01, probe_pred_rmse_max_used = .015,
    theta_global_rmse_lagged = .04, theta_global_rmse_max_used = .05,
    hub_anchored = TRUE, probe_edges_min_for_stop_used = 6L, stop_blocker_codes = codes)
  }
  s <- task10_link_state()
  s$controller$multi_spoke_mode <- "concurrent"
  s$controller$link_budget_refit_id <- .adaptive_link_refit_window_id(s)
  s$controller$link_budget_map <- list(`2` = list(B_spoke_refit_budget = 0L), `3` = list(B_spoke_refit_budget = 2L))
  expect_identical(.adaptive_link_probe_active_progress_guard(s, s$controller, integer())$budgeted_spokes, integer())
  expect_identical(.adaptive_link_probe_active_progress_guard(s, s$controller)$budgeted_spokes, 3L)
  s$controller$link_budget_refit_id <- NA_integer_
  expect_identical(.adaptive_link_probe_active_progress_guard(s, s$controller)$budgeted_spokes, integer())
  for (bad in c("", NA_character_)) {
   surf <- surface(bad)
   expect_error(.adaptive_link_probe_validate_blocker_surface(surf, 0L, 6L, 2L, "test"),
    "canonical stop blockers are unavailable")
  }
  surf <- surface("reliability_link_global")
  expect_error(.adaptive_link_probe_validate_blocker_surface(surf, 0L, 6L, 2L, "test"), "are inconsistent")
  surf <- surface("probe_edges_min_for_stop")
  expect_identical(.adaptive_link_probe_validate_blocker_surface(surf, 0L, 6L, 2L, "test"), "probe_edges_min_for_stop")
  expect_identical(.adaptive_link_probe_released_cap_when_active(list(allow_when_active = TRUE,
    effective_cap = 0L)), 0L)
  for (n in c(3L, 4L, 9L)) {
   plan <- list(allow_when_active = TRUE, effective_cap = 3L, active_floor_used = 3L, active_nonprobe_since_refit = n)
   expect_identical(.adaptive_link_probe_released_cap_when_active(plan), min(3L, n - 2L))
  }
  s <- task10_link_state()
  s$controller$link_budget_refit_id <- .adaptive_link_refit_window_id(s)
  s$controller$link_budget_map <- list(`2` = list(B_spoke_refit_budget = 0L))
  s$linking$probe <- .adaptive_link_probe_empty_state()
  expect_identical(.adaptive_link_probe_next_holdout_spoke(s, s$controller, 2L), 2L)
  expect_true(is.na(.adaptive_link_probe_next_holdout_spoke(s, s$controller, integer())))
  expect_true(is.na(.adaptive_link_probe_next_holdout_spoke(s, s$controller, 2L, TRUE)))
  expect_identical(.adaptive_link_probe_cache_predictions(s, 1L, 2L), s)
  expect_identical(nrow(.adaptive_link_probe_realized_log_for_panel(s, 2L, 1L)), 0L)
  expect_identical(nrow(.adaptive_link_probe_realized_rows_from_entry(s, list())), 0L)
  expect_identical(.adaptive_link_probe_realized_count_since_step(s, 2L), 0L)
  expect_identical(.adaptive_link_probe_holdout_total_since_last_refit(s), 0L)
  expect_identical(.adaptive_link_probe_panel_feasible_size(4L), 4L)
  expect_identical(.adaptive_link_probe_panel_feasible_size(4L, 3L, active_reserve_frac = NaN), 3L)
  expect_false(.adaptive_link_probe_panel_reallocation_used(tibble::tibble(probe_panel_reallocation_used = NA)))
  expect_identical(.adaptive_link_is_holdout_probe_rows(tibble::tibble()), logical())
  s$link_stage_log <- tibble::tibble(refit_id = 1L, spoke_id = 3L)
  expect_identical(nrow(.adaptive_link_probe_last_stage_row(s, 2L)), 0L)
  probe <- .adaptive_link_probe_empty_state()
  probe$realized_index_by_panel <- "malformed"
  expect_identical(.adaptive_link_probe_realized_index_reconcile(probe)$realized_index_by_panel, list())
  probe$realized_edges <- tibble::tibble(spoke_id = 2L, link_epoch_id = 1L, probe_panel_id = "p",
    step_id = 1L, pair_key = "a:c")
  probe <- .adaptive_link_probe_realized_index_reconcile(probe)
  expect_identical(.adaptive_link_probe_realized_index_reconcile(probe, validate_existing = TRUE), probe)
})

test_that("startup and taper recovery distinguish missing and completed spoke refits", {
  state <- task09_link_state()
  state$controller$link_refit_stats_by_spoke <- list()
  f <- .adaptive_link_phase_b_startup_gap_for_spoke
  expect_true(f(state, 2L))
  state$step_log <- tibble::tibble(pair_id = 1L, is_cross_set = TRUE, link_spoke_id = 3L)
  expect_true(f(state, 2L))
  state$step_log$link_spoke_id <- 2L
  expect_true(f(state, 2L))
  state$link_stage_log <- tibble::tibble(spoke_id = 2L)
  expect_false(f(state, 2L))
  state$controller$link_refit_stats_by_spoke <- list(`2` = list(refit_id = 1L))
  expect_false(f(state, 2L))
  state$linking$phase_a$phase <- "phase_a"
  expect_false(f(state, 2L))
  state$controller$run_mode <- "within_set"
  expect_false(f(state, 2L))
  taper <- .adaptive_link_reconstruct_taper_from_logs
  expect_true(is.na(taper(tibble::tibble())))
  expect_false(taper(tibble::tibble(quota_taper_applied = FALSE)))
  expect_true(taper(tibble::tibble(quota_long_link_raw = 3L, quota_long_link_effective = 2L)))
  expect_false(taper(tibble::tibble(quota_long_link_raw = 3L, quota_long_link_effective = 3L)))
  expect_true(is.na(taper(tibble::tibble(quota_long_link_raw = NA_integer_, quota_long_link_effective = NA_integer_))))
})

test_that("held-out probes become available after sufficient disjoint active comparisons", {
  state <- task10_link_state(2L)
  ctl <- state$controller
  ctl$probe_panel_edges <- 4L
  ctl$probe_pairs_per_refit_per_spoke <- 4L
  ctl$probe_active_floor_enabled <- TRUE
  ctl$probe_active_floor_min <- 1L
  ctl$probe_active_floor_frac <- 0
  ctl$probe_active_floor_requires_anchor_progress <- TRUE
  ctl$link_budget_refit_id <- .adaptive_link_refit_window_id(state)
  ctl$link_budget_map <- list(`2` = list(B_spoke_refit_budget = 8L,
    B_spoke_refit_budget_source = "single_spoke_controller"))
  state$controller <- ctl
  for (i in 1:4) state$step_log <- append_step_log(state$step_log, list(
    step_id = i, pair_id = i, i = 1L, j = 3L, A = 1L, B = 3L, Y = 1L, set_i = 1L, set_j = 2L,
    is_cross_set = TRUE, link_spoke_id = 2L, is_probe_step = FALSE,
    run_mode = "link_multi_spoke", link_stage = "anchor_link", round_stage = "anchor_link"))
  state$history_pairs <- tibble::tibble(A_id = rep("a", 4L), B_id = rep("c", 4L))
  state <- .adaptive_link_probe_ensure_panels(state, ctl, 2L)
  panel <- state$linking$probe$panels_by_spoke[["2"]]
  expect_false(make_unordered_key("a", "c") %in% panel$pair_key)
  plan <- .adaptive_link_probe_effort_plan(state, ctl, 2L)
  expect_true(plan$allow_when_active)
  expect_identical(plan$active_nonprobe_since_refit, 4L)
  expect_identical(.adaptive_link_probe_next_holdout_spoke(state, ctl, 2L, TRUE), 2L)
  # The public selector still rejects this same state.
  expect_error(select_next_pair(state), class = "pairwiseLLM_link_selector_unvalidated")
})
