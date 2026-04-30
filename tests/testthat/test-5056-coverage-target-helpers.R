make_cov_target_phase_a_state <- function() {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s1", "s2"),
    text = c("h1", "h2", "s1", "s2"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs1", "gs2")
  )

  state <- adaptive_rank_start(
    items,
    seed = 17L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("ready", "pending"),
      validation_message = c("ok", "pending"),
      artifact_path = c(NA_character_, NA_character_)
    )
  )
  state
}

test_that("adaptive run phase-a scope and unresolved helpers update canonical fields", {
  phase_a_state <- make_cov_target_phase_a_state()

  scope <- .adaptive_link_phase_a_scope(phase_a_state)
  expect_identical(scope$active_set_id, 2L)
  expect_identical(scope$active_set_n, 2L)

  scoped_controller <- .adaptive_controller_with_phase_scope(phase_a_state)
  expect_identical(scoped_controller$phase_a_active_set_id, 2L)
  expect_identical(scoped_controller$phase_a_active_n, 2L)

  failed <- .adaptive_phase_a_mark_unresolved(
    phase_a_state,
    set_id = 2L,
    message = "artifact_missing"
  )
  expect_identical(failed$linking$phase_a$set_status$status[[2L]], "failed")
  expect_identical(
    failed$linking$phase_a$set_status$validation_message[[2L]],
    "artifact_missing"
  )
  expect_identical(failed$meta$stop_reason_detail, "artifact_missing")

  untouched <- .adaptive_phase_a_mark_unresolved(
    phase_a_state,
    set_id = 99L,
    message = "still_recorded"
  )
  expect_identical(
    untouched$linking$phase_a$set_status$status,
    phase_a_state$linking$phase_a$set_status$status
  )
  expect_identical(untouched$meta$stop_reason_detail, "still_recorded")
})

test_that("adaptive run refit summary helpers validate, store, and seed cumulative counts", {
  entry <- .adaptive_link_refit_summary_empty(refit_id = 2L, spoke_id = 3L)
  entry$n_pairs_cross_set_done <- 5L
  entry$n_pairs_cross_set_active_done <- 4L
  entry$n_pairs_cross_set_probe_done <- 1L
  entry$n_unique_cross_pairs_seen <- 4L
  entry$n_cross_edges_active_since_last_refit <- 2L
  entry$n_cross_edges_probe_since_last_refit <- 1L
  entry$n_cross_edges_total_since_last_refit <- 3L
  entry$stage_realized <- list(anchor_link = 1L, long_link = 1L)

  validated <- .adaptive_link_refit_summary_validate(entry, context = "test")
  expect_identical(
    unname(validated$stage_realized[.adaptive_stage_order()]),
    c(1L, 1L, 0L, 0L)
  )
  expect_false(validated$probe_panel_acceleration_used_since_last_refit)

  state <- .adaptive_link_refit_summary_store(list(refit_meta = list()), validated)
  latest <- .adaptive_link_refit_summary_latest_for_spoke(state, spoke_id = 3L)
  expect_identical(latest$n_pairs_cross_set_done, 5L)
  expect_identical(latest$n_pairs_cross_set_probe_done, 1L)

  seeded <- .adaptive_link_refit_summary_seed_for_refit(
    state,
    refit_id = 3L,
    spoke_id = 3L
  )
  expect_identical(seeded$n_pairs_cross_set_done, 5L)
  expect_identical(seeded$n_pairs_cross_set_active_done, 4L)
  expect_identical(seeded$n_pairs_cross_set_probe_done, 1L)
  expect_identical(seeded$n_unique_cross_pairs_seen, 4L)

  bad_total <- validated
  bad_total$n_pairs_cross_set_done <- 6L
  expect_error(
    .adaptive_link_refit_summary_validate(bad_total, context = "test"),
    "cumulative total cross-edge count must equal cumulative active plus probe counts"
  )

  bad_stage <- validated
  bad_stage$stage_realized <- list(anchor_link = 3L)
  expect_error(
    .adaptive_link_refit_summary_validate(bad_stage, context = "test"),
    "stage-realized counts exceed current-window active cross-edge count"
  )

  bad_unique <- validated
  bad_unique$n_unique_cross_pairs_seen <- 6L
  expect_error(
    .adaptive_link_refit_summary_validate(bad_unique, context = "test"),
    "unique cross-pair count exceeds cumulative committed cross-edge count"
  )

  bad_flag <- validated
  bad_flag$probe_panel_acceleration_used_since_last_refit <- c(TRUE, FALSE)
  expect_error(
    .adaptive_link_refit_summary_validate(bad_flag, context = "test"),
    "must be TRUE or FALSE"
  )
})

test_that("adaptive run probe index and panel helpers cover edge validation branches", {
  realized <- dplyr::bind_rows(
    .adaptive_link_probe_empty_realized_log(),
    tibble::tibble(
      step_id = c(1L, 2L, 3L),
      pair_id = c(11L, 12L, 13L),
      run_mode = c("link_probe_holdout", "link_probe_holdout", "link_probe_holdout"),
      spoke_id = c(2L, 2L, 2L),
      link_epoch_id = c(1L, 1L, 1L),
      probe_panel_id = c("panel-a", "panel-a", "panel-a"),
      hub_item_id = c("h1", "h1", "h2"),
      spoke_item_id = c("s1", "s1", "s2"),
      pair_key = c(
        make_unordered_key("h1", "s1"),
        make_unordered_key("h1", "s1"),
        make_unordered_key("h2", "s2")
      ),
      Y = c(1L, 0L, 1L)
    )
  )

  expect_identical(
    .adaptive_link_probe_realized_index_key(2L, 1L, c("panel-a", "panel-b")),
    "2::1::panel-a"
  )

  index <- .adaptive_link_probe_realized_index_build(realized)
  key <- .adaptive_link_probe_realized_index_key(2L, 1L, "panel-a")
  entry <- index[[key]]
  expect_identical(entry$row_ids, c(2L, 3L))
  expect_identical(entry$realized_count, 2L)
  expect_identical(entry$last_realized_step_id, 3L)

  state <- list(
    linking = list(
      probe = list(
        realized_edges = realized,
        realized_index_by_panel = index
      )
    )
  )
  rows <- .adaptive_link_probe_realized_rows_from_entry(state, entry)
  expect_identical(as.integer(rows$step_id), c(2L, 3L))

  bad_entry <- entry
  bad_entry$realized_count <- 3L
  expect_error(
    .adaptive_link_probe_realized_rows_from_entry(state, bad_entry),
    "realized count does not match stored row count"
  )

  rebuilt <- .adaptive_link_probe_realized_index_rebuild_state(
    list(linking = list(probe = list(realized_edges = realized, realized_index_by_panel = "bad"))),
    context = "test"
  )
  expect_true(is.list(rebuilt$linking$probe$realized_index_by_panel))

  expect_identical(
    .adaptive_link_probe_panel_identity(tibble::tibble(probe_panel_id = "p1"), 2L, 1L),
    "p1"
  )
  expect_true(is.na(.adaptive_link_probe_panel_identity(
    .adaptive_link_probe_empty_panel(),
    2L,
    1L
  )))
  expect_error(
    .adaptive_link_probe_panel_identity(
      tibble::tibble(probe_panel_id = c("p1", "p2")),
      2L,
      1L
    ),
    "multiple `probe_panel_id` values"
  )

  expect_identical(
    .adaptive_link_probe_planned_edges(tibble::tibble(probe_edges_planned = c(5L, 5L))),
    5L
  )
  expect_identical(.adaptive_link_probe_planned_edges(tibble::tibble(x = 1:2)), 2L)
  expect_false(.adaptive_link_probe_panel_reallocation_used(tibble::tibble()))
  expect_true(.adaptive_link_probe_panel_reallocation_used(
    tibble::tibble(probe_panel_reallocation_used = c(TRUE, TRUE))
  ))
  expect_error(
    .adaptive_link_probe_panel_reallocation_used(
      tibble::tibble(probe_panel_reallocation_used = c(TRUE, FALSE))
    ),
    "multiple `probe_panel_reallocation_used` values"
  )

  expect_identical(
    .adaptive_link_is_holdout_probe_rows(tibble::tibble(
      run_mode = c("link_probe_holdout", "within_set"),
      is_holdout_probe_step = c(FALSE, TRUE)
    )),
    c(TRUE, FALSE)
  )
})

test_that("adaptive run blocker-surface helpers cover fallback and trigger branches", {
  expect_identical(
    .adaptive_link_probe_surface_value(list(metric = list(7L)), "metric", default = 1L),
    7L
  )
  expect_identical(
    .adaptive_link_probe_surface_value(list(metric = list()), "metric", default = 1L),
    1L
  )
  expect_identical(
    .adaptive_link_probe_parse_blocker_codes(
      " none | probe_edges_min_for_stop, reliability | probe_edges_min_for_stop "
    ),
    c("probe_edges_min_for_stop", "reliability")
  )
  expect_error(
    .adaptive_link_probe_required_surface_fields(
      surface_row = list(stop_blocker_codes = "probe_edges_min_for_stop"),
      required_fields = c("stop_blocker_codes", "probe_brier"),
      spoke_id = 2L,
      source = "controller_stats"
    ),
    "is incomplete for probe_brier"
  )

  none_surface <- testthat::with_mocked_bindings(
    .adaptive_link_probe_last_stage_row = function(...) tibble::tibble(),
    .adaptive_link_probe_runtime_surface_row(
      state = list(),
      controller = list(link_refit_stats_by_spoke = list()),
      spoke_id = 2L
    ),
    .package = "pairwiseLLM"
  )
  expect_identical(none_surface$source, "none")

  merged_surface <- testthat::with_mocked_bindings(
    .adaptive_link_probe_last_stage_row = function(...) tibble::tibble(stage_only = 5L),
    .adaptive_link_probe_runtime_surface_row(
      state = list(),
      controller = list(
        link_refit_stats_by_spoke = list(`2` = list(stop_blocker_codes = "probe_edges_min_for_stop"))
      ),
      spoke_id = 2L
    ),
    .package = "pairwiseLLM"
  )
  expect_identical(merged_surface$source, "controller_stats+link_stage_log")
  expect_identical(merged_surface$row$stage_only, 5L)
  expect_identical(merged_surface$row$stop_blocker_codes, "probe_edges_min_for_stop")

  blockers <- .adaptive_link_probe_validate_blocker_surface(
    surface_row = list(
      stop_blocker_codes = "probe_edges_min_for_stop",
      link_diagnostics_pass = TRUE,
      link_lag_eligible = TRUE,
      link_min_refit_eligible = TRUE,
      reliability_link_global = 0.99,
      link_stop_reliability_min_used = 0.90,
      probe_brier = 0.01,
      probe_brier_max_used = 0.10,
      probe_pred_rmse_lagged = 0.01,
      probe_pred_rmse_max_used = 0.10,
      theta_global_rmse_lagged = 0.01,
      theta_global_rmse_max_used = 0.10,
      hub_anchored = TRUE,
      probe_edges_min_for_stop_used = 30L,
      probe_quality_pass = TRUE
    ),
    realized_before_refit = 20L,
    realized_min = 30L,
    spoke_id = 2L,
    source = "controller_stats"
  )
  expect_identical(blockers, "probe_edges_min_for_stop")
})

test_that("adaptive select history-state validators and resolvers reject invalid cache variants", {
  ids <- c("a", "b")
  valid_cache <- .adaptive_history_state_empty(ids)

  expect_error(.adaptive_history_state_validate(1, ids, context = "test"), "must be a list")

  missing_field <- valid_cache
  missing_field$recent_pairs <- NULL
  expect_error(
    .adaptive_history_state_validate(missing_field, ids, context = "test"),
    "missing required fields"
  )

  bad_n_pairs <- valid_cache
  bad_n_pairs$n_pairs <- -1L
  expect_error(
    .adaptive_history_state_validate(bad_n_pairs, ids, context = "test"),
    "single non-negative integer"
  )

  bad_deg_type <- valid_cache
  bad_deg_type$deg <- c(a = 0, b = 0)
  expect_error(
    .adaptive_history_state_validate(bad_deg_type, ids, context = "test"),
    "`history_state\\$deg` must be an integer vector"
  )

  bad_deg_names <- valid_cache
  bad_deg_names$deg <- stats::setNames(as.integer(c(0, 0)), c("a", "c"))
  expect_error(
    .adaptive_history_state_validate(bad_deg_names, ids, context = "test"),
    "names must exactly match"
  )

  bad_pair_count <- valid_cache
  bad_pair_count$pair_count <- as.integer(1)
  expect_error(
    .adaptive_history_state_validate(bad_pair_count, ids, context = "test"),
    "must be named when non-empty"
  )

  bad_pair_last <- valid_cache
  bad_pair_last$pair_last_order <- list("a|b" = c("a", "a"))
  expect_error(
    .adaptive_history_state_validate(bad_pair_last, ids, context = "test"),
    "length-2 non-self character vectors"
  )

  bad_recent_cols <- valid_cache
  bad_recent_cols$recent_pairs <- tibble::tibble(A_id = "a")
  expect_error(
    .adaptive_history_state_validate(bad_recent_cols, ids, context = "test"),
    "must contain `A_id` and `B_id`"
  )

  bad_recent_n <- valid_cache
  bad_recent_n$n_pairs <- 1L
  expect_error(
    .adaptive_history_state_validate(bad_recent_n, ids, context = "test"),
    "must contain exactly min\\(n_pairs, 2000\\) rows"
  )

  bad_recent_window <- valid_cache
  bad_recent_window$recent_window_n <- 2001L
  expect_error(
    .adaptive_history_state_validate(bad_recent_window, ids, context = "test"),
    "recent_window_n"
  )

  bad_recent_deg <- valid_cache
  bad_recent_deg$recent_deg <- c(a = 0, b = 0)
  expect_error(
    .adaptive_history_state_validate(bad_recent_deg, ids, context = "test"),
    "`history_state\\$recent_deg` must be an integer vector"
  )

  state <- list(
    item_ids = ids,
    history_pairs = tibble::tibble(A_id = "a", B_id = "b"),
    history_state = bad_deg_type
  )
  rebuilt <- .adaptive_history_state_resolve(state, validate_existing = FALSE, context = "test")
  expect_identical(rebuilt$n_pairs, 1L)
  expect_error(
    .adaptive_history_state_resolve(state, validate_existing = TRUE, context = "test"),
    "`history_state\\$deg` must be an integer vector"
  )

  stale_state <- list(
    item_ids = ids,
    history_pairs = tibble::tibble(A_id = "a", B_id = "b"),
    history_state = valid_cache
  )
  stale_resolved <- .adaptive_history_state_resolve(stale_state, context = "test")
  expect_identical(stale_resolved$n_pairs, 1L)

  legacy_cache <- valid_cache
  legacy_cache$recent_window_n <- NULL
  legacy_cache$recent_deg <- NULL
  upgraded <- .adaptive_history_state_resolve(
    list(
      item_ids = ids,
      history_pairs = tibble::tibble(A_id = "a", B_id = "b"),
      history_state = legacy_cache
    ),
    validate_existing = TRUE,
    context = "test"
  )
  expect_identical(upgraded$recent_deg, c(a = 1L, b = 1L))
})

test_that("adaptive select history update and utility helpers cover remaining branches", {
  ids <- c("a", "b", "c")
  cache <- .adaptive_history_state_empty(ids)
  updated <- .adaptive_history_state_update(cache, "a", "b")
  expect_identical(updated$n_pairs, 1L)
  expect_identical(updated$deg[c("a", "b", "c")], c(a = 1L, b = 1L, c = 0L))
  expect_identical(
    updated$recent_window_n,
    pairwiseLLM:::.adaptive_history_state_live_recent_window(ids)
  )
  expect_identical(updated$recent_deg[c("a", "b", "c")], c(a = 1L, b = 1L, c = 0L))
  expect_identical(updated$pair_count[[make_unordered_key("a", "b")]], 1L)
  expect_error(
    .adaptive_history_state_update(updated, "a", "a"),
    "invalid committed pair"
  )

  updated$recent_pairs <- tibble::tibble(
    A_id = c("a", "x", "b"),
    B_id = c("b", "b", "c")
  )
  expect_identical(
    .adaptive_history_state_recent_deg(updated, ids = ids, W_cap = 2L),
    c(a = 0L, b = 1L, c = 1L)
  )
  expect_identical(
    .adaptive_history_state_recent_deg(updated, ids = ids, W_cap = 0L),
    c(a = 0L, b = 0L, c = 0L)
  )
  expect_error(
    .adaptive_history_state_recent_deg(
      utils::modifyList(
        updated,
        list(
          n_pairs = 5000L,
          recent_pairs = tibble::tibble(A_id = rep("a", 3L), B_id = rep("b", 3L))
        )
      ),
      ids = ids,
      W_cap = 3000L
    ),
    "Rebuild from canonical history"
  )

  expect_false(.adaptive_selection_mode_is_linking("link_one_spoke", is_cross_set = FALSE))
  expect_true(.adaptive_selection_mode_is_linking("link_one_spoke", is_cross_set = TRUE))
  expect_identical(
    .adaptive_linking_utility_mode("anchored_joint"),
    "linking_d_optimal_anchored_joint"
  )
  expect_true(.adaptive_is_linking_d_optimal_mode("linking_d_optimal", allow_legacy = TRUE))
  expect_false(.adaptive_is_linking_d_optimal_mode("linking_d_optimal", allow_legacy = FALSE))
  expect_identical(
    .adaptive_selection_utility_mode(
      run_mode = "link_one_spoke",
      is_cross_set = TRUE,
      link_estimation_mode = "anchored_joint"
    ),
    "linking_d_optimal_anchored_joint"
  )
  expect_identical(.adaptive_resolve_selection_column("linking_d_optimal"), "link_d_opt_gain")
})

test_that("adaptive round candidate helpers cover fallback, refresh, and phase-a guard branches", {
  items <- make_test_items(3)
  state <- make_test_state(
    items,
    make_test_trueskill_state(items, mu = c(0.3, 0.2, 0.1))
  )
  state$refit_meta$last_refit_round_id <- 4L
  state$btl_fit <- list(theta_mean = c("1" = Inf, "2" = 0.2, "3" = 0.1))

  proxy <- .adaptive_rank_proxy(state, prefer_btl = TRUE)
  expect_identical(proxy$source, "trueskill_mu")
  expect_identical(proxy$refit_id, 4L)

  expect_identical(.adaptive_bucket_counts(5L, c(1, 1, 1)), c(2L, 2L, 1L))

  defaults <- adaptive_defaults(12L)
  defaults$anchor_refresh_on_round <- TRUE
  state$round <- list(
    anchor_ids = c("1", "2"),
    anchor_refit_round_id = 1L,
    anchor_round_id = 1L,
    round_id = 2L
  )
  state$refit_meta$last_refit_round_id <- 1L
  expect_true(.adaptive_round_anchor_needs_refresh(state, defaults))

  defaults$anchor_refresh_on_round <- FALSE
  expect_false(.adaptive_round_anchor_needs_refresh(state, defaults))

  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_link_phase_a_theta_map = function(...) {
        stats::setNames(c(0.1, NA_real_), c("a", "b"))
      },
      .adaptive_link_require_phase_a_theta_map(
        state = list(),
        set_id = 2L,
        field = "theta_raw_mean",
        required_item_ids = c("a", "b"),
        helper_name = "Linking routing"
      ),
      .package = "pairwiseLLM"
    ),
    "missing/non-finite"
  )

  theta_vals <- testthat::with_mocked_bindings(
    .adaptive_link_phase_a_theta_map = function(...) {
      stats::setNames(c(0.1, 0.2), c("a", "b"))
    },
    .adaptive_link_require_phase_a_theta_map(
      state = list(),
      set_id = 2L,
      field = "theta_raw_mean",
      required_item_ids = c("b", "a"),
      helper_name = "Linking routing"
    ),
    .package = "pairwiseLLM"
  )
  expect_identical(theta_vals, c(b = 0.2, a = 0.1))

  expect_identical(
    .adaptive_link_spoke_bins(c("s3", "s1", "s2"), c(s1 = 1, s2 = 2, s3 = 3), bins = 2L),
    c(s3 = 1L, s2 = 1L, s1 = 2L)
  )
  expect_identical(
    .adaptive_link_probe_quantile_bins(c("s1", "s2", "s3"), c(s1 = 1, s2 = 2, s3 = 3), bins = 2L),
    c(s1 = 1L, s2 = 1L, s3 = 2L)
  )
})

test_that("adaptive run summary cache helpers cover remaining invariant branches", {
  expect_identical(
    .adaptive_link_refit_summary_cache(list(refit_meta = list(
      link_refit_summary_cache_by_refit_spoke = "bad"
    ))),
    list()
  )
  expect_identical(
    .adaptive_link_unique_cross_pair_keys(list(refit_meta = list(
      link_unique_cross_pair_keys_by_spoke = "bad"
    ))),
    list()
  )

  entry <- .adaptive_link_refit_summary_empty(refit_id = 1L, spoke_id = 2L)

  bad_count <- entry
  bad_count$n_pairs_cross_set_done <- NA_integer_
  expect_error(
    .adaptive_link_refit_summary_validate(bad_count, context = "test"),
    "must be a non-negative integer"
  )

  bad_stage_realized <- entry
  bad_stage_realized$stage_realized <- list(anchor_link = -1L)
  expect_error(
    .adaptive_link_refit_summary_validate(bad_stage_realized, context = "test"),
    "must contain non-negative integer counts"
  )

  bad_window_total <- entry
  bad_window_total$n_pairs_cross_set_done <- 1L
  bad_window_total$n_pairs_cross_set_active_done <- 1L
  bad_window_total$n_unique_cross_pairs_seen <- 1L
  bad_window_total$n_cross_edges_active_since_last_refit <- 1L
  expect_error(
    .adaptive_link_refit_summary_validate(bad_window_total, context = "test"),
    "current-window total cross-edge count must equal current-window active plus probe counts"
  )

  expect_null(.adaptive_link_refit_summary_latest_for_spoke(list(), spoke_id = 2L))
  expect_null(.adaptive_link_refit_summary_latest_for_spoke(
    list(refit_meta = list(link_refit_summary_cache_by_refit_spoke = list(NULL))),
    spoke_id = 2L
  ))
  expect_null(.adaptive_link_refit_summary_latest_for_spoke(
    list(refit_meta = list(
      link_refit_summary_cache_by_refit_spoke = list(
        "1::3" = .adaptive_link_refit_summary_empty(refit_id = 1L, spoke_id = 3L)
      )
    )),
    spoke_id = 2L
  ))
  expect_null(.adaptive_link_refit_summary_latest_for_spoke(
    list(refit_meta = list(
      link_refit_summary_cache_by_refit_spoke = list(
        "2::2" = .adaptive_link_refit_summary_empty(refit_id = 2L, spoke_id = 2L)
      )
    )),
    spoke_id = 2L,
    refit_id = 1L
  ))
  expect_identical(
    .adaptive_link_refit_summary_seed_for_refit(list(), refit_id = 2L, spoke_id = 2L),
    .adaptive_link_refit_summary_empty(refit_id = 2L, spoke_id = 2L)
  )

  canonical <- .adaptive_link_refit_summary_empty(refit_id = 1L, spoke_id = 2L)
  cached_flag <- canonical
  cached_flag$probe_panel_acceleration_used_since_last_refit <- TRUE
  expect_error(
    .adaptive_link_refit_summary_compare(cached_flag, canonical, refit_id = 1L, spoke_id = 2L),
    "cached probe-acceleration flag does not match canonical"
  )

  canonical_stage <- canonical
  canonical_stage$n_cross_edges_active_since_last_refit <- 1L
  canonical_stage$n_cross_edges_total_since_last_refit <- 1L

  cached_stage <- canonical_stage
  cached_stage$stage_realized <- list(anchor_link = 1L)
  expect_error(
    .adaptive_link_refit_summary_compare(cached_stage, canonical_stage, refit_id = 1L, spoke_id = 2L),
    "cached stage-realized counts do not match canonical"
  )

  step_state <- list(
    item_ids = c("h1", "s1"),
    step_log = tibble::tibble(
      pair_id = c(11L, 12L),
      is_cross_set = c(TRUE, TRUE),
      link_spoke_id = c(2L, 2L),
      step_id = c(1L, 2L),
      A = c(1L, 1L),
      B = c(2L, 2L),
      run_mode = c("link_one_spoke", "link_probe_holdout"),
      fallback_used = c(NA_character_, "probe_panel_acceleration"),
      round_stage = c("anchor_link", "local_link")
    )
  )
  from_round_stage <- .adaptive_link_refit_summary_from_step_log(
    state = step_state,
    refit_id = 1L,
    spoke_id = 2L,
    refit_context = list(last_refit_step = 0L)
  )
  expect_identical(from_round_stage$n_pairs_cross_set_done, 2L)
  expect_identical(from_round_stage$n_pairs_cross_set_probe_done, 1L)
  expect_true(from_round_stage$probe_panel_acceleration_used_since_last_refit)
  expect_identical(from_round_stage$stage_realized[["anchor_link"]], 1L)

  expect_identical(
    .adaptive_link_refit_summary_ensure_current_entries(step_state, spoke_ids = integer()),
    step_state
  )

  mismatch_state <- list(
    item_ids = c("h1", "s1"),
    step_log = tibble::tibble(
      pair_id = 21L,
      is_cross_set = TRUE,
      link_spoke_id = 2L,
      A = 1L,
      B = 2L
    ),
    refit_meta = list(
      link_unique_cross_pair_keys_by_spoke = list(`2` = "wrong|pair")
    )
  )
  expect_error(
    .adaptive_link_refit_summary_rebuild_current(
      mismatch_state,
      current_refit_id = 1L,
      spoke_ids = 2L
    ),
    "persisted cumulative unique cross-pair keys do not match canonical"
  )
})

test_that("adaptive run probe realization helpers cover mismatch and memo branches", {
  panel <- tibble::tibble(
    probe_panel_id = "panel-a",
    link_epoch_id = 1L,
    pair_key = make_unordered_key("h1", "s1"),
    realized = FALSE,
    realized_step_id = NA_integer_,
    realized_pair_id = NA_integer_,
    realized_run_mode = NA_character_
  )
  realized <- dplyr::bind_rows(
    .adaptive_link_probe_empty_realized_log(),
    tibble::tibble(
      step_id = 3L,
      pair_id = 41L,
      run_mode = "link_probe_holdout",
      spoke_id = 2L,
      link_epoch_id = 1L,
      probe_panel_id = "panel-a",
      hub_item_id = "h1",
      spoke_item_id = "s1",
      pair_key = make_unordered_key("h1", "s1"),
      Y = 1L
    )
  )
  index <- .adaptive_link_probe_realized_index_build(realized)
  entry <- index[[1L]]
  state <- list(
    linking = list(
      probe = list(
        panels_by_spoke = list(`2` = panel),
        realized_edges = realized,
        realized_index_by_panel = index
      )
    )
  )

  bad_field <- index
  bad_field[[1L]]$probe_panel_id <- "panel-b"
  expect_error(
    .adaptive_link_probe_realized_index_compare(bad_field, index, context = "test"),
    "indexed `probe_panel_id` does not match canonical"
  )

  bad_rows <- entry
  bad_rows$spoke_id <- 99L
  expect_error(
    .adaptive_link_probe_realized_rows_from_entry(state, bad_rows),
    "stored `spoke_id`"
  )

  bad_last <- entry
  bad_last$last_realized_step_id <- 99L
  expect_error(
    .adaptive_link_probe_realized_rows_from_entry(state, bad_last),
    "last_realized_step_id"
  )

  out_of_range_state <- list(
    linking = list(
      probe = list(
        realized_edges = realized,
        realized_index_by_panel = list(
          broken = list(
            spoke_id = 2L,
            link_epoch_id = 1L,
            probe_panel_id = "panel-a",
            row_ids = 99L,
            realized_count = 1L,
            last_realized_step_id = 3L
          )
        )
      )
    )
  )
  expect_error(
    .adaptive_link_probe_realized_log_for_epoch(out_of_range_state, spoke_id = 2L, epoch_id = 1L),
    "epoch row ids are out of range"
  )

  expect_error(
    .adaptive_link_probe_panel_size(n_spoke_items = 4L, probe_panel_edges = 0L),
    "must be >= 1"
  )
  expect_error(
    .adaptive_link_probe_panel_size(n_spoke_items = 4L, probe_panel_edges = c(1L, 2L)),
    "single integer"
  )

  incompatible_state <- state
  incompatible_state$linking$probe$realized_edges$pair_key[[1L]] <- make_unordered_key("h2", "s2")
  incompatible_state$linking$probe$realized_index_by_panel <-
    .adaptive_link_probe_realized_index_build(incompatible_state$linking$probe$realized_edges)
  expect_error(
    .adaptive_link_probe_panel_for_spoke(incompatible_state, spoke_id = 2L, epoch_id = 1L),
    "canonical realized probe edges are not contained"
  )

  cache_state <- list(
    refit_meta = list(
      link_refit_summary_cache_by_refit_spoke = list(
        "1::2" = c(
          .adaptive_link_refit_summary_empty(refit_id = 1L, spoke_id = 2L),
          list(
            n_pairs_cross_set_done = 3L,
            n_pairs_cross_set_probe_done = 3L,
            n_unique_cross_pairs_seen = 3L,
            n_cross_edges_probe_since_last_refit = 3L,
            n_cross_edges_total_since_last_refit = 3L
          )
        )
      )
    ),
    step_log = tibble::tibble()
  )
  expect_identical(testthat::with_mocked_bindings(
    .adaptive_link_refit_summary_current = function(...) {
      list(n_cross_edges_probe_since_last_refit = 3L)
    },
    .adaptive_link_probe_holdout_total_since_last_refit(cache_state),
    .package = "pairwiseLLM"
  ), 3L)

  expect_null(.adaptive_link_refit_local_memo_env(list(refit_meta = list(
    link_refit_local_memo_env = list()
  ))))
  expect_identical(.adaptive_link_refit_local_step_id(list(step_log = tibble::tibble(x = 1L))), 0L)
  expect_true(is.na(.adaptive_link_refit_local_probe_panel_id(state, spoke_id = 2L, epoch_id = 9L)))

  memo <- new.env(parent = emptyenv())
  memo$keep <- list(context = list(refit_id = 1L, step_id = 2L))
  memo$drop <- list(context = list(refit_id = 1L, step_id = 1L))
  .adaptive_link_refit_local_memo_prune(memo, refit_id = 1L, step_id = 2L)
  expect_identical(ls(memo), "keep")
})

test_that("adaptive run blocker and stop helpers cover remaining fallback branches", {
  expect_identical(.adaptive_link_probe_parse_blocker_codes(NA_character_), character())

  blocker_surface <- list(
    stop_blocker_codes = "probe_edges_min_for_stop",
    link_diagnostics_pass = TRUE,
    link_lag_eligible = TRUE,
    link_min_refit_eligible = TRUE,
    reliability_link_global = 0.9,
    link_stop_reliability_min_used = 0.8,
    probe_brier = 0.1,
    probe_brier_max_used = 0.2,
    probe_pred_rmse_lagged = 0.1,
    probe_pred_rmse_max_used = 0.2,
    theta_global_rmse_lagged = 0.1,
    theta_global_rmse_max_used = 0.2,
    hub_anchored = TRUE,
    probe_edges_min_for_stop_used = 99L
  )
  expect_error(
    .adaptive_link_probe_validate_blocker_surface(
      surface_row = blocker_surface,
      realized_before_refit = 5L,
      realized_min = 30L,
      spoke_id = 2L,
      source = "controller_stats"
    ),
    "does not match the current controller threshold"
  )

  expect_identical(testthat::with_mocked_bindings(
    .adaptive_runtime_controller_resolve = function(...) list(),
    .adaptive_link_probe_budget_info_for_spoke(list(), controller = list(), spoke_id = NA_integer_),
    .package = "pairwiseLLM"
  ),
    list(
      B_spoke_refit_budget = 0L,
      B_spoke_refit_budget_source = "single_spoke_default"
    )
  )
  expect_identical(
    .adaptive_link_probe_released_cap_when_active(list(
      allow_when_active = TRUE,
      effective_cap = 0L,
      active_nonprobe_since_refit = 3L,
      active_floor_used = 2L
    )),
    0L
  )

  expect_false(testthat::with_mocked_bindings(
    .adaptive_controller_resolve = function(...) list(),
    .adaptive_link_mode_active = function(...) TRUE,
    .adaptive_link_phase_context = function(...) list(phase = "phase_b", active_spokes = integer()),
    .adaptive_link_all_spokes_stopped(list()),
    .package = "pairwiseLLM"
  ))
  expect_identical(testthat::with_mocked_bindings(
    .adaptive_runtime_controller_resolve = function(...) list(run_mode = "within_set"),
    .adaptive_link_probe_active_progress_guard(list(), controller = list()),
    .package = "pairwiseLLM"
  )$block_probes, FALSE)
  expect_false(testthat::with_mocked_bindings(
    .adaptive_controller_resolve = function(...) list(),
    .adaptive_link_mode_active = function(...) TRUE,
    .adaptive_link_phase_context = function(...) list(phase = "phase_b", pending_run_sets = 2L),
    .adaptive_global_stop_allowed(list()),
    .package = "pairwiseLLM"
  ))

  bootstrapped <- .adaptive_stop_boundary_bootstrap(list(
    meta = list(
      stop_boundary_step_id = 4L,
      pairs_committed_after_stop = -1L
    ),
    step_log = tibble::tibble(step_id = c(3L, 5L), pair_id = c(NA_integer_, 11L))
  ))
  expect_identical(bootstrapped$meta$pairs_committed_after_stop, 1L)

  budget_status <- .adaptive_stop_boundary_budget_status(
    state = list(meta = list(stop_boundary_step_id = 4L, pairs_committed_after_stop = -2L)),
    controller = list(max_pairs_after_stop = -1L)
  )
  expect_true(budget_status$active)
  expect_true(budget_status$exhausted)
  expect_identical(budget_status$max_pairs_after_stop, 0L)
  expect_identical(budget_status$pairs_after_stop, 0L)
})

test_that("adaptive round candidate domain and ordering helpers cover remaining branches", {
  fill_defaults <- adaptive_defaults(5L)
  fill_defaults$anchor_frac_total <- 0.8
  fill_defaults$anchor_top_weight <- 0
  fill_defaults$anchor_mid_weight <- 3
  fill_defaults$anchor_bottom_weight <- 0
  expect_length(
    .adaptive_select_rolling_anchors(
      scores = c(a = 5, b = 4, c = 3, d = 2, e = 1),
      defaults = fill_defaults
    ),
    4L
  )

  trim_defaults <- adaptive_defaults(5L)
  trim_defaults$anchor_frac_total <- 0.4
  trim_defaults$anchor_top_weight <- 2
  trim_defaults$anchor_mid_weight <- 3
  trim_defaults$anchor_bottom_weight <- 2
  expect_length(
    .adaptive_select_rolling_anchors(
      scores = c(a = 5, b = 4, c = 3, d = 2, e = 1),
      defaults = trim_defaults
    ),
    2L
  )

  all_top <- adaptive_defaults(3L)
  all_top$top_band_pct <- 1
  strata <- .adaptive_assign_strata(c(a = 3, b = 2, c = 1), all_top)
  expect_identical(length(strata$top_band_ids), 3L)

  expect_error(
    .adaptive_link_assert_active_domain_count(
      stage_name = "local_link",
      n_candidates_after_active_domain = 3L,
      active_hub_ids = "h1",
      spoke_ids = c("s1", "s2"),
      spoke_id = 2L
    ),
    "exceeds the maximum possible active-domain cross-set pairs"
  )

  set_map <- c(h1 = 1L, h2 = 1L, s1 = 2L, s2 = 2L)
  expect_error(
    .adaptive_link_assert_non_anchor_candidate_domain(
      candidates = tibble::tibble(i = c("h1", "s1"), j = c("s1", "s2")),
      stage_name = "local_link",
      spoke_id = 2L,
      hub_id = 1L,
      active_hub_ids = "h1",
      set_map = set_map
    ),
    "fell outside active_link_items"
  )
  expect_error(
    .adaptive_link_assert_non_anchor_candidate_domain(
      candidates = tibble::tibble(i = "h1", j = "s1"),
      stage_name = "local_link",
      spoke_id = 2L,
      hub_id = 1L,
      active_hub_ids = "h1",
      reserved_keys = make_unordered_key("h1", "s1"),
      set_map = set_map
    ),
    "reserved"
  )

  expect_error(
    .adaptive_link_direct_cross_pairs(
      hub_item_ids = "h1",
      spoke_ids = "s1",
      rank_index = c(h1 = NA_integer_, s1 = 2L),
      stratum_map = c(h1 = 1L, s1 = 1L)
    ),
    "finite routing ranks"
  )
  expect_error(
    .adaptive_link_direct_cross_pairs(
      hub_item_ids = "h1",
      spoke_ids = "s1",
      rank_index = c(h1 = 1L, s1 = 2L),
      stratum_map = c(h1 = NA_integer_, s1 = 1L)
    ),
    "finite routing strata"
  )

  expect_error(
    .adaptive_link_backfill_order(
      candidates = tibble::tibble(i = "h1", j = "s1"),
      hub_id = 1L,
      set_map = set_map,
      spoke_id = 2L
    ),
    "link_d_opt_gain"
  )
  expect_error(
    .adaptive_link_backfill_order(
      candidates = tibble::tibble(i = "h1", j = "s1", link_d_opt_gain = NA_real_),
      hub_id = 1L,
      set_map = set_map,
      spoke_id = 2L
    ),
    "all `link_d_opt_gain` values were non-finite"
  )
})

test_that("adaptive select posterior and predictive helpers cover remaining edge branches", {
  expect_identical(
    .adaptive_recent_deg(
      history = tibble::tibble(
        A_id = c("a", "a", "x"),
        B_id = c("a", "b", "b")
      ),
      ids = c("a", "b"),
      W_cap = 3L
    ),
    c(a = 1L, b = 1L)
  )

  ids <- c("a", "b")
  cache <- .adaptive_history_state_empty(ids)

  bad_pos <- cache
  bad_pos$posA <- stats::setNames(as.integer(c(0L, -1L)), ids)
  expect_error(
    .adaptive_history_state_validate(bad_pos, ids, context = "test"),
    "must be non-missing and non-negative"
  )

  bad_pair_count_type <- cache
  bad_pair_count_type$pair_count <- c("a|b" = 1)
  expect_error(
    .adaptive_history_state_validate(bad_pair_count_type, ids, context = "test"),
    "pair_count` must be an integer vector"
  )

  bad_pair_count_neg <- cache
  bad_pair_count_neg$pair_count <- stats::setNames(as.integer(-1L), "a|b")
  expect_error(
    .adaptive_history_state_validate(bad_pair_count_neg, ids, context = "test"),
    "pair_count` must be non-missing and non-negative"
  )

  bad_last_list <- cache
  bad_last_list$pair_last_order <- 1L
  expect_error(
    .adaptive_history_state_validate(bad_last_list, ids, context = "test"),
    "pair_last_order` must be a list"
  )

  bad_last_names <- cache
  bad_last_names$pair_last_order <- stats::setNames(list(c("a", "b")), "")
  expect_error(
    .adaptive_history_state_validate(bad_last_names, ids, context = "test"),
    "must use non-empty pair-key names"
  )

  expect_false(testthat::with_mocked_bindings(
    .adaptive_controller_resolve = function(...) list(run_mode = "link_one_spoke"),
    .adaptive_link_phase_context = function(...) list(phase = "phase_a", active_phase_a_set = 2L),
    .adaptive_long_link_gate_has_posterior(list(
      btl_fit = list(btl_posterior_draws = matrix(c(1, 0), nrow = 1L)),
      round_log = tibble::tibble(
        diagnostics_pass = TRUE,
        phase_scope = "phase_a_set",
        phase_scope_set_id = 99L
      )
    )),
    .package = "pairwiseLLM"
  ))

  expect_true(is.na(.adaptive_long_link_gate_posterior_prob(
    state = list(
      item_ids = c("a", "b", "c"),
      btl_fit = list(btl_posterior_draws = matrix(1:4, ncol = 2))
    ),
    i_id = "a",
    j_id = "b"
  )))

  prob <- .adaptive_long_link_gate_posterior_prob(
    state = list(
      item_ids = c("a", "b"),
      btl_fit = list(
        btl_posterior_draws = matrix(c(0.2, -0.2), ncol = 2),
        beta_draws = Inf,
        epsilon_draws = c(Inf, -Inf),
        beta_mean = 0.1,
        epsilon_mean = 0.25
      )
    ),
    i_id = "a",
    j_id = "b"
  )
  expect_true(is.finite(prob))
  probs_vec <- .adaptive_long_link_gate_posterior_prob_vec(
    state = list(
      item_ids = c("a", "b"),
      btl_fit = list(
        btl_posterior_draws = matrix(c(0.2, -0.2, 0.1, -0.1), ncol = 2, byrow = TRUE),
        beta_draws = c(0.1, -0.2),
        epsilon_draws = c(0.25, 0.10)
      )
    ),
    i_id = c("a", "a"),
    j_id = c("b", "b"),
    block_size = 1L
  )
  expect_length(probs_vec, 2L)
  expect_equal(probs_vec[[1L]], probs_vec[[2L]])
  expect_error(
    .adaptive_long_link_gate_posterior_prob_vec(
      state = list(
        item_ids = c("a", "b"),
        btl_fit = list(btl_posterior_draws = matrix(c(0.2, -0.2), ncol = 2))
      ),
      i_id = c("a", "b"),
      j_id = "b"
    ),
    "same length"
  )

  expect_identical(
    .adaptive_repeat_pair_has_order(
      unordered_key = c("a:b", "b:c", "c:d"),
      pair_count = c(0L, 1L, 2L),
      pair_last_order = list(`b:c` = c("b", "c"))
    ),
    c(TRUE, TRUE, FALSE)
  )

  theta_map <- testthat::with_mocked_bindings(
    .adaptive_link_phase_a_theta_map = function(...) c(h1 = 0.3),
    .adaptive_anchored_joint_artifact_copy_init = function(...) {
      list(theta_spoke_global_mean = c(s1 = 0.1))
    },
    .adaptive_link_theta_global_map_for_items(
      state = list(
        items = tibble::tibble(item_id = c("h1", "s1"), set_id = c(1L, 2L)),
        linking = list(anchored_joint = list())
      ),
      controller = list(link_estimation_mode = "anchored_joint", hub_id = 1L),
      item_ids = c("h1", "s1")
    ),
    .package = "pairwiseLLM"
  )
  expect_identical(theta_map, c(h1 = 0.3, s1 = 0.1))

  expect_identical(
    .adaptive_link_model_d_prob_vec(
      theta_a = c(NA_real_, 0.1),
      theta_b = c(0.2, 0.2),
      beta = Inf,
      epsilon = Inf
    ),
    c(NA_real_, stats::plogis(-0.1))
  )
  expect_true(all(is.na(.adaptive_link_model_d_pbar_vec(
    theta_h = NA_real_,
    theta_x = 0.1,
    beta = 0,
    epsilon = 0
  ))))
  expect_true(is.na(.adaptive_link_d_opt_gain_logdet_from_start(
    it = matrix(0, nrow = 1L, ncol = 1L),
    ipair = matrix(-1, nrow = 1L, ncol = 1L),
    logdet_start = NA_real_,
    ridge = 0
  )))
  expect_error(
    .adaptive_link_d_opt_matrix_dim(
      transform_mode = "shift_only",
      link_estimation_mode = "anchored_joint",
      free_block_dim = 0L
    ),
    "positive `free_block_dim`"
  )

  cand <- tibble::tibble(i = "h1", j = "s1")
  missing_theta <- testthat::with_mocked_bindings(
    .adaptive_link_theta_global_map_for_items = function(...) c(h1 = 0.2),
    .adaptive_link_attach_predictive_utility(
      candidates = cand,
      state = list(items = tibble::tibble(item_id = c("h1", "s1"), set_id = c(1L, 2L))),
      controller = list(),
      spoke_id = 2L
    ),
    .package = "pairwiseLLM"
  )
  expect_true(all(is.na(missing_theta$link_u)))

  predictive <- testthat::with_mocked_bindings(
    .adaptive_link_theta_global_map_for_items = function(...) c(h1 = 0.2, s1 = 0.1),
    .adaptive_link_phase_b_startup_gap_for_spoke = function(...) FALSE,
    .adaptive_link_judge_params = function(...) list(epsilon = Inf, beta = -Inf),
    .adaptive_link_transform_state_for_spoke = function(...) "shift_only",
    .adaptive_link_d_opt_state_get = function(...) list(it = matrix(1, nrow = 1L, ncol = 1L)),
    .adaptive_link_safe_theta_map = function(...) c(h1 = 0.2, s1 = 0.1),
    .adaptive_link_refit_window_id = function(...) 1L,
    .adaptive_link_attach_predictive_utility(
      candidates = cand,
      state = list(items = tibble::tibble(item_id = c("h1", "s1"), set_id = c(1L, 2L))),
      controller = list(
        hub_id = 1L,
        link_refit_stats_by_spoke = list(`2` = list(delta_spoke_mean = Inf))
      ),
      spoke_id = 2L
    ),
    .package = "pairwiseLLM"
  )
  expect_true(is.finite(predictive$link_p[[1L]]))
  expect_true(is.finite(predictive$link_d_opt_gain[[1L]]))

  expect_true(is.na(testthat::with_mocked_bindings(
    .adaptive_link_theta_global_map_for_items = function(...) c(h1 = 0.1),
    .adaptive_link_predictive_prob_oriented(
      state = list(),
      controller = list(),
      spoke_id = 2L,
      A_id = "h1",
      B_id = "s1"
    ),
    .package = "pairwiseLLM"
  )))
  expect_true(is.na(testthat::with_mocked_bindings(
    .adaptive_link_theta_global_map_for_items = function(...) c(h1 = NA_real_, s1 = 0.1),
    .adaptive_link_phase_b_startup_gap_for_spoke = function(...) FALSE,
    .adaptive_link_judge_params = function(...) list(epsilon = Inf, beta = Inf),
    .adaptive_link_predictive_prob_oriented(
      state = list(),
      controller = list(),
      spoke_id = 2L,
      A_id = "h1",
      B_id = "s1"
    ),
    .package = "pairwiseLLM"
  )))
})

test_that("adaptive round candidate helper guards cover remaining empty and invalid branches", {
  empty_pairs <- tibble::tibble(
    i = character(),
    j = character(),
    dist_stratum_global = integer()
  )

  expect_no_error(
    .adaptive_link_assert_active_domain_count(
      stage_name = "local_link",
      n_candidates_after_active_domain = NA_integer_,
      active_hub_ids = "h1",
      spoke_ids = c("s1", "s2"),
      spoke_id = 2L
    )
  )

  expect_error(
    .adaptive_within_set_same_group_pairs(
      item_ids = c("a", "b"),
      rank_index = c(a = 1L, b = NA_integer_),
      dist_stratum_global = 0L
    ),
    "finite ranks"
  )

  expect_identical(
    .adaptive_within_set_cross_group_pairs(
      left_ids = character(),
      right_ids = "b",
      rank_index = c(a = 1L, b = 2L),
      stratum_map = c(a = 1L, b = 2L)
    ),
    empty_pairs
  )
  expect_error(
    .adaptive_within_set_cross_group_pairs(
      left_ids = "a",
      right_ids = "b",
      rank_index = c(a = 1L, b = NA_integer_),
      stratum_map = c(a = 1L, b = 2L)
    ),
    "finite ranks"
  )
  expect_error(
    .adaptive_within_set_cross_group_pairs(
      left_ids = "a",
      right_ids = "b",
      rank_index = c(a = 1L, b = 2L),
      stratum_map = c(a = 1L, b = NA_integer_)
    ),
    "finite strata"
  )

  expect_identical(
    .adaptive_within_set_direct_pairs(
      ids = "a",
      anchor_ids = character(),
      rank_index = c(a = 1L),
      stratum_map = c(a = 1L),
      stage_name = "local_link",
      bounds = list(min = 0L, max = 0L)
    ),
    empty_pairs
  )
  expect_error(
    .adaptive_within_set_direct_pairs(
      ids = c("a", "b"),
      anchor_ids = character(),
      rank_index = c(a = 1L, b = 2L),
      stratum_map = c(a = 1L, b = NA_integer_),
      stage_name = "local_link",
      bounds = list(min = 0L, max = 1L)
    ),
    "finite strata"
  )
  expect_identical(
    .adaptive_within_set_direct_pairs(
      ids = c("a", "b"),
      anchor_ids = character(),
      rank_index = c(a = 1L, b = 2L),
      stratum_map = c(a = 1L, b = 2L),
      stage_name = "local_link",
      bounds = list(min = 3L, max = 4L)
    ),
    empty_pairs
  )
  expect_identical(
    .adaptive_within_set_direct_pairs(
      ids = c("a", "b"),
      anchor_ids = character(),
      rank_index = c(a = 1L, b = 2L),
      stratum_map = c(a = 1L, b = 2L),
      stage_name = "local_link",
      bounds = list(min = 0L, max = 0L)
    ),
    empty_pairs
  )

  expect_identical(
    .adaptive_link_backfill_order(
      candidates = tibble::tibble(),
      hub_id = 1L,
      set_map = integer(),
      spoke_id = 2L
    ),
    integer()
  )
})
