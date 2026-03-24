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

  trigger <- testthat::with_mocked_bindings(
    .adaptive_link_probe_validate_blocker_surface = function(...) "probe_edges_min_for_stop",
    .adaptive_link_probe_sole_blocker_trigger(
      surface_row = list(stop_blocker_codes = "probe_edges_min_for_stop"),
      surface_source = "controller_stats",
      controller = list(
        probe_sole_blocker_acceleration_enabled = TRUE,
        probe_sole_blocker_min_realized = 20L
      ),
      spoke_id = 2L,
      realized_before_refit = 20L,
      realized_min = 30L,
      panel_shortfall_start = 1L
    ),
    .package = "pairwiseLLM"
  )
  expect_true(trigger)
  expect_false(.adaptive_link_probe_sole_blocker_trigger(
    surface_row = list(),
    surface_source = "controller_stats",
    controller = list(
      probe_sole_blocker_acceleration_enabled = FALSE,
      probe_sole_blocker_min_realized = 20L
    ),
    spoke_id = 2L,
    realized_before_refit = 20L,
    realized_min = 30L,
    panel_shortfall_start = 1L
  ))
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
})

test_that("adaptive select history update and utility helpers cover remaining branches", {
  ids <- c("a", "b", "c")
  cache <- .adaptive_history_state_empty(ids)
  updated <- .adaptive_history_state_update(cache, "a", "b")
  expect_identical(updated$n_pairs, 1L)
  expect_identical(updated$deg[c("a", "b", "c")], c(a = 1L, b = 1L, c = 0L))
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
