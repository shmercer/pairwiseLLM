test_that("run_one_step commits valid results transactionally", {
  items <- make_test_items(3)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)
  judge <- make_deterministic_judge("i_wins")

  before_mu <- state$trueskill_state$items$mu
  before_sigma <- state$trueskill_state$items$sigma
  withr::local_seed(1)
  out <- pairwiseLLM:::run_one_step(state, judge)

  expect_equal(nrow(out$step_log), 1L)
  expect_equal(out$step_log$status[[1L]], "ok")
  expect_equal(out$step_log$utility_mode[[1L]], "pairing_trueskill_u0")
  expect_false(is.na(out$step_log$pair_id[[1L]]))
  expect_equal(out$step_log$Y[[1L]], 1L)
  expect_false(isTRUE(all.equal(before_mu, out$trueskill_state$items$mu)))
  expect_false(isTRUE(all.equal(before_sigma, out$trueskill_state$items$sigma)))

  expect_equal(nrow(out$history_pairs), 1L)
  expect_equal(nrow(out$item_step_log), out$n_items)
})

test_that("run_one_step logs invalid results without mutating state", {
  items <- make_test_items(3)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)
  judge <- make_deterministic_judge("invalid")

  snapshot <- snapshot_state_core(state)
  withr::local_seed(1)
  out <- pairwiseLLM:::run_one_step(state, judge)

  expect_equal(nrow(out$step_log), 1L)
  expect_equal(out$step_log$status[[1L]], "invalid")
  expect_true(is.na(out$step_log$pair_id[[1L]]))
  expect_true(is.na(out$step_log$Y[[1L]]))
  expect_true(is.na(out$step_log$p_ij[[1L]]))
  expect_true(is.na(out$step_log$U0_ij[[1L]]))

  expect_equal(snapshot, snapshot_state_core(out))
})

test_that("held-out probe commits do not mutate the shared history-state cache", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21"),
    set_id = c(1L, 1L, 2L),
    global_item_id = c("gh1", "gh2", "gs21")
  )
  state <- adaptive_rank_start(
    items,
    seed = 19L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$history_pairs <- tibble::tibble(A_id = "h1", B_id = "h2")
  state$history_state <- pairwiseLLM:::.adaptive_history_state_rebuild(
    state$history_pairs,
    state$item_ids
  )
  state$linking$probe <- list(
    panels_by_spoke = list(
      `2` = tibble::tibble(
        probe_panel_id = "panel-2",
        link_epoch_id = 1L,
        spoke_id = 2L,
        hub_item_id = "h1",
        spoke_item_id = "s21",
        planned_rank = 1L,
        pair_key = pairwiseLLM:::make_unordered_key("h1", "s21"),
        realized = FALSE,
        realized_step_id = NA_integer_,
        realized_pair_id = NA_integer_,
        realized_run_mode = NA_character_
      )
    ),
    prediction_cache = pairwiseLLM:::.adaptive_link_probe_empty_cache(),
    realized_edges = pairwiseLLM:::.adaptive_link_probe_empty_realized_log(),
    realized_index_by_panel = pairwiseLLM:::.adaptive_link_probe_empty_realized_index(),
    collect_holdout_now_by_spoke = list()
  )

  before_history <- state$history_pairs
  before_cache <- state$history_state
  out <- testthat::with_mocked_bindings(
    .adaptive_link_refit_summary_update_after_commit = function(state_before, state_after, step_row) {
      state_after
    },
    pairwiseLLM:::apply_step_update(
      state,
      list(
        row = list(
          step_id = 2L,
          timestamp = as.POSIXct("2026-01-03 00:00:00", tz = "UTC"),
          pair_id = 2L,
          status = "ok",
          A = 1L,
          B = 3L,
          Y = 1L,
          set_i = 1L,
          set_j = 2L,
          is_cross_set = TRUE,
          is_probe_step = TRUE,
          run_mode = "link_probe_holdout",
          link_spoke_id = 2L
        ),
        is_valid = TRUE,
        A_id = "h1",
        B_id = "s21",
        Y = 1L
      )
    ),
    .package = "pairwiseLLM"
  )

  expect_identical(out$history_pairs, before_history)
  expect_identical(out$history_state, before_cache)
  expect_identical(nrow(out$linking$probe$realized_edges), 1L)
  expect_history_state_matches_history(out)
})

test_that("apply_step_update updates history-state from the pre-commit cache", {
  items <- make_test_items(3)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(
    items,
    trueskill_state,
    history = tibble::tibble(A_id = "1", B_id = "2")
  )
  before_history <- state$history_pairs
  before_cache <- state$history_state
  resolve_rows_seen <- NULL
  history_update_orig <- pairwiseLLM:::.adaptive_history_state_update

  out <- testthat::with_mocked_bindings(
    .adaptive_history_state_resolve = function(state, ids = NULL, validate_existing = FALSE, context = "runtime") {
      resolve_rows_seen <<- nrow(state$history_pairs)
      before_cache
    },
    .adaptive_history_state_update = function(cache, A_id, B_id) {
      expect_identical(as.integer(cache$n_pairs), as.integer(nrow(before_history)))
      history_update_orig(cache, A_id, B_id)
    },
    .adaptive_link_refit_summary_update_after_commit = function(state_before, state_after, step_row) {
      state_after
    },
    pairwiseLLM:::apply_step_update(
      state,
      list(
        row = list(
          step_id = 2L,
          timestamp = as.POSIXct("2026-01-03 00:00:00", tz = "UTC"),
          pair_id = 2L,
          status = "ok",
          A = 1L,
          B = 3L,
          Y = 1L,
          set_i = 1L,
          set_j = 1L,
          is_cross_set = FALSE,
          is_probe_step = FALSE,
          run_mode = "within_set"
        ),
        is_valid = TRUE,
        A_id = "1",
        B_id = "3",
        Y = 1L
      )
    ),
    .package = "pairwiseLLM"
  )

  expect_identical(resolve_rows_seen, nrow(before_history))
  expect_identical(nrow(out$history_pairs), nrow(before_history) + 1L)
  expect_identical(as.integer(out$history_state$n_pairs), as.integer(nrow(before_history) + 1L))
  expect_history_state_matches_history(out)
})

test_that("run_one_step enforces canonical judge contract", {
  items <- make_test_items(3)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)
  judge <- function(A, B, state, ...) list(Y = 1L)

  snapshot <- snapshot_state_core(state)
  withr::local_seed(1)
  out <- pairwiseLLM:::run_one_step(state, judge)

  expect_equal(nrow(out$step_log), 1L)
  expect_equal(out$step_log$status[[1L]], "invalid")
  expect_true(is.na(out$step_log$pair_id[[1L]]))
  expect_true(is.na(out$step_log$Y[[1L]]))
  expect_true(is.na(out$step_log$p_ij[[1L]]))
  expect_true(is.na(out$step_log$U0_ij[[1L]]))

  expect_equal(snapshot, snapshot_state_core(out))
})

test_that("run_one_step consumes warm-start pairs only on valid results", {
  items <- make_test_items(3)
  state <- adaptive_rank_start(items, seed = 42L)
  judge_ok <- make_deterministic_judge("i_wins")
  judge_bad <- make_deterministic_judge("invalid")

  first_pair <- state$warm_start_pairs[1, , drop = FALSE]
  out_bad <- pairwiseLLM:::run_one_step(state, judge_bad)
  expect_equal(out_bad$warm_start_idx, 1L)
  expect_false(out_bad$warm_start_done)

  out_ok <- pairwiseLLM:::run_one_step(out_bad, judge_ok)
  unordered <- pairwiseLLM:::make_unordered_key(
    out_ok$history_pairs$A_id[[1L]],
    out_ok$history_pairs$B_id[[1L]]
  )
  expected <- pairwiseLLM:::make_unordered_key(first_pair$i_id[[1L]], first_pair$j_id[[1L]])
  expect_equal(unordered, expected)
  expect_equal(out_ok$warm_start_idx, 2L)
})

test_that("run_one_step populates linking scaffold columns for cross-set rows", {
  items <- tibble::tibble(
    item_id = c("a", "b"),
    set_id = c(1L, 2L),
    global_item_id = c("ga", "gb")
  )
  state <- adaptive_rank_start(
    items,
    seed = 7L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      link_transform_mode = "auto",
      hub_lock_mode = "soft_lock",
      hub_lock_kappa = 0.75
    )
  )
  judge_ok <- make_deterministic_judge("i_wins")

  out <- pairwiseLLM:::run_one_step(state, judge_ok)
  row <- out$step_log[nrow(out$step_log), , drop = FALSE]

  expect_equal(sort(c(row$set_i[[1L]], row$set_j[[1L]])), c(1L, 2L))
  expect_true(isTRUE(row$is_cross_set[[1L]]))
  expect_equal(row$link_spoke_id[[1L]], 2L)
  expect_equal(row$run_mode[[1L]], "link_one_spoke")
  expect_equal(row$link_transform_policy[[1L]], "auto")
  expect_equal(row$link_transform_state[[1L]], "shift_only")
  expect_equal(row$utility_mode[[1L]], "linking_d_optimal_transform")
  expect_equal(row$hub_lock_mode[[1L]], "soft_lock")
  expect_equal(row$hub_lock_kappa[[1L]], 0.75)
  expect_false(isTRUE(row$is_probe_step[[1L]]))
  expect_false(isTRUE(row$is_holdout_probe_step[[1L]]))
  expect_false(isTRUE(row$is_drift_probe_step[[1L]]))
  expect_false(is.na(row$posterior_win_prob_pre[[1L]]))
  expect_false(is.na(row$cross_set_utility_pre[[1L]]))
})

test_that("run_one_step logs hub_lock_kappa as NA unless hub_lock_mode is soft_lock", {
  items <- tibble::tibble(
    item_id = c("a", "b"),
    set_id = c(1L, 2L),
    global_item_id = c("ga", "gb")
  )
  judge_ok <- make_deterministic_judge("i_wins")

  state_hard <- adaptive_rank_start(
    items,
    seed = 8L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      hub_lock_mode = "hard_lock",
      hub_lock_kappa = 0.75
    )
  )
  out_hard <- pairwiseLLM:::run_one_step(state_hard, judge_ok)
  row_hard <- out_hard$step_log[nrow(out_hard$step_log), , drop = FALSE]
  expect_equal(row_hard$hub_lock_mode[[1L]], "hard_lock")
  expect_true(is.na(row_hard$hub_lock_kappa[[1L]]))
})

test_that("run_one_step logs linking pre-step transform estimates when available", {
  items <- tibble::tibble(
    item_id = c("a", "b"),
    set_id = c(1L, 2L),
    global_item_id = c("ga", "gb")
  )
  state <- adaptive_rank_start(
    items,
    seed = 17L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      link_transform_mode = "auto"
    )
  )
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_state = "shift_scale",
      delta_spoke_mean = 0.12,
      delta_spoke_sd = 0.03,
      log_alpha_spoke_mean = 0.04,
      log_alpha_spoke_sd = 0.02
    )
  )
  judge_ok <- make_deterministic_judge("i_wins")

  out <- pairwiseLLM:::run_one_step(state, judge_ok)
  row <- out$step_log[nrow(out$step_log), , drop = FALSE]

  expect_equal(row$link_transform_policy[[1L]], "auto")
  expect_equal(row$link_transform_state[[1L]], "shift_scale")
  expect_equal(row$delta_spoke_estimate_pre[[1L]], 0.12, tolerance = 1e-12)
  expect_equal(row$delta_spoke_sd_pre[[1L]], 0.03, tolerance = 1e-12)
  expect_equal(row$log_alpha_spoke_estimate_pre[[1L]], 0.04, tolerance = 1e-12)
  expect_equal(row$log_alpha_spoke_sd_pre[[1L]], 0.02, tolerance = 1e-12)
})

test_that("run_one_step retires frozen spoke work without emitting a new step", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22")
  )
  state <- adaptive_rank_start(
    items,
    seed = 37L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("ready", "ready"),
      validation_message = c("ok", "ok"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(
      `1` = list(
        items = tibble::tibble(
          global_item_id = c("gh1", "gh2"),
          theta_raw_mean = c(0.2, -0.2),
          theta_raw_sd = c(0.1, 0.1),
          rank_mu_raw = c(1, 2)
        )
      ),
      `2` = list(
        items = tibble::tibble(
          global_item_id = c("gs21", "gs22"),
          theta_raw_mean = c(0.1, -0.1),
          theta_raw_sd = c(0.1, 0.1),
          rank_mu_raw = c(1, 2)
        )
      )
    ),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L),
    set_stop_pass_by_set = list(`1` = TRUE, `2` = TRUE),
    phase = "phase_b",
    ready_spokes = 2L,
    active_phase_a_set = NA_integer_,
    phase_b_started_at_step = 1L
  )
  state$controller$link_state_frozen_by_spoke <- list(`2` = TRUE)
  state$controller$link_transform_frozen_by_spoke <- list(`2` = FALSE)
  state$controller$link_transform_frozen_delta_by_spoke <- list(`2` = 0)
  state$controller$link_transform_state_by_spoke <- list(`2` = "shift_only")
  state$controller$link_stage_coverage_bins_used <- list(`2` = 3L)
  state$controller$link_stage_coverage_source <- list(`2` = "linking_global_score")
  state$controller$link_refit_stats_by_spoke <- list(`2` = list(
    link_transform_state = "shift_only",
    delta_spoke_mean = 0,
    delta_spoke_sd = 0.1
  ))

  n_before <- nrow(state$step_log)
  out <- pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins"))

  expect_identical(nrow(out$step_log), n_before)
  expect_equal(nrow(out$history_pairs), 0L)
  expect_identical(out$controller$link_stage_coverage_bins_used[["2"]], 3L)
  expect_identical(out$controller$link_stage_coverage_source[["2"]], "linking_global_score")
})

test_that("run_one_step gives active-link work precedence over held-out probes", {
  items <- tibble::tibble(
    item_id = c(paste0("h", seq_len(10L)), paste0("s2", seq_len(6L))),
    set_id = c(rep(1L, 10L), rep(2L, 6L)),
    global_item_id = c(paste0("gh", seq_len(10L)), paste0("gs2", seq_len(6L)))
  )
  state <- adaptive_rank_start(
    items,
    seed = 52L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("ready", "ready"),
      validation_message = c("ok", "ok"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(
      `1` = list(items = tibble::tibble(
        global_item_id = paste0("gh", seq_len(10L)),
        theta_raw_mean = seq(0.5, -0.4, length.out = 10L),
        theta_raw_sd = rep(0.1, 10L),
        rank_mu_raw = seq_len(10L)
      )),
      `2` = list(items = tibble::tibble(
        global_item_id = paste0("gs2", seq_len(6L)),
        theta_raw_mean = seq(0.4, -0.1, length.out = 6L),
        theta_raw_sd = rep(0.1, 6L),
        rank_mu_raw = seq_len(6L)
      ))
    ),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L),
    set_stop_pass_by_set = list(`1` = TRUE, `2` = TRUE),
    phase = "phase_b",
    ready_spokes = 2L,
    active_phase_a_set = NA_integer_,
    phase_b_started_at_step = 1L
  )
  state$refit_meta$refit_pairs_target_current <- 3L
  state$controller$refit_pairs_target <- 3L
  state$controller$probe_pairs_per_refit_per_spoke <- 1L
  state$controller$probe_edges_min_for_stop <- 3L
  state$controller$link_refit_stats_by_spoke <- list(`2` = list(
    link_identified = TRUE,
    link_stop_eligible = FALSE,
    link_epoch_id = 1L
  ))
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    pairwiseLLM:::new_link_stage_log(),
    list(
      refit_id = 1L,
      spoke_id = 2L,
      hub_id = 1L,
      link_estimation_mode = "transform",
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )
  active_selection <- pairwiseLLM:::select_next_pair(state, step_id = 1L)
  expect_false(isTRUE(active_selection$candidate_starved))

  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins")),
    .adaptive_link_probe_next_holdout_spoke = function(..., allow_when_active = FALSE) {
      if (isTRUE(allow_when_active)) {
        return(NA_integer_)
      }
      rlang::abort("probe fallback should not run when active-link work is legal")
    },
    .adaptive_link_probe_select_holdout = function(...) {
      rlang::abort("probe selection should not run when active-link work is legal")
    }
  )
  row <- out$step_log[nrow(out$step_log), , drop = FALSE]
  expect_identical(as.character(row$run_mode[[1L]]), "link_one_spoke")
  expect_false(isTRUE(row$is_probe_step[[1L]]))
  expect_false(isTRUE(row$is_holdout_probe_step[[1L]]))
  expect_identical(as.character(row$utility_mode[[1L]]), "linking_d_optimal_transform")
  expect_equal(nrow(out$history_pairs), 1L)
  expect_true(is.list(out$linking$probe$panels_by_spoke))
  expect_identical(nrow(out$linking$probe$realized_edges), 0L)
  expect_true(nrow(out$linking$probe$panels_by_spoke[["2"]]) >= 1L)
})

test_that("run_one_step preserves a legal active selection after probe acceleration opens", {
  items <- tibble::tibble(
    item_id = c(paste0("h", seq_len(4L)), paste0("s2", seq_len(4L))),
    set_id = c(rep(1L, 4L), rep(2L, 4L)),
    global_item_id = c(paste0("gh", seq_len(4L)), paste0("gs2", seq_len(4L)))
  )
  state <- adaptive_rank_start(
    items,
    seed = 521L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("ready", "ready"),
      validation_message = c("ok", "ok"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(
      `1` = list(items = tibble::tibble(
        global_item_id = paste0("gh", seq_len(4L)),
        theta_raw_mean = seq(0.4, -0.2, length.out = 4L),
        theta_raw_sd = rep(0.1, 4L),
        rank_mu_raw = seq_len(4L)
      )),
      `2` = list(items = tibble::tibble(
        global_item_id = paste0("gs2", seq_len(4L)),
        theta_raw_mean = seq(0.2, -0.4, length.out = 4L),
        theta_raw_sd = rep(0.1, 4L),
        rank_mu_raw = seq_len(4L)
      ))
    ),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L),
    set_stop_pass_by_set = list(`1` = TRUE, `2` = TRUE),
    phase = "phase_b",
    ready_spokes = 2L,
    active_phase_a_set = NA_integer_,
    phase_b_started_at_step = 1L
  )
  state$refit_meta$refit_pairs_target_current <- 4L
  state$controller$refit_pairs_target <- 4L

  active_selection <- pairwiseLLM:::select_next_pair(state, step_id = 1L)
  expect_false(isTRUE(active_selection$candidate_starved))

  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins")),
    select_next_pair = function(...) active_selection,
    .adaptive_link_probe_next_holdout_spoke = function(..., allow_when_active = FALSE) {
      if (isTRUE(allow_when_active)) {
        return(NA_integer_)
      }
      rlang::abort("probe fallback should not run when active-link work is legal")
    },
    .adaptive_link_probe_select_holdout = function(...) {
      rlang::abort("probe selection should not run while active-link work is legal")
    },
    .package = "pairwiseLLM"
  )

  row <- out$step_log[nrow(out$step_log), , drop = FALSE]
  expect_identical(as.character(row$run_mode[[1L]]), "link_one_spoke")
  expect_false(isTRUE(row$is_probe_step[[1L]]))
  expect_false(isTRUE(row$is_holdout_probe_step[[1L]]))
  expect_identical(nrow(out$linking$probe$realized_edges), 0L)
})

test_that("run_one_step can commit accelerated holdout work without prior starvation", {
  append_active_step <- function(state, step_id, A_id, B_id, spoke_id, stage_name) {
    A <- match(A_id, state$item_ids)
    B <- match(B_id, state$item_ids)
    state$step_log <- pairwiseLLM:::append_step_log(
      state$step_log,
      list(
        step_id = as.integer(step_id),
        timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + as.integer(step_id),
        pair_id = as.integer(step_id),
        i = as.integer(A),
        j = as.integer(B),
        A = as.integer(A),
        B = as.integer(B),
        Y = 1L,
        set_i = as.integer(state$set_ids[[A]]),
        set_j = as.integer(state$set_ids[[B]]),
        is_cross_set = TRUE,
        link_spoke_id = as.integer(spoke_id),
        run_mode = "link_one_spoke",
        is_probe_step = FALSE,
        candidate_starved = FALSE,
        round_stage = as.character(stage_name),
        link_stage = as.character(stage_name)
      )
    )
    state
  }

  items <- tibble::tibble(
    item_id = c(paste0("h", seq_len(4L)), paste0("s2", seq_len(4L))),
    set_id = c(rep(1L, 4L), rep(2L, 4L)),
    global_item_id = c(paste0("gh", seq_len(4L)), paste0("gs2", seq_len(4L)))
  )
  state <- adaptive_rank_start(
    items,
    seed = 523L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("ready", "ready"),
      validation_message = c("ok", "ok"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(
      `1` = list(items = tibble::tibble(
        global_item_id = paste0("gh", seq_len(4L)),
        theta_raw_mean = seq(0.4, -0.2, length.out = 4L),
        theta_raw_sd = rep(0.1, 4L),
        rank_mu_raw = seq_len(4L)
      )),
      `2` = list(items = tibble::tibble(
        global_item_id = paste0("gs2", seq_len(4L)),
        theta_raw_mean = seq(0.2, -0.4, length.out = 4L),
        theta_raw_sd = rep(0.1, 4L),
        rank_mu_raw = seq_len(4L)
      ))
    ),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L),
    set_stop_pass_by_set = list(`1` = TRUE, `2` = TRUE),
    phase = "phase_b",
    ready_spokes = 2L,
    active_phase_a_set = NA_integer_,
    phase_b_started_at_step = 1L
  )
  state$refit_meta$refit_pairs_target_current <- 4L
  state$controller$refit_pairs_target <- 4L
  state$controller$probe_pairs_per_refit_per_spoke <- 1L
  state$controller$probe_pairs_per_refit_per_spoke_bootstrap_max <- 3L
  state$controller$probe_edges_min_for_stop <- 12L
  state$controller$probe_accel_bootstrap_target <- 12L
  state$controller$probe_active_floor_min <- 2L
  state$controller$probe_active_floor_frac <- 0.5
  state$controller$probe_active_floor_requires_anchor_progress <- TRUE
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    pairwiseLLM:::new_link_stage_log(),
    list(
      refit_id = 1L,
      spoke_id = 2L,
      hub_id = 1L,
      link_estimation_mode = "transform",
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )

  state <- append_active_step(state, 1L, "h1", "s21", 2L, "anchor_link")
  state <- append_active_step(state, 2L, "h2", "s22", 2L, "long_link")
  active_selection <- pairwiseLLM:::select_next_pair(state, step_id = 3L)
  expect_false(isTRUE(active_selection$candidate_starved))
  expect_identical(as.integer(active_selection$link_spoke_id_selected), 2L)

  out1 <- testthat::with_mocked_bindings(
    pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins")),
    select_next_pair = function(...) active_selection,
    .package = "pairwiseLLM"
  )
  row1 <- out1$step_log[nrow(out1$step_log), , drop = FALSE]
  expect_identical(as.character(row1$run_mode[[1L]]), "link_probe_holdout")
  expect_true(isTRUE(row1$is_probe_step[[1L]]))
  expect_identical(as.character(row1$fallback_used[[1L]]), "probe_panel_acceleration")
  expect_identical(nrow(out1$linking$probe$realized_edges), 1L)

  out2 <- testthat::with_mocked_bindings(
    pairwiseLLM:::run_one_step(out1, make_deterministic_judge("i_wins")),
    select_next_pair = function(...) active_selection,
    .package = "pairwiseLLM"
  )
  row2 <- out2$step_log[nrow(out2$step_log), , drop = FALSE]
  expect_identical(as.character(row2$run_mode[[1L]]), "link_one_spoke")
  expect_false(isTRUE(row2$is_probe_step[[1L]]))
  expect_identical(sum(out2$step_log$run_mode %in% "link_probe_holdout", na.rm = TRUE), 1L)
})

test_that("run_one_step uses link_probe_holdout after active-link starvation", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21", "s22"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gs21", "gs22")
  )
  state <- adaptive_rank_start(
    items,
    seed = 41L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("ready", "ready"),
      validation_message = c("ok", "ok"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(
      `1` = list(items = tibble::tibble(
        global_item_id = c("gh1", "gh2"),
        theta_raw_mean = c(0.2, -0.2),
        theta_raw_sd = c(0.1, 0.1),
        rank_mu_raw = c(1, 2)
      )),
      `2` = list(items = tibble::tibble(
        global_item_id = c("gs21", "gs22"),
        theta_raw_mean = c(0.1, -0.1),
        theta_raw_sd = c(0.1, 0.1),
        rank_mu_raw = c(1, 2)
      ))
    ),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L),
    set_stop_pass_by_set = list(`1` = TRUE, `2` = TRUE),
    phase = "phase_b",
    ready_spokes = 2L,
    active_phase_a_set = NA_integer_,
    phase_b_started_at_step = 1L
  )
  state$refit_meta$refit_pairs_target_current <- 3L
  state$controller$refit_pairs_target <- 3L

  starved_selection <- list(
    i = NA_integer_,
    j = NA_integer_,
    A = NA_integer_,
    B = NA_integer_,
    is_explore_step = FALSE,
    explore_mode = NA_character_,
    explore_reason = NA_character_,
    candidate_starved = TRUE,
    fallback_used = "global_safe",
    fallback_path = "active_link",
    starvation_reason = "filtered_by_active_domain",
    round_id = as.integer(state$round$round_id %||% NA_integer_),
    round_stage = "anchor_link",
    pair_type = "anchor_link",
    explore_rate_used = NA_real_,
    local_priority_mode = NA_character_,
    long_gate_pass = NA,
    long_gate_reason = NA_character_,
    star_override_used = NA,
    star_override_reason = NA_character_,
    used_in_round_i = NA_integer_,
    used_in_round_j = NA_integer_,
    is_anchor_i = NA,
    is_anchor_j = NA,
    stratum_i = NA_integer_,
    stratum_j = NA_integer_,
    dist_stratum = NA_integer_,
    dist_stratum_global = NA_integer_,
    coverage_bins_used = NA_integer_,
    coverage_source = NA_character_,
    link_spoke_id_selected = 2L,
    stage_committed_so_far = 0L,
    stage_quota = 0L,
    n_candidates_generated = 0L,
    n_candidates_after_route_filters = 0L,
    n_candidates_after_active_domain = 0L,
    n_candidates_after_stage_filters = 0L,
    n_candidates_after_exposure_filters = 0L,
    n_candidates_after_hard_filters = 0L,
    n_candidates_after_duplicates = 0L,
    n_candidates_after_star_caps = 0L,
    n_candidates_scored = 0L,
    hard_filter_collapse_stage = NA_character_,
    deg_i = NA_integer_,
    deg_j = NA_integer_,
    recent_deg_i = NA_integer_,
    recent_deg_j = NA_integer_,
    mu_i = NA_real_,
    mu_j = NA_real_,
    sigma_i = NA_real_,
    sigma_j = NA_real_,
    p_ij = NA_real_,
    U0_ij = NA_real_,
    link_u = NA_real_,
    link_d_opt_gain = NA_real_,
    utility_mode = NA_character_,
    star_cap_rejects = 0L,
    star_cap_reject_items = 0L
  )

  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins")),
    select_next_pair = function(...) starved_selection
  )
  row <- out$step_log[nrow(out$step_log), , drop = FALSE]
  expect_identical(as.character(row$run_mode[[1L]]), "link_probe_holdout")
  expect_true(isTRUE(row$is_probe_step[[1L]]))
  expect_true(isTRUE(row$is_holdout_probe_step[[1L]]))
  expect_false(isTRUE(row$is_drift_probe_step[[1L]]))
  expect_true(is.na(row$utility_mode[[1L]]))
  expect_true(is.na(row$cross_set_utility_pre[[1L]]))
  expect_identical(as.character(row$fallback_used[[1L]]), "probe_panel_after_active_unavailable")
  expect_identical(as.character(row$fallback_path[[1L]]), "active_link>probe_panel_after_active_unavailable")
  expect_equal(nrow(out$history_pairs), 0L)
  expect_true(is.list(out$linking$probe$panels_by_spoke))
  expect_true(nrow(out$linking$probe$realized_edges) >= 1L)
  expect_true(nrow(out$linking$probe$panels_by_spoke[["2"]]) >= 1L)
})

test_that("run_one_step keeps holdout probe work within the ordinary per-refit cap", {
  items <- tibble::tibble(
    item_id = c(paste0("h", seq_len(10L)), paste0("s2", seq_len(6L))),
    set_id = c(rep(1L, 10L), rep(2L, 6L)),
    global_item_id = c(paste0("gh", seq_len(10L)), paste0("gs2", seq_len(6L)))
  )
  state <- adaptive_rank_start(
    items,
    seed = 52L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("run", "run"),
      status = c("ready", "ready"),
      validation_message = c("ok", "ok"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(
      `1` = list(items = tibble::tibble(
        global_item_id = paste0("gh", seq_len(10L)),
        theta_raw_mean = seq(0.5, -0.4, length.out = 10L),
        theta_raw_sd = rep(0.1, 10L),
        rank_mu_raw = seq_len(10L)
      )),
      `2` = list(items = tibble::tibble(
        global_item_id = paste0("gs2", seq_len(6L)),
        theta_raw_mean = seq(0.4, -0.1, length.out = 6L),
        theta_raw_sd = rep(0.1, 6L),
        rank_mu_raw = seq_len(6L)
      ))
    ),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L),
    set_stop_pass_by_set = list(`1` = TRUE, `2` = TRUE),
    phase = "phase_b",
    ready_spokes = 2L,
    active_phase_a_set = NA_integer_,
    phase_b_started_at_step = 1L
  )
  state$refit_meta$refit_pairs_target_current <- 5L
  state$controller$refit_pairs_target <- 5L
  state$controller$probe_pairs_per_refit_per_spoke <- 1L
  state$controller$probe_edges_min_for_stop <- 3L
  state$controller$link_refit_stats_by_spoke <- list(`2` = list(
    link_identified = TRUE,
    link_stop_eligible = FALSE,
    link_epoch_id = 1L
  ))
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    pairwiseLLM:::new_link_stage_log(),
    list(
      refit_id = 1L,
      spoke_id = 2L,
      hub_id = 1L,
      link_estimation_mode = "transform",
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      linking_identified = TRUE,
      link_stop_eligible = FALSE,
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )

  starved_selection <- list(
    i = NA_integer_,
    j = NA_integer_,
    A = NA_integer_,
    B = NA_integer_,
    is_explore_step = FALSE,
    explore_mode = NA_character_,
    explore_reason = NA_character_,
    candidate_starved = TRUE,
    fallback_used = "global_safe",
    fallback_path = "active_link",
    starvation_reason = "filtered_by_active_domain",
    round_id = as.integer(state$round$round_id %||% NA_integer_),
    round_stage = "anchor_link",
    pair_type = "anchor_link",
    explore_rate_used = NA_real_,
    local_priority_mode = NA_character_,
    long_gate_pass = NA,
    long_gate_reason = NA_character_,
    star_override_used = NA,
    star_override_reason = NA_character_,
    used_in_round_i = NA_integer_,
    used_in_round_j = NA_integer_,
    is_anchor_i = NA,
    is_anchor_j = NA,
    stratum_i = NA_integer_,
    stratum_j = NA_integer_,
    dist_stratum = NA_integer_,
    dist_stratum_global = NA_integer_,
    coverage_bins_used = NA_integer_,
    coverage_source = NA_character_,
    link_spoke_id_selected = 2L,
    stage_committed_so_far = 0L,
    stage_quota = 0L,
    n_candidates_generated = 0L,
    n_candidates_after_route_filters = 0L,
    n_candidates_after_active_domain = 0L,
    n_candidates_after_stage_filters = 0L,
    n_candidates_after_exposure_filters = 0L,
    n_candidates_after_hard_filters = 0L,
    n_candidates_after_duplicates = 0L,
    n_candidates_after_star_caps = 0L,
    n_candidates_scored = 0L,
    hard_filter_collapse_stage = NA_character_,
    deg_i = NA_integer_,
    deg_j = NA_integer_,
    recent_deg_i = NA_integer_,
    recent_deg_j = NA_integer_,
    mu_i = NA_real_,
    mu_j = NA_real_,
    sigma_i = NA_real_,
    sigma_j = NA_real_,
    p_ij = NA_real_,
    U0_ij = NA_real_,
    link_u = NA_real_,
    link_d_opt_gain = NA_real_,
    utility_mode = NA_character_,
    star_cap_rejects = 0L,
    star_cap_reject_items = 0L
  )

  step1 <- testthat::with_mocked_bindings(
    pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins")),
    select_next_pair = function(...) starved_selection
  )
  step2 <- pairwiseLLM:::run_one_step(step1, make_deterministic_judge("i_wins"))
  rows <- tail(step2$step_log, 2L)

  expect_identical(as.character(rows$run_mode[[1L]]), "link_probe_holdout")
  expect_true(isTRUE(rows$is_probe_step[[1L]]))
  expect_true(isTRUE(rows$is_holdout_probe_step[[1L]]))
  expect_identical(
    as.character(rows$fallback_used[[1L]]),
    "probe_panel_after_active_unavailable"
  )
  expect_false(isTRUE(rows$is_probe_step[[2L]]))
  expect_false(isTRUE(rows$is_holdout_probe_step[[2L]]))
  expect_true(isTRUE(rows$is_cross_set[[2L]]))
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_next_holdout_spoke(
      step1,
      controller = step1$controller,
      eligible_spoke_ids = 2L
    ),
    NA_integer_
  )
  expect_identical(nrow(step2$history_pairs), 1L)
  expect_identical(
    pairwiseLLM:::.adaptive_link_probe_realized_count(step2, 2L, epoch_id = 1L),
    1L
  )
})

test_that("run_one_step keeps independent multi-spoke holdout probes on the active spoke", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs31", "gs32")
  )
  state <- adaptive_rank_start(
    items,
    seed = 410L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L
    )
  )
  draws <- matrix(
    c(
      1.00, 0.80, 0.60, -0.70, -0.90, -0.20, -0.40,
      1.05, 0.85, 0.65, -0.60, -0.85, -0.15, -0.35,
      0.95, 0.75, 0.55, -0.75, -0.95, -0.30, -0.45,
      1.10, 0.90, 0.70, -0.65, -0.80, -0.10, -0.30
    ),
    nrow = 4,
    byrow = TRUE
  )
  colnames(draws) <- state$item_ids
  state$btl_fit <- make_test_btl_fit(state$item_ids, draws = draws, model_variant = "btl_e_b")
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L, 3L),
      source = c("run", "run", "run"),
      status = c("ready", "ready", "ready"),
      validation_message = c("ok", "ok", "ok"),
      artifact_path = c(NA_character_, NA_character_, NA_character_)
    ),
    artifacts = list(
      `1` = list(
        items = tibble::tibble(
          global_item_id = c("gh1", "gh2", "gh3"),
          theta_raw_mean = c(0.80, 0.40, 0.10),
          theta_raw_sd = c(0.15, 0.15, 0.15),
          rank_mu_raw = c(1L, 2L, 3L)
        )
      ),
      `2` = list(
        items = tibble::tibble(
          global_item_id = c("gs21", "gs22"),
          theta_raw_mean = c(-0.30, -0.60),
          theta_raw_sd = c(0.15, 0.15),
          rank_mu_raw = c(1L, 2L)
        )
      ),
      `3` = list(
        items = tibble::tibble(
          global_item_id = c("gs31", "gs32"),
          theta_raw_mean = c(0.20, -0.10),
          theta_raw_sd = c(0.15, 0.15),
          rank_mu_raw = c(1L, 2L)
        )
      )
    ),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L, 3L),
    set_stop_pass_by_set = list(`1` = TRUE, `2` = TRUE, `3` = TRUE),
    phase = "phase_b",
    ready_spokes = c(2L, 3L),
    active_phase_a_set = NA_integer_,
    phase_b_started_at_step = 1L
  )
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  state$controller$multi_spoke_mode <- "independent"
  state$controller$current_link_spoke_id <- 2L
  state$refit_meta$refit_pairs_target_current <- 6L
  state$controller$refit_pairs_target <- 6L
  state$controller$probe_pairs_per_refit_per_spoke <- 2L
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      delta_spoke_mean = 0.25,
      log_alpha_spoke_mean = NA_real_,
      link_epoch_id = 3L
    ),
    `3` = list(
      delta_spoke_mean = 0.05,
      log_alpha_spoke_mean = NA_real_,
      link_epoch_id = 1L
    )
  )
  state$linking$probe <- list(
    panels_by_spoke = list(
      `2` = tibble::tibble(
        probe_panel_id = "panel-2",
        link_epoch_id = 3L,
        spoke_id = 2L,
        hub_item_id = "h1",
        spoke_item_id = "s21",
        planned_rank = 1L,
        pair_key = pairwiseLLM:::make_unordered_key("h1", "s21"),
        realized = FALSE,
        realized_step_id = NA_integer_,
        realized_pair_id = NA_integer_,
        realized_run_mode = NA_character_
      ),
      `3` = tibble::tibble(
        probe_panel_id = "panel-3",
        link_epoch_id = 1L,
        spoke_id = 3L,
        hub_item_id = "h1",
        spoke_item_id = "s31",
        planned_rank = 1L,
        pair_key = pairwiseLLM:::make_unordered_key("h1", "s31"),
        realized = FALSE,
        realized_step_id = NA_integer_,
        realized_pair_id = NA_integer_,
        realized_run_mode = NA_character_
      )
    ),
    prediction_cache = pairwiseLLM:::.adaptive_link_probe_empty_cache(),
    realized_edges = pairwiseLLM:::.adaptive_link_probe_empty_realized_log(),
    collect_holdout_now_by_spoke = list()
  )
  state$controller$link_budget_refit_id <- pairwiseLLM:::.adaptive_link_refit_window_id(state)
  state$controller$link_budget_map <- list(
    `2` = list(
      B_spoke_refit_budget = 4L,
      B_spoke_refit_budget_source = "single_spoke_controller"
    ),
    `3` = list(
      B_spoke_refit_budget = 0L,
      B_spoke_refit_budget_source = "independent_inactive_spoke"
    )
  )
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    pairwiseLLM:::new_link_stage_log(),
    list(
      refit_id = 1L,
      spoke_id = 2L,
      hub_id = 1L,
      link_estimation_mode = "transform",
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    state$link_stage_log,
    list(
      refit_id = 1L,
      spoke_id = 3L,
      hub_id = 1L,
      link_estimation_mode = "transform",
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )

  starved_selection <- list(
    i = NA_integer_,
    j = NA_integer_,
    A = NA_integer_,
    B = NA_integer_,
    is_explore_step = FALSE,
    explore_mode = NA_character_,
    explore_reason = NA_character_,
    candidate_starved = TRUE,
    fallback_used = "global_safe",
    fallback_path = "active_link",
    starvation_reason = "filtered_by_active_domain",
    round_id = as.integer(state$round$round_id %||% NA_integer_),
    round_stage = "anchor_link",
    pair_type = "anchor_link",
    explore_rate_used = NA_real_,
    local_priority_mode = NA_character_,
    long_gate_pass = NA,
    long_gate_reason = NA_character_,
    star_override_used = NA,
    star_override_reason = NA_character_,
    used_in_round_i = NA_integer_,
    used_in_round_j = NA_integer_,
    is_anchor_i = NA,
    is_anchor_j = NA,
    stratum_i = NA_integer_,
    stratum_j = NA_integer_,
    dist_stratum = NA_integer_,
    dist_stratum_global = NA_integer_,
    coverage_bins_used = NA_integer_,
    coverage_source = NA_character_,
    link_spoke_id_selected = 2L,
    stage_committed_so_far = 0L,
    stage_quota = 0L,
    n_candidates_generated = 0L,
    n_candidates_after_route_filters = 0L,
    n_candidates_after_active_domain = 0L,
    n_candidates_after_stage_filters = 0L,
    n_candidates_after_exposure_filters = 0L,
    n_candidates_after_hard_filters = 0L,
    n_candidates_after_duplicates = 0L,
    n_candidates_after_star_caps = 0L,
    n_candidates_scored = 0L,
    hard_filter_collapse_stage = NA_character_,
    deg_i = NA_integer_,
    deg_j = NA_integer_,
    recent_deg_i = NA_integer_,
    recent_deg_j = NA_integer_,
    mu_i = NA_real_,
    mu_j = NA_real_,
    sigma_i = NA_real_,
    sigma_j = NA_real_,
    p_ij = NA_real_,
    U0_ij = NA_real_,
    link_u = NA_real_,
    link_d_opt_gain = NA_real_,
    utility_mode = NA_character_,
    star_cap_rejects = 0L,
    star_cap_reject_items = 0L
  )

  out <- testthat::with_mocked_bindings(
    .adaptive_link_probe_effort_plan = function(state, controller, spoke_id) {
      if (identical(as.integer(spoke_id), 2L)) {
        list(
          realized_total = 0L,
          realized_refit = 0L,
          effective_cap = 2L,
          remaining_to_min_start = 30L,
          acceleration_used = FALSE
        )
      } else {
        list(
          realized_total = 29L,
          realized_refit = 0L,
          effective_cap = 2L,
          remaining_to_min_start = 1L,
          acceleration_used = FALSE
        )
      }
    },
    select_next_pair = function(...) starved_selection,
    pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins")),
    .package = "pairwiseLLM"
  )

  row <- out$step_log[nrow(out$step_log), , drop = FALSE]
  expect_identical(as.character(row$run_mode[[1L]]), "link_probe_holdout")
  expect_identical(as.integer(row$link_spoke_id[[1L]]), 2L)
  expect_true(isTRUE(row$is_probe_step[[1L]]))
  expect_identical(as.integer(out$controller$current_link_spoke_id), 2L)
  expect_identical(as.integer(out$linking$probe$realized_edges$spoke_id[[1L]]), 2L)
})

test_that("run_one_step keeps accelerated concurrent holdout routing on the active spoke", {
  append_active_step <- function(state, step_id, A_id, B_id, spoke_id, stage_name) {
    A <- match(A_id, state$item_ids)
    B <- match(B_id, state$item_ids)
    state$step_log <- pairwiseLLM:::append_step_log(
      state$step_log,
      list(
        step_id = as.integer(step_id),
        timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + as.integer(step_id),
        pair_id = as.integer(step_id),
        i = as.integer(A),
        j = as.integer(B),
        A = as.integer(A),
        B = as.integer(B),
        Y = 1L,
        set_i = as.integer(state$set_ids[[A]]),
        set_j = as.integer(state$set_ids[[B]]),
        is_cross_set = TRUE,
        link_spoke_id = as.integer(spoke_id),
        run_mode = "link_multi_spoke",
        is_probe_step = FALSE,
        candidate_starved = FALSE,
        round_stage = as.character(stage_name),
        link_stage = as.character(stage_name)
      )
    )
    state
  }

  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs31", "gs32")
  )
  state <- adaptive_rank_start(
    items,
    seed = 524L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      multi_spoke_mode = "concurrent"
    )
  )
  state$warm_start_done <- TRUE
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L, 3L),
      source = c("run", "run", "run"),
      status = c("ready", "ready", "ready"),
      validation_message = c("ok", "ok", "ok"),
      artifact_path = c(NA_character_, NA_character_, NA_character_)
    ),
    artifacts = list(
      `1` = list(items = tibble::tibble(
        global_item_id = c("gh1", "gh2", "gh3"),
        theta_raw_mean = c(0.8, 0.4, 0.1),
        theta_raw_sd = c(0.1, 0.1, 0.1),
        rank_mu_raw = 1:3
      )),
      `2` = list(items = tibble::tibble(
        global_item_id = c("gs21", "gs22"),
        theta_raw_mean = c(-0.2, -0.5),
        theta_raw_sd = c(0.1, 0.1),
        rank_mu_raw = 1:2
      )),
      `3` = list(items = tibble::tibble(
        global_item_id = c("gs31", "gs32"),
        theta_raw_mean = c(0.2, -0.1),
        theta_raw_sd = c(0.1, 0.1),
        rank_mu_raw = 1:2
      ))
    ),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L, 3L),
    set_stop_pass_by_set = list(`1` = TRUE, `2` = TRUE, `3` = TRUE),
    phase = "phase_b",
    ready_spokes = c(2L, 3L),
    active_phase_a_set = NA_integer_,
    phase_b_started_at_step = 1L
  )
  state$refit_meta$refit_pairs_target_current <- 8L
  state$controller$refit_pairs_target <- 8L
  state$controller$probe_pairs_per_refit_per_spoke <- 1L
  state$controller$probe_pairs_per_refit_per_spoke_bootstrap_max <- 3L
  state$controller$probe_edges_min_for_stop <- 12L
  state$controller$probe_accel_bootstrap_target <- 12L
  state$controller$probe_active_floor_min <- 2L
  state$controller$probe_active_floor_frac <- 0.5
  state$controller$probe_active_floor_requires_anchor_progress <- TRUE
  state$controller$link_budget_refit_id <- pairwiseLLM:::.adaptive_link_refit_window_id(state)
  state$controller$link_budget_map <- list(
    `2` = list(
      B_spoke_refit_budget = 4L,
      B_spoke_refit_budget_source = "concurrent_allocator"
    ),
    `3` = list(
      B_spoke_refit_budget = 4L,
      B_spoke_refit_budget_source = "concurrent_allocator"
    )
  )
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    pairwiseLLM:::new_link_stage_log(),
    list(
      refit_id = 1L,
      spoke_id = 2L,
      hub_id = 1L,
      link_estimation_mode = "transform",
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )
  state$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    state$link_stage_log,
    list(
      refit_id = 1L,
      spoke_id = 3L,
      hub_id = 1L,
      link_estimation_mode = "transform",
      link_transform_policy = "auto",
      link_transform_state = "shift_only",
      link_stop_pass = FALSE,
      link_state_frozen = FALSE
    )
  )
  state <- append_active_step(state, 1L, "h1", "s21", 2L, "anchor_link")
  state <- append_active_step(state, 2L, "h2", "s22", 2L, "long_link")

  active_selection <- pairwiseLLM:::select_next_pair(state, step_id = 3L)
  active_selection$link_spoke_id_selected <- 2L
  expect_false(isTRUE(active_selection$candidate_starved))

  out <- testthat::with_mocked_bindings(
    pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins")),
    select_next_pair = function(...) active_selection,
    .package = "pairwiseLLM"
  )

  row <- out$step_log[nrow(out$step_log), , drop = FALSE]
  expect_identical(as.character(row$run_mode[[1L]]), "link_probe_holdout")
  expect_identical(as.integer(row$link_spoke_id[[1L]]), 2L)
  expect_identical(as.integer(out$linking$probe$realized_edges$spoke_id[[1L]]), 2L)
})

test_that("invalid linking step does not mutate controller link routing state", {
  items <- tibble::tibble(
    item_id = c("a", "b"),
    set_id = c(1L, 2L),
    global_item_id = c("ga", "gb")
  )
  state <- adaptive_rank_start(
    items,
    seed = 8L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L
    )
  )
  state$controller$current_link_spoke_id <- 99L
  state$controller$link_stage_coverage_bins_used <- list(`99` = 3L)
  state$controller$link_stage_coverage_source <- list(`99` = "seed")
  judge_bad <- make_deterministic_judge("invalid")

  out <- pairwiseLLM:::run_one_step(state, judge_bad)

  expect_equal(out$step_log$status[[1L]], "invalid")
  expect_identical(out$controller$current_link_spoke_id, 99L)
  expect_identical(out$controller$link_stage_coverage_bins_used, list(`99` = 3L))
  expect_identical(out$controller$link_stage_coverage_source, list(`99` = "seed"))
})

test_that("run_one_step handles starved selections with NA linking endpoints", {
  items <- make_test_items(2)
  state <- adaptive_rank_start(items, seed = 2L)
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  judge_ok <- make_deterministic_judge("i_wins")

  out <- state
  for (idx in seq_len(6L)) {
    out <- pairwiseLLM:::run_one_step(out, judge_ok)
    if (identical(utils::tail(out$step_log$status, 1L), "starved")) {
      break
    }
  }

  row <- out$step_log[nrow(out$step_log), , drop = FALSE]
  expect_equal(row$status[[1L]], "starved")
  expect_true(is.na(row$set_i[[1L]]))
  expect_true(is.na(row$set_j[[1L]]))
  expect_true(is.na(row$is_cross_set[[1L]]))
  expect_true(is.na(row$link_spoke_id[[1L]]))
})

test_that("run_one_step uses selected spoke fallback for non-hub cross-set rows", {
  items <- tibble::tibble(
    item_id = c("h1", "s21", "s31"),
    set_id = c(1L, 2L, 3L),
    global_item_id = c("gh1", "gs21", "gs31")
  )
  state <- adaptive_rank_start(
    items,
    seed = 13L,
    adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$linking$phase_a$phase <- "phase_b"
  state$linking$phase_a$ready_for_phase_b <- TRUE
  state$linking$phase_a$set_status <- tibble::tibble(
    set_id = c(1L, 2L, 3L),
    source = c("run", "run", "run"),
    status = c("ready", "ready", "ready"),
    validation_message = c("ok", "ok", "ok"),
    artifact_path = c(NA_character_, NA_character_, NA_character_)
  )
  state$linking$phase_a$artifacts <- list(
    `1` = list(items = tibble::tibble(
      global_item_id = "gh1",
      theta_raw_mean = 0,
      theta_raw_sd = 0.1,
      rank_mu_raw = 1
    )),
    `2` = list(items = tibble::tibble(
      global_item_id = "gs21",
      theta_raw_mean = 0,
      theta_raw_sd = 0.1,
      rank_mu_raw = 1
    )),
    `3` = list(items = tibble::tibble(
      global_item_id = "gs31",
      theta_raw_mean = 0,
      theta_raw_sd = 0.1,
      rank_mu_raw = 1
    ))
  )
  state$controller$probe_edges_min_for_stop <- 0L
  state$controller$link_refit_stats_by_spoke <- list(`2` = list(), `3` = list())

  judge <- make_deterministic_judge("i_wins")
  mocked <- testthat::with_mocked_bindings(
    select_next_pair = function(state, step_id = NULL, candidates = NULL) {
      list(
        i = 2L,
        j = 3L,
        A = 2L,
        B = 3L,
        p_ij = 0.5,
        U0_ij = 0.25,
        link_u = 0.25,
        link_d_opt_gain = 0.2,
        utility_mode = "linking_d_optimal_transform",
        run_mode = "link_multi_spoke",
        link_spoke_id_selected = 2L,
        long_gate_pass = NA,
        long_gate_reason = NA_character_,
        star_override_used = FALSE,
        star_override_reason = NA_character_,
        is_explore_step = FALSE,
        explore_mode = NA_character_,
        explore_reason = NA_character_,
        explore_rate_used = 0,
        local_priority_mode = NA_character_,
        candidate_starved = FALSE,
        fallback_used = "base",
        fallback_path = "base",
        starvation_reason = NA_character_,
        round_id = 1L,
        round_stage = "anchor_link",
        pair_type = "anchor_link",
        used_in_round_i = 0L,
        used_in_round_j = 0L,
        is_anchor_i = FALSE,
        is_anchor_j = FALSE,
        stratum_i = 1L,
        stratum_j = 1L,
        dist_stratum = 0L,
        dist_stratum_global = 0L,
        stage_committed_so_far = 0L,
        stage_quota = 1L,
        n_candidates_generated = 1L,
        n_candidates_after_hard_filters = 1L,
        n_candidates_after_duplicates = 1L,
        n_candidates_after_star_caps = 1L,
        n_candidates_scored = 1L,
        deg_i = 0L,
        deg_j = 0L,
        recent_deg_i = 0L,
        recent_deg_j = 0L,
        mu_i = 0,
        mu_j = 0,
        sigma_i = 1,
        sigma_j = 1,
        star_cap_rejects = 0L,
        star_cap_reject_items = 0L
      )
    },
    pairwiseLLM:::run_one_step(state, judge),
    .package = "pairwiseLLM"
  )

  row <- mocked$step_log[nrow(mocked$step_log), , drop = FALSE]
  expect_true(isTRUE(row$is_cross_set[[1L]]))
  expect_identical(row$link_spoke_id[[1L]], 2L)
})
