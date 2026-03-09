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
  expect_equal(row$utility_mode[[1L]], "linking_d_optimal")
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

test_that("run_one_step logs probe rows without linking d-opt utility fields", {
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
  state$controller$link_transform_frozen_by_spoke <- list(`2` = TRUE)
  state$controller$link_transform_frozen_delta_by_spoke <- list(`2` = 0)
  state$controller$link_transform_state_by_spoke <- list(`2` = "shift_only")
  state$controller$link_refit_stats_by_spoke <- list(`2` = list(
    link_transform_state = "shift_only",
    delta_spoke_mean = 0,
    delta_spoke_sd = 0.1
  ))

  out <- pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins"))
  row <- out$step_log[nrow(out$step_log), , drop = FALSE]

  expect_identical(as.character(row$run_mode[[1L]]), "link_probe")
  expect_true(isTRUE(row$is_probe_step[[1L]]))
  expect_false(isTRUE(row$is_holdout_probe_step[[1L]]))
  expect_true(isTRUE(row$is_drift_probe_step[[1L]]))
  expect_true(is.na(row$utility_mode[[1L]]))
  expect_true(is.na(row$cross_set_utility_pre[[1L]]))
  expect_equal(nrow(out$history_pairs), 0L)
})

test_that("run_one_step uses link_probe_holdout for planned phase_b probe pairs", {
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
  out <- pairwiseLLM:::run_one_step(state, make_deterministic_judge("i_wins"))
  row <- out$step_log[nrow(out$step_log), , drop = FALSE]
  expect_identical(as.character(row$run_mode[[1L]]), "link_probe_holdout")
  expect_true(isTRUE(row$is_probe_step[[1L]]))
  expect_true(isTRUE(row$is_holdout_probe_step[[1L]]))
  expect_false(isTRUE(row$is_drift_probe_step[[1L]]))
  expect_true(is.na(row$utility_mode[[1L]]))
  expect_true(is.na(row$cross_set_utility_pre[[1L]]))
  expect_equal(nrow(out$history_pairs), 0L)
  expect_true(is.list(out$linking$probe$panels_by_spoke))
  expect_true(nrow(out$linking$probe$realized_edges) >= 1L)
  expect_true(nrow(out$linking$probe$panels_by_spoke[["2"]]) >= 1L)
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
        utility_mode = "linking_d_optimal",
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
