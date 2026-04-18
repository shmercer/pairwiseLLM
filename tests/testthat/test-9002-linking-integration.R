make_linking_items_two_set_small <- function() {
  tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs23")
  )
}

make_linking_items_two_set <- function() {
  hub_ids <- paste0("h", seq_len(10L))
  spoke_ids <- paste0("s2", seq_len(6L))
  tibble::tibble(
    item_id = c(hub_ids, spoke_ids),
    set_id = c(rep(1L, length(hub_ids)), rep(2L, length(spoke_ids))),
    global_item_id = c(paste0("g", hub_ids), paste0("g", spoke_ids))
  )
}

make_linking_items_three_set <- function() {
  hub_ids <- paste0("h", seq_len(10L))
  spoke2_ids <- paste0("s2", seq_len(6L))
  spoke3_ids <- paste0("s3", seq_len(6L))
  tibble::tibble(
    item_id = c(hub_ids, spoke2_ids, spoke3_ids),
    set_id = c(
      rep(1L, length(hub_ids)),
      rep(2L, length(spoke2_ids)),
      rep(3L, length(spoke3_ids))
    ),
    global_item_id = c(
      paste0("g", hub_ids),
      paste0("g", spoke2_ids),
      paste0("g", spoke3_ids)
    )
  )
}

make_phase_a_import_artifacts <- function(state, spoke_shift = -1.5) {
  ids <- as.character(state$item_ids)
  draws <- matrix(
    seq_along(ids),
    nrow = 4,
    ncol = length(ids),
    byrow = TRUE
  )
  colnames(draws) <- ids
  state$btl_fit <- make_test_btl_fit(ids, draws = draws, model_variant = "btl_e_b")

  set_ids <- sort(unique(as.integer(state$items$set_id)))
  artifacts <- lapply(set_ids, function(set_id) {
    art <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, set_id = as.integer(set_id))
    if (!identical(as.integer(set_id), 1L)) {
      art$items$theta_raw_mean <- as.double(art$items$theta_raw_mean + spoke_shift)
    }
    art$quality_gate_accepted <- TRUE
    art
  })
  names(artifacts) <- as.character(set_ids)
  artifacts
}

make_score_judge <- function(scores) {
  score_names <- names(scores)
  scores <- as.double(scores)
  names(scores) <- score_names
  default_score <- function(item_id) {
    item_id <- as.character(item_id)
    if (grepl("^h\\d+$", item_id)) {
      rank <- as.integer(sub("^h", "", item_id))
      return(-1.0 + (0.16 * rank))
    }
    if (grepl("^s\\d\\d+$", item_id)) {
      set_id <- as.integer(substr(item_id, 2L, 2L))
      rank <- as.integer(sub("^s\\d", "", item_id))
      return((0.1 * set_id) + (0.22 * rank))
    }
    0
  }
  function(A, B, state, ...) {
    a <- as.character(A$item_id[[1L]])
    b <- as.character(B$item_id[[1L]])
    a_score <- scores[a]
    b_score <- scores[b]
    a_score <- if (!is.na(a_score)) as.double(a_score) else default_score(a)
    b_score <- if (!is.na(b_score)) as.double(b_score) else default_score(b)
    y <- as.integer(a_score >= b_score)
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }
}

test_that("two-set linking recovers spoke offset from cross-set outcomes", {
  withr::local_seed(20260213)

  items <- make_linking_items_two_set()
  state <- adaptive_rank_start(items, seed = 1L)
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts <- make_phase_a_import_artifacts(state, spoke_shift = -2)
  fit_stub <- make_deterministic_fit_fn(as.character(state$item_ids))

  judge <- make_score_judge(c(
    h1 = -0.6, h2 = 0.1, h3 = 0.8,
    s21 = -0.2, s22 = 0.4, s23 = 1.0
  ))

  out <- adaptive_rank_run_live(
    state = state,
    judge = judge,
    n_steps = 18L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      link_estimation_mode = "transform",
      phase_a_mode = "import",
      phase_a_artifacts = artifacts
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 3L)),
    progress = "none"
  )

  expect_true(nrow(out$link_stage_log) >= 1L)
  rows <- out$link_stage_log[out$link_stage_log$spoke_id == 2L, , drop = FALSE]
  expect_true(nrow(rows) >= 1L)
  expect_true(is.finite(rows$delta_spoke_mean[[nrow(rows)]]))
  expect_true(rows$delta_spoke_mean[[nrow(rows)]] > -6)
  expect_true(all(c(
    "feasible_stage_capacity_anchor_link",
    "feasible_stage_capacity_long_link",
    "feasibility_budget_released",
    "probe_brier_max_used",
    "theta_global_rmse_pass"
  ) %in% names(rows)))
  expect_true(all(rows$link_fit_method == "cmdstan_hmc"))
  expect_true(all(rows$link_uncertainty_approximation == "cmdstan_posterior_draws"))
})

test_that("joint_refit integration records joint mode and soft-lock runtime fields", {
  withr::local_seed(20260213)

  items <- make_linking_items_two_set()
  state <- adaptive_rank_start(items, seed = 11L)
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts <- make_phase_a_import_artifacts(state, spoke_shift = -1.2)
  fit_stub <- make_deterministic_fit_fn(as.character(state$item_ids))
  judge <- make_score_judge(c(
    h1 = -0.5, h2 = 0.1, h3 = 0.7,
    s21 = -0.2, s22 = 0.3, s23 = 0.9
  ))

  out <- adaptive_rank_run_live(
    state = state,
    judge = judge,
    n_steps = 18L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      link_estimation_mode = "transform",
      link_refit_mode = "joint_refit",
      hub_lock_mode = "soft_lock",
      hub_lock_kappa = 0.75,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 2L)),
    progress = "none"
  )

  expect_true(nrow(out$link_stage_log) >= 1L)
  rows <- out$link_stage_log[out$link_stage_log$spoke_id == 2L, , drop = FALSE]
  expect_true(nrow(rows) >= 1L)
  expect_true(all(rows$link_refit_mode == "joint_refit"))
  expect_true(all(rows$hub_lock_mode == "soft_lock"))
  expect_true(is.finite(rows$delta_spoke_mean[[nrow(rows)]]))

  contract <- out$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  expect_true(isTRUE(contract$joint_refit$used))
  expect_true(all(c("theta_hub", "theta_spoke", "delta_s") %in% contract$parameters))
})

test_that("joint_refit integration supports free hub lock", {
  withr::local_seed(20260316)

  items <- make_linking_items_two_set()
  state <- adaptive_rank_start(items, seed = 12L)
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts <- make_phase_a_import_artifacts(state, spoke_shift = -1.2)
  fit_stub <- make_deterministic_fit_fn(as.character(state$item_ids))
  judge <- make_score_judge(c(
    h1 = -0.5, h2 = 0.1, h3 = 0.7,
    s21 = -0.2, s22 = 0.3, s23 = 0.9
  ))

  out <- adaptive_rank_run_live(
    state = state,
    judge = judge,
    n_steps = 18L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      link_estimation_mode = "transform",
      link_refit_mode = "joint_refit",
      hub_lock_mode = "free",
      phase_a_mode = "import",
      phase_a_artifacts = artifacts
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 2L)),
    progress = "none"
  )

  rows <- out$link_stage_log[out$link_stage_log$spoke_id == 2L, , drop = FALSE]
  expect_true(nrow(rows) >= 1L)
  expect_true(all(rows$link_refit_mode == "joint_refit"))
  expect_true(all(rows$hub_lock_mode == "free"))
  expect_false(any(rows$hub_anchored %in% TRUE))
  expect_false(any(rows$link_stop_pass %in% TRUE))

  contract <- out$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  expect_true(isTRUE(contract$joint_refit$used))
  expect_identical(contract$lock$hub_lock_mode, "free")
  expect_true(is.na(contract$lock$hub_lock_kappa))
  expect_true(contract$joint_refit$n_hub_items_estimated >= 1L)
})

test_that("three-set linking stays hub-spoke only and authorizes one independent spoke per refit", {
  withr::local_seed(20260213)

  items <- make_linking_items_three_set()
  state <- adaptive_rank_start(items, seed = 3L)
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts <- make_phase_a_import_artifacts(state, spoke_shift = -1)
  fit_stub <- make_deterministic_fit_fn(as.character(state$item_ids))

  judge <- make_score_judge(c(
    h1 = -0.5, h2 = 0.0, h3 = 0.7,
    s21 = -0.2, s22 = 0.3, s23 = 1.1,
    s31 = -0.4, s32 = 0.2, s33 = 0.9
  ))

  out <- adaptive_rank_run_live(
    state = state,
    judge = judge,
    n_steps = 30L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      multi_spoke_mode = "independent",
      phase_a_mode = "import",
      phase_a_artifacts = artifacts
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
    progress = "none"
  )

  committed <- out$step_log[!is.na(out$step_log$pair_id) & out$step_log$is_cross_set %in% TRUE, , drop = FALSE]
  expect_true(nrow(committed) > 0L)

  is_hub_i <- committed$set_i == 1L
  is_hub_j <- committed$set_j == 1L
  expect_true(all(xor(is_hub_i, is_hub_j)))

  link_rows <- out$link_stage_log
  expect_true(any(link_rows$spoke_id == 2L))
  expect_true(any(link_rows$spoke_id == 3L))
  positive_budget_counts <- tapply(
    link_rows$B_spoke_refit_budget > 0L,
    link_rows$refit_id,
    sum
  )
  expect_true(all(as.integer(positive_budget_counts) == 1L))
  zero_budget_sources <- unique(as.character(
    link_rows$B_spoke_refit_budget_source[link_rows$B_spoke_refit_budget == 0L]
  ))
  expect_true("independent_inactive_spoke" %in% zero_budget_sources)
})

test_that("phase_a_mode=run finalizes artifacts in-run before cross-set linking", {
  withr::local_seed(20260213)

  items <- make_linking_items_two_set()
  state <- adaptive_rank_start(items, seed = 21L)
  fit_stub <- make_deterministic_fit_fn(as.character(state$item_ids))
  judge <- make_score_judge(c(
    h1 = -0.4, h2 = 0.1, h3 = 0.8,
    s21 = -0.3, s22 = 0.2, s23 = 0.9
  ))

  out <- adaptive_rank_run_live(
    state = state,
    judge = judge,
    n_steps = 8L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "run"
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
    progress = "none"
  )

  expect_true(length(out$linking$phase_a$artifacts) >= 2L)
  expect_true(isTRUE(out$linking$phase_a$ready_for_phase_b))
  expect_true(any(out$step_log$is_cross_set %in% TRUE))
  first_cross <- which(out$step_log$is_cross_set %in% TRUE)[[1L]]
  phase_a_rows <- out$step_log[seq_len(max(1L, first_cross - 1L)), , drop = FALSE]
  phase_a_rows <- phase_a_rows[!is.na(phase_a_rows$pair_id), , drop = FALSE]
  expect_true(nrow(phase_a_rows) >= 1L)
  expect_true(all(phase_a_rows$is_cross_set %in% FALSE))
})

test_that("public Phase B probe controls change HubEligible and preserve planned targets", {
  withr::local_seed(20260316)

  items <- make_linking_items_two_set()
  state_anchor <- adaptive_rank_start(items, seed = 71L)
  state_anchor$warm_start_done <- TRUE
  state_anchor$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts_anchor <- make_phase_a_import_artifacts(state_anchor, spoke_shift = -1.3)
  fit_anchor <- make_deterministic_fit_fn(as.character(state_anchor$item_ids))
  judge <- make_score_judge(c(
    h1 = -0.5, h2 = 0.1, h3 = 0.8,
    s21 = -0.2, s22 = 0.4, s23 = 1.0
  ))

  out_anchor <- adaptive_rank_run_live(
    state = state_anchor,
    judge = judge,
    n_steps = 1L,
    fit_fn = fit_anchor$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts_anchor,
      probe_panel_edges = 60L,
      hub_anchor_required_phase_b = TRUE
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 5L)),
    progress = "none"
  )

  panel_anchor <- out_anchor$linking$probe$panels_by_spoke[["2"]]
  routing_scores <- pairwiseLLM:::.adaptive_link_phase_b_routing_scores(
    state = out_anchor,
    controller = out_anchor$controller,
    active_ids = as.character(out_anchor$item_ids),
    hub_id = 1L
  )
  hub_ids <- as.character(out_anchor$items$item_id[out_anchor$items$set_id == 1L])
  hub_anchors <- pairwiseLLM:::.adaptive_link_phase_b_hub_anchors(
    state = out_anchor,
    hub_ids = hub_ids,
    hub_scores = routing_scores,
    defaults = pairwiseLLM:::adaptive_defaults(out_anchor$n_items)
  )

  expect_identical(unique(as.integer(panel_anchor$probe_edges_planned)), 60L)
  expect_true(nrow(panel_anchor) < 60L)
  expect_setequal(unique(as.character(panel_anchor$hub_item_id)), as.character(hub_anchors))
  anchor_step <- out_anchor$step_log[nrow(out_anchor$step_log), , drop = FALSE]
  expect_identical(as.character(anchor_step$run_mode[[1L]]), "link_probe_holdout")
  expect_true(isTRUE(anchor_step$is_probe_step[[1L]]))

  state_full_hub <- adaptive_rank_start(items, seed = 71L)
  state_full_hub$warm_start_done <- TRUE
  state_full_hub$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts_full_hub <- make_phase_a_import_artifacts(state_full_hub, spoke_shift = -1.3)
  fit_full_hub <- make_deterministic_fit_fn(as.character(state_full_hub$item_ids))

  out_full_hub <- adaptive_rank_run_live(
    state = state_full_hub,
    judge = judge,
    n_steps = 1L,
    fit_fn = fit_full_hub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts_full_hub,
      probe_panel_edges = 60L,
      hub_anchor_required_phase_b = FALSE
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 5L)),
    progress = "none"
  )

  panel_full_hub <- out_full_hub$linking$probe$panels_by_spoke[["2"]]
  expect_identical(unique(as.integer(panel_full_hub$probe_edges_planned)), 60L)
  expect_identical(nrow(panel_full_hub), 60L)
  expect_setequal(unique(as.character(panel_full_hub$hub_item_id)), hub_ids)
  full_hub_step <- out_full_hub$step_log[nrow(out_full_hub$step_log), , drop = FALSE]
  expect_identical(as.character(full_hub_step$run_mode[[1L]]), "link_probe_holdout")
  expect_true(isTRUE(full_hub_step$is_probe_step[[1L]]))
})

test_that("linking run keeps warm-start during Phase A and bypasses warm-start in Phase B", {
  withr::local_seed(20260213)

  items <- make_linking_items_two_set()
  state <- adaptive_rank_start(items, seed = 24L)
  fit_stub <- make_deterministic_fit_fn(as.character(state$item_ids))
  judge <- make_score_judge(c(
    h1 = -0.5, h2 = 0.1, h3 = 0.7,
    s21 = -0.4, s22 = 0.2, s23 = 0.9
  ))

  out <- adaptive_rank_run_live(
    state = state,
    judge = judge,
    n_steps = 20L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "run"
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
    progress = "none"
  )

  expect_true(any(out$step_log$is_cross_set %in% TRUE))
  first_cross <- which(out$step_log$is_cross_set %in% TRUE)[[1L]]
  phase_a_rows <- out$step_log[seq_len(max(1L, first_cross - 1L)), , drop = FALSE]
  phase_a_rows <- phase_a_rows[!is.na(phase_a_rows$pair_id), , drop = FALSE]
  expect_true(any(phase_a_rows$round_stage == "warm_start"))

  phase_b_rows <- out$step_log[first_cross:nrow(out$step_log), , drop = FALSE]
  phase_b_rows <- phase_b_rows[!is.na(phase_b_rows$pair_id), , drop = FALSE]
  expect_true(nrow(phase_b_rows) >= 1L)
  expect_false(any(phase_b_rows$round_stage == "warm_start"))
})

test_that("non-linking runs preserve warm-start behavior", {
  state <- adaptive_rank_start(make_linking_items_two_set_small(), seed = 31L)
  judge <- make_deterministic_judge("i_wins")

  out <- adaptive_rank_run_live(
    state = state,
    judge = judge,
    n_steps = 1L,
    adaptive_config = list(run_mode = "within_set"),
    progress = "none"
  )

  expect_equal(out$step_log$round_stage[[1L]], "warm_start")
})

test_that("mixed run/import mode combines imported and in-run artifacts by set", {
  withr::local_seed(20260213)

  items <- make_linking_items_two_set()
  state <- adaptive_rank_start(items, seed = 22L)
  import_artifacts <- make_phase_a_import_artifacts(state, spoke_shift = -1)
  fit_stub <- make_deterministic_fit_fn(as.character(state$item_ids))
  judge <- make_score_judge(c(
    h1 = -0.2, h2 = 0.2, h3 = 0.7,
    s21 = -0.4, s22 = 0.3, s23 = 1.1
  ))

  out <- adaptive_rank_run_live(
    state = state,
    judge = judge,
    n_steps = 8L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "mixed",
      phase_a_set_source = c(`1` = "import", `2` = "run"),
      phase_a_artifacts = list(`1` = import_artifacts[["1"]])
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
    progress = "none"
  )

  status <- tibble::as_tibble(out$linking$phase_a$set_status)
  expect_equal(status$source[match(1L, status$set_id)], "import")
  expect_equal(status$source[match(2L, status$set_id)], "run")
  expect_true(all(status$status == "ready"))
  expect_true(all(c("1", "2") %in% names(out$linking$phase_a$artifacts)))
})

test_that("independent and concurrent multi-spoke modes both execute and log mode-specific fields", {
  withr::local_seed(20260213)

  items <- make_linking_items_three_set()
  state_ind <- adaptive_rank_start(items, seed = 9L)
  state_ind$warm_start_done <- TRUE
  state_ind$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts_ind <- make_phase_a_import_artifacts(state_ind, spoke_shift = -1)
  fit_ind <- make_deterministic_fit_fn(as.character(state_ind$item_ids))

  judge <- make_score_judge(c(
    h1 = -0.7, h2 = 0.0, h3 = 0.9,
    s21 = -0.1, s22 = 0.5, s23 = 1.2,
    s31 = -0.3, s32 = 0.2, s33 = 1.0
  ))

  out_ind <- adaptive_rank_run_live(
    state = state_ind,
    judge = judge,
    n_steps = 24L,
    fit_fn = fit_ind$fit_fn,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      multi_spoke_mode = "independent",
      phase_a_mode = "import",
      phase_a_artifacts = artifacts_ind
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
    progress = "none"
  )

  stats_ind <- out_ind$controller$link_refit_stats_by_spoke
  expect_true(length(stats_ind) >= 2L)
  expect_false(any(vapply(stats_ind, function(x) !is.null(x$concurrent_target_pairs), logical(1L))))

  state_con <- adaptive_rank_start(items, seed = 9L)
  state_con$warm_start_done <- TRUE
  state_con$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts_con <- make_phase_a_import_artifacts(state_con, spoke_shift = -1)
  fit_con <- make_deterministic_fit_fn(as.character(state_con$item_ids))

  out_con <- adaptive_rank_run_live(
    state = state_con,
    judge = judge,
    n_steps = 24L,
    fit_fn = fit_con$fit_fn,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      multi_spoke_mode = "concurrent",
      hub_lock_mode = "soft_lock",
      min_cross_set_pairs_per_spoke_per_refit = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts_con
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
    progress = "none"
  )

  stats_con <- out_con$controller$link_refit_stats_by_spoke
  expect_true(length(stats_con) >= 2L)
  expect_true(all(vapply(stats_con, function(x) !is.null(x$concurrent_target_pairs), logical(1L))))
  expect_true(all(c(
    "probe_acceleration_mode_used",
    "probe_active_floor_used",
    "probe_only_blocker_trigger",
    "probe_acceleration_used",
    "probe_effort_base_cap",
    "probe_effort_effective_cap",
    "probe_remaining_to_min_start"
  ) %in% names(out_con$link_stage_log)))
  active_budget_rows <- out_con$link_stage_log[
    out_con$link_stage_log$B_spoke_refit_budget > 0L,
    ,
    drop = FALSE
  ]
  expect_true(nrow(active_budget_rows) >= 1L)
  expect_true(all(as.integer(active_budget_rows$n_cross_edges_active_since_last_refit) >= 1L))
  probe_audit_rows <- out_con$link_stage_log[
    !is.na(out_con$link_stage_log$probe_effort_base_cap) &
      !is.na(out_con$link_stage_log$probe_effort_effective_cap),
    ,
    drop = FALSE
  ]
  expect_true(nrow(probe_audit_rows) >= 1L)
  expect_false(any(probe_audit_rows$probe_acceleration_used %in% TRUE))
  expect_true(all(
    as.integer(probe_audit_rows$probe_effort_effective_cap) ==
      as.integer(probe_audit_rows$probe_effort_base_cap)
  ))
  expect_true(all(
    as.integer(probe_audit_rows$probe_remaining_to_min_start) >= 0L
  ))
  expect_true(all(
    as.logical(probe_audit_rows$probe_acceleration_used) ==
      (as.integer(probe_audit_rows$probe_effort_effective_cap) >
        as.integer(probe_audit_rows$probe_effort_base_cap))
  ))
  expect_true(all(
    as.integer(probe_audit_rows$n_cross_edges_probe_since_last_refit) <=
      as.integer(probe_audit_rows$probe_effort_effective_cap)
  ))

  committed_con <- out_con$step_log[
    !is.na(out_con$step_log$pair_id) & out_con$step_log$is_cross_set %in% TRUE,
    ,
    drop = FALSE
  ]
  expect_true(nrow(committed_con) > 0L)
  is_hub_i <- committed_con$set_i == 1L
  is_hub_j <- committed_con$set_j == 1L
  expect_true(all(xor(is_hub_i, is_hub_j)))
})

test_that("live concurrent budget candidate counts reconcile to the canonical feasibility summary", {
  withr::local_seed(20260407)

  items <- make_linking_items_three_set()
  state <- adaptive_rank_start(items, seed = 17L)
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts <- make_phase_a_import_artifacts(state, spoke_shift = -1)
  fit_stub <- make_deterministic_fit_fn(as.character(state$item_ids))
  judge <- make_score_judge(c(
    h1 = -0.7, h2 = 0.0, h3 = 0.9,
    s21 = -0.1, s22 = 0.5, s23 = 1.2,
    s31 = -0.3, s32 = 0.2, s33 = 1.0
  ))

  out <- adaptive_rank_run_live(
    state = state,
    judge = judge,
    n_steps = 1L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      multi_spoke_mode = "concurrent",
      min_cross_set_pairs_per_spoke_per_refit = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 3L)),
    progress = "none"
  )

  controller <- utils::modifyList(out$controller, list(
    link_budget_refit_id = NA_integer_,
    link_budget_map = list()
  ))
  budget_map <- pairwiseLLM:::.adaptive_link_budget_map_for_refit(
    state = out,
    controller = controller,
    eligible_spoke_ids = c(2L, 3L),
    seed = 1L
  )
  expect_true(all(c("2", "3") %in% names(budget_map)))

  defaults <- adaptive_defaults(as.integer(out$n_items))
  for (spoke_id in c(2L, 3L)) {
    summary <- pairwiseLLM:::.adaptive_link_stage_feasibility_snapshot(
      state = out,
      controller = controller,
      spoke_id = spoke_id,
      stage_order = pairwiseLLM:::.adaptive_stage_order(),
      C_max = defaults$C_max,
      seed_base = as.integer(1L + spoke_id),
      seed_stride = 1L
    )
    expect_identical(
      as.integer(budget_map[[as.character(spoke_id)]]$concurrent_candidate_count),
      as.integer(summary$candidate_count)
    )
  }
})

test_that("live concurrent Phase B can commit accelerated holdout work without prior starvation", {
  withr::local_seed(20260320)

  items <- make_linking_items_three_set()
  state <- adaptive_rank_start(items, seed = 19L)
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts <- make_phase_a_import_artifacts(state, spoke_shift = -1)
  fit_stub <- make_deterministic_fit_fn(as.character(state$item_ids))
  judge <- make_score_judge(c(
    h1 = -0.6, h2 = 0.0, h3 = 0.6,
    s21 = -0.3, s22 = 0.2, s23 = 1.0,
    s31 = -0.4, s32 = 0.1, s33 = 0.9
  ))

  out <- adaptive_rank_run_live(
    state = state,
    judge = judge,
    n_steps = 24L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      multi_spoke_mode = "concurrent",
      min_cross_set_pairs_per_spoke_per_refit = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts,
      probe_pairs_per_refit_per_spoke = 1L,
      probe_pairs_per_refit_per_spoke_bootstrap_max = 3L,
      probe_edges_min_for_stop = 12L,
      probe_accel_bootstrap_target = 12L,
      probe_active_floor_min = 1L,
      probe_active_floor_frac = 0,
      probe_active_floor_requires_anchor_progress = FALSE
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 4L)),
    progress = "none"
  )

  accelerated_rows <- out$step_log[
    out$step_log$run_mode %in% "link_probe_holdout" &
      out$step_log$fallback_used %in% "probe_panel_acceleration",
    ,
    drop = FALSE
  ]
  expect_true(nrow(accelerated_rows) >= 1L)
  expect_false(any(accelerated_rows$candidate_starved %in% TRUE))

  later_active_rows <- out$step_log[
    out$step_log$step_id > accelerated_rows$step_id[[1L]] &
      out$step_log$run_mode %in% "link_multi_spoke" &
      out$step_log$is_probe_step %in% FALSE,
    ,
    drop = FALSE
  ]
  expect_true(nrow(later_active_rows) >= 1L)
})

test_that("linking starvation paths in tiny domains are logged with fallback metadata", {
  withr::local_seed(20260213)

  items <- tibble::tibble(
    item_id = c("h1", "s21"),
    set_id = c(1L, 2L),
    global_item_id = c("gh1", "gs21")
  )
  state <- adaptive_rank_start(items, seed = 4L)
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts <- make_phase_a_import_artifacts(state, spoke_shift = -1)
  fit_stub <- make_deterministic_fit_fn(as.character(state$item_ids))
  judge <- make_score_judge(c(h1 = 0, s21 = 1))

  out <- adaptive_rank_run_live(
    state = state,
    judge = judge,
    n_steps = 12L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
    progress = "none"
  )

  starved <- out$step_log[out$step_log$status == "starved", , drop = FALSE]
  expect_true(nrow(starved) >= 1L)
  expect_true(any(!is.na(starved$fallback_path)))
  if (!is.na(out$meta$stop_reason)) {
    expect_true(out$meta$stop_reason %in% c("candidate_starvation", "all_spokes_exhausted", "btl_converged"))
  }
})

test_that("round_log and link_stage_log canonically reconcile probe and active work", {
  withr::local_seed(20260213)

  items <- make_linking_items_three_set()
  state <- adaptive_rank_start(items, seed = 5L)
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts <- make_phase_a_import_artifacts(state, spoke_shift = -1)
  fit_stub <- make_deterministic_fit_fn(as.character(state$item_ids))
  judge <- make_score_judge(c(
    h1 = -0.5, h2 = 0.0, h3 = 0.7,
    s21 = -0.2, s22 = 0.3, s23 = 1.1,
    s31 = -0.4, s32 = 0.2, s33 = 0.9
  ))

  out <- adaptive_rank_run_live(
    state = state,
    judge = judge,
    n_steps = 24L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      multi_spoke_mode = "concurrent",
      min_cross_set_pairs_per_spoke_per_refit = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
    progress = "none"
  )

  round_log <- out$round_log
  step_log <- out$step_log
  link_stage_log <- out$link_stage_log
  expect_true(all(c(
    "new_active_pairs_since_last_refit",
    "new_probe_pairs_since_last_refit",
    "new_total_cross_pairs_since_last_refit"
  ) %in% names(round_log)))
  expect_true(all(c(
    "probe_edges_realized_before_refit",
    "probe_edges_realized_delta_since_last_refit",
    "probe_shortfall_reason"
  ) %in% names(link_stage_log)))

  phase_b_rounds <- round_log[!is.na(round_log$new_total_cross_pairs_since_last_refit), , drop = FALSE]
  expect_true(nrow(phase_b_rounds) >= 1L)

  for (idx in seq_len(nrow(phase_b_rounds))) {
    refit_id <- as.integer(phase_b_rounds$refit_id[[idx]])
    step_hi <- as.integer(phase_b_rounds$step_id_at_refit[[idx]])
    step_lo <- if (idx == 1L) {
      0L
    } else {
      as.integer(phase_b_rounds$step_id_at_refit[[idx - 1L]])
    }
    committed_window <- step_log[
      as.integer(step_log$step_id) > step_lo &
        as.integer(step_log$step_id) <= step_hi &
        !is.na(step_log$pair_id),
      ,
      drop = FALSE
    ]
    link_rows <- link_stage_log[as.integer(link_stage_log$refit_id) == refit_id, , drop = FALSE]
    expect_true(nrow(link_rows) >= 1L)
    expect_identical(
      as.integer(phase_b_rounds$new_pairs_since_last_refit[[idx]]),
      as.integer(nrow(committed_window))
    )
    expect_identical(
      as.integer(phase_b_rounds$new_active_pairs_since_last_refit[[idx]]),
      as.integer(sum(link_rows$n_cross_edges_active_since_last_refit, na.rm = TRUE))
    )
    expect_identical(
      as.integer(phase_b_rounds$new_probe_pairs_since_last_refit[[idx]]),
      as.integer(sum(link_rows$n_cross_edges_probe_since_last_refit, na.rm = TRUE))
    )
    expect_identical(
      as.integer(phase_b_rounds$new_total_cross_pairs_since_last_refit[[idx]]),
      as.integer(sum(link_rows$n_cross_edges_total_since_last_refit, na.rm = TRUE))
    )
    active_window <- committed_window[
      !(committed_window$is_probe_step %in% TRUE),
      ,
      drop = FALSE
    ]
    stage_col <- if ("link_stage" %in% names(active_window)) {
      "link_stage"
    } else {
      "round_stage"
    }
    for (stage_name in pairwiseLLM:::.adaptive_stage_order()) {
      raw_stage_count <- as.integer(sum(as.character(active_window[[stage_col]]) == stage_name, na.rm = TRUE))
      realized_col <- paste0("stage_realized_", stage_name)
      expect_identical(
        as.integer(sum(link_rows[[realized_col]], na.rm = TRUE)),
        raw_stage_count
      )
    }
    expect_identical(
      as.integer(phase_b_rounds$total_pairs_done[[idx]]),
      as.integer(sum(!is.na(step_log$pair_id[seq_len(step_hi)])))
    )
  }

  expect_true(all(
    as.integer(link_stage_log$probe_edges_realized_before_refit) +
      as.integer(link_stage_log$probe_edges_realized_delta_since_last_refit) ==
      as.integer(link_stage_log$probe_edges_realized)
  ))
  latest_probe_key_idx <- integer()
  if (nrow(link_stage_log) > 0L) {
    probe_keys <- vapply(
      seq_len(nrow(link_stage_log)),
      function(idx) {
        panel_id <- as.character(link_stage_log$probe_panel_id[[idx]])
        epoch_id <- as.integer(link_stage_log$link_epoch_id[[idx]])
        spoke_id <- as.integer(link_stage_log$spoke_id[[idx]])
        realized_count <- as.integer(link_stage_log$probe_edges_realized[[idx]])
        if (is.na(panel_id) || !nzchar(panel_id) || !is.finite(epoch_id) ||
          !is.finite(spoke_id) || !is.finite(realized_count) || realized_count < 1L) {
          return(NA_character_)
        }
        paste(spoke_id, epoch_id, panel_id, sep = "::")
      },
      character(1L)
    )
    probe_keys_ok <- !is.na(probe_keys)
    latest_probe_key_idx <- vapply(
      split(seq_len(nrow(link_stage_log))[probe_keys_ok], probe_keys[probe_keys_ok]),
      max,
      integer(1L)
    )
  }
  for (idx in sort(as.integer(latest_probe_key_idx))) {
    entry <- pairwiseLLM:::.adaptive_link_probe_realized_index_entry_get(
      state = out,
      spoke_id = as.integer(link_stage_log$spoke_id[[idx]]),
      epoch_id = as.integer(link_stage_log$link_epoch_id[[idx]]),
      probe_panel_id = as.character(link_stage_log$probe_panel_id[[idx]])
    )
    expect_false(is.null(entry))
    expect_identical(
      as.integer(entry$realized_count),
      as.integer(link_stage_log$probe_edges_realized[[idx]])
    )
  }
  expect_true(all(
    ifelse(
      as.integer(link_stage_log$probe_panel_shortfall) > 0L &
        !is.na(as.character(link_stage_log$lag_domain_reset_reason)) &
        as.character(link_stage_log$lag_domain_reset_reason) == "probe_panel_rebuild",
      as.character(link_stage_log$probe_shortfall_reason) == "probe_panel_rebuild",
      TRUE
    )
  ))
})

test_that("judge parameter mode mismatch rejects incompatible imported Phase A artifacts", {
  withr::local_seed(20260213)

  items <- make_linking_items_two_set()
  state <- adaptive_rank_start(items, seed = 7L)
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts <- make_phase_a_import_artifacts(state, spoke_shift = -1)
  judge <- make_score_judge(c(
    h1 = -0.5, h2 = 0.1, h3 = 0.7,
    s21 = -0.2, s22 = 0.4, s23 = 0.9
  ))

  expect_error(
    adaptive_rank_run_live(
      state = state,
      judge = judge,
      n_steps = 1L,
      adaptive_config = list(
        run_mode = "link_one_spoke",
        hub_id = 1L,
        judge_param_mode = "phase_specific",
        phase_a_mode = "import",
        phase_a_artifacts = artifacts
      ),
      progress = "none"
    ),
    "within-set fit incompatibility"
  )
})

test_that("single-set runs remain behaviorally equivalent when linking controls are present but inactive", {
  withr::local_seed(20260213)

  items <- make_test_items(7)
  judge <- make_deterministic_judge("i_wins")
  state_a <- adaptive_rank_start(items, seed = 77L)
  state_b <- adaptive_rank_start(items, seed = 77L)
  fit_a <- make_deterministic_fit_fn(as.character(state_a$item_ids))
  fit_b <- make_deterministic_fit_fn(as.character(state_b$item_ids))

  withr::local_seed(77)
  out_a <- adaptive_rank_run_live(
    state = state_a,
    judge = judge,
    n_steps = 10L,
    fit_fn = fit_a$fit_fn,
    btl_config = test_link_btl_config(list(refit_pairs_target = 5L)),
    progress = "none"
  )

  withr::local_seed(77)
  out_b <- adaptive_rank_run_live(
    state = state_b,
    judge = judge,
    n_steps = 10L,
    fit_fn = fit_b$fit_fn,
    adaptive_config = list(
      run_mode = "within_set",
      hub_id = 1L,
      link_estimation_mode = "transform",
      link_transform_mode = "auto",
      link_refit_mode = "shift_only",
      phase_a_mode = "run"
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 5L)),
    progress = "none"
  )

  cols <- c("status", "i", "j", "A", "B", "pair_id", "round_stage")
  expect_equal(out_a$step_log[, cols, drop = FALSE], out_b$step_log[, cols, drop = FALSE])
})

test_that("independent mode ignores concurrent allocation controls under seeded runs", {
  withr::local_seed(20260213)

  items <- make_linking_items_three_set()
  state_base <- adaptive_rank_start(items, seed = 31L)
  state_base$warm_start_done <- TRUE
  state_base$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts_base <- make_phase_a_import_artifacts(state_base, spoke_shift = -1)
  fit_base <- make_deterministic_fit_fn(as.character(state_base$item_ids))

  judge <- make_score_judge(c(
    h1 = -0.7, h2 = 0.0, h3 = 0.9,
    s21 = -0.1, s22 = 0.5, s23 = 1.2,
    s31 = -0.3, s32 = 0.2, s33 = 1.0
  ))

  withr::local_seed(31)
  out_base <- adaptive_rank_run_live(
    state = state_base,
    judge = judge,
    n_steps = 24L,
    fit_fn = fit_base$fit_fn,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      multi_spoke_mode = "independent",
      phase_a_mode = "import",
      phase_a_artifacts = artifacts_base
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
    progress = "none"
  )

  state_tuned <- adaptive_rank_start(items, seed = 31L)
  state_tuned$warm_start_done <- TRUE
  state_tuned$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts_tuned <- make_phase_a_import_artifacts(state_tuned, spoke_shift = -1)
  fit_tuned <- make_deterministic_fit_fn(as.character(state_tuned$item_ids))

  withr::local_seed(31)
  out_tuned <- adaptive_rank_run_live(
    state = state_tuned,
    judge = judge,
    n_steps = 24L,
    fit_fn = fit_tuned$fit_fn,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      multi_spoke_mode = "independent",
      min_cross_set_pairs_per_spoke_per_refit = 50L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts_tuned
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
    progress = "none"
  )

  cols <- c("status", "i", "j", "A", "B", "pair_id", "round_stage", "link_spoke_id")
  expect_equal(out_base$step_log[, cols, drop = FALSE], out_tuned$step_log[, cols, drop = FALSE])
})

test_that("linking run stops via all-spokes-stopped gate when every spoke is stopped", {
  withr::local_seed(20260213)

  items <- make_linking_items_two_set()
  state <- adaptive_rank_start(
    items,
    seed = 33L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts <- make_phase_a_import_artifacts(state, spoke_shift = -1)
  judge <- make_score_judge(c(
    h1 = -0.5, h2 = 0.0, h3 = 0.7,
    s21 = -0.2, s22 = 0.3, s23 = 1.1
  ))

  out_init <- adaptive_rank_run_live(
    state = state,
    judge = judge,
    n_steps = 1L,
    session_dir = withr::local_tempdir(),
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts
    ),
    progress = "none"
  )
  out_init$controller$link_stopped_by_spoke <- list(`2` = TRUE)
  out_init$controller$probe_pairs_per_refit_per_spoke <- 2L
  out_init$linking$phase_a$phase <- "phase_b"
  out_init$linking$phase_a$ready_for_phase_b <- TRUE
  out_init$linking$phase_a$strict_ready_for_phase_b <- TRUE
  out_init$linking$phase_a$ready_spokes <- 2L

  n_before <- nrow(out_init$step_log)
  out <- adaptive_rank_run_live(
    state = out_init,
    judge = judge,
    n_steps = 5L,
    adaptive_config = list(max_pairs_after_stop = 3L),
    progress = "none"
  )

  expect_identical(out$meta$stop_reason, "all_spokes_stopped")
  expect_identical(nrow(out$step_log), n_before)
})

test_that("phase_b aborts when required sets are ready but strict phase_a stop-pass is missing", {
  withr::local_seed(20260217)

  items <- make_linking_items_two_set()
  state <- adaptive_rank_start(items, seed = 41L)
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts <- make_phase_a_import_artifacts(state, spoke_shift = -1.1)
  for (nm in names(artifacts)) {
    artifacts[[nm]]$quality_gate_accepted <- FALSE
    artifacts[[nm]]$diagnostics$diagnostics_pass <- FALSE
  }

  expect_error(
    adaptive_rank_run_live(
      state = state,
      judge = make_deterministic_judge("i_wins"),
      n_steps = 3L,
      fit_fn = make_deterministic_fit_fn(as.character(state$item_ids))$fit_fn,
      adaptive_config = list(
        run_mode = "link_one_spoke",
        hub_id = 1L,
        phase_a_mode = "import",
        phase_a_artifacts = artifacts
      ),
      btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
      progress = "none"
    ),
    "Phase B linking cannot start"
  )
})

test_that("anchored-joint linking run records accepted-state refits and NA transform fields", {
  withr::local_seed(20260315)

  items <- make_linking_items_two_set()
  state <- adaptive_rank_start(items, seed = 51L)
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts <- make_phase_a_import_artifacts(state, spoke_shift = -1.4)
  fit_stub <- make_deterministic_fit_fn(as.character(state$item_ids))
  judge <- make_score_judge(c(
    h1 = -0.5, h2 = 0.1, h3 = 0.8,
    s21 = -0.1, s22 = 0.4, s23 = 1.0
  ))

  out <- adaptive_rank_run_live(
    state = state,
    judge = judge,
    n_steps = 16L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts,
      link_estimation_mode = "anchored_joint",
      hub_lock_mode = "hard_lock"
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
    progress = "none"
  )

  rows <- out$link_stage_log[out$link_stage_log$spoke_id == 2L, , drop = FALSE]
  expect_true(nrow(rows) >= 1L)
  expect_true(all(as.character(rows$link_estimation_mode) == "anchored_joint"))
  expect_true(all(is.na(rows$link_transform_policy)))
  expect_true(all(is.na(rows$link_transform_state)))
  expect_true(all(is.na(rows$link_refit_mode)))
  expect_true(all(as.character(rows$hub_lock_mode) == "hard_lock"))
  expect_true(all(as.character(rows$link_fit_method) == "map_laplace"))

  accepted <- out$linking$anchored_joint$accepted_state_by_spoke[["2"]]
  expect_false(is.null(accepted))
  expect_true(accepted$anchored_joint_init_state_method %in% c("phase_b_refit", "phase_a_only_init_refit"))
  expect_true(all(is.finite(accepted$theta_spoke_global_mean)))
})

test_that("concurrent anchored-joint linking stays spoke-separable and keeps escalation disabled", {
  withr::local_seed(20260315)

  items <- make_linking_items_three_set()
  state <- adaptive_rank_start(items, seed = 61L)
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts <- make_phase_a_import_artifacts(state, spoke_shift = -1.2)
  fit_stub <- make_deterministic_fit_fn(as.character(state$item_ids))
  judge <- make_score_judge(c(
    h1 = -0.6, h2 = 0.0, h3 = 0.9,
    s21 = -0.2, s22 = 0.4, s23 = 1.0,
    s31 = -0.3, s32 = 0.2, s33 = 0.8
  ))

  out <- adaptive_rank_run_live(
    state = state,
    judge = judge,
    n_steps = 24L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      multi_spoke_mode = "concurrent",
      min_cross_set_pairs_per_spoke_per_refit = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts,
      link_estimation_mode = "anchored_joint",
      hub_lock_mode = "hard_lock"
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
    progress = "none"
  )

  rows <- out$link_stage_log[out$link_stage_log$spoke_id %in% c(2L, 3L), , drop = FALSE]
  expect_true(nrow(rows) >= 2L)
  expect_true(all(as.character(rows$link_estimation_mode) == "anchored_joint"))
  expect_true(all(is.na(rows$link_transform_policy)))
  expect_true(all(is.na(rows$link_transform_state)))
  expect_true(all(is.na(rows$link_refit_mode)))
  expect_true(all(as.character(rows$hub_lock_mode) == "hard_lock"))
  expect_true(all(is.na(rows$alternative_fit_method)))
  expect_true(all(is.na(rows$escalation_recent_pass_count)))
  expect_true(all(rows$alt_eval_converged %in% FALSE))
  expect_true(all(rows$escalated_this_refit %in% FALSE))

  committed <- out$step_log[!is.na(out$step_log$pair_id) & out$step_log$is_cross_set %in% TRUE, , drop = FALSE]
  expect_true(nrow(committed) >= 1L)
  expect_true(all(xor(committed$set_i == 1L, committed$set_j == 1L)))

  accepted_2 <- out$linking$anchored_joint$accepted_state_by_spoke[["2"]]
  accepted_3 <- out$linking$anchored_joint$accepted_state_by_spoke[["3"]]
  expect_false(is.null(accepted_2))
  expect_false(is.null(accepted_3))
  expect_setequal(names(accepted_2$theta_spoke_global_mean), paste0("s2", seq_len(6L)))
  expect_setequal(names(accepted_3$theta_spoke_global_mean), paste0("s3", seq_len(6L)))
  expect_false(any(names(accepted_2$theta_spoke_global_mean) %in% paste0("s3", seq_len(6L))))
  expect_false(any(names(accepted_3$theta_spoke_global_mean) %in% paste0("s2", seq_len(6L))))
})
