make_linking_items_two_set <- function() {
  tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs23")
  )
}

make_linking_items_three_set <- function() {
  tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23", "s31", "s32", "s33"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs23", "gs31", "gs32", "gs33")
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
  function(A, B, state, ...) {
    a <- as.character(A$item_id[[1L]])
    b <- as.character(B$item_id[[1L]])
    y <- as.integer(scores[[a]] >= scores[[b]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }
}

test_that("two-set linking recovers positive spoke offset from cross-set outcomes", {
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
      phase_a_mode = "import",
      phase_a_artifacts = artifacts
    ),
    btl_config = list(refit_pairs_target = 3L),
    progress = "none"
  )

  expect_true(nrow(out$link_stage_log) >= 1L)
  rows <- out$link_stage_log[out$link_stage_log$spoke_id == 2L, , drop = FALSE]
  expect_true(nrow(rows) >= 1L)
  expect_true(is.finite(rows$delta_spoke_mean[[nrow(rows)]]))
  expect_true(rows$delta_spoke_mean[[nrow(rows)]] > 0)
})

test_that("three-set linking remains hub-spoke only and rotates across spokes", {
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
    btl_config = list(refit_pairs_target = 1L),
    progress = "none"
  )

  committed <- out$step_log[!is.na(out$step_log$pair_id) & out$step_log$is_cross_set %in% TRUE, , drop = FALSE]
  expect_true(nrow(committed) > 0L)
  expect_true(all(sort(unique(committed$link_spoke_id)) == c(2L, 3L)))

  is_hub_i <- committed$set_i == 1L
  is_hub_j <- committed$set_j == 1L
  expect_true(all(xor(is_hub_i, is_hub_j)))
  expect_true(any(committed$set_i == 2L | committed$set_j == 2L))
  expect_true(any(committed$set_i == 3L | committed$set_j == 3L))

  link_rows <- out$link_stage_log
  expect_true(any(link_rows$spoke_id == 2L))
  expect_true(any(link_rows$spoke_id == 3L))
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
    btl_config = list(refit_pairs_target = 1L),
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
      phase_a_artifacts = list(`1` = import_artifacts[["1"]]),
      phase_a_compatible_config_hashes = import_artifacts[["1"]]$fit_config_hash
    ),
    btl_config = list(refit_pairs_target = 1L),
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
    btl_config = list(refit_pairs_target = 1L),
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
    btl_config = list(refit_pairs_target = 1L),
    progress = "none"
  )

  stats_con <- out_con$controller$link_refit_stats_by_spoke
  expect_true(length(stats_con) >= 2L)
  expect_true(all(vapply(stats_con, function(x) !is.null(x$concurrent_target_pairs), logical(1L))))

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
    btl_config = list(refit_pairs_target = 1L),
    progress = "none"
  )

  starved <- out$step_log[out$step_log$status == "starved", , drop = FALSE]
  expect_true(nrow(starved) >= 1L)
  expect_true(any(!is.na(starved$fallback_path)))
  if (!is.na(out$meta$stop_reason)) {
    expect_identical(out$meta$stop_reason, "candidate_starvation")
  }
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
    "config hash incompatibility"
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
    btl_config = list(refit_pairs_target = 5L),
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
      link_transform_mode = "auto",
      link_refit_mode = "shift_only",
      phase_a_mode = "run"
    ),
    btl_config = list(refit_pairs_target = 5L),
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
    btl_config = list(refit_pairs_target = 1L),
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
    btl_config = list(refit_pairs_target = 1L),
    progress = "none"
  )

  cols <- c("status", "i", "j", "A", "B", "pair_id", "round_stage", "link_spoke_id")
  expect_equal(out_base$step_log[, cols, drop = FALSE], out_tuned$step_log[, cols, drop = FALSE])
})
