make_linking_refit_state <- function(adaptive_config = list()) {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s31", "s32"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 3L, 3L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs31", "gs32")
  )
  base_cfg <- list(run_mode = "link_multi_spoke", hub_id = 1L)
  state <- adaptive_rank_start(
    items,
    seed = 123L,
    adaptive_config = utils::modifyList(base_cfg, adaptive_config)
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
  colnames(draws) <- as.character(state$item_ids)
  state$btl_fit <- make_test_btl_fit(state$item_ids, draws = draws, model_variant = "btl_e_b")

  art <- function(set_id, ids, thetas) {
    list(
      set_id = as.integer(set_id),
      items = tibble::tibble(
        global_item_id = ids,
        theta_raw_mean = as.double(thetas),
        theta_raw_sd = rep(0.15, length(ids))
      )
    )
  }
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L, 3L),
      source = c("run", "run", "run"),
      status = c("ready", "ready", "ready"),
      validation_message = c("ok", "ok", "ok"),
      artifact_path = c(NA_character_, NA_character_, NA_character_)
    ),
    artifacts = list(
      `1` = art(1L, c("gh1", "gh2", "gh3"), c(0.80, 0.40, 0.10)),
      `2` = art(2L, c("gs21", "gs22"), c(-0.30, -0.60)),
      `3` = art(3L, c("gs31", "gs32"), c(0.15, -0.10))
    ),
    ready_for_phase_b = TRUE,
    phase = "phase_b"
  )

  state
}

append_cross_step <- function(state, step_id, A_id, B_id, Y, spoke_id) {
  ids <- as.character(state$item_ids)
  A <- match(A_id, ids)
  B <- match(B_id, ids)
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
      Y = as.integer(Y),
      set_i = as.integer(state$set_ids[[A]]),
      set_j = as.integer(state$set_ids[[B]]),
      is_cross_set = TRUE,
      link_spoke_id = as.integer(spoke_id)
    )
  )
  state
}

test_that("linking refit contract fields follow transform mode", {
  state_shift <- make_linking_refit_state(
    list(link_transform_mode = "shift_only", link_refit_mode = "shift_only")
  )
  state_shift <- append_cross_step(state_shift, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state_shift <- append_cross_step(state_shift, 2L, "h2", "s22", 0L, spoke_id = 2L)

  state_shift <- pairwiseLLM:::.adaptive_linking_refit_update_state(
    state_shift,
    refit_context = list(last_refit_step = 0L)
  )
  rows_shift <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state_shift,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row_shift <- rows_shift[rows_shift$spoke_id == 2L, , drop = FALSE]
  expect_true(is.finite(row_shift$delta_spoke_mean[[1L]]))
  expect_true(is.na(row_shift$log_alpha_spoke_mean[[1L]]))

  state_scale <- make_linking_refit_state(
    list(link_transform_mode = "shift_scale", link_refit_mode = "shift_only")
  )
  state_scale <- append_cross_step(state_scale, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state_scale <- append_cross_step(state_scale, 2L, "h2", "s22", 0L, spoke_id = 2L)
  state_scale <- pairwiseLLM:::.adaptive_linking_refit_update_state(
    state_scale,
    refit_context = list(last_refit_step = 0L)
  )
  rows_scale <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state_scale,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row_scale <- rows_scale[rows_scale$spoke_id == 2L, , drop = FALSE]
  expect_true(is.finite(row_scale$delta_spoke_mean[[1L]]))
  expect_true(is.finite(row_scale$log_alpha_spoke_mean[[1L]]))
})

test_that("hub lock boundary kappa=0 matches hard lock in joint refit", {
  base <- make_linking_refit_state(
    list(link_refit_mode = "joint_refit", link_transform_mode = "shift_only")
  )
  base <- append_cross_step(base, 1L, "s21", "h1", 1L, spoke_id = 2L)
  base <- append_cross_step(base, 2L, "h2", "s22", 0L, spoke_id = 2L)

  hard <- base
  hard <- pairwiseLLM:::.adaptive_apply_controller_config(
    hard,
    list(hub_lock_mode = "hard_lock", hub_lock_kappa = 0.75)
  )
  hard <- pairwiseLLM:::.adaptive_linking_refit_update_state(hard, list(last_refit_step = 0L))

  soft0 <- base
  soft0 <- pairwiseLLM:::.adaptive_apply_controller_config(
    soft0,
    list(hub_lock_mode = "soft_lock", hub_lock_kappa = 0)
  )
  soft0 <- pairwiseLLM:::.adaptive_linking_refit_update_state(soft0, list(last_refit_step = 0L))

  free <- base
  free <- pairwiseLLM:::.adaptive_apply_controller_config(
    free,
    list(hub_lock_mode = "free", hub_lock_kappa = 1)
  )
  free <- pairwiseLLM:::.adaptive_linking_refit_update_state(free, list(last_refit_step = 0L))

  d_hard <- hard$controller$link_refit_stats_by_spoke[["2"]]$delta_spoke_mean
  d_soft0 <- soft0$controller$link_refit_stats_by_spoke[["2"]]$delta_spoke_mean
  d_free <- free$controller$link_refit_stats_by_spoke[["2"]]$delta_spoke_mean

  expect_equal(d_hard, d_soft0, tolerance = 1e-10)
  expect_false(isTRUE(all.equal(d_hard, d_free, tolerance = 1e-10)))
})

test_that("auto escalation triggers after consecutive PPC failures and is one-way", {
  state <- make_linking_refit_state(
    list(
      link_transform_mode = "auto",
      link_refit_mode = "shift_only",
      cross_set_ppc_mae_max = 0.01,
      link_transform_escalation_refits_required = 2L,
      link_transform_escalation_is_one_way = TRUE
    )
  )

  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "s22", "h2", 1L, spoke_id = 2L)
  state <- pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
  expect_identical(state$controller$link_transform_mode_by_spoke[["2"]], NULL)

  state <- append_cross_step(state, 3L, "s21", "h3", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 4L, "s22", "h1", 1L, spoke_id = 2L)
  state <- pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 2L))
  expect_identical(state$controller$link_transform_mode_by_spoke[["2"]], "shift_scale")

  state <- append_cross_step(state, 5L, "s21", "h1", 0L, spoke_id = 2L)
  state <- append_cross_step(state, 6L, "s22", "h2", 0L, spoke_id = 2L)
  state <- pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 4L))
  expect_identical(state$controller$link_transform_mode_by_spoke[["2"]], "shift_scale")
})

test_that("invalid linking mode combinations fail validation", {
  defaults <- pairwiseLLM:::.adaptive_controller_defaults(8L)
  expect_identical(defaults$shift_only_theta_treatment, "fixed_eap")
  expect_true(is.list(defaults$link_transform_mode_by_spoke))
  expect_true(is.list(defaults$link_transform_bad_refits_by_spoke))
  expect_true(is.list(defaults$link_refit_stats_by_spoke))

  keys <- pairwiseLLM:::.adaptive_controller_public_keys()
  expect_true("shift_only_theta_treatment" %in% keys)

  ok <- pairwiseLLM:::.adaptive_validate_controller_config(
    list(shift_only_theta_treatment = "normal_prior"),
    n_items = 5L,
    set_ids = c(1L, 2L)
  )
  expect_identical(ok$shift_only_theta_treatment, "normal_prior")

  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(shift_only_theta_treatment = "bad"),
      n_items = 5L,
      set_ids = c(1L, 2L)
    ),
    "must be one of"
  )

  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(run_mode = "link_multi_spoke", multi_spoke_mode = "concurrent", hub_lock_mode = "free"),
      n_items = 5L,
      set_ids = c(1L, 2L, 3L)
    ),
    "must be `hard_lock` or `soft_lock`"
  )
})

test_that("pair selection remains independent of linking refit posterior fields", {
  state <- make_linking_refit_state()
  state$warm_start_done <- TRUE
  state$controller$current_link_spoke_id <- 2L

  before <- pairwiseLLM:::generate_stage_candidates_from_state(
    state,
    stage_name = "mid_link",
    fallback_name = "base",
    C_max = 5000L,
    seed = 99L
  )

  state$btl_fit$link_draws <- matrix(runif(20), nrow = 4)
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(delta_spoke_mean = 3, delta_spoke_sd = 9, log_alpha_spoke_mean = -2, log_alpha_spoke_sd = 8)
  )

  after <- pairwiseLLM:::generate_stage_candidates_from_state(
    state,
    stage_name = "mid_link",
    fallback_name = "base",
    C_max = 5000L,
    seed = 99L
  )

  expect_identical(before, after)
})

test_that("concurrent allocation uses uncertainty weights and enforces floor", {
  alloc <- pairwiseLLM:::.adaptive_link_concurrent_targets(
    spoke_stats = list(
      `2` = list(uncertainty = 0.8),
      `3` = list(uncertainty = 0.2)
    ),
    total_pairs = 16L,
    floor_pairs = 5L
  )
  expect_true(all(alloc >= 5L))
  expect_identical(sum(alloc), 16L)
  expect_true(alloc[["2"]] > alloc[["3"]])

  state <- make_linking_refit_state(
    list(
      multi_spoke_mode = "concurrent",
      min_cross_set_pairs_per_spoke_per_refit = 2L,
      hub_lock_mode = "soft_lock"
    )
  )
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "s22", "h2", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 3L, "s31", "h1", 0L, spoke_id = 3L)

  state <- pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
  stats2 <- state$controller$link_refit_stats_by_spoke[["2"]]
  stats3 <- state$controller$link_refit_stats_by_spoke[["3"]]

  expect_true(isTRUE(stats2$concurrent_floor_met))
  expect_false(isTRUE(stats3$concurrent_floor_met))
  expect_true(stats2$concurrent_target_pairs >= 2L)
  expect_true(stats3$concurrent_target_pairs >= 2L)
})

test_that("adaptive_state validation branches for linking controls are covered", {
  expect_error(
    pairwiseLLM:::.adaptive_state_normalize_items(tibble::tibble(item_id = "a", set_id = "x")),
    "integer-like"
  )
  expect_error(
    pairwiseLLM:::.adaptive_state_normalize_items(tibble::tibble(item_id = "a", set_id = 0L)),
    "must be >= 1"
  )
  expect_error(
    pairwiseLLM:::.adaptive_state_normalize_items(
      tibble::tibble(item_id = "a", global_item_id = NA_character_)
    ),
    "global_item_id"
  )
  expect_error(
    pairwiseLLM:::.adaptive_state_normalize_items(
      tibble::tibble(item_id = c("a", "b"), global_item_id = c("g", "g"))
    ),
    "must be unique"
  )

  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      structure(list(1L), names = ""),
      n_items = 5L
    ),
    "named list with non-empty names"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(hub_lock_kappa = "x"),
      n_items = 5L
    ),
    "single numeric value"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(link_transform_escalation_is_one_way = "x"),
      n_items = 5L
    ),
    "must be TRUE or FALSE"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(run_mode = ""),
      n_items = 5L
    ),
    "single string value"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(phase_a_compatible_config_hashes = c("ok", NA_character_)),
      n_items = 5L
    ),
    "character vector"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(phase_a_artifacts = 1L),
      n_items = 5L
    ),
    "named list"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(phase_a_set_source = c("run")),
      n_items = 5L
    ),
    "named character vector"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(phase_a_set_source = c(`1` = "bad")),
      n_items = 5L
    ),
    "values must be `run` or `import`"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(run_mode = "link_multi_spoke", hub_id = 99L),
      n_items = 5L,
      set_ids = c(1L, 2L, 3L)
    ),
    "must match one observed"
  )

  q <- pairwiseLLM:::.adaptive_round_compute_quotas(
    round_id = 1L,
    n_items = 10L,
    controller = list(
      run_mode = "link_multi_spoke",
      current_link_spoke_id = 2L,
      linking_identified_by_spoke = list(`2` = TRUE),
      linking_identified = FALSE
    )
  )
  expect_true(q[["long_link"]] <= 8L)
})
