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

  c_shift <- state_shift$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  expect_identical(c_shift$parameters, c("delta_s"))
  expect_identical(c_shift$link_transform_mode, "shift_only")

  c_scale <- state_scale$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  expect_identical(c_scale$parameters, c("delta_s", "log_alpha_s"))
  expect_identical(c_scale$link_transform_mode, "shift_scale")
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

  hard_contract <- hard$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  expect_equal(hard_contract$joint_refit$n_hub_items_estimated, 0L)

  expect_equal(d_hard, d_soft0, tolerance = 1e-10)
  expect_false(isTRUE(all.equal(d_hard, d_free, tolerance = 1e-10)))
})

test_that("soft lock uses artifact uncertainty and kappa strength", {
  base <- make_linking_refit_state(
    list(link_refit_mode = "joint_refit", link_transform_mode = "shift_only")
  )
  base <- append_cross_step(base, 1L, "s21", "h1", 1L, spoke_id = 2L)
  base <- append_cross_step(base, 2L, "h2", "s22", 0L, spoke_id = 2L)

  arts <- base$linking$phase_a$artifacts
  arts[["1"]]$items$theta_raw_sd <- c(0.02, 0.02, 0.02)
  base$linking$phase_a$artifacts <- arts

  soft_high <- pairwiseLLM:::.adaptive_apply_controller_config(
    base,
    list(hub_lock_mode = "soft_lock", hub_lock_kappa = 1)
  )
  soft_low <- pairwiseLLM:::.adaptive_apply_controller_config(
    base,
    list(hub_lock_mode = "soft_lock", hub_lock_kappa = 0.1)
  )

  out_high <- pairwiseLLM:::.adaptive_linking_refit_update_state(soft_high, list(last_refit_step = 0L))
  out_low <- pairwiseLLM:::.adaptive_linking_refit_update_state(soft_low, list(last_refit_step = 0L))

  sd_high <- out_high$controller$link_refit_stats_by_spoke[["2"]]$delta_spoke_sd
  sd_low <- out_low$controller$link_refit_stats_by_spoke[["2"]]$delta_spoke_sd
  expect_true(is.finite(sd_high))
  expect_true(is.finite(sd_low))
  c_high <- out_high$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  c_low <- out_low$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  expect_identical(c_high$lock$hub_lock_mode, "soft_lock")
  expect_equal(c_high$lock$hub_lock_kappa, 1, tolerance = 1e-12)
  expect_equal(c_low$lock$hub_lock_kappa, 0.1, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(
    out_high$controller$link_refit_stats_by_spoke[["2"]]$delta_spoke_mean,
    out_low$controller$link_refit_stats_by_spoke[["2"]]$delta_spoke_mean,
    tolerance = 1e-10
  )))
})

test_that("joint_refit fit contract records joint theta estimation", {
  state <- make_linking_refit_state(
    list(link_refit_mode = "joint_refit", link_transform_mode = "shift_only")
  )
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "h2", "s22", 0L, spoke_id = 2L)

  out <- pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
  contract <- out$controller$link_refit_stats_by_spoke[["2"]]$fit_contract

  expect_identical(contract$link_refit_mode, "joint_refit")
  expect_true(all(c("theta_hub", "theta_spoke", "delta_s") %in% contract$parameters))
  expect_true(isTRUE(contract$joint_refit$used))
  expect_true(contract$joint_refit$n_hub_items_estimated >= 1L)
  expect_true(contract$joint_refit$n_spoke_items_estimated >= 1L)
})

test_that("joint_refit utility uses current theta state rather than Phase A summaries", {
  state <- make_linking_refit_state(list(link_refit_mode = "joint_refit"))
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      delta_spoke_mean = 0,
      log_alpha_spoke_mean = NA_real_,
      link_transform_mode = "shift_only"
    )
  )
  cand <- tibble::tibble(i = "h1", j = "s21")

  out_joint <- pairwiseLLM:::.adaptive_link_attach_predictive_utility(
    candidates = cand,
    state = state,
    controller = state$controller,
    spoke_id = 2L
  )

  state_shift <- pairwiseLLM:::.adaptive_apply_controller_config(state, list(link_refit_mode = "shift_only"))
  out_shift <- pairwiseLLM:::.adaptive_link_attach_predictive_utility(
    candidates = cand,
    state = state_shift,
    controller = state_shift$controller,
    spoke_id = 2L
  )

  theta_cur <- state$btl_fit$theta_mean
  expected_joint <- stats::plogis(theta_cur[["h1"]] - theta_cur[["s21"]])

  expect_true(is.finite(out_joint$link_p[[1L]]))
  expect_equal(out_joint$link_p[[1L]], expected_joint, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(out_joint$link_p[[1L]], out_shift$link_p[[1L]], tolerance = 1e-12)))
})

test_that("soft-lock joint refit keeps Phase A prior center and uses current theta only for initialization", {
  state <- make_linking_refit_state(list(link_refit_mode = "joint_refit", hub_lock_mode = "soft_lock"))
  state$btl_fit$theta_mean[c("h1", "h2", "h3")] <- c(10, 9, 8)
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)

  captured <- list()
  testthat::with_mocked_bindings(
    .adaptive_link_fit_transform = function(cross_edges, hub_theta, spoke_theta, transform_mode) {
      captured$hub_theta <<- hub_theta
      captured$spoke_theta <<- spoke_theta
      list(
        delta_mean = 0,
        delta_sd = 1,
        log_alpha_mean = NA_real_,
        log_alpha_sd = NA_real_
      )
    },
    .adaptive_link_ppc_mae_cross = function(...) 0,
    .package = "pairwiseLLM",
    {
      pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
    }
  )

  hub_prior_center <- attr(captured$hub_theta, "theta_prior_center", exact = TRUE)
  hub_init <- attr(captured$hub_theta, "theta_init", exact = TRUE)
  expect_true(length(hub_prior_center) > 0L)
  expect_true(length(hub_init) > 0L)

  phase_a_hub <- state$linking$phase_a$artifacts[["1"]]$items
  phase_center_map <- stats::setNames(as.double(phase_a_hub$theta_raw_mean), c("h1", "h2", "h3"))
  expect_equal(unname(hub_prior_center[c("h1", "h2", "h3")]), unname(phase_center_map[c("h1", "h2", "h3")]))
  expect_equal(unname(hub_init[c("h1", "h2", "h3")]), c(10, 9, 8))
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

test_that("auto escalation refits shift_scale parameters in the same refit", {
  state <- make_linking_refit_state(
    list(
      link_transform_mode = "auto",
      link_refit_mode = "shift_only",
      cross_set_ppc_mae_max = 0.01,
      link_transform_escalation_refits_required = 1L
    )
  )
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  modes <- character()

  out <- testthat::with_mocked_bindings(
    .adaptive_link_fit_transform = function(cross_edges, hub_theta, spoke_theta, transform_mode) {
      modes <<- c(modes, as.character(transform_mode))
      if (identical(transform_mode, "shift_scale")) {
        return(list(delta_mean = 0.1, delta_sd = 0.05, log_alpha_mean = 0.33, log_alpha_sd = 0.04))
      }
      list(delta_mean = 0.1, delta_sd = 0.05, log_alpha_mean = NA_real_, log_alpha_sd = NA_real_)
    },
    .adaptive_link_ppc_mae_cross = function(...) 0.5,
    .package = "pairwiseLLM",
    {
      pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
    }
  )

  stats <- out$controller$link_refit_stats_by_spoke[["2"]]
  expect_identical(modes[[1L]], "shift_only")
  expect_true(any(modes == "shift_scale"))
  expect_identical(stats$link_transform_mode, "shift_scale")
  expect_equal(stats$log_alpha_spoke_mean, 0.33, tolerance = 1e-12)
})

test_that("auto escalation streak is preserved across NA PPC windows", {
  state <- make_linking_refit_state(
    list(
      link_transform_mode = "auto",
      link_refit_mode = "shift_only",
      cross_set_ppc_mae_max = 0.01,
      link_transform_escalation_refits_required = 2L
    )
  )
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)

  ppc_vals <- c(0.5, NA_real_, 0.5)
  ppc_idx <- 0L
  out <- testthat::with_mocked_bindings(
    .adaptive_link_ppc_mae_cross = function(...) {
      ppc_idx <<- ppc_idx + 1L
      ppc_vals[[min(ppc_idx, length(ppc_vals))]]
    },
    .package = "pairwiseLLM",
    {
      s1 <- pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
      s2 <- pairwiseLLM:::.adaptive_linking_refit_update_state(s1, list(last_refit_step = 0L))
      pairwiseLLM:::.adaptive_linking_refit_update_state(s2, list(last_refit_step = 0L))
    }
  )

  expect_identical(out$controller$link_transform_mode_by_spoke[["2"]], "shift_scale")
})

test_that("judge parameter mode controls linking judge scope in fit contract", {
  state <- make_linking_refit_state(
    list(link_transform_mode = "shift_only", link_refit_mode = "shift_only", judge_param_mode = "phase_specific")
  )
  state$btl_fit$beta_mean <- 0.01
  state$btl_fit$epsilon_mean <- 0.02
  state$btl_fit$beta_link_mean <- 0.15
  state$btl_fit$epsilon_link_mean <- 0.25
  state$btl_fit$beta_within_mean <- -0.1
  state$btl_fit$epsilon_within_mean <- 0.05
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "h2", "s22", 0L, spoke_id = 2L)

  state <- pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
  contract <- state$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  expect_identical(contract$judge$mode, "phase_specific")
  expect_identical(contract$judge$scope, "link")
  expect_equal(contract$judge$beta, 0.15, tolerance = 1e-12)
  expect_equal(contract$judge$epsilon, 0.25, tolerance = 1e-12)
})

test_that("phase-specific judge mode allows startup fallback but aborts after startup when link params are missing", {
  state <- make_linking_refit_state(
    list(link_transform_mode = "shift_only", link_refit_mode = "shift_only", judge_param_mode = "phase_specific")
  )
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "h2", "s22", 0L, spoke_id = 2L)
  state$btl_fit$beta_within_mean <- 0.03
  state$btl_fit$epsilon_within_mean <- 0.02
  state$btl_fit$beta_link_mean <- NULL
  state$btl_fit$epsilon_link_mean <- NULL

  out_startup <- expect_no_error(
    pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
  )
  expect_true(isTRUE(
    out_startup$controller$link_refit_stats_by_spoke[["2"]]$fit_contract$judge$cold_start_fallback_used
  ))

  out_startup$btl_fit$beta_link_mean <- NULL
  out_startup$btl_fit$epsilon_link_mean <- NULL
  out_startup <- append_cross_step(out_startup, 3L, "s21", "h3", 1L, spoke_id = 2L)

  expect_error(
    pairwiseLLM:::.adaptive_linking_refit_update_state(out_startup, list(last_refit_step = 2L)),
    "Phase-specific judge mode requires `beta_link_mean`"
  )
})

test_that("startup-gap helper and edge extractors cover fallback edge paths", {
  state <- make_linking_refit_state()

  expect_true(isTRUE(pairwiseLLM:::.adaptive_link_phase_b_startup_gap_for_spoke(state, 2L)))
  expect_equal(nrow(pairwiseLLM:::.adaptive_link_cross_edges(state, spoke_id = 2L)), 0L)
  expect_equal(nrow(pairwiseLLM:::.adaptive_link_within_edges(state, set_id = 1L)), 0L)

  state$controller$link_refit_stats_by_spoke <- list(`2` = list(delta_spoke_mean = 0))
  expect_false(isTRUE(pairwiseLLM:::.adaptive_link_phase_b_startup_gap_for_spoke(state, 2L)))
})

test_that("joint shift_scale fit consumes within-set edges and records lock/joint fields", {
  edges <- tibble::tibble(
    spoke_item = c("s1", "s2"),
    hub_item = c("h1", "h2"),
    y_spoke = c(1L, 0L),
    step_id = c(1L, 2L),
    spoke_in_A = c(TRUE, FALSE)
  )
  attr(edges, "judge_params") <- list(beta = 0.1, epsilon = 0.05, mode = "phase_specific", scope = "link")
  attr(edges, "refit_contract") <- list(
    link_refit_mode = "joint_refit",
    hub_lock_mode = "hard_like",
    hub_lock_kappa = 0.5
  )
  attr(edges, "within_hub_edges") <- tibble::tibble(
    A_item = c("h1", "h2"),
    B_item = c("h2", "h1"),
    y_A = c(1L, 0L),
    step_id = c(3L, 4L)
  )
  attr(edges, "within_spoke_edges") <- tibble::tibble(
    A_item = c("s1", "s2"),
    B_item = c("s2", "s1"),
    y_A = c(1L, 0L),
    step_id = c(5L, 6L)
  )

  hub_theta <- c(h1 = 0.4, h2 = -0.1)
  spoke_theta <- c(s1 = -0.3, s2 = 0.2)
  attr(hub_theta, "theta_sd") <- c(h1 = 0.1, h2 = 0.1)
  attr(spoke_theta, "theta_sd") <- c(s1 = 0.2, s2 = 0.2)

  fit <- pairwiseLLM:::.adaptive_link_fit_transform(
    edges,
    hub_theta = hub_theta,
    spoke_theta = spoke_theta,
    transform_mode = "shift_scale"
  )
  expect_true(is.finite(fit$delta_mean))
  expect_true(is.finite(fit$log_alpha_mean))
  expect_equal(length(fit$theta_hub_post), 2L)
  expect_equal(length(fit$theta_spoke_post), 2L)
  expect_identical(fit$fit_contract$link_refit_mode, "joint_refit")
  expect_true(isTRUE(fit$fit_contract$joint_refit$used))
})

test_that("link likelihood applies signed beta by original presentation side", {
  edges_mixed <- tibble::tibble(
    spoke_item = c("s1", "s1"),
    hub_item = c("h1", "h1"),
    y_spoke = c(1L, 0L),
    step_id = c(1L, 2L),
    spoke_in_A = c(TRUE, FALSE)
  )
  edges_all_a <- edges_mixed
  edges_all_a$spoke_in_A <- c(TRUE, TRUE)
  attr(edges_mixed, "judge_params") <- list(beta = 1, epsilon = 0, mode = "phase_specific", scope = "link")
  attr(edges_all_a, "judge_params") <- list(beta = 1, epsilon = 0, mode = "phase_specific", scope = "link")

  hub_theta <- c(h1 = 0)
  spoke_theta <- c(s1 = 0)

  fit_mixed <- pairwiseLLM:::.adaptive_link_fit_transform(
    edges_mixed,
    hub_theta,
    spoke_theta,
    transform_mode = "shift_only"
  )
  fit_all_a <- pairwiseLLM:::.adaptive_link_fit_transform(
    edges_all_a,
    hub_theta,
    spoke_theta,
    transform_mode = "shift_only"
  )

  expect_equal(fit_mixed$delta_mean, 0, tolerance = 1e-8)
  expect_true(fit_all_a$delta_mean < -0.1)
})

test_that("shift_only theta treatment normal_prior propagates uncertainty", {
  fixed <- make_linking_refit_state(
    list(
      link_transform_mode = "shift_only",
      link_refit_mode = "shift_only",
      shift_only_theta_treatment = "fixed_eap"
    )
  )
  normal <- make_linking_refit_state(
    list(
      link_transform_mode = "shift_only",
      link_refit_mode = "shift_only",
      shift_only_theta_treatment = "normal_prior"
    )
  )

  fixed <- append_cross_step(fixed, 1L, "s21", "h1", 1L, spoke_id = 2L)
  fixed <- append_cross_step(fixed, 2L, "h2", "s22", 0L, spoke_id = 2L)
  normal <- append_cross_step(normal, 1L, "s21", "h1", 1L, spoke_id = 2L)
  normal <- append_cross_step(normal, 2L, "h2", "s22", 0L, spoke_id = 2L)

  out_fixed <- pairwiseLLM:::.adaptive_linking_refit_update_state(fixed, list(last_refit_step = 0L))
  out_normal <- pairwiseLLM:::.adaptive_linking_refit_update_state(normal, list(last_refit_step = 0L))

  sd_fixed <- out_fixed$controller$link_refit_stats_by_spoke[["2"]]$delta_spoke_sd
  sd_normal <- out_normal$controller$link_refit_stats_by_spoke[["2"]]$delta_spoke_sd
  expect_true(is.finite(sd_fixed))
  expect_true(is.finite(sd_normal))
  c_fixed <- out_fixed$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  c_normal <- out_normal$controller$link_refit_stats_by_spoke[["2"]]$fit_contract
  expect_identical(c_fixed$theta_treatment, "fixed_eap")
  expect_identical(c_normal$theta_treatment, "normal_prior")
  expect_false(isTRUE(all.equal(sd_normal, sd_fixed, tolerance = 1e-10)))
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

test_that("concurrent spoke routing enforces floor before uncertainty targets", {
  state <- make_linking_refit_state(
    list(
      multi_spoke_mode = "concurrent",
      min_cross_set_pairs_per_spoke_per_refit = 2L
    )
  )
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state$refit_meta$last_refit_step <- 0L
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(uncertainty = 0.01),
    `3` = list(uncertainty = 10)
  )

  # Spoke 3 is below floor while spoke 2 already has one edge.
  pick <- pairwiseLLM:::.adaptive_link_active_spoke(state, state$controller)
  expect_identical(pick, 3L)

  state <- append_cross_step(state, 2L, "s31", "h1", 1L, spoke_id = 3L)
  state <- append_cross_step(state, 3L, "s32", "h2", 1L, spoke_id = 3L)
  # Both spokes satisfy floor now; routing should follow uncertainty target deficit.
  pick2 <- pairwiseLLM:::.adaptive_link_active_spoke(state, state$controller)
  expect_identical(pick2, 2L)
})

test_that("concurrent routing uses uncertainty-weighted deficit, not least-used balancing", {
  state <- make_linking_refit_state(
    list(
      multi_spoke_mode = "concurrent",
      min_cross_set_pairs_per_spoke_per_refit = 1L
    )
  )
  state$refit_meta$last_refit_step <- 0L
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "s31", "h1", 1L, spoke_id = 3L)
  state <- append_cross_step(state, 3L, "s32", "h2", 1L, spoke_id = 3L)
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(uncertainty = 0.01),
    `3` = list(uncertainty = 100)
  )

  # Least-used balancing would pick spoke 2 (1 vs 2), but target-deficit routing picks spoke 3.
  pick <- pairwiseLLM:::.adaptive_link_active_spoke(state, state$controller)
  expect_identical(pick, 3L)
})

test_that("concurrent floor is enforced as a routing floor when feasible", {
  state <- make_linking_refit_state(
    list(
      multi_spoke_mode = "concurrent",
      min_cross_set_pairs_per_spoke_per_refit = 2L
    )
  )
  state$refit_meta$last_refit_step <- 0L
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(uncertainty = 1),
    `3` = list(uncertainty = 1)
  )

  for (step_id in 1:4) {
    spoke <- pairwiseLLM:::.adaptive_link_active_spoke(state, state$controller)
    if (identical(spoke, 2L)) {
      state <- append_cross_step(state, step_id, "s21", "h1", 1L, spoke_id = 2L)
    } else {
      state <- append_cross_step(state, step_id, "s31", "h1", 1L, spoke_id = 3L)
    }
  }

  step_subset <- state$step_log[
    !is.na(state$step_log$pair_id) &
      state$step_log$is_cross_set %in% TRUE &
      as.integer(state$step_log$step_id) > 0L,
    ,
    drop = FALSE
  ]
  counts <- table(factor(as.integer(step_subset$link_spoke_id), levels = c(2L, 3L)))
  expect_true(all(as.integer(counts) >= 2L))
})

test_that("concurrent sparse-domain fallback is deterministic when floors cannot be met", {
  state <- make_linking_refit_state(
    list(
      multi_spoke_mode = "concurrent",
      min_cross_set_pairs_per_spoke_per_refit = 5L
    )
  )
  state$refit_meta$last_refit_step <- 0L
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(uncertainty = 0),
    `3` = list(uncertainty = 0)
  )
  # One observed pair per spoke leaves equal floor deficits that cannot be satisfied quickly;
  # tie-breaks must be deterministic.
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "s31", "h1", 1L, spoke_id = 3L)

  picks <- integer()
  for (idx in 1:4) {
    spoke <- pairwiseLLM:::.adaptive_link_active_spoke(state, state$controller)
    picks <- c(picks, as.integer(spoke))
    if (identical(spoke, 2L)) {
      state <- append_cross_step(state, idx + 2L, "s21", "h1", 1L, spoke_id = 2L)
    } else {
      state <- append_cross_step(state, idx + 2L, "s31", "h1", 1L, spoke_id = 3L)
    }
  }

  expect_identical(picks, c(2L, 3L, 2L, 3L))
})

test_that("active linking transformed reliability is computed on active hub-spoke items only", {
  state <- make_linking_refit_state()
  state$round$anchor_ids <- character()
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state <- append_cross_step(state, 2L, "s22", "h2", 0L, spoke_id = 2L)

  active <- pairwiseLLM:::.adaptive_link_active_item_ids(state, spoke_id = 2L, hub_id = 1L)
  rel_active <- pairwiseLLM:::.adaptive_link_reliability_transformed_active(
    state = state,
    active_ids = active$active_all,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_scale",
    delta_mean = 0.25,
    log_alpha_mean = log(1.3)
  )
  draws <- state$btl_fit$btl_posterior_draws
  active_draws <- draws[, active$active_all, drop = FALSE]
  spoke_cols <- colnames(active_draws) %in% c("s21", "s22")
  active_draws[, spoke_cols] <- 0.25 + 1.3 * active_draws[, spoke_cols, drop = FALSE]
  rel_manual <- pairwiseLLM:::compute_reliability_EAP(active_draws)
  rel_all <- pairwiseLLM:::compute_reliability_EAP(draws)

  expect_equal(rel_active, rel_manual, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(rel_active, rel_all, tolerance = 1e-12)))
})

test_that("link stop gating enforces diagnostics and lag eligibility", {
  state <- make_linking_refit_state()
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_mode = "shift_only",
      delta_spoke_mean = 0.1,
      delta_spoke_sd = 0.02,
      delta_change_lagged = 0.01,
      delta_change_pass = TRUE,
      log_alpha_change_pass = NA,
      delta_sd_pass = TRUE,
      log_alpha_sd_pass = NA,
      link_reliability = 0.95,
      link_reliability_stop_pass = TRUE,
      ts_btl_rank_spearman_active = 0.94,
      lag_eligible = FALSE,
      rank_stability_lagged = NA_real_,
      rank_stability_pass = FALSE,
      link_identified = TRUE,
      active_item_count_hub = 1L,
      active_item_count_spoke = 2L
    )
  )
  state$controller$linking_identified_by_spoke <- list(`2` = TRUE)
  state$round_log <- pairwiseLLM:::append_round_log(
    state$round_log,
    list(refit_id = 1L, diagnostics_pass = TRUE)
  )

  rows_lag <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row_lag <- rows_lag[rows_lag$spoke_id == 2L, , drop = FALSE]
  expect_false(isTRUE(row_lag$link_stop_eligible[[1L]]))
  expect_false(isTRUE(row_lag$link_stop_pass[[1L]]))

  state$controller$link_refit_stats_by_spoke[["2"]]$lag_eligible <- TRUE
  state$controller$link_refit_stats_by_spoke[["2"]]$rank_stability_lagged <- 0.99
  state$controller$link_refit_stats_by_spoke[["2"]]$rank_stability_pass <- TRUE
  state$round_log$diagnostics_pass[[nrow(state$round_log)]] <- FALSE
  rows_diag <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row_diag <- rows_diag[rows_diag$spoke_id == 2L, , drop = FALSE]
  expect_true(isTRUE(row_diag$link_stop_eligible[[1L]]))
  expect_false(isTRUE(row_diag$link_stop_pass[[1L]]))

  state$controller$link_refit_stats_by_spoke[["2"]]$delta_change_pass <- NA
  state$round_log$diagnostics_pass[[nrow(state$round_log)]] <- TRUE
  rows_missing <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row_missing <- rows_missing[rows_missing$spoke_id == 2L, , drop = FALSE]
  expect_false(isTRUE(row_missing$link_stop_eligible[[1L]]))
  expect_false(isTRUE(row_missing$link_stop_pass[[1L]]))
})

test_that("runtime linking_identified uses active TS-BTL rank threshold and not lagged rank stability", {
  base_state <- make_linking_refit_state(
    list(
      link_identified_reliability_min = 0.80,
      link_rank_corr_min = 0.90,
      delta_sd_max = 100
    )
  )
  base_state <- append_cross_step(base_state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  base_state <- append_cross_step(base_state, 2L, "h2", "s22", 0L, spoke_id = 2L)

  out_pass <- testthat::with_mocked_bindings(
    .adaptive_link_rank_stability_lagged = function(...) {
      list(lag_eligible = FALSE, rho_rank_lagged = NA_real_, rho_rank_lagged_pass = FALSE)
    },
    .adaptive_link_ts_btl_rank_spearman_active = function(...) 0.94,
    .adaptive_link_reliability_transformed_active = function(...) 0.95,
    .adaptive_linking_refit_update_state(base_state, list(last_refit_step = 0L)),
    .package = "pairwiseLLM"
  )
  stats_pass <- out_pass$controller$link_refit_stats_by_spoke[["2"]]
  expect_false(isTRUE(stats_pass$lag_eligible))
  expect_true(isTRUE(stats_pass$link_rank_corr_pass))
  expect_true(isTRUE(stats_pass$link_identified))

  out_fail <- testthat::with_mocked_bindings(
    .adaptive_link_rank_stability_lagged = function(...) {
      list(lag_eligible = FALSE, rho_rank_lagged = NA_real_, rho_rank_lagged_pass = FALSE)
    },
    .adaptive_link_ts_btl_rank_spearman_active = function(...) 0.40,
    .adaptive_link_reliability_transformed_active = function(...) 0.95,
    .adaptive_linking_refit_update_state(base_state, list(last_refit_step = 0L)),
    .package = "pairwiseLLM"
  )
  stats_fail <- out_fail$controller$link_refit_stats_by_spoke[["2"]]
  expect_false(isTRUE(stats_fail$link_rank_corr_pass))
  expect_false(isTRUE(stats_fail$link_identified))
})

test_that("link stop decision is reproducible from link_stage_log fields", {
  state <- make_linking_refit_state()
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_mode = "shift_scale",
      delta_spoke_mean = 0.1,
      delta_spoke_sd = 0.01,
      log_alpha_spoke_mean = 0.02,
      log_alpha_spoke_sd = 0.03,
      delta_change_lagged = 0.01,
      log_alpha_change_lagged = 0.01,
      delta_change_pass = TRUE,
      log_alpha_change_pass = TRUE,
      delta_sd_pass = TRUE,
      log_alpha_sd_pass = TRUE,
      link_reliability = 0.95,
      link_reliability_stop_pass = TRUE,
      ts_btl_rank_spearman_active = 0.95,
      lag_eligible = TRUE,
      rank_stability_lagged = 0.99,
      rank_stability_pass = TRUE,
      link_identified = TRUE,
      active_item_count_hub = 1L,
      active_item_count_spoke = 2L,
      delta_sd_max_used = 0.05
    )
  )
  state$controller$linking_identified_by_spoke <- list(`2` = TRUE)
  state$round_log <- pairwiseLLM:::append_round_log(
    state$round_log,
    list(refit_id = 1L, diagnostics_pass = TRUE)
  )

  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row <- rows[rows$spoke_id == 2L, , drop = FALSE]
  hub_ids <- as.character(state$items$item_id[state$items$set_id == 1L])
  hub_theta <- as.double(state$btl_fit$theta_mean[hub_ids])
  reconstructed <- pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = row,
    diagnostics_pass = TRUE,
    hub_theta_sd = stats::sd(hub_theta),
    controller = state$controller
  )
  expect_identical(row$link_stop_pass[[1L]], reconstructed)
})

test_that("link stage log stores active TS-BTL correlation separately from lagged rank stability", {
  state <- make_linking_refit_state()
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_mode = "shift_only",
      delta_spoke_mean = 0.1,
      delta_spoke_sd = 0.02,
      delta_change_lagged = 0.01,
      delta_change_pass = TRUE,
      delta_sd_max_used = 0.05,
      delta_sd_pass = TRUE,
      log_alpha_sd_pass = NA,
      log_alpha_change_pass = NA,
      link_reliability = 0.91,
      link_reliability_stop_pass = TRUE,
      ts_btl_rank_spearman_active = 0.93,
      lag_eligible = TRUE,
      rank_stability_lagged = 0.99,
      rank_stability_pass = TRUE
    )
  )
  state$round_log <- pairwiseLLM:::append_round_log(
    state$round_log,
    list(refit_id = 1L, diagnostics_pass = TRUE)
  )
  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row <- rows[rows$spoke_id == 2L, , drop = FALSE]
  expect_equal(row$ts_btl_rank_spearman[[1L]], 0.93, tolerance = 1e-12)
  expect_equal(row$rank_stability_lagged[[1L]], 0.99, tolerance = 1e-12)
})

test_that("link stop reconstruction can use link-stage pass flags only", {
  row <- tibble::tibble(
    link_stop_eligible = TRUE,
    reliability_stop_pass = TRUE,
    delta_sd_pass = TRUE,
    log_alpha_sd_pass = NA,
    delta_change_pass = TRUE,
    log_alpha_change_pass = NA,
    rank_stability_pass = TRUE
  )
  out <- pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = row,
    diagnostics_pass = TRUE,
    hub_theta_sd = NA_real_,
    controller = list()
  )
  expect_true(isTRUE(out))

  row$rank_stability_pass <- FALSE
  out2 <- pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = row,
    diagnostics_pass = TRUE,
    hub_theta_sd = NA_real_,
    controller = list()
  )
  expect_false(isTRUE(out2))
})

test_that("link stop reconstruction fallback path honors numeric gates", {
  controller <- list(
    link_stop_reliability_min = 0.90,
    delta_sd_max = 0.10,
    log_alpha_sd_max = 0.10,
    delta_change_max = 0.05,
    log_alpha_change_max = 0.05
  )
  row_shift <- tibble::tibble(
    link_stop_eligible = TRUE,
    reliability_EAP_link = 0.95,
    delta_spoke_sd = 0.01,
    link_transform_mode = "shift_only",
    log_alpha_spoke_sd = NA_real_,
    delta_change_lagged = 0.01,
    log_alpha_change_lagged = NA_real_,
    delta_change_pass = TRUE,
    log_alpha_change_pass = NA,
    rank_stability_lagged = 0.99
  )
  pass_shift <- pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = row_shift,
    diagnostics_pass = TRUE,
    hub_theta_sd = 0.5,
    controller = controller
  )
  expect_true(isTRUE(pass_shift))

  row_scale <- tibble::tibble(
    link_stop_eligible = TRUE,
    reliability_EAP_link = 0.95,
    delta_spoke_sd = 0.01,
    link_transform_mode = "shift_scale",
    log_alpha_spoke_sd = 0.03,
    delta_change_lagged = 0.01,
    log_alpha_change_lagged = 0.01,
    delta_change_pass = TRUE,
    log_alpha_change_pass = TRUE,
    rank_stability_lagged = 0.99
  )
  pass_scale <- pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = row_scale,
    diagnostics_pass = TRUE,
    hub_theta_sd = 0.5,
    controller = controller
  )
  expect_true(isTRUE(pass_scale))

  row_scale$log_alpha_change_lagged <- 0.25
  row_scale$log_alpha_change_pass <- FALSE
  fail_scale <- pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = row_scale,
    diagnostics_pass = TRUE,
    hub_theta_sd = 0.5,
    controller = controller
  )
  expect_false(isTRUE(fail_scale))

  row_rank <- row_shift
  row_rank$rank_stability_lagged <- 0.90
  fail_rank <- pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = row_rank,
    diagnostics_pass = TRUE,
    hub_theta_sd = 0.5,
    controller = controller
  )
  expect_false(isTRUE(fail_rank))
})

test_that("linking identified state is reconstructable from canonical link-stage fields", {
  row <- tibble::tibble(
    link_transform_mode = "shift_scale",
    reliability_EAP_link = 0.92,
    ts_btl_rank_spearman = 0.93,
    delta_sd_pass = TRUE,
    log_alpha_sd_pass = TRUE
  )
  identified <- pairwiseLLM:::.adaptive_link_reconstruct_identified_from_logs(
    link_row = row,
    controller = list(
      link_identified_reliability_min = 0.80,
      link_rank_corr_min = 0.90
    )
  )
  expect_true(isTRUE(identified))

  row$ts_btl_rank_spearman <- 0.85
  identified_fail <- pairwiseLLM:::.adaptive_link_reconstruct_identified_from_logs(
    link_row = row,
    controller = list(
      link_identified_reliability_min = 0.80,
      link_rank_corr_min = 0.90
    )
  )
  expect_false(isTRUE(identified_fail))
})

test_that("linking refit stats carry latest coverage metadata for link-stage log rows", {
  state <- make_linking_refit_state()
  state <- append_cross_step(state, 1L, "s21", "h1", 1L, spoke_id = 2L)
  state$controller$link_stage_coverage_bins_used <- list(`2` = 3L)
  state$controller$link_stage_coverage_source <- list(`2` = "phase_a_rank_mu_raw")

  state <- pairwiseLLM:::.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  row <- rows[rows$spoke_id == 2L, , drop = FALSE]
  expect_identical(row$coverage_bins_used[[1L]], 3L)
  expect_identical(row$coverage_source[[1L]], "phase_a_rank_mu_raw")
})

test_that("taper decisions are reconstructable from canonical link-stage quota fields", {
  row_raw <- tibble::tibble(
    quota_taper_applied = NA,
    quota_long_link_raw = 8L,
    quota_long_link_effective = 4L
  )
  row_notaper <- tibble::tibble(
    quota_taper_applied = NA,
    quota_long_link_raw = 8L,
    quota_long_link_effective = 8L
  )
  row_explicit <- tibble::tibble(
    quota_taper_applied = TRUE,
    quota_long_link_raw = 8L,
    quota_long_link_effective = 8L
  )

  expect_true(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_taper_from_logs(row_raw)))
  expect_false(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_taper_from_logs(row_notaper)))
  expect_true(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_taper_from_logs(row_explicit)))
})

test_that("item log keeps raw summaries separate from transformed global summaries in linking mode", {
  state <- make_linking_refit_state(
    list(link_transform_mode = "shift_scale", multi_spoke_mode = "independent")
  )
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_mode = "shift_scale",
      delta_spoke_mean = 0.3,
      log_alpha_spoke_mean = log(1.2)
    ),
    `3` = list(
      link_transform_mode = "shift_only",
      delta_spoke_mean = -0.2,
      log_alpha_spoke_mean = NA_real_
    )
  )

  item_log <- pairwiseLLM:::.adaptive_build_item_log_refit(state, refit_id = 1L)
  row_s2 <- item_log[item_log$item_id == "s21", , drop = FALSE]
  row_s3 <- item_log[item_log$item_id == "s31", , drop = FALSE]
  row_h <- item_log[item_log$item_id == "h1", , drop = FALSE]

  expect_equal(row_s2$theta_raw_eap[[1L]], -0.30, tolerance = 1e-12)
  expect_equal(row_s2$theta_global_eap[[1L]], 0.3 + 1.2 * (-0.30), tolerance = 1e-12)
  expect_equal(row_s3$theta_raw_eap[[1L]], 0.15, tolerance = 1e-12)
  expect_equal(row_s3$theta_global_eap[[1L]], -0.2 + 0.15, tolerance = 1e-12)
  expect_equal(row_h$theta_raw_eap[[1L]], 0.80, tolerance = 1e-12)
  expect_equal(row_h$theta_global_eap[[1L]], 0.80, tolerance = 1e-12)
})

test_that("item log uses typed NA global summaries when spoke transform parameters are unavailable", {
  state <- make_linking_refit_state(
    list(link_transform_mode = "shift_scale", multi_spoke_mode = "independent")
  )
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_mode = "shift_scale",
      delta_spoke_mean = 0.25,
      log_alpha_spoke_mean = NA_real_
    ),
    `3` = list(
      link_transform_mode = "shift_only",
      delta_spoke_mean = NA_real_
    )
  )

  item_log <- pairwiseLLM:::.adaptive_build_item_log_refit(state, refit_id = 1L)
  row_s2 <- item_log[item_log$item_id == "s21", , drop = FALSE]
  row_s3 <- item_log[item_log$item_id == "s31", , drop = FALSE]
  row_h <- item_log[item_log$item_id == "h1", , drop = FALSE]

  expect_true(is.na(row_s2$theta_global_eap[[1L]]))
  expect_true(is.na(row_s2$theta_global_sd[[1L]]))
  expect_true(is.na(row_s3$theta_global_eap[[1L]]))
  expect_true(is.na(row_s3$theta_global_sd[[1L]]))
  expect_true(is.finite(row_h$theta_global_eap[[1L]]))
  expect_true(is.finite(row_h$theta_global_sd[[1L]]))
})

test_that("non-linking item log keeps current raw/global behavior under seeded setup", {
  state <- adaptive_rank_start(make_test_items(5), seed = 15L)
  ids <- as.character(state$item_ids)
  draws <- matrix(
    c(
      0.9, 0.5, 0.2, -0.2, -0.6,
      0.8, 0.6, 0.1, -0.1, -0.7,
      1.0, 0.4, 0.3, -0.3, -0.5,
      0.7, 0.3, 0.0, -0.4, -0.8
    ),
    nrow = 4,
    byrow = TRUE
  )
  colnames(draws) <- ids
  state$btl_fit <- make_test_btl_fit(ids, draws = draws, model_variant = "btl_e_b")

  item_log <- pairwiseLLM:::.adaptive_build_item_log_refit(state, refit_id = 1L)
  expect_equal(item_log$theta_raw_eap, item_log$theta_global_eap, tolerance = 1e-12)
  expect_equal(item_log$theta_global_sd, item_log$theta_sd, tolerance = 1e-12)
  expect_identical(
    item_log$rank_global_eap,
    as.integer(rank(-as.double(item_log$theta_global_eap), ties.method = "first"))
  )
})

test_that("lagged rank stability gate uses Spearman threshold of at least 0.98", {
  state <- make_linking_refit_state()
  ids <- c("h1", "h2", "s21", "s22")
  base <- c(h1 = 4, h2 = 3, s21 = 2, s22 = 1)
  same <- c(h1 = 3.9, h2 = 2.9, s21 = 2.1, s22 = 1.1)
  swapped <- c(h1 = 1, h2 = 4, s21 = 2, s22 = 3)
  state$refit_meta$theta_mean_history <- list(base, same, swapped)

  pass <- pairwiseLLM:::.adaptive_link_rank_stability_lagged(
    state = state,
    active_ids = ids,
    stability_lag = 1L,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = 0,
    log_alpha_mean = NA_real_,
    lag_row = tibble::tibble(
      link_transform_mode = "shift_only",
      delta_spoke_mean = 0,
      log_alpha_spoke_mean = NA_real_
    )
  )
  expect_true(isTRUE(pass$lag_eligible))
  expect_false(is.na(pass$rho_rank_lagged))
  expect_false(isTRUE(pass$rho_rank_lagged_pass))

  state$refit_meta$theta_mean_history <- list(base, same)
  pass2 <- pairwiseLLM:::.adaptive_link_rank_stability_lagged(
    state = state,
    active_ids = ids,
    stability_lag = 1L,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = 0,
    log_alpha_mean = NA_real_,
    lag_row = tibble::tibble(
      link_transform_mode = "shift_only",
      delta_spoke_mean = 0,
      log_alpha_spoke_mean = NA_real_
    )
  )
  expect_true(isTRUE(pass2$lag_eligible))
  expect_true(isTRUE(pass2$rho_rank_lagged_pass))
  expect_true(pass2$rho_rank_lagged >= 0.98)
})

test_that("single-set mode does not emit linking stage rows", {
  state <- adaptive_rank_start(make_test_items(4), seed = 21L)
  rows <- pairwiseLLM:::.adaptive_link_stage_refit_rows(
    state = state,
    refit_id = 1L,
    refit_context = list(last_refit_step = 0L)
  )
  expect_equal(nrow(rows), 0L)
})

test_that("linking active-domain helper guard branches return typed NA outputs", {
  state <- make_linking_refit_state()

  mode <- pairwiseLLM:::.adaptive_link_transform_mode_for_spoke(
    controller = list(link_transform_mode = "auto", link_transform_mode_by_spoke = list(`2` = "bad")),
    spoke_id = 2L
  )
  expect_identical(mode, "shift_only")

  bad_draws <- state
  bad_draws$btl_fit$btl_posterior_draws <- NULL
  expect_true(is.na(pairwiseLLM:::.adaptive_link_reliability_active(bad_draws, c("h1", "h2"))))
  expect_true(is.na(pairwiseLLM:::.adaptive_link_reliability_active(state, c("missing_a", "missing_b"))))
  expect_true(is.na(pairwiseLLM:::.adaptive_link_reliability_active(state, c("h1"))))

  no_ts <- state
  no_ts$trueskill_state <- NULL
  expect_true(is.na(pairwiseLLM:::.adaptive_link_ts_btl_rank_spearman_active(no_ts, c("h1", "h2"))))
  expect_true(is.na(pairwiseLLM:::.adaptive_link_ts_btl_rank_spearman_active(state, c("missing_a", "missing_b"))))
  expect_true(is.na(
    pairwiseLLM:::.adaptive_link_ts_btl_rank_spearman_active(state, c("h1", "h2"), theta_mean = c(1, 2))
  ))
  theta_nonfinite <- setNames(c(NA_real_, 1), c("h1", "h2"))
  expect_true(is.na(
    pairwiseLLM:::.adaptive_link_ts_btl_rank_spearman_active(state, c("h1", "h2"), theta_mean = theta_nonfinite)
  ))
  theta_tied <- setNames(c(1, 1), c("h1", "h2"))
  state$trueskill_state$items$mu[state$trueskill_state$items$item_id %in% c("h1", "h2")] <- 1
  expect_true(is.na(
    pairwiseLLM:::.adaptive_link_ts_btl_rank_spearman_active(state, c("h1", "h2"), theta_mean = theta_tied)
  ))

  no_hist <- state
  no_hist$refit_meta$theta_mean_history <- list()
  lag_none <- pairwiseLLM:::.adaptive_link_rank_stability_lagged(
    no_hist,
    c("h1", "h2"),
    stability_lag = 2L,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = 0,
    log_alpha_mean = NA_real_,
    lag_row = tibble::tibble()
  )
  expect_false(isTRUE(lag_none$lag_eligible))
  expect_true(is.na(lag_none$rho_rank_lagged))

  bad_hist <- state
  bad_hist$refit_meta$theta_mean_history <- list(c(h1 = 1, h2 = 2), c(h1 = NA_real_, h2 = 3), c(h1 = 2, h2 = 1))
  lag_bad <- pairwiseLLM:::.adaptive_link_rank_stability_lagged(
    bad_hist,
    c("h1", "h2"),
    stability_lag = 1L,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = 0,
    log_alpha_mean = NA_real_,
    lag_row = tibble::tibble(
      link_transform_mode = "shift_only",
      delta_spoke_mean = 0,
      log_alpha_spoke_mean = NA_real_
    )
  )
  expect_true(isTRUE(lag_bad$lag_eligible))
  expect_false(isTRUE(lag_bad$rho_rank_lagged_pass))
  bad_hist2 <- state
  bad_hist2$refit_meta$theta_mean_history <- list(c(h1 = 1, h2 = 2), "bad", c(h1 = 2, h2 = 1))
  lag_bad2 <- pairwiseLLM:::.adaptive_link_rank_stability_lagged(
    bad_hist2,
    c("h1", "h2"),
    stability_lag = 1L,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = 0,
    log_alpha_mean = NA_real_,
    lag_row = tibble::tibble(
      link_transform_mode = "shift_only",
      delta_spoke_mean = 0,
      log_alpha_spoke_mean = NA_real_
    )
  )
  expect_true(isTRUE(lag_bad2$lag_eligible))
  expect_false(isTRUE(lag_bad2$rho_rank_lagged_pass))
  bad_hist3 <- state
  bad_hist3$refit_meta$theta_mean_history <- list(c(h1 = 1, h2 = 2), c(h1 = 2, h2 = 1))
  lag_bad3 <- pairwiseLLM:::.adaptive_link_rank_stability_lagged(
    bad_hist3,
    c("h1", "missing"),
    stability_lag = 1L,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = 0,
    log_alpha_mean = NA_real_,
    lag_row = tibble::tibble(
      link_transform_mode = "shift_only",
      delta_spoke_mean = 0,
      log_alpha_spoke_mean = NA_real_
    )
  )
  expect_true(isTRUE(lag_bad3$lag_eligible))
  expect_false(isTRUE(lag_bad3$rho_rank_lagged_pass))

  short_hub <- state
  short_hub$btl_fit$theta_mean <- c(h1 = 1)
  expect_true(is.na(pairwiseLLM:::.adaptive_link_delta_sd_max_derived(short_hub, hub_id = 1L, delta_sd_mult = 0.1)))
  unnamed_hub <- state
  unnamed_hub$btl_fit$theta_mean <- c(1, 2)
  expect_true(is.na(pairwiseLLM:::.adaptive_link_delta_sd_max_derived(unnamed_hub, hub_id = 1L, delta_sd_mult = 0.1)))
})

test_that("active-domain TS-BTL correlation is computed on active items only", {
  state <- make_linking_refit_state()
  ts <- state$trueskill_state$items
  ts$mu[match(c("h1", "h2", "s21", "s22", "s31", "s32"), ts$item_id)] <- c(6, 5, 4, 3, 2, 1)
  state$trueskill_state$items <- ts

  theta <- c(h1 = 0.1, h2 = 0.2, s21 = 0.9, s22 = 1.0, s31 = 0.8, s32 = 0.7)
  active_ids <- c("h1", "h2", "s21", "s22")
  rho_active <- pairwiseLLM:::.adaptive_link_ts_btl_rank_spearman_active(
    state = state,
    active_ids = active_ids,
    theta_mean = theta
  )
  rho_full <- pairwiseLLM:::.adaptive_link_ts_btl_rank_spearman_active(
    state = state,
    active_ids = names(theta),
    theta_mean = theta
  )
  expect_equal(rho_active, -1, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(rho_active, rho_full, tolerance = 1e-12)))
})

test_that("transformed-domain helper and reconstruction guard branches are covered", {
  state <- make_linking_refit_state()

  empty_theta <- pairwiseLLM:::.adaptive_link_transform_theta_mean_for_spoke(
    state = state,
    theta_mean = c(1, 2),
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = 0.2
  )
  expect_identical(length(empty_theta), 0L)

  theta <- c(h1 = 1, s21 = 2, s31 = 3)
  transformed_bad_mode <- pairwiseLLM:::.adaptive_link_transform_theta_mean_for_spoke(
    state = state,
    theta_mean = theta,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "bad_mode",
    delta_mean = 0.2
  )
  expect_equal(transformed_bad_mode[["h1"]], 1, tolerance = 1e-12)
  expect_equal(transformed_bad_mode[["s21"]], 2.2, tolerance = 1e-12)
  expect_false("s31" %in% names(transformed_bad_mode))

  transformed_bad_alpha <- pairwiseLLM:::.adaptive_link_transform_theta_mean_for_spoke(
    state = state,
    theta_mean = theta,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_scale",
    delta_mean = 0.2,
    log_alpha_mean = NA_real_
  )
  expect_true(all(is.na(transformed_bad_alpha)))

  active <- pairwiseLLM:::.adaptive_link_active_item_ids(state, spoke_id = 2L, hub_id = 1L)
  rel_bad_mode <- pairwiseLLM:::.adaptive_link_reliability_transformed_active(
    state = state,
    active_ids = active$active_all,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "bad_mode",
    delta_mean = 0.1
  )
  expect_true(is.finite(rel_bad_mode))
  rel_bad_delta <- pairwiseLLM:::.adaptive_link_reliability_transformed_active(
    state = state,
    active_ids = active$active_all,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = NA_real_
  )
  expect_true(is.na(rel_bad_delta))
  rel_bad_alpha <- pairwiseLLM:::.adaptive_link_reliability_transformed_active(
    state = state,
    active_ids = active$active_all,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_scale",
    delta_mean = 0.1,
    log_alpha_mean = NA_real_
  )
  expect_true(is.na(rel_bad_alpha))

  lag_state <- state
  lag_state$refit_meta$theta_mean_history <- list(c(h1 = 1, h2 = 2), "bad")
  lag <- pairwiseLLM:::.adaptive_link_rank_stability_lagged(
    state = lag_state,
    active_ids = c("h1", "h2"),
    stability_lag = 1L,
    spoke_id = 2L,
    hub_id = 1L,
    transform_mode = "shift_only",
    delta_mean = 0.1,
    lag_row = tibble::tibble(
      link_transform_mode = "shift_only",
      delta_spoke_mean = 0.1,
      log_alpha_spoke_mean = NA_real_
    )
  )
  expect_true(isTRUE(lag$lag_eligible))
  expect_false(isTRUE(lag$rho_rank_lagged_pass))

  expect_error(
    pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
      link_row = tibble::tibble(link_stop_eligible = c(TRUE, TRUE)),
      diagnostics_pass = TRUE,
      hub_theta_sd = 1,
      controller = list()
    ),
    "exactly one row"
  )
  expect_false(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = tibble::tibble(link_stop_eligible = FALSE),
    diagnostics_pass = TRUE,
    hub_theta_sd = 1,
    controller = list()
  )))
  expect_false(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_stop_from_logs(
    link_row = tibble::tibble(link_stop_eligible = TRUE),
    diagnostics_pass = FALSE,
    hub_theta_sd = 1,
    controller = list()
  )))

  expect_error(
    pairwiseLLM:::.adaptive_link_reconstruct_identified_from_logs(
      link_row = tibble::tibble(link_transform_mode = c("shift_only", "shift_only")),
      controller = list()
    ),
    "exactly one row"
  )
  expect_false(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_identified_from_logs(
    link_row = tibble::tibble(
      link_transform_mode = "shift_scale",
      reliability_EAP_link = 0.95,
      ts_btl_rank_spearman = 0.95
    ),
    controller = list(link_identified_reliability_min = 0.80, link_rank_corr_min = 0.90)
  )))
  expect_false(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_identified_from_logs(
    link_row = tibble::tibble(
      link_transform_mode = "shift_scale",
      reliability_EAP_link = 0.95,
      ts_btl_rank_spearman = 0.95,
      delta_sd_pass = TRUE
    ),
    controller = list(link_identified_reliability_min = 0.80, link_rank_corr_min = 0.90)
  )))
  expect_true(isTRUE(pairwiseLLM:::.adaptive_link_reconstruct_identified_from_logs(
    link_row = tibble::tibble(
      link_transform_mode = "shift_only",
      reliability_EAP_link = 0.95,
      ts_btl_rank_spearman = 0.95,
      delta_sd_pass = TRUE
    ),
    controller = list(link_identified_reliability_min = 0.80, link_rank_corr_min = 0.90)
  )))
})

test_that("phase-specific judge scope uses explicit phase boundary metadata before first cross-set row", {
  state <- make_linking_refit_state(
    list(run_mode = "link_multi_spoke", hub_id = 1L, judge_param_mode = "phase_specific")
  )
  state$linking$phase_a$phase <- "phase_b"
  state$linking$phase_a$phase_b_started_at_step <- 1L

  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(
      step_id = 1L,
      pair_id = 1L,
      i = 1L,
      j = 2L,
      A = 1L,
      B = 2L,
      Y = 1L,
      set_i = 1L,
      set_j = 1L,
      is_cross_set = FALSE,
      link_spoke_id = NA_integer_,
      run_mode = "link_one_spoke",
      link_stage = "local_link"
    )
  )

  results <- pairwiseLLM:::.adaptive_results_from_step_log(state)
  expect_true(nrow(results) == 1L)
  expect_identical(results$phase[[1L]], "phase3")
  expect_identical(results$judge_scope[[1L]], "link")
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
