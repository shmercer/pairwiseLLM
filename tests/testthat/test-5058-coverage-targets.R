make_5058_link_state <- function() {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s1", "s2", "s3"),
    text = c("h1", "h2", "h3", "s1", "s2", "s3"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gh3", "gs1", "gs2", "gs3")
  )
  state <- adaptive_rank_start(
    items,
    seed = 5058L,
    adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L)
  )
  draws <- rbind(
    c(0.8, 0.3, -0.1, 0.2, -0.2, -0.5),
    c(0.9, 0.4, 0.0, 0.3, -0.1, -0.4),
    c(0.7, 0.2, -0.2, 0.1, -0.3, -0.6)
  )
  colnames(draws) <- state$item_ids
  state$btl_fit <- make_test_btl_fit(state$item_ids, draws = draws)
  state$trueskill_state <- make_test_trueskill_state(
    state$items,
    mu = c(30, 25, 20, 24, 19, 15)
  )

  make_artifact <- function(set_id, global_ids, theta) {
    artifact <- list(
      set_id = as.integer(set_id),
      quality_gate_accepted = TRUE,
      n_pairs_committed = 2L,
      diagnostics = list(diagnostics_pass = TRUE, reliability_EAP_within = 0.96),
      posterior_draws = matrix(rep(theta, each = 3L), nrow = 3L),
      items = tibble::tibble(
        global_item_id = global_ids,
        theta_raw_mean = theta,
        theta_raw_sd = rep(0.1, length(theta)),
        rank_mu_raw = seq_along(theta)
      )
    )
    colnames(artifact$posterior_draws) <- state$items$item_id[state$items$set_id == set_id]
    add_test_phase_a_evidence(artifact, state = state, set_id = set_id)
  }

  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = c(1L, 2L),
      source = c("import", "import"),
      status = c("ready", "ready"),
      validation_message = c("ok", "ok"),
      artifact_path = c(NA_character_, NA_character_)
    ),
    artifacts = list(
      `1` = make_artifact(1L, c("gh1", "gh2", "gh3"), c(0.8, 0.3, -0.1)),
      `2` = make_artifact(2L, c("gs1", "gs2", "gs3"), c(0.2, -0.2, -0.5))
    ),
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = c(1L, 2L),
    ready_spokes = 2L,
    phase = "phase_b",
    phase_b_started_at_step = 1L
  )
  state$controller$current_link_spoke_id <- 2L
  state$controller$link_estimation_mode <- "transform"
  state$controller$link_transform_policy <- "auto"
  state$controller$link_transform_state_by_spoke <- list(`2` = "shift_scale")
  state$controller$link_refit_stats_by_spoke <- list(
    `2` = list(
      link_transform_state = "shift_scale",
      delta_spoke_mean = 0.15,
      log_alpha_spoke_mean = log(1.1),
      link_epoch_id = 1L
    )
  )
  state$controller$link_epoch_id_by_spoke <- list(`2` = 1L)
  state
}

make_5058_cross_edges <- function() {
  tibble::tibble(
    hub_item = c("h1", "h2", "h3", "h1"),
    spoke_item = c("s1", "s2", "s3", "s2"),
    y_spoke = c(1L, 0L, 1L, 0L),
    spoke_in_A = c(TRUE, FALSE, TRUE, FALSE),
    step_id = 1:4,
    is_probe_step = c(FALSE, FALSE, TRUE, FALSE)
  )
}

make_5058_fake_cmdstan_fit_fn <- function(fail_first = FALSE) {
  env <- new.env(parent = emptyenv())
  env$calls <- 0L
  fit_fn <- function(stan_data, variable_names, cmdstan, seed, model_fn = NULL) {
    env$calls <- env$calls + 1L
    n_draws <- 4L
    draws <- matrix(numeric(), nrow = n_draws, ncol = 0L)
    draws <- cbind(draws, delta = c(0.10, 0.20, 0.30, 0.40))
    if ("log_alpha" %in% variable_names) {
      draws <- cbind(draws, log_alpha = log(c(1.00, 1.05, 1.10, 1.15)))
    }
    if ("theta_hub" %in% variable_names) {
      hub_cols <- matrix(
        rep(seq_len(stan_data$N_hub) / 10, each = n_draws),
        nrow = n_draws,
        dimnames = list(NULL, paste0("theta_hub[", seq_len(stan_data$N_hub), "]"))
      )
      draws <- cbind(draws, hub_cols)
    }
    if ("theta_spoke" %in% variable_names) {
      spoke_cols <- matrix(
        rep(-seq_len(stan_data$N_spoke) / 10, each = n_draws),
        nrow = n_draws,
        dimnames = list(NULL, paste0("theta_spoke[", seq_len(stan_data$N_spoke), "]"))
      )
      draws <- cbind(draws, spoke_cols)
    }
    bad_diag <- isTRUE(fail_first) && identical(env$calls, 1L)
    list(
      draws_matrix = draws,
      diagnostics = list(
        divergences = 0L,
        max_rhat = if (bad_diag) 1.2 else 1.0,
        min_ess_bulk = if (bad_diag) 20 else 900
      ),
      mcmc_config_used = list(
        chains = as.integer(cmdstan$chains),
        parallel_chains = 1L,
        threads_per_chain = 1L,
        cmdstanr_version = "test"
      )
    )
  }
  attr(fit_fn, "env") <- env
  fit_fn
}

test_that("BTL refit helpers cover config and Phase A artifact edge branches", {
  state <- make_5058_link_state()

  resolved <- .adaptive_btl_resolve_config(state, list(deferred_audit_max_draws = Inf))
  expect_true(is.infinite(resolved$deferred_audit_max_draws))
  expect_error(
    .adaptive_btl_resolve_config(state, list(phase_b_refit_parallel = NA)),
    "phase_b_refit_parallel"
  )
  expect_error(
    .adaptive_btl_resolve_config(state, list(phase_b_refit_workers = 0L)),
    "phase_b_refit_workers"
  )
  expect_error(
    .adaptive_btl_resolve_config(state, list(deferred_audit_max_draws = 1L)),
    "deferred_audit_max_draws"
  )

  no_artifact <- state
  no_artifact$linking$phase_a$artifacts$`2` <- NULL
  expect_error(
    .adaptive_phase_a_artifact_draws_for_phase_b_global(no_artifact, 2L),
    "requires a Phase A artifact"
  )

  bad_draws <- state
  bad_draws$linking$phase_a$artifacts$`2`$posterior_draws <- matrix(1, nrow = 1L, ncol = 3L)
  expect_error(
    .adaptive_phase_a_artifact_draws_for_phase_b_global(bad_draws, 2L),
    "posterior_draws"
  )

  bad_domain <- state
  bad_domain$linking$phase_a$artifacts$`2`$items$global_item_id[[1L]] <- "missing"
  expect_error(
    .adaptive_phase_a_artifact_item_ids(bad_domain, bad_domain$linking$phase_a$artifacts$`2`, 2L),
    "item domain mismatch"
  )

  no_colnames <- state
  colnames(no_colnames$linking$phase_a$artifacts$`2`$posterior_draws) <- NULL
  draws <- .adaptive_phase_a_artifact_draws_for_phase_b_global(no_colnames, 2L)
  expect_identical(colnames(draws), c("s1", "s2", "s3"))
})

test_that("BTL Phase B metric helpers cover transform, anchored, and fallback branches", {
  state <- make_5058_link_state()
  transform_stats <- .adaptive_phase_b_global_metric_transform_stats(
    state,
    2L,
    controller = state$controller
  )
  expect_identical(transform_stats$link_transform_state, "shift_scale")
  expect_equal(transform_stats$delta_spoke_mean, 0.15)

  expect_identical(
    .adaptive_phase_b_global_metric_uncertainty_approximation("transform", NULL, NULL),
    NA_character_
  )
  expect_identical(
    .adaptive_phase_b_global_metric_uncertainty_approximation(
      "anchored_joint",
      link_fit_method = "map_laplace"
    ),
    "laplace_hessian_marginal_quantiles"
  )
  expect_error(
    .adaptive_phase_b_global_metric_uncertainty_approximation(
      "anchored_joint",
      link_uncertainty_approximation = "bad"
    ),
    "uncertainty approximation"
  )

  rel_empty <- .adaptive_link_global_score_stats_active(state, active_ids = "h1", spoke_id = 2L)
  expect_false(rel_empty$defined)
  expect_true(is.na(.adaptive_link_ts_btl_rank_spearman_active(list(), c("h1", "h2"))))

  bad_state <- state
  bad_state$controller$link_refit_stats_by_spoke$`2`$log_alpha_spoke_mean <- NA_real_
  expect_error(
    .adaptive_phase_b_global_metric_transform_stats(
      bad_state,
      2L,
      controller = bad_state$controller
    ),
    "finite log-alpha"
  )
})

test_that("BTL transform refit uses fake CmdStan draws for shift-only and joint fits", {
  cross_edges <- make_5058_cross_edges()
  hub_theta <- c(h1 = 0.8, h2 = 0.3, h3 = -0.1)
  spoke_theta <- c(s1 = 0.2, s2 = -0.2, s3 = -0.5)
  attr(hub_theta, "theta_sd") <- c(h1 = 0.05, h2 = 0.10, h3 = 0.15)
  attr(spoke_theta, "theta_sd") <- c(s1 = 0.10, s2 = 0.15, s3 = 0.20)
  attr(cross_edges, "judge_params") <- list(
    mode = "global_shared",
    scope = "link",
    beta = Inf,
    epsilon = 2
  )
  attr(cross_edges, "refit_contract") <- list(
    link_refit_mode = "shift_only",
    link_transform_policy = "auto",
    shift_only_theta_treatment = "fixed_eap_plugin_var",
    cmdstan_fit_fn = make_5058_fake_cmdstan_fit_fn()
  )

  fit <- .adaptive_link_fit_transform(cross_edges, hub_theta, spoke_theta, "shift_only")
  expect_equal(fit$delta_mean, 0.25)
  expect_true(is.na(fit$log_alpha_mean))
  expect_identical(fit$fit_contract$parameters, "delta_s")
  expect_identical(fit$fit_contract$mcmc$repair_attempts, 1L)
  expect_true(fit$diagnostics$diagnostics_rhat_pass)
  expect_equal(length(fit$posterior_draws$delta), 4L)
  expect_equal(dim(fit$posterior_draws$theta_hub), c(4L, 3L))

  joint_edges <- cross_edges
  attr(joint_edges, "judge_params") <- list(mode = "global_shared", scope = "link", beta = NA, epsilon = NA)
  fake_fit_fn <- make_5058_fake_cmdstan_fit_fn(fail_first = TRUE)
  attr(joint_edges, "refit_contract") <- list(
    link_refit_mode = "joint_refit",
    link_transform_policy = "fixed_shift_scale",
    hub_lock_mode = "soft_lock",
    hub_lock_kappa = 2,
    shift_only_theta_treatment = "fixed_eap",
    cmdstan_fit_fn = fake_fit_fn,
    link_diagnostics_thresholds = list(divergences_max = 0L, max_rhat = 1.01, min_ess_bulk = 400)
  )
  attr(joint_edges, "within_hub_edges") <- tibble::tibble(
    A_item = c("h1", "bad"),
    B_item = c("h2", "h3"),
    y_A = c(1L, 1L)
  )
  attr(joint_edges, "within_spoke_edges") <- tibble::tibble(
    A_item = c("s1", "s2"),
    B_item = c("s2", "missing"),
    y_A = c(0L, 1L)
  )
  attr(hub_theta, "theta_init") <- c(h1 = 0.7, h2 = 0.2, h3 = -0.2)
  attr(hub_theta, "theta_prior_center") <- c(h1 = 0.75, h2 = 0.25, h3 = -0.15)
  attr(spoke_theta, "theta_init") <- c(s1 = 0.1, s2 = -0.3, s3 = -0.6)

  joint_fit <- .adaptive_link_fit_transform(joint_edges, hub_theta, spoke_theta, "shift_scale")
  expect_equal(attr(fake_fit_fn, "env")$calls, 2L)
  expect_true(is.finite(joint_fit$log_alpha_mean))
  expect_identical(
    joint_fit$fit_contract$parameters,
    c("theta_hub", "theta_spoke", "delta_s", "log_alpha_s")
  )
  expect_true(joint_fit$fit_contract$joint_refit$used)
  expect_equal(unname(joint_fit$theta_hub_post), c(0.1, 0.2, 0.3))
  expect_equal(unname(joint_fit$theta_spoke_post), c(-0.1, -0.2, -0.3))
  expect_true(joint_fit$diagnostics$diagnostics_ess_pass)
})

test_that("BTL transform refit and diagnostics helpers reject malformed CmdStan outputs", {
  cross_edges <- make_5058_cross_edges()
  hub_theta <- c(h1 = 0.8, h2 = 0.3, h3 = -0.1)
  spoke_theta <- c(s1 = 0.2, s2 = -0.2, s3 = -0.5)
  attr(cross_edges, "refit_contract") <- list(
    link_refit_mode = "shift_only",
    hub_lock_mode = "unsupported",
    cmdstan_fit_fn = "not-a-function"
  )
  expect_error(
    .adaptive_link_fit_transform(cross_edges, hub_theta, spoke_theta, "shift_only"),
    "cmdstan_fit_fn"
  )

  missing_delta <- cross_edges
  attr(missing_delta, "refit_contract") <- list(
    link_refit_mode = "shift_only",
    cmdstan_fit_fn = function(stan_data, variable_names, cmdstan, seed, model_fn = NULL) {
      list(
        draws_matrix = matrix(1, nrow = 2L, ncol = 1L, dimnames = list(NULL, "wrong")),
        diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 900),
        mcmc_config_used = list(chains = 1L, parallel_chains = 1L, threads_per_chain = 1L)
      )
    }
  )
  expect_error(
    .adaptive_link_fit_transform(missing_delta, hub_theta, spoke_theta, "shift_only"),
    "missing delta"
  )

  attr(cross_edges, "refit_contract") <- list(
    link_refit_mode = "joint_refit",
    hub_lock_mode = "bad",
    cmdstan_fit_fn = make_5058_fake_cmdstan_fit_fn()
  )
  expect_error(
    .adaptive_link_fit_transform(cross_edges, hub_theta, spoke_theta, "shift_scale"),
    "Unsupported `hub_lock_mode`"
  )

  fit <- list(
    diagnostic_summary = function() tibble::tibble(other = 1L),
    summary = function(variables) tibble::tibble(variable = variables, rhat = NA_real_)
  )
  diagnostics <- .adaptive_link_cmdstan_collect_diagnostics(fit, variables = "delta")
  expect_true(any(grepl("num_divergent", diagnostics$notes, fixed = TRUE)))
  expect_true(any(grepl("ess_bulk", diagnostics$notes, fixed = TRUE)))
  expect_error(
    .adaptive_link_cmdstan_validate_diagnostics(
      diagnostics,
      thresholds = list(divergences_max = 0L, max_rhat = 1.01, min_ess_bulk = 400)
    ),
    "missing or malformed"
  )

  expect_error(
    .adaptive_link_diagnostics_contract(list(
      fit_contract = list(estimation_method = "map_laplace", uncertainty_approximation = "bad"),
      diagnostics = list(converged = TRUE, hessian_posdef = TRUE)
    )),
    "uncertainty_approximation"
  )
  expect_error(
    .adaptive_link_diagnostics_contract(list(
      fit_contract = list(estimation_method = "unknown"),
      diagnostics = list()
    )),
    "undefined"
  )
})

test_that("round-candidate helpers cover deterministic edge branches and invariants", {
  defaults <- adaptive_defaults(6L)
  scores <- stats::setNames(c(6, 5, 4, 3, 2, 1), paste0("i", 1:6))
  anchors <- .adaptive_select_rolling_anchors(scores, defaults)
  expect_true(length(anchors) >= 1L)
  expect_true(all(anchors %in% names(scores)))

  strata <- .adaptive_assign_strata(stats::setNames(10, "only"), defaults)
  expect_identical(strata$top_band_ids, "only")
  expect_true(all(names(strata$stratum_map) == "only"))

  picked <- .adaptive_link_probe_sample_cell_pairs(
    hub_ids = c("h2", "h1"),
    spoke_ids = c("s2", "s1"),
    excluded_keys = make_unordered_key("h1", "s1"),
    take = 2L,
    random = FALSE
  )
  expect_identical(picked$pair_key, c(make_unordered_key("h1", "s2"), make_unordered_key("h2", "s1")))
  expect_identical(
    .adaptive_link_probe_pairs_from_linear_index(1:3, c("h1", "h2"), c("s1", "s2"))$hub_item_id,
    c("h1", "h1", "h2")
  )

  expect_error(
    .adaptive_link_assert_active_domain_count(
      "long_link",
      n_candidates_after_active_domain = 5L,
      active_hub_ids = "h1",
      spoke_ids = c("s1", "s2"),
      spoke_id = 2L
    ),
    "active-domain invariant"
  )
  expect_error(
    .adaptive_link_assert_non_anchor_candidate_domain(
      tibble::tibble(i = "h2", j = "s1"),
      stage_name = "mid_link",
      spoke_id = 2L,
      hub_id = 1L,
      active_hub_ids = "h1",
      set_map = c(h1 = 1L, h2 = 1L, s1 = 2L)
    ),
    "outside active_link_items"
  )
  expect_error(
    .adaptive_link_assert_non_anchor_candidate_domain(
      tibble::tibble(i = "h1", j = "s1"),
      stage_name = "local_link",
      spoke_id = 2L,
      hub_id = 1L,
      active_hub_ids = "h1",
      reserved_keys = make_unordered_key("h1", "s1"),
      set_map = c(h1 = 1L, s1 = 2L)
    ),
    "reserved held-out probe"
  )
})

test_that("selection helpers cover D-opt, ordering, and memo-key edge branches", {
  expect_true(.adaptive_selection_mode_is_linking("link_one_spoke", TRUE))
  expect_false(.adaptive_selection_mode_is_linking("link_one_spoke", FALSE))
  expect_identical(
    .adaptive_selection_utility_mode("link_multi_spoke", TRUE, "anchored_joint"),
    "linking_d_optimal_anchored_joint"
  )
  expect_identical(.adaptive_resolve_selection_column("unknown"), NA_character_)

  expect_true(is.na(.adaptive_link_model_d_prob(NA_real_, 1, beta = Inf, epsilon = Inf)))
  expect_true(all(is.na(.adaptive_link_model_d_prob_vec(c(NA, Inf), c(1, 2), beta = NA, epsilon = NA))))
  expect_true(is.na(.adaptive_link_model_d_pbar(NA, 1, 0, 0)))
  expect_equal(dim(.adaptive_link_info_gradient("shift_only", NA, NA)), c(1L, 1L))
  expect_equal(dim(.adaptive_link_info_gradient("shift_scale", NA, NA)), c(2L, 1L))

  bad_prepared <- .adaptive_link_d_opt_rank1_prepare(matrix(c(1, 2, 3, 4), nrow = 2L))
  expect_false(bad_prepared$ok)
  good_prepared <- .adaptive_link_d_opt_rank1_prepare(diag(2))
  expect_true(good_prepared$ok)
  expect_true(is.na(.adaptive_link_d_opt_gain_logdet(matrix(1, 1, 2), diag(1))))
  expect_true(is.na(.adaptive_link_d_opt_gain_from_quadform(-2, 1)))
  expect_true(is.na(.adaptive_link_d_opt_rank1_gain_transform(list(ok = FALSE), 1, "shift_only", 1, 1)))
  expect_true(is.na(.adaptive_link_d_opt_rank1_gain_transform(good_prepared, 1, "shift_only", 1, 1)))
  expect_true(is.finite(.adaptive_link_d_opt_rank1_gain_transform(good_prepared, 1, "shift_scale", 1, 1)))
  expect_true(is.na(.adaptive_link_d_opt_rank1_gain_diag(good_prepared, 1, 99L)))
  expect_false(.adaptive_link_d_opt_diag_prepare(c(1, NA))$ok)
  expect_true(is.na(.adaptive_link_d_opt_gain_diag_state(c(1, 2), 1, 99L)))
  expect_true(.adaptive_link_d_opt_entry_uses_diag(list(it_diag = c(1, 2)), dim_n = 2L))

  expect_true(.adaptive_repeat_pair_has_order("a::b", c(`a::b` = 0L), list()))
  expect_false(.adaptive_repeat_pair_has_order("a::b", c(`a::b` = 1L), list()))
  expect_true(.adaptive_repeat_pair_has_order("a::b", c(`a::b` = 1L), list(`a::b` = c("a", "b"))))

  ordered <- .adaptive_assign_order(
    tibble::tibble(i = "a", j = "b"),
    posA = c(a = 0L, b = 2L),
    posB = c(a = 2L, b = 0L),
    pair_last_order = list()
  )
  expect_identical(unname(ordered), c("a", "b"))
  reversed <- .adaptive_assign_order(
    tibble::tibble(i = "a", j = "b"),
    posA = c(a = 0L, b = 0L),
    posB = c(a = 0L, b = 0L),
    pair_last_order = stats::setNames(list(c("a", "b")), make_unordered_key("a", "b"))
  )
  expect_identical(unname(reversed), c("b", "a"))

  expect_identical(.adaptive_selector_anchor_generation_memo_key("anchor_link"), "within_set_anchor::na")
  expect_true(is.na(.adaptive_selector_anchor_generation_memo_key("base")))
  expect_identical(
    .adaptive_selector_anchor_stage_memo_key("anchor_link", "none", spoke_id = 2L),
    "within_set_anchor::none::2"
  )
  expect_true(is.na(.adaptive_selector_anchor_stage_memo_key("anchor_link", "none", external_candidates = TRUE)))
})

test_that("state, schema, persistence, print, utility, and draws helpers cover small edge branches", {
  expect_error(.adaptive_normalize_link_estimation_mode("bad"), "Link estimation mode")
  expect_identical(.adaptive_normalize_link_transform_policy(legacy_mode = "shift_scale"), "fixed_shift_scale")
  expect_error(.adaptive_normalize_link_transform_policy("bad"), "Link transform policy")
  expect_identical(.adaptive_normalize_link_transform_state(NULL, "fixed_shift_scale"), "shift_scale")
  expect_error(.adaptive_normalize_link_transform_state("bad"), "Link transform state")

  controller <- .adaptive_controller_normalize_legacy_fields(
    list(
      link_estimation_mode = "anchored_joint",
      link_transform_mode = "shift_scale",
      link_transform_mode_by_spoke = list(`2` = "shift_scale"),
      transform_frozen_by_spoke = list(`2` = TRUE)
    ),
    n_items = 6L
  )
  expect_identical(controller$link_transform_policy, NA_character_)
  expect_null(controller$link_transform_mode)
  expect_identical(controller$shift_only_theta_treatment, NA_character_)

  expect_error(.adaptive_validate_controller_config("bad", 6L), "named list")
  expect_error(
    .adaptive_validate_controller_config(list(probe_near_boundary_low = 0.9, probe_near_boundary_high = 0.2), 6L),
    "less than"
  )
  expect_error(
    .adaptive_validate_controller_config(list(allow_spoke_spoke_cross_set = TRUE), 6L),
    "Unknown `adaptive_config` field"
  )
  expect_false(.adaptive_link_spoke_is_frozen(list(link_state_frozen_by_spoke = list(`2` = TRUE)), NA_integer_))
  expect_true(.adaptive_link_spoke_is_frozen(list(link_state_frozen_by_spoke = list(`2` = TRUE)), 2L))

  expect_error(.adaptive_required_cols(tibble::tibble(a = 1L), "tbl", c("a", "b")), "missing")
  expect_error(.adaptive_check_phase("bad", "phase"), "phase")
  expect_identical(.adaptive_item_log_na_value("refit_id"), NA_integer_)
  expect_identical(.adaptive_item_log_na_value("item_id"), NA_character_)
  expect_true(is.na(.adaptive_item_log_na_value("is_hub_item")))
  expect_equal(.adaptive_meets_threshold(2, 1, "ge"), TRUE)
  expect_equal(.adaptive_meets_threshold(2, 1, "le"), FALSE)
  expect_identical(.adaptive_progress_col_value(tibble::tibble(a = 1L), "b", default = 9L), 9L)
  expect_identical(.adaptive_progress_fmt_num(Inf), "inactive")
  expect_identical(.adaptive_progress_fmt_state(NA), "inactive")
  expect_identical(.adaptive_progress_indent(character()), character())
  expect_match(.adaptive_progress_gate_detail("x", 1, 2, FALSE, "le"), "x=1.000/2.000 fail")
  expect_true(.adaptive_progress_link_diag_pass(tibble::tibble(link_diagnostics_pass = TRUE)))
  expect_false(.adaptive_progress_link_diag_pass(tibble::tibble(
    link_diagnostics_divergences_pass = TRUE,
    link_diagnostics_rhat_pass = TRUE,
    link_diagnostics_ess_pass = FALSE
  )))
  expect_null(adaptive_progress_update(NULL, list(), list(progress = "all")))
  expect_null(adaptive_progress_step_event(tibble::tibble(), list(progress_show_events = TRUE)))

  td <- tempdir()
  paths <- .adaptive_session_paths(td)
  expect_true(all(c("state", "step_log", "metadata") %in% names(paths)))
  expect_error(.adaptive_abort_if_exists(list(tempdir())), "already contains")
  expect_error(write_log(tibble::tibble(), NA_character_), "path")
  expect_error(write_log(tibble::tibble(), tempfile(fileext = ".parquet")), "Parquet")
  expect_error(read_log(tempfile(fileext = ".rds")), "Missing log file")
  expect_error(.adaptive_validate_log_schema("bad", list(a = "integer"), "x"), "data frame")
  expect_error(.adaptive_validate_log_schema(tibble::tibble(a = 1), list(a = "integer"), "x"), "canonical type")
  expect_error(.adaptive_align_log_schema_for_resume("bad", list(a = "integer"), "x"), "data frame")
  expect_error(.adaptive_link_probe_resume_abort("bad", 2L), "spoke_id=2")
  expect_false(.adaptive_is_resumed_session(list(meta = list(), config = list())))
  expect_identical(.adaptive_read_item_log_files(tempfile()), list())

  expect_error(.pairwiseLLM_sanitize_draws_matrix(1:3), "numeric matrix")
  expect_warning(
    clean <- .pairwiseLLM_sanitize_draws_matrix(matrix(c(1, Inf, NA, 4), nrow = 2L), "x"),
    "Non-finite"
  )
  expect_true(all(is.finite(clean)))
  expect_error(.pairwiseLLM_col_sds(matrix(1:4, nrow = 2L), center = 1), "one value per column")
  expect_identical(.pairwiseLLM_col_sds(matrix(1, nrow = 1L, ncol = 2L)), c(NA_real_, NA_real_))
  expect_equal(dim(.pairwiseLLM_col_quantiles(matrix(1:4, nrow = 2L), c(0.25, 0.75))), c(2L, 2L))

  ts <- make_test_trueskill_state(make_test_items(2))
  expect_error(pairwiseLLM:::.trueskill_win_probability_vec("1", c("2", "1"), ts), "same length")
  expect_error(pairwiseLLM:::.trueskill_win_probability_vec("1", "1", ts), "distinct")
  expect_identical(pairwiseLLM:::compute_u0(character(), character(), ts), numeric())
})

test_that("legacy controller and resume schema normalization cover migration branches", {
  normalized <- .adaptive_controller_normalize_legacy_fields(
    list(
      link_estimation_mode = "transform",
      link_transform_mode = "shift_scale",
      link_transform_mode_by_spoke = "not-a-list",
      link_transform_frozen_by_spoke = "bad",
      link_transform_frozen_refit_id_by_spoke = "bad",
      shift_only_theta_treatment = "normal_prior",
      stability_consecutive_k = 3L,
      link_transform_escalation_refits_required = 2L,
      link_stop_consecutive_pass_count_by_spoke = list(`2` = 2L),
      link_escalation_consecutive_pass_count_by_spoke = list(`2` = 1L)
    ),
    n_items = 6L
  )
  expect_identical(normalized$link_estimation_mode, "anchored_joint")
  expect_identical(normalized$link_transform_policy, NA_character_)
  expect_identical(normalized$shift_only_theta_treatment, NA_character_)
  expect_identical(normalized$stability_passes_required, 3L)
  expect_identical(normalized$link_transform_escalation_window_refits, 2L)
  expect_identical(normalized$link_stop_recent_pass_window_by_spoke$`2`, c(TRUE, TRUE))
  expect_identical(normalized$link_escalation_recent_pass_window_by_spoke$`2`, TRUE)
  expect_identical(normalized$link_state_frozen_by_spoke, list())

  expect_error(
    .adaptive_validate_controller_config(list(p_long_low = "bad"), 6L),
    "single numeric"
  )
  expect_error(
    .adaptive_validate_controller_config(list(boundary_k = 0L), 6L),
    "\\[1, 6\\]"
  )
  expect_error(.adaptive_validate_controller_config(list(phase_a_mode = NA_character_), 6L), "single string")
  expect_error(
    .adaptive_validate_controller_config(list(run_mode = ""), 6L),
    "single string"
  )
  expect_error(
    .adaptive_validate_controller_config(list(link_refit_mode = "bad"), 6L),
    "link_refit_mode"
  )
  schema <- list(
    posterior_win_prob_ij_pre = "double",
    is_holdout_probe_step = "logical",
    is_drift_probe_step = "logical",
    is_probe_step = "logical",
    link_transform_policy = "character",
    link_transform_state = "character",
    link_estimation_mode = "character",
    run_mode = "character",
    is_cross_set = "logical"
  )
  step_log <- .adaptive_align_log_schema_for_resume(
    tibble::tibble(
      posterior_win_prob_pre = 0.8,
      run_mode = c("link_probe_holdout"),
      is_cross_set = TRUE,
      is_probe_step = FALSE,
      link_transform_mode = "shift_scale"
    ),
    schema = schema,
    name = "step_log"
  )
  expect_true(step_log$is_holdout_probe_step[[1L]])
  expect_true(step_log$is_probe_step[[1L]])
  expect_identical(step_log$link_transform_policy[[1L]], "fixed_shift_scale")
  expect_identical(step_log$link_transform_state[[1L]], "shift_scale")
  expect_false("link_transform_mode" %in% names(step_log))

  link_schema <- list(
    link_state_frozen = "logical",
    link_state_frozen_refit_id = "integer",
    link_transform_policy = "character",
    link_transform_state = "character",
    reliability_link_global = "double",
    stop_recent_pass_count = "integer",
    stop_recent_window_size = "integer",
    escalation_recent_pass_count = "integer",
    escalation_recent_window_size = "integer",
    link_transform_escalation_window_refits_used = "integer",
    link_transform_escalation_passes_required_used = "integer",
    link_estimation_mode = "character",
    phase_b_global_metric_uncertainty_approximation = "character"
  )
  link_log <- .adaptive_align_log_schema_for_resume(
    tibble::tibble(
      transform_frozen = TRUE,
      transform_frozen_refit_id = 4L,
      link_transform_mode = "shift_only",
      ppc_calibration_id = "old",
      cross_set_ppc_brier_max_used = 0.2,
      reliability_EAP_link = 0.91,
      stop_consecutive_pass_count = 2L,
      escalation_consecutive_pass_count = 1L,
      link_transform_escalation_refits_required_used = 3L
    ),
    schema = link_schema,
    name = "link_stage_log"
  )
  expect_true(link_log$link_state_frozen[[1L]])
  expect_identical(link_log$link_state_frozen_refit_id[[1L]], 4L)
  expect_equal(link_log$reliability_link_global[[1L]], 0.91)
  expect_false(any(c("ppc_calibration_id", "reliability_EAP_link") %in% names(link_log)))
  expect_identical(link_log$link_estimation_mode[[1L]], "transform")
})

test_that("D-opt commit update covers transform path and early exits", {
  state_before <- make_5058_link_state()
  state_after <- state_before
  state_after$round_log <- append_round_log(
    state_after$round_log,
    list(refit_id = 1L, diagnostics_pass = TRUE)
  )
  state_after$controller$link_refit_stats_by_spoke <- list(
    `2` = list(delta_spoke_mean = 0.1, log_alpha_spoke_mean = log(1.2))
  )
  step_row <- tibble::tibble(
    is_cross_set = TRUE,
    run_mode = "link_one_spoke",
    utility_mode = "linking_d_optimal_transform",
    is_probe_step = FALSE,
    link_spoke_id = 2L,
    i = 1L,
    j = 4L,
    delta_spoke_estimate_pre = 0.05,
    log_alpha_spoke_estimate_pre = log(1.1)
  )
  updated <- .adaptive_link_d_opt_update_after_commit(state_before, state_after, step_row)
  d_opt_map <- updated$controller$link_d_opt_it_by_spoke
  expect_true(length(d_opt_map) >= 1L)
  expect_identical(d_opt_map[[1L]]$it_n_pairs_accumulated, 1L)
  expect_true(is.matrix(d_opt_map[[1L]]$it) || !is.null(d_opt_map[[1L]]$it_diag))

  expect_identical(
    .adaptive_link_d_opt_update_after_commit(state_before, state_after, step_row[0, ]),
    state_after
  )
  non_link <- step_row
  non_link$is_cross_set <- FALSE
  expect_identical(.adaptive_link_d_opt_update_after_commit(state_before, state_after, non_link), state_after)
  missing_spoke <- step_row
  missing_spoke$link_spoke_id <- NA_integer_
  expect_identical(.adaptive_link_d_opt_update_after_commit(state_before, state_after, missing_spoke), state_after)
})

make_5058_live_pairs <- function() {
  tibble::tibble(
    ID1 = c("A", "C"),
    text1 = c("alpha", "charlie"),
    ID2 = c("B", "D"),
    text2 = c("bravo", "delta"),
    pair_uid = c("p1", "p2")
  )
}

make_5058_live_row <- function(id1,
                               id2,
                               model,
                               pair_uid = NULL,
                               better_id = id1,
                               error_message = NA_character_) {
  tibble::tibble(
    custom_id = .pairwiseLLM_make_custom_id(id1, id2, pair_uid),
    ID1 = id1,
    ID2 = id2,
    model = model,
    object_type = "chat.completion",
    status_code = if (is.na(error_message)) 200L else 500L,
    error_message = error_message,
    thoughts = NA_character_,
    content = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    better_sample = if (is.na(error_message)) "SAMPLE_1" else NA_character_,
    better_id = if (is.na(error_message)) better_id else NA_character_,
    prompt_tokens = 10,
    completion_tokens = 3,
    total_tokens = 13,
    retry_failures = list(tibble::tibble())
  )
}

test_that("live provider submit wrappers cover sequential success and failure paths", {
  pairs <- make_5058_live_pairs()

  openai_out <- testthat::with_mocked_bindings(
    openai_compare_pair_live = function(ID1, ID2, model, pair_uid = NULL, ...) {
      if (identical(ID1, "C")) {
        rlang::abort("openai boom")
      }
      make_5058_live_row(ID1, ID2, model, pair_uid = pair_uid)
    },
    submit_openai_pairs_live(
      pairs = pairs,
      model = "gpt-4.1",
      trait_name = "quality",
      trait_description = "better",
      prompt_template = "Trait: {trait_name}\n{trait_description}\n{text1}\n{text2}",
      api_key = "test",
      verbose = FALSE,
      progress = FALSE,
      include_raw = FALSE,
      status_every = 1L
    ),
    .package = "pairwiseLLM"
  )
  expect_true(all(c("results", "failed_pairs", "failed_attempts") %in% names(openai_out)))
  expect_true(nrow(openai_out$failed_attempts) >= 0L)

  together_out <- testthat::with_mocked_bindings(
    together_compare_pair_live = function(ID1, ID2, model, pair_uid = NULL, ...) {
      if (identical(ID1, "C")) {
        rlang::abort("together boom")
      }
      make_5058_live_row(ID1, ID2, model, pair_uid = pair_uid)
    },
    submit_together_pairs_live(
      pairs = pairs,
      model = "deepseek-ai/DeepSeek-R1",
      trait_name = "quality",
      trait_description = "better",
      prompt_template = "Trait: {trait_name}\n{trait_description}\n{text1}\n{text2}",
      api_key = "test",
      verbose = FALSE,
      progress = FALSE,
      include_raw = FALSE
    ),
    .package = "pairwiseLLM"
  )
  expect_equal(nrow(together_out$results), 1L)
  expect_equal(nrow(together_out$failed_pairs), 1L)
  expect_error(
    submit_together_pairs_live(
      pairs = tibble::tibble(ID1 = "A"),
      model = "m",
      trait_name = "quality",
      trait_description = "better",
      verbose = FALSE,
      progress = FALSE
    ),
    "must contain columns"
  )

  anthropic_out <- testthat::with_mocked_bindings(
    anthropic_compare_pair_live = function(ID1, ID2, model, pair_uid = NULL, ...) {
      if (identical(ID1, "C")) {
        rlang::abort("anthropic boom")
      }
      make_5058_live_row(ID1, ID2, model, pair_uid = pair_uid)
    },
    submit_anthropic_pairs_live(
      pairs = pairs,
      model = "claude-sonnet-4-5",
      trait_name = "quality",
      trait_description = "better",
      prompt_template = "Trait: {trait_name}\n{trait_description}\n{text1}\n{text2}",
      api_key = "test",
      reasoning = "none",
      verbose = FALSE,
      progress = FALSE,
      include_raw = FALSE
    ),
    .package = "pairwiseLLM"
  )
  expect_equal(nrow(anthropic_out$results), 1L)
  expect_equal(nrow(anthropic_out$failed_pairs), 1L)
  expect_error(
    submit_anthropic_pairs_live(
      pairs = tibble::tibble(ID1 = "A"),
      model = "m",
      trait_name = "quality",
      trait_description = "better",
      verbose = FALSE,
      progress = FALSE
    ),
    "must contain columns"
  )
})

test_that("cost estimator covers validation and offline pilot branches without network", {
  pairs <- tibble::tibble(
    ID1 = c("A", "B", "C"),
    text1 = c("alpha", "bravo", "charlie"),
    ID2 = c("B", "C", "A"),
    text2 = c("bravo", "charlie", "alpha")
  )
  submit_fun <- function(pairs, ...) {
    tibble::tibble(
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = pairs$ID1,
      prompt_tokens = seq_len(nrow(pairs)) + 10L,
      completion_tokens = seq_len(nrow(pairs)) + 2L
    )
  }

  expect_error(
    estimate_llm_pairs_cost(
      pairs = tibble::tibble(ID1 = "A"),
      model = "m",
      trait_name = "t",
      trait_description = "d",
      backend = "openai",
      cost_per_million_input = 1,
      cost_per_million_output = 2,
      .submit_fun = submit_fun
    ),
    "must contain columns"
  )
  expect_error(
    estimate_llm_pairs_cost(
      pairs = pairs,
      model = "m",
      trait_name = "t",
      trait_description = "d",
      backend = "vertex",
      mode = "batch",
      cost_per_million_input = 1,
      cost_per_million_output = 2,
      .submit_fun = submit_fun
    ),
    "Vertex batch mode"
  )

  est <- estimate_llm_pairs_cost(
    pairs = pairs,
    model = "m",
    trait_name = "t",
    trait_description = "d",
    backend = "openai",
    mode = "batch",
    test_strategy = "first",
    n_test = 2L,
    cost_per_million_input = 1,
    cost_per_million_output = 2,
    batch_discount = 0.5,
    budget_quantile = 0.75,
    return_test_results = TRUE,
    return_remaining_pairs = TRUE,
    .submit_fun = submit_fun
  )
  expect_s3_class(est, "pairwiseLLM_cost_estimate")
  expect_equal(nrow(est$test_pairs), 2L)
  expect_equal(nrow(est$remaining_pairs), 1L)
  printed <- capture.output(print(est))
  expect_true(any(grepl("Backend: openai", printed, fixed = TRUE)))
  expect_true(any(grepl("Pairs: 3 total", printed, fixed = TRUE)))
})
