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
