# -------------------------------------------------------------------------
# Adaptive state constructor.
# -------------------------------------------------------------------------

.adaptive_state_normalize_items <- function(items) {
  if (is.null(items)) {
    rlang::abort("`items` must be provided.")
  }
  if (is.vector(items) && !is.list(items)) {
    items <- tibble::tibble(item_id = items)
  } else if (is.data.frame(items)) {
    items <- tibble::as_tibble(items)
  } else {
    rlang::abort("`items` must be a vector or data frame.")
  }

  if (!"item_id" %in% names(items)) {
    if ("id" %in% names(items)) {
      items$item_id <- items$id
    } else if ("ID" %in% names(items)) {
      items$item_id <- items$ID
    } else {
      rlang::abort("`items` must include an `item_id` column.")
    }
  }

  items <- dplyr::relocate(items, "item_id")
  item_id <- as.character(items$item_id)
  if (any(is.na(item_id) | item_id == "")) {
    rlang::abort("`items$item_id` must be non-missing.")
  }
  if (anyDuplicated(item_id)) {
    rlang::abort("`items$item_id` must be unique.")
  }
  items$item_id <- item_id

  if (!"set_id" %in% names(items)) {
    items$set_id <- rep.int(1L, nrow(items))
  }
  if (!.adaptive_is_integerish(items$set_id) || any(is.na(items$set_id))) {
    rlang::abort("`items$set_id` must be non-missing integer-like values.")
  }
  items$set_id <- as.integer(items$set_id)
  if (any(items$set_id < 1L)) {
    rlang::abort("`items$set_id` must be >= 1.")
  }

  if (!"global_item_id" %in% names(items)) {
    items$global_item_id <- as.character(items$item_id)
  }
  global_item_id <- as.character(items$global_item_id)
  if (any(is.na(global_item_id) | global_item_id == "")) {
    rlang::abort("`items$global_item_id` must be non-missing.")
  }
  if (anyDuplicated(global_item_id)) {
    rlang::abort("`items$global_item_id` must be unique.")
  }
  items$global_item_id <- global_item_id

  items <- dplyr::relocate(items, "set_id", "global_item_id", .after = "item_id")

  items
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_empty_panel <- function() {
  tibble::tibble(
    probe_panel_id = character(),
    link_epoch_id = integer(),
    spoke_id = integer(),
    hub_item_id = character(),
    spoke_item_id = character(),
    spoke_bin = integer(),
    hub_bin = integer(),
    planned_rank = integer(),
    pair_key = character(),
    realized = logical(),
    realized_step_id = integer(),
    realized_pair_id = integer(),
    realized_run_mode = character()
  )
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_empty_cache <- function() {
  tibble::tibble(
    refit_id = integer(),
    spoke_id = integer(),
    link_epoch_id = integer(),
    probe_panel_id = character(),
    hub_item_id = character(),
    spoke_item_id = character(),
    pred_prob = double()
  )
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_empty_realized_log <- function() {
  tibble::tibble(
    step_id = integer(),
    pair_id = integer(),
    run_mode = character(),
    spoke_id = integer(),
    link_epoch_id = integer(),
    probe_panel_id = character(),
    hub_item_id = character(),
    spoke_item_id = character(),
    pair_key = character(),
    Y = integer()
  )
}

#' @keywords internal
#' @noRd
.adaptive_link_probe_empty_state <- function() {
  list(
    panels_by_spoke = list(),
    prediction_cache = .adaptive_link_probe_empty_cache(),
    realized_edges = .adaptive_link_probe_empty_realized_log(),
    collect_holdout_now_by_spoke = list()
  )
}

#' @keywords internal
#' @noRd
.adaptive_stage_order <- function() {
  c("anchor_link", "long_link", "mid_link", "local_link")
}

#' @keywords internal
#' @noRd
.adaptive_link_transform_policy_levels <- function() {
  c("auto", "fixed_shift_only", "fixed_shift_scale")
}

#' @keywords internal
#' @noRd
.adaptive_link_transform_state_levels <- function() {
  c("shift_only", "shift_scale")
}

#' @keywords internal
#' @noRd
.adaptive_shift_only_theta_treatment_levels <- function() {
  c("fixed_eap_plugin_var", "fixed_eap")
}

#' @keywords internal
#' @noRd
.adaptive_normalize_link_transform_policy <- function(policy = NULL, legacy_mode = NULL) {
  value <- policy %||% legacy_mode %||% "auto"
  if (!is.character(value) || length(value) != 1L || is.na(value) || value == "") {
    rlang::abort("Link transform policy must be a single non-empty string.")
  }
  if (identical(value, "shift_only")) {
    value <- "fixed_shift_only"
  } else if (identical(value, "shift_scale")) {
    value <- "fixed_shift_scale"
  }
  if (!value %in% .adaptive_link_transform_policy_levels()) {
    rlang::abort(
      paste0(
        "Link transform policy must be one of: ",
        paste(.adaptive_link_transform_policy_levels(), collapse = ", "),
        "."
      )
    )
  }
  value
}

#' @keywords internal
#' @noRd
.adaptive_default_link_transform_state <- function(link_transform_policy) {
  policy <- .adaptive_normalize_link_transform_policy(link_transform_policy)
  if (identical(policy, "fixed_shift_scale")) {
    return("shift_scale")
  }
  "shift_only"
}

#' @keywords internal
#' @noRd
.adaptive_normalize_link_transform_state <- function(state = NULL, link_transform_policy = "auto") {
  value <- state %||% .adaptive_default_link_transform_state(link_transform_policy)
  if (!is.character(value) || length(value) != 1L || is.na(value) || value == "") {
    rlang::abort("Link transform state must be a single non-empty string.")
  }
  if (!value %in% .adaptive_link_transform_state_levels()) {
    rlang::abort(
      paste0(
        "Link transform state must be one of: ",
        paste(.adaptive_link_transform_state_levels(), collapse = ", "),
        "."
      )
    )
  }
  value
}

#' @keywords internal
#' @noRd
.adaptive_controller_normalize_legacy_fields <- function(controller, n_items) {
  out <- controller %||% list()
  defaults <- .adaptive_controller_defaults(n_items)

  out$link_transform_policy <- .adaptive_normalize_link_transform_policy(
    policy = out$link_transform_policy %||% NULL,
    legacy_mode = out$link_transform_mode %||% NULL
  )
  out$link_transform_mode <- NULL

  state_map <- out$link_transform_state_by_spoke %||% out$link_transform_mode_by_spoke %||% list()
  if (!is.list(state_map)) {
    state_map <- list()
  }
  if (length(state_map) > 0L) {
    state_map <- lapply(
      state_map,
      function(value) .adaptive_normalize_link_transform_state(value, out$link_transform_policy)
    )
  }
  out$link_transform_state_by_spoke <- state_map
  out$link_transform_mode_by_spoke <- NULL

  theta_treatment <- out$shift_only_theta_treatment %||% defaults$shift_only_theta_treatment
  if (identical(theta_treatment, "normal_prior")) {
    theta_treatment <- "fixed_eap_plugin_var"
  }
  if (!theta_treatment %in% .adaptive_shift_only_theta_treatment_levels()) {
    rlang::abort(
      paste0(
        "`adaptive_config$shift_only_theta_treatment` must be one of: ",
        paste(.adaptive_shift_only_theta_treatment_levels(), collapse = ", "),
        "."
      )
    )
  }
  out$shift_only_theta_treatment <- theta_treatment
  out$cross_set_ppc_brier_max <- NULL
  out$ppc_calibration_id <- NULL

  out
}

#' @keywords internal
#' @noRd
.adaptive_controller_defaults <- function(n_items) {
  defaults <- adaptive_defaults(n_items)
  list(
    global_identified = FALSE,
    global_identified_reliability_min = as.double(defaults$global_identified_reliability_min),
    global_identified_rank_corr_min = as.double(defaults$global_identified_rank_corr_min),
    p_long_low = as.double(defaults$p_long_low),
    p_long_high = as.double(defaults$p_long_high),
    long_taper_mult = as.double(defaults$long_taper_mult),
    long_frac_floor = as.double(defaults$long_frac_floor),
    mid_bonus_frac = as.double(defaults$mid_bonus_frac),
    explore_taper_mult = as.double(defaults$explore_taper_mult),
    boundary_k = as.integer(defaults$boundary_k),
    boundary_window = as.integer(defaults$boundary_window),
    boundary_frac = as.double(defaults$boundary_frac),
    p_star_override_margin = as.double(defaults$p_star_override_margin),
    star_override_budget_per_round = as.integer(defaults$star_override_budget_per_round),
    run_mode = "within_set",
    hub_id = 1L,
    link_transform_policy = "auto",
    link_refit_mode = "shift_only",
    shift_only_theta_treatment = "fixed_eap_plugin_var",
    judge_param_mode = "global_shared",
    hub_lock_mode = "soft_lock",
    hub_lock_kappa = 0.75,
    link_identified_reliability_min = 0.80,
    link_stop_reliability_min = 0.90,
    link_rank_corr_min = 0.90,
    delta_sd_max = 0.10,
    delta_change_max = 0.05,
    log_alpha_sd_max = 0.10,
    log_alpha_change_max = 0.05,
    link_transform_escalation_refits_required = 2L,
    link_transform_escalation_is_one_way = TRUE,
    max_pairs_after_stop = 0L,
    probe_pairs_per_refit_per_spoke = 2L,
    probe_edges_min_for_stop = 30L,
    probe_brier_delta_min = 0.005,
    probe_brier_max = 0.19,
    probe_pred_rmse_max = 0.015,
    theta_global_rmse_max = 0.04,
    theta_global_rmse_scope = "direct_evidence_spoke",
    min_cross_set_edges_k = 1L,
    stability_consecutive_k = 2L,
    min_refits_in_phase_b = 3L,
    hub_theta_rmse_max = 0.02,
    logalpha_sd_guardrail = 0.10,
    shift_scale_min_cross_set_edges = 18L,
    shift_scale_min_distinct_spoke_items_per_bin = 2L,
    reliability_var_mu_epsilon = 1e-6,
    reliability_total_var_epsilon = 1e-6,
    probe_edges_count_toward_active_constraints = FALSE,
    spoke_quantile_coverage_bins = 3L,
    spoke_quantile_coverage_min_per_bin_per_refit = 1L,
    allow_spoke_spoke_cross_set = FALSE,
    multi_spoke_mode = "independent",
    multi_spoke_budget_rule = "utility_mass_topk",
    multi_spoke_budget_top_k = 10L,
    min_cross_set_pairs_per_spoke_per_refit = 5L,
    stage_quota_frac_anchor_link = 0.25,
    stage_quota_frac_long_link = 0.35,
    stage_quota_frac_mid_link = 0.25,
    stage_quota_frac_local_link = 0.15,
    stage_quota_floor_anchor_link = 2L,
    stage_quota_floor_long_link = 2L,
    stage_quota_floor_mid_link = 1L,
    stage_quota_floor_local_link = 0L,
    long_link_taper_multiplier = 0.50,
    long_link_taper_floor = 2L,
    cross_set_utility = "linking_d_optimal",
    phase_a_mode = "run",
    phase_a_import_failure_policy = "fail_fast",
    phase_a_required_reliability_min = 0.80,
    phase_a_compatible_model_ids = "btl_e_b",
    phase_a_compatible_config_hashes = character(),
    phase_a_artifacts = list(),
    phase_a_set_source = character(),
    reliability_EAP = NA_real_,
    ts_btl_rank_spearman = NA_real_,
    current_link_spoke_id = NA_integer_,
    linking_identified = FALSE,
    linking_identified_by_spoke = list(),
    link_transform_state_by_spoke = list(),
    link_transform_bad_refits_by_spoke = list(),
    link_transform_last_delta_by_spoke = list(),
    link_transform_last_log_alpha_by_spoke = list(),
    link_transform_frozen_by_spoke = list(),
    link_transform_frozen_delta_by_spoke = list(),
    link_transform_frozen_log_alpha_by_spoke = list(),
    link_transform_frozen_refit_id_by_spoke = list(),
    link_refit_stats_by_spoke = list(),
    link_d_opt_it_by_spoke = list(),
    link_stopped_by_spoke = list(),
    link_stop_refit_id_by_spoke = list(),
    link_stop_reason_by_spoke = list(),
    link_epoch_id_by_spoke = list(),
    link_epoch_signature_by_spoke = list(),
    link_epoch_start_step_by_spoke = list(),
    link_stop_consecutive_pass_count_by_spoke = list(),
    link_escalation_consecutive_pass_count_by_spoke = list(),
    link_lag_domain_key_by_spoke = list(),
    link_lag_domain_reset_refit_id_by_spoke = list(),
    link_stage_coverage_bins_used = list(),
    link_stage_coverage_source = list()
  )
}

#' @keywords internal
#' @noRd
.adaptive_controller_public_keys <- function() {
  c(
    "global_identified_reliability_min",
    "global_identified_rank_corr_min",
    "p_long_low",
    "p_long_high",
    "long_taper_mult",
    "long_frac_floor",
    "mid_bonus_frac",
    "explore_taper_mult",
    "boundary_k",
    "boundary_window",
    "boundary_frac",
    "p_star_override_margin",
    "star_override_budget_per_round",
    "run_mode",
    "hub_id",
    "link_transform_policy",
    "link_transform_mode",
    "link_refit_mode",
    "shift_only_theta_treatment",
    "judge_param_mode",
    "hub_lock_mode",
    "hub_lock_kappa",
    "link_identified_reliability_min",
    "link_stop_reliability_min",
    "link_rank_corr_min",
    "delta_sd_max",
    "delta_change_max",
    "log_alpha_sd_max",
    "log_alpha_change_max",
    "link_transform_escalation_refits_required",
    "link_transform_escalation_is_one_way",
    "max_pairs_after_stop",
    "probe_pairs_per_refit_per_spoke",
    "probe_edges_min_for_stop",
    "probe_brier_delta_min",
    "probe_brier_max",
    "probe_pred_rmse_max",
    "theta_global_rmse_max",
    "theta_global_rmse_scope",
    "min_cross_set_edges_k",
    "stability_consecutive_k",
    "min_refits_in_phase_b",
    "hub_theta_rmse_max",
    "logalpha_sd_guardrail",
    "shift_scale_min_cross_set_edges",
    "shift_scale_min_distinct_spoke_items_per_bin",
    "reliability_var_mu_epsilon",
    "reliability_total_var_epsilon",
    "probe_edges_count_toward_active_constraints",
    "spoke_quantile_coverage_bins",
    "spoke_quantile_coverage_min_per_bin_per_refit",
    "allow_spoke_spoke_cross_set",
    "multi_spoke_mode",
    "multi_spoke_budget_rule",
    "multi_spoke_budget_top_k",
    "min_cross_set_pairs_per_spoke_per_refit",
    "stage_quota_frac_anchor_link",
    "stage_quota_frac_long_link",
    "stage_quota_frac_mid_link",
    "stage_quota_frac_local_link",
    "stage_quota_floor_anchor_link",
    "stage_quota_floor_long_link",
    "stage_quota_floor_mid_link",
    "stage_quota_floor_local_link",
    "long_link_taper_multiplier",
    "long_link_taper_floor",
    "cross_set_utility",
    "phase_a_mode",
    "phase_a_import_failure_policy",
    "phase_a_required_reliability_min",
    "phase_a_compatible_model_ids",
    "phase_a_compatible_config_hashes",
    "phase_a_artifacts",
    "phase_a_set_source"
  )
}

#' @keywords internal
#' @noRd
.adaptive_validate_controller_config <- function(adaptive_config, n_items, set_ids = NULL) {
  if (is.null(adaptive_config)) {
    return(list())
  }
  if (!is.list(adaptive_config)) {
    rlang::abort("`adaptive_config` must be a named list when provided.")
  }
  cfg_names <- names(adaptive_config)
  if (length(adaptive_config) > 0L && (is.null(cfg_names) || any(cfg_names == ""))) {
    rlang::abort("`adaptive_config` must be a named list with non-empty names.")
  }

  allowed <- .adaptive_controller_public_keys()
  unknown <- setdiff(cfg_names, allowed)
  if (length(unknown) > 0L) {
    rlang::abort(c(
      "Unknown `adaptive_config` field(s).",
      x = paste(unknown, collapse = ", "),
      i = paste("Allowed fields:", paste(allowed, collapse = ", "))
    ))
  }

  out <- adaptive_config
  read_double <- function(name, lower = -Inf, upper = Inf) {
    value <- out[[name]]
    if (is.null(value)) {
      return(NULL)
    }
    if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
      rlang::abort(paste0("`adaptive_config$", name, "` must be a single numeric value."))
    }
    value <- as.double(value)
    if (value < lower || value > upper) {
      rlang::abort(paste0(
        "`adaptive_config$", name, "` must be in [",
        format(lower, scientific = FALSE),
        ", ",
        format(upper, scientific = FALSE),
        "]."
      ))
    }
    value
  }
  read_integer <- function(name, lower = -Inf, upper = Inf) {
    value <- out[[name]]
    if (is.null(value)) {
      return(NULL)
    }
    if (!.adaptive_is_integerish(value) || length(value) != 1L || is.na(value)) {
      rlang::abort(paste0("`adaptive_config$", name, "` must be a single integer value."))
    }
    value <- as.integer(value)
    if (value < lower || value > upper) {
      rlang::abort(paste0(
        "`adaptive_config$", name, "` must be in [",
        format(lower, scientific = FALSE),
        ", ",
        format(upper, scientific = FALSE),
        "]."
      ))
    }
    value
  }
  read_logical <- function(name) {
    value <- out[[name]]
    if (is.null(value)) {
      return(NULL)
    }
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      rlang::abort(paste0("`adaptive_config$", name, "` must be TRUE or FALSE."))
    }
    isTRUE(value)
  }
  read_choice <- function(name, choices) {
    value <- out[[name]]
    if (is.null(value)) {
      return(NULL)
    }
    if (!is.character(value) || length(value) != 1L || is.na(value) || value == "") {
      rlang::abort(paste0("`adaptive_config$", name, "` must be a single string value."))
    }
    if (!value %in% choices) {
      rlang::abort(paste0(
        "`adaptive_config$", name, "` must be one of: ",
        paste(choices, collapse = ", "),
        "."
      ))
    }
    value
  }

  out$global_identified_reliability_min <- read_double("global_identified_reliability_min", 0, 1)
  out$global_identified_rank_corr_min <- read_double("global_identified_rank_corr_min", 0, 1)
  out$p_long_low <- read_double("p_long_low", 0, 1)
  out$p_long_high <- read_double("p_long_high", 0, 1)
  out$long_taper_mult <- read_double("long_taper_mult", 0, 1)
  out$long_frac_floor <- read_double("long_frac_floor", 0, 1)
  out$mid_bonus_frac <- read_double("mid_bonus_frac", 0, 1)
  out$explore_taper_mult <- read_double("explore_taper_mult", 0, 1)
  out$boundary_k <- read_integer("boundary_k", 1L, as.integer(n_items))
  out$boundary_window <- read_integer("boundary_window", 1L, as.integer(n_items))
  out$boundary_frac <- read_double("boundary_frac", 0, 1)
  out$p_star_override_margin <- read_double("p_star_override_margin", 0, 0.5)
  out$star_override_budget_per_round <- read_integer("star_override_budget_per_round", 0L, Inf)
  out$run_mode <- read_choice("run_mode", c("within_set", "link_one_spoke", "link_multi_spoke"))
  out$hub_id <- read_integer("hub_id", 1L, Inf)
  policy_value <- out$link_transform_policy %||% out$link_transform_mode %||% NULL
  if (!is.null(policy_value)) {
    out$link_transform_policy <- .adaptive_normalize_link_transform_policy(policy = policy_value)
  }
  out$link_transform_mode <- NULL
  out$link_refit_mode <- read_choice("link_refit_mode", c("shift_only", "joint_refit"))
  if (!is.null(out$shift_only_theta_treatment)) {
    if (!is.character(out$shift_only_theta_treatment) ||
      length(out$shift_only_theta_treatment) != 1L ||
      is.na(out$shift_only_theta_treatment) ||
      out$shift_only_theta_treatment == "") {
      rlang::abort("`adaptive_config$shift_only_theta_treatment` must be a single string value.")
    }
    if (identical(out$shift_only_theta_treatment, "normal_prior")) {
      out$shift_only_theta_treatment <- "fixed_eap_plugin_var"
    }
    if (!out$shift_only_theta_treatment %in% .adaptive_shift_only_theta_treatment_levels()) {
      rlang::abort(paste0(
        "`adaptive_config$shift_only_theta_treatment` must be one of: ",
        paste(.adaptive_shift_only_theta_treatment_levels(), collapse = ", "),
        "."
      ))
    }
  }
  out$judge_param_mode <- read_choice("judge_param_mode", c("global_shared", "phase_specific"))
  out$hub_lock_mode <- read_choice("hub_lock_mode", c("hard_lock", "soft_lock"))
  out$hub_lock_kappa <- read_double("hub_lock_kappa", 0, 1)
  out$link_identified_reliability_min <- read_double("link_identified_reliability_min", 0, 1)
  out$link_stop_reliability_min <- read_double("link_stop_reliability_min", 0, 1)
  out$link_rank_corr_min <- read_double("link_rank_corr_min", 0, 1)
  out$delta_sd_max <- read_double("delta_sd_max", 0, Inf)
  out$delta_change_max <- read_double("delta_change_max", 0, Inf)
  out$log_alpha_sd_max <- read_double("log_alpha_sd_max", 0, Inf)
  out$log_alpha_change_max <- read_double("log_alpha_change_max", 0, Inf)
  out$link_transform_escalation_refits_required <- read_integer(
    "link_transform_escalation_refits_required",
    1L,
    Inf
  )
  out$link_transform_escalation_is_one_way <- read_logical("link_transform_escalation_is_one_way")
  out$max_pairs_after_stop <- read_integer("max_pairs_after_stop", 0L, Inf)
  out$probe_pairs_per_refit_per_spoke <- read_integer("probe_pairs_per_refit_per_spoke", 0L, Inf)
  out$probe_edges_min_for_stop <- read_integer("probe_edges_min_for_stop", 1L, Inf)
  out$probe_brier_delta_min <- read_double("probe_brier_delta_min", 0, 1)
  out$probe_brier_max <- read_double("probe_brier_max", 0, 1)
  out$probe_pred_rmse_max <- read_double("probe_pred_rmse_max", 0, Inf)
  out$theta_global_rmse_max <- read_double("theta_global_rmse_max", 0, Inf)
  out$theta_global_rmse_scope <- read_choice(
    "theta_global_rmse_scope",
    c("direct_evidence_spoke", "all_spoke_items", "min_cross_set_edges_k")
  )
  out$min_cross_set_edges_k <- read_integer("min_cross_set_edges_k", 1L, Inf)
  out$stability_consecutive_k <- read_integer("stability_consecutive_k", 1L, Inf)
  out$min_refits_in_phase_b <- read_integer("min_refits_in_phase_b", 1L, Inf)
  out$hub_theta_rmse_max <- read_double("hub_theta_rmse_max", 0, Inf)
  out$logalpha_sd_guardrail <- read_double("logalpha_sd_guardrail", 0, Inf)
  out$shift_scale_min_cross_set_edges <- read_integer("shift_scale_min_cross_set_edges", 1L, Inf)
  out$shift_scale_min_distinct_spoke_items_per_bin <- read_integer(
    "shift_scale_min_distinct_spoke_items_per_bin",
    1L,
    Inf
  )
  out$reliability_var_mu_epsilon <- read_double("reliability_var_mu_epsilon", 0, Inf)
  out$reliability_total_var_epsilon <- read_double("reliability_total_var_epsilon", 0, Inf)
  out$probe_edges_count_toward_active_constraints <- read_logical(
    "probe_edges_count_toward_active_constraints"
  )
  out$spoke_quantile_coverage_bins <- read_integer("spoke_quantile_coverage_bins", 1L, Inf)
  out$spoke_quantile_coverage_min_per_bin_per_refit <- read_integer(
    "spoke_quantile_coverage_min_per_bin_per_refit",
    1L,
    Inf
  )
  out$allow_spoke_spoke_cross_set <- read_logical("allow_spoke_spoke_cross_set")
  out$multi_spoke_mode <- read_choice("multi_spoke_mode", c("independent", "concurrent"))
  out$multi_spoke_budget_rule <- read_choice("multi_spoke_budget_rule", c("utility_mass_topk"))
  out$multi_spoke_budget_top_k <- read_integer("multi_spoke_budget_top_k", 1L, Inf)
  out$min_cross_set_pairs_per_spoke_per_refit <- read_integer(
    "min_cross_set_pairs_per_spoke_per_refit",
    1L,
    Inf
  )
  out$stage_quota_frac_anchor_link <- read_double("stage_quota_frac_anchor_link", 0, 1)
  out$stage_quota_frac_long_link <- read_double("stage_quota_frac_long_link", 0, 1)
  out$stage_quota_frac_mid_link <- read_double("stage_quota_frac_mid_link", 0, 1)
  out$stage_quota_frac_local_link <- read_double("stage_quota_frac_local_link", 0, 1)
  out$stage_quota_floor_anchor_link <- read_integer("stage_quota_floor_anchor_link", 0L, Inf)
  out$stage_quota_floor_long_link <- read_integer("stage_quota_floor_long_link", 0L, Inf)
  out$stage_quota_floor_mid_link <- read_integer("stage_quota_floor_mid_link", 0L, Inf)
  out$stage_quota_floor_local_link <- read_integer("stage_quota_floor_local_link", 0L, Inf)
  out$long_link_taper_multiplier <- read_double("long_link_taper_multiplier", 0, 1)
  out$long_link_taper_floor <- read_integer("long_link_taper_floor", 0L, Inf)
  out$cross_set_utility <- read_choice(
    "cross_set_utility",
    c("linking_d_optimal")
  )
  out$phase_a_mode <- read_choice("phase_a_mode", c("run", "import", "mixed"))
  out$phase_a_import_failure_policy <- read_choice(
    "phase_a_import_failure_policy",
    c("fail_fast", "fallback_to_run")
  )
  out$phase_a_required_reliability_min <- read_double("phase_a_required_reliability_min", 0, 1)

  if (!is.null(out$phase_a_compatible_model_ids)) {
    if (!is.character(out$phase_a_compatible_model_ids) ||
      any(is.na(out$phase_a_compatible_model_ids) | out$phase_a_compatible_model_ids == "")) {
      rlang::abort("`adaptive_config$phase_a_compatible_model_ids` must be a non-empty character vector.")
    }
  }
  if (!is.null(out$phase_a_compatible_config_hashes)) {
    if (!is.character(out$phase_a_compatible_config_hashes) ||
      any(is.na(out$phase_a_compatible_config_hashes) | out$phase_a_compatible_config_hashes == "")) {
      rlang::abort("`adaptive_config$phase_a_compatible_config_hashes` must be a character vector.")
    }
  }
  if (!is.null(out$phase_a_artifacts) && !is.list(out$phase_a_artifacts)) {
    rlang::abort("`adaptive_config$phase_a_artifacts` must be a named list.")
  }
  if (!is.null(out$phase_a_set_source)) {
    if (!is.character(out$phase_a_set_source) || is.null(names(out$phase_a_set_source)) ||
      any(names(out$phase_a_set_source) == "")) {
      rlang::abort("`adaptive_config$phase_a_set_source` must be a named character vector.")
    }
    allowed_sources <- c("run", "import")
    if (!all(out$phase_a_set_source %in% allowed_sources)) {
      rlang::abort("`adaptive_config$phase_a_set_source` values must be `run` or `import`.")
    }
  }

  if (!is.null(out$p_long_low) &&
    !is.null(out$p_long_high) &&
    out$p_long_low >= out$p_long_high) {
    rlang::abort("`adaptive_config$p_long_low` must be strictly less than `adaptive_config$p_long_high`.")
  }

  resolved <- utils::modifyList(.adaptive_controller_defaults(n_items), out)
  resolved <- .adaptive_controller_normalize_legacy_fields(resolved, n_items = n_items)
  frac_sum <- sum(c(
    resolved$stage_quota_frac_anchor_link,
    resolved$stage_quota_frac_long_link,
    resolved$stage_quota_frac_mid_link,
    resolved$stage_quota_frac_local_link
  ))
  if (!isTRUE(all.equal(frac_sum, 1, tolerance = 1e-8))) {
    rlang::abort(
      paste0(
        "Linking stage quota fractions must sum to 1.0; got ",
        format(frac_sum, digits = 8),
        "."
      )
    )
  }
  run_mode <- resolved$run_mode
  set_ids <- as.integer(set_ids %||% 1L)
  n_sets <- length(unique(set_ids))
  is_link_mode <- run_mode %in% c("link_one_spoke", "link_multi_spoke")
  if (isTRUE(is_link_mode) && n_sets < 2L) {
    rlang::abort("Linking run modes require multi-set input (`items$set_id` with at least two sets).")
  }
  if (isTRUE(is_link_mode) && !resolved$hub_id %in% unique(set_ids)) {
    rlang::abort("`adaptive_config$hub_id` must match one observed `items$set_id` in linking mode.")
  }
  if (isTRUE(is_link_mode) && run_mode == "link_one_spoke") {
    spoke_ids <- setdiff(unique(set_ids), resolved$hub_id)
    if (length(spoke_ids) != 1L) {
      rlang::abort("`run_mode = \"link_one_spoke\"` requires exactly one spoke set.")
    }
  }
  if (identical(resolved$hub_lock_mode, "soft_lock") &&
    (!is.finite(resolved$hub_lock_kappa) || resolved$hub_lock_kappa <= 0 || resolved$hub_lock_kappa > 1)) {
    rlang::abort(
      "`adaptive_config$hub_lock_kappa` must be strictly in (0, 1] when `hub_lock_mode = \"soft_lock\"`."
    )
  }
  if (isTRUE(is_link_mode) &&
    identical(resolved$multi_spoke_mode, "concurrent") &&
    !resolved$hub_lock_mode %in% c("hard_lock", "soft_lock")) {
    rlang::abort(paste0(
      "`adaptive_config$hub_lock_mode` must be `hard_lock` or `soft_lock` ",
      "when `adaptive_config$multi_spoke_mode = \"concurrent\"`."
    ))
  }
  out
}

#' @keywords internal
#' @noRd
.adaptive_sync_round_controller <- function(state) {
  out <- state
  round <- out$round %||% NULL
  if (is.null(round) || !is.list(round)) {
    return(out)
  }
  controller <- .adaptive_controller_resolve(out)
  phase_ctx <- .adaptive_link_phase_context(out, controller = controller)
  controller_for_quota <- controller
  controller_for_quota$link_phase <- as.character(phase_ctx$phase %||% "phase_a")
  round$star_override_budget_per_round <- as.integer(controller$star_override_budget_per_round)

  can_refresh_quotas <- as.integer(round$round_committed %||% 0L) == 0L &&
    as.integer(round$stage_index %||% 1L) == 1L &&
    all(as.integer(round$stage_committed %||% integer()) == 0L)

  if (isTRUE(can_refresh_quotas)) {
    stage_quotas <- .adaptive_round_compute_quotas(
      round_id = as.integer(round$round_id %||% 1L),
      n_items = as.integer(out$n_items),
      controller = controller_for_quota
    )
    quota_meta <- attr(stage_quotas, "quota_meta") %||% list()
    round$stage_quotas <- stage_quotas
    round$global_identified <- isTRUE(quota_meta$global_identified %||% FALSE)
    round$long_quota_raw <- as.integer(quota_meta$long_quota_raw %||% NA_integer_)
    round$long_quota_effective <- as.integer(quota_meta$long_quota_effective %||% NA_integer_)
    round$long_quota_removed <- as.integer(quota_meta$long_quota_removed %||% NA_integer_)
    round$realloc_to_mid <- as.integer(quota_meta$realloc_to_mid %||% NA_integer_)
    round$realloc_to_local <- as.integer(quota_meta$realloc_to_local %||% NA_integer_)
  }

  out$round <- round
  out
}

#' @keywords internal
#' @noRd
.adaptive_sync_linking_meta <- function(state) {
  out <- state
  controller <- .adaptive_controller_resolve(out)
  linking <- out$linking %||% list()
  set_ids <- sort(unique(as.integer(out$items$set_id)))
  hub_id <- as.integer(controller$hub_id %||% 1L)
  spoke_ids <- setdiff(set_ids, hub_id)
  out$linking <- utils::modifyList(linking, list(
    run_mode = as.character(controller$run_mode),
    hub_id = hub_id,
    spoke_ids = as.integer(spoke_ids),
    is_multi_set = length(set_ids) > 1L,
    phase_a = linking$phase_a %||% list(
      set_status = .adaptive_phase_a_empty_state(set_ids),
      artifacts = list(),
      ready_for_phase_b = FALSE,
      strict_ready_for_phase_b = FALSE,
      required_sets = as.integer(sort(unique(set_ids))),
      set_stop_pass_by_set = list(),
      phase = "phase_a",
      phase_b_started_at_step = NA_integer_
    )
  ))
  out
}

#' @keywords internal
#' @noRd
.adaptive_apply_controller_config <- function(state, adaptive_config = NULL) {
  out <- state
  overrides <- .adaptive_validate_controller_config(
    adaptive_config,
    n_items = out$n_items,
    set_ids = out$items$set_id
  )
  if (length(overrides) == 0L) {
    return(.adaptive_sync_linking_meta(out))
  }
  out$controller <- utils::modifyList(.adaptive_controller_resolve(out), overrides)
  out <- .adaptive_sync_round_controller(out)
  .adaptive_sync_linking_meta(out)
}

#' @keywords internal
#' @noRd
.adaptive_controller_resolve <- function(state_or_n_items) {
  if (inherits(state_or_n_items, "adaptive_state")) {
    n_items <- as.integer(state_or_n_items$n_items)
    controller <- state_or_n_items$controller %||% list()
  } else {
    n_items <- as.integer(state_or_n_items)
    controller <- list()
  }
  controller <- .adaptive_controller_normalize_legacy_fields(controller, n_items = n_items)
  defaults <- .adaptive_controller_defaults(n_items)
  utils::modifyList(defaults, controller)
}

#' @keywords internal
#' @noRd
.adaptive_link_budget_fields <- function() {
  c(
    "B_spoke_refit_budget",
    "B_spoke_refit_budget_source",
    "concurrent_target_pairs",
    "concurrent_floor_pairs",
    "concurrent_floor_met",
    "concurrent_target_met",
    "concurrent_utility_mass",
    "concurrent_top_k_used",
    "concurrent_candidate_count"
  )
}

#' @keywords internal
#' @noRd
.adaptive_link_refit_budget_default <- function(n_items, controller = NULL) {
  controller <- utils::modifyList(.adaptive_controller_defaults(n_items), controller %||% list())
  budget <- controller$refit_pairs_target %||% adaptive_defaults(n_items)$refit_pairs_target
  budget <- as.integer(budget %||% NA_integer_)
  if (!is.finite(budget) || is.na(budget) || budget < 0L) {
    rlang::abort("Linking budget invariant failed: refit budget must be a non-negative integer.")
  }
  budget
}

#' @keywords internal
#' @noRd
.adaptive_link_stage_quota_inputs <- function(controller) {
  fractions <- c(
    anchor_link = as.double(controller$stage_quota_frac_anchor_link),
    long_link = as.double(controller$stage_quota_frac_long_link),
    mid_link = as.double(controller$stage_quota_frac_mid_link),
    local_link = as.double(controller$stage_quota_frac_local_link)
  )
  floors <- c(
    anchor_link = as.integer(controller$stage_quota_floor_anchor_link),
    long_link = as.integer(controller$stage_quota_floor_long_link),
    mid_link = as.integer(controller$stage_quota_floor_mid_link),
    local_link = as.integer(controller$stage_quota_floor_local_link)
  )
  list(fractions = fractions, floors = floors)
}

#' @keywords internal
#' @noRd
.adaptive_weighted_largest_remainder <- function(total_units, weights, tie_order) {
  total_units <- as.integer(total_units %||% 0L)
  weight_vals <- as.double(weights[tie_order])
  names(weight_vals) <- tie_order
  if (total_units <= 0L) {
    out <- stats::setNames(rep.int(0L, length(tie_order)), tie_order)
    return(list(add = out, remainders = stats::setNames(weight_vals * 0, tie_order)))
  }
  weight_vals[!is.finite(weight_vals) | weight_vals < 0] <- 0
  weight_sum <- sum(weight_vals)
  if (weight_sum <= 0) {
    weight_vals[] <- 1 / length(weight_vals)
  } else {
    weight_vals <- weight_vals / weight_sum
  }
  names(weight_vals) <- tie_order
  scaled <- weight_vals * total_units
  add <- floor(scaled)
  names(add) <- tie_order
  remainders <- scaled - add
  left <- as.integer(total_units - sum(add))
  if (left > 0L) {
    ord <- order(-remainders, match(names(remainders), tie_order))
    add[ord[seq_len(left)]] <- add[ord[seq_len(left)]] + 1L
  }
  add <- stats::setNames(as.integer(add), tie_order)
  remainders <- stats::setNames(as.double(remainders), tie_order)
  list(add = add, remainders = remainders)
}

#' @keywords internal
#' @noRd
.adaptive_link_compute_stage_targets <- function(budget,
                                                 controller,
                                                 linking_identified = FALSE) {
  budget <- as.integer(budget %||% NA_integer_)
  if (!is.finite(budget) || is.na(budget) || budget < 0L) {
    rlang::abort(
      "Linking budget invariant failed: stage target computation requires a non-negative integer budget."
    )
  }
  params <- .adaptive_link_stage_quota_inputs(controller)
  fractions <- params$fractions
  floors <- params$floors
  stage_order <- .adaptive_stage_order()
  reduce_order <- c("local_link", "mid_link", "long_link", "anchor_link")
  remainder_order <- c("long_link", "anchor_link", "mid_link", "local_link")
  taper_redist_order <- c("anchor_link", "mid_link", "local_link")

  targets <- as.integer(floors[stage_order])
  names(targets) <- stage_order
  while (sum(targets) > budget) {
    reduced <- FALSE
    for (stage in reduce_order) {
      if (sum(targets) <= budget) {
        break
      }
      if (targets[[stage]] > 0L) {
        targets[[stage]] <- targets[[stage]] - 1L
        reduced <- TRUE
      }
    }
    if (!isTRUE(reduced)) {
      break
    }
  }

  remaining <- as.integer(budget - sum(targets))
  fractional <- fractions * remaining
  add_floor <- floor(fractional)
  targets <- targets + as.integer(add_floor[stage_order])
  leftover <- as.integer(budget - sum(targets))
  if (leftover > 0L) {
    remainders <- fractional - add_floor
    ord <- order(-remainders[remainder_order], seq_along(remainder_order))
    for (stage in remainder_order[ord][seq_len(leftover)]) {
      targets[[stage]] <- targets[[stage]] + 1L
    }
  }

  long_pre_taper <- as.integer(targets[["long_link"]])
  long_post_taper <- as.integer(long_pre_taper)
  taper_applied <- FALSE
  if (isTRUE(linking_identified) && long_pre_taper > 0L) {
    taper_applied <- TRUE
    long_post_taper <- max(
      as.integer(controller$long_link_taper_floor %||% 2L),
      as.integer(round(as.double(controller$long_link_taper_multiplier %||% 0.5) * long_pre_taper))
    )
    long_post_taper <- min(long_post_taper, long_pre_taper)
    freed <- as.integer(long_pre_taper - long_post_taper)
    targets[["long_link"]] <- as.integer(long_post_taper)
    if (freed > 0L) {
      blocker_weights <- .adaptive_link_blocker_weights_for_spoke(
        controller = controller,
        spoke_id = as.integer(controller$current_link_spoke_id %||% NA_integer_)
      )
      stage_weights <- .adaptive_link_blocker_stage_weights(
        blocker_weights = blocker_weights,
        linking_identified = TRUE
      )
      redist <- .adaptive_weighted_largest_remainder(
        total_units = freed,
        weights = fractions[taper_redist_order] * stage_weights[taper_redist_order],
        tie_order = taper_redist_order
      )
      targets[taper_redist_order] <- targets[taper_redist_order] + redist$add[taper_redist_order]
    }
  }

  if (sum(targets) != budget) {
    rlang::abort("Linking budget invariant failed: stage targets must sum exactly to the budget.")
  }

  meta <- list(
    budget = as.integer(budget),
    stage_target_anchor_link = as.integer(targets[["anchor_link"]]),
    stage_target_long_link = as.integer(targets[["long_link"]]),
    stage_target_mid_link = as.integer(targets[["mid_link"]]),
    stage_target_local_link = as.integer(targets[["local_link"]]),
    stage_target_long_link_pre_taper = as.integer(long_pre_taper),
    stage_target_long_link_post_taper = as.integer(long_post_taper),
    long_link_taper_applied = isTRUE(taper_applied)
  )
  attr(targets, "quota_meta") <- meta
  targets
}

#' @keywords internal
#' @noRd
.adaptive_link_blocker_weights <- function(stats_row, controller = NULL) {
  row <- stats_row %||% list()
  if (is.data.frame(row)) {
    row <- if (nrow(row) > 0L) as.list(row[1L, , drop = FALSE]) else list()
  }
  controller <- controller %||% list()

  read_metric <- function(name, default = NA_real_) {
    val <- row[[name]] %||% default
    val <- suppressWarnings(as.double(val))
    if (length(val) != 1L || !is.finite(val)) {
      return(NA_real_)
    }
    as.double(val)
  }

  read_threshold <- function(row_name, controller_name, fallback) {
    val <- row[[row_name]] %||% controller[[controller_name]] %||% fallback
    val <- suppressWarnings(as.double(val))
    if (length(val) != 1L || !is.finite(val) || val <= 0) {
      return(as.double(fallback))
    }
    as.double(val)
  }

  probe_shortfall <- read_metric("probe_panel_shortfall", default = 0)
  if (!is.finite(probe_shortfall) || probe_shortfall < 0) {
    probe_shortfall <- 0
  }
  probe_min <- read_threshold(
    row_name = "probe_edges_min_for_stop_used",
    controller_name = "probe_edges_min_for_stop",
    fallback = 30
  )
  probe_brier <- read_metric("probe_brier")
  probe_brier_max <- read_threshold(
    row_name = "probe_brier_max_used",
    controller_name = "probe_brier_max",
    fallback = 0.19
  )
  probe_pred_rmse <- read_metric("probe_pred_rmse_lagged")
  probe_pred_rmse_max <- read_threshold(
    row_name = "probe_pred_rmse_max_used",
    controller_name = "probe_pred_rmse_max",
    fallback = 0.015
  )
  theta_rmse <- read_metric("theta_global_rmse_lagged")
  theta_rmse_max <- read_threshold(
    row_name = "theta_global_rmse_max_used",
    controller_name = "theta_global_rmse_max",
    fallback = 0.04
  )
  delta_sd <- read_metric("delta_spoke_sd")
  delta_sd_max <- read_threshold(
    row_name = "delta_sd_max_used",
    controller_name = "delta_sd_max",
    fallback = 0.10
  )

  excess_ratio <- function(value, threshold) {
    if (!is.finite(value) || !is.finite(threshold) || threshold <= 0) {
      return(0)
    }
    max(0, (value - threshold) / threshold)
  }

  weights <- c(
    probe_panel_shortfall = as.double(probe_shortfall / max(1, probe_min)),
    probe_brier = as.double(excess_ratio(probe_brier, probe_brier_max)),
    probe_pred_rmse_lagged = as.double(excess_ratio(probe_pred_rmse, probe_pred_rmse_max)),
    theta_global_rmse_lagged = as.double(excess_ratio(theta_rmse, theta_rmse_max)),
    delta_spoke_sd = as.double(excess_ratio(delta_sd, delta_sd_max))
  )
  weights[!is.finite(weights) | weights < 0] <- 0
  weights
}

#' @keywords internal
#' @noRd
.adaptive_link_blocker_weights_for_spoke <- function(controller, spoke_id = NA_integer_) {
  controller <- controller %||% list()
  spoke_id <- as.integer(spoke_id %||% controller$current_link_spoke_id %||% NA_integer_)
  if (is.na(spoke_id)) {
    return(.adaptive_link_blocker_weights(list(), controller = controller))
  }
  stats_row <- (controller$link_refit_stats_by_spoke %||% list())[[as.character(spoke_id)]] %||% list()
  .adaptive_link_blocker_weights(stats_row = stats_row, controller = controller)
}

#' @keywords internal
#' @noRd
.adaptive_link_blocker_stage_weights <- function(blocker_weights, linking_identified = FALSE) {
  weights <- as.double(blocker_weights %||% numeric())
  names(weights) <- names(blocker_weights %||% numeric())
  read_weight <- function(name) {
    val <- as.double(weights[[name]] %||% 0)
    if (!is.finite(val) || val < 0) {
      return(0)
    }
    val
  }

  probe_shortfall <- read_weight("probe_panel_shortfall")
  probe_brier <- read_weight("probe_brier")
  probe_rmse <- read_weight("probe_pred_rmse_lagged")
  theta_rmse <- read_weight("theta_global_rmse_lagged")
  delta_sd <- read_weight("delta_spoke_sd")

  stage_weights <- c(anchor_link = 1, long_link = 1, mid_link = 1, local_link = 1)

  stage_weights[["anchor_link"]] <- stage_weights[["anchor_link"]] +
    (3.00 * probe_shortfall) +
    (0.45 * probe_brier) +
    (2.50 * delta_sd)
  stage_weights[["long_link"]] <- stage_weights[["long_link"]] +
    (0.35 * probe_shortfall) +
    (0.20 * probe_brier) +
    (0.95 * delta_sd) +
    (0.15 * theta_rmse)
  stage_weights[["mid_link"]] <- stage_weights[["mid_link"]] +
    (0.60 * theta_rmse) +
    (0.40 * probe_rmse) +
    (0.15 * probe_brier)
  stage_weights[["local_link"]] <- stage_weights[["local_link"]] +
    (0.95 * theta_rmse) +
    (0.55 * probe_rmse)

  stage_weights[!is.finite(stage_weights) | stage_weights <= 0] <- 1
  stage_weights
}

#' @keywords internal
#' @noRd
.adaptive_round_compute_quotas <- function(round_id, n_items, controller = NULL) {
  round_id <- as.integer(round_id %||% 1L)
  defaults <- adaptive_defaults(n_items)
  controller <- utils::modifyList(
    .adaptive_controller_defaults(n_items),
    controller %||% list()
  )
  is_link_mode <- as.character(controller$run_mode %||% "within_set") %in% c("link_one_spoke", "link_multi_spoke")
  round_pairs_target <- as.integer(defaults$round_pairs_target)
  if (isTRUE(is_link_mode)) {
    link_spoke <- as.integer(controller$current_link_spoke_id %||% NA_integer_)
    link_key <- as.character(link_spoke)
    identified_by_spoke <- controller$linking_identified_by_spoke %||% list()
    linking_identified <- !is.na(link_spoke) &&
      !is.null(identified_by_spoke[[link_key]]) &&
      isTRUE(identified_by_spoke[[link_key]])
    budget <- as.integer(controller$B_spoke_refit_budget %||% NA_integer_)
    if (!is.finite(budget) || is.na(budget) || budget < 0L) {
      budget <- .adaptive_link_refit_budget_default(n_items, controller = controller)
    }
    quotas <- .adaptive_link_compute_stage_targets(
      budget = as.integer(budget),
      controller = controller,
      linking_identified = isTRUE(linking_identified)
    )
    quota_meta <- attr(quotas, "quota_meta") %||% list()
    attr(quotas, "quota_meta") <- list(
      global_identified = isTRUE(controller$global_identified),
      linking_identified = isTRUE(linking_identified),
      link_spoke_id = as.integer(link_spoke),
      B_spoke_refit_budget = as.integer(budget),
      B_spoke_refit_budget_source = as.character(
        controller$B_spoke_refit_budget_source %||% "single_spoke_default"
      ),
      taper_applied = isTRUE(quota_meta$long_link_taper_applied %||% FALSE),
      long_quota_raw = as.integer(quota_meta$stage_target_long_link_pre_taper %||% NA_integer_),
      long_quota_effective = as.integer(quota_meta$stage_target_long_link_post_taper %||% NA_integer_),
      long_quota_removed = as.integer(
        max(
          0L,
          as.integer(quota_meta$stage_target_long_link_pre_taper %||% 0L) -
            as.integer(quota_meta$stage_target_long_link_post_taper %||% 0L)
        )
      ),
      stage_target_long_link_pre_taper = as.integer(
        quota_meta$stage_target_long_link_pre_taper %||% NA_integer_
      ),
      stage_target_long_link_post_taper = as.integer(
        quota_meta$stage_target_long_link_post_taper %||% NA_integer_
      ),
      long_link_taper_applied = as.logical(quota_meta$long_link_taper_applied %||% FALSE)
    )
    return(quotas)
  }

  anchor_frac <- if (round_id <= defaults$anchor_rounds_early) {
    defaults$anchor_frac_early
  } else {
    defaults$anchor_frac_late
  }
  long_frac <- if (round_id <= defaults$long_rounds_early) {
    defaults$long_frac_early
  } else {
    defaults$long_frac_late
  }
  mid_frac <- defaults$mid_frac

  anchor_quota <- as.integer(ceiling(anchor_frac * round_pairs_target))
  long_quota_raw <- as.integer(ceiling(long_frac * round_pairs_target))
  long_quota_effective <- long_quota_raw
  long_quota_removed <- 0L
  realloc_to_mid <- 0L
  realloc_to_local <- 0L
  if (isTRUE(controller$global_identified)) {
    long_frac_effective <- max(
      as.double(controller$long_frac_floor),
      as.double(long_frac) * as.double(controller$long_taper_mult)
    )
    long_quota_effective <- as.integer(ceiling(long_frac_effective * round_pairs_target))
    long_quota_removed <- as.integer(max(0L, long_quota_raw - long_quota_effective))
    realloc_to_mid <- as.integer(ceiling(as.double(controller$mid_bonus_frac) * long_quota_removed))
    realloc_to_local <- as.integer(long_quota_removed - realloc_to_mid)
  }
  long_quota <- as.integer(long_quota_raw)
  mid_quota <- as.integer(ceiling(mid_frac * round_pairs_target))
  local_quota <- as.integer(round_pairs_target - (anchor_quota + long_quota + mid_quota))
  if (isTRUE(controller$global_identified)) {
    long_quota <- as.integer(long_quota_effective)
    mid_quota <- as.integer(mid_quota + realloc_to_mid)
    local_quota <- as.integer(local_quota + realloc_to_local)
  }

  quotas <- c(
    anchor_link = anchor_quota,
    long_link = long_quota,
    mid_link = mid_quota,
    local_link = local_quota
  )

  # Ensure exact target sum deterministically when rounding overshoots.
  while (sum(quotas) > round_pairs_target) {
    for (name in c("mid_link", "long_link", "anchor_link", "local_link")) {
      if (sum(quotas) <= round_pairs_target) {
        break
      }
      if (quotas[[name]] > 0L) {
        quotas[[name]] <- quotas[[name]] - 1L
      }
    }
  }

  attr(quotas, "quota_meta") <- list(
    global_identified = isTRUE(controller$global_identified),
    long_quota_raw = as.integer(long_quota_raw),
    long_quota_effective = as.integer(long_quota_effective),
    long_quota_removed = as.integer(long_quota_removed),
    realloc_to_mid = as.integer(realloc_to_mid),
    realloc_to_local = as.integer(realloc_to_local)
  )
  quotas
}

#' @keywords internal
#' @noRd
.adaptive_new_round_state <- function(item_ids, round_id = 1L, staged_active = FALSE, controller = NULL) {
  ids <- as.character(item_ids)
  round_id <- as.integer(round_id %||% 1L)
  controller <- utils::modifyList(.adaptive_controller_defaults(length(ids)), controller %||% list())
  effective_n <- as.integer(length(ids))
  mode <- as.character(controller$run_mode %||% "within_set")
  phase <- as.character(controller$link_phase %||% "phase_a")
  if (mode %in% c("link_one_spoke", "link_multi_spoke") && !identical(phase, "phase_b")) {
    scoped_n <- as.integer(controller$phase_a_active_n %||% NA_integer_)
    if (is.finite(scoped_n) && scoped_n >= 2L) {
      effective_n <- scoped_n
    }
  }
  defaults <- adaptive_defaults(effective_n)
  stage_order <- .adaptive_stage_order()
  quota_controller <- controller
  if (mode %in% c("link_one_spoke", "link_multi_spoke") && !identical(phase, "phase_b")) {
    quota_controller$run_mode <- "within_set"
  }
  stage_quotas <- .adaptive_round_compute_quotas(
    round_id = round_id,
    n_items = effective_n,
    controller = quota_controller
  )
  quota_meta <- attr(stage_quotas, "quota_meta") %||% list()
  stage_committed <- stats::setNames(rep.int(0L, length(stage_order)), stage_order)
  round_pairs_target <- if (isTRUE(as.character(controller$run_mode %||% "within_set") %in%
    c("link_one_spoke", "link_multi_spoke"))) {
    as.integer(min(defaults$round_pairs_target, sum(stage_quotas)))
  } else {
    as.integer(defaults$round_pairs_target)
  }

  list(
    round_id = round_id,
    staged_active = isTRUE(staged_active),
    stage_order = stage_order,
    stage_index = 1L,
    stage_quotas = stage_quotas,
    stage_committed = stage_committed,
    stage_shortfalls = stats::setNames(rep.int(0L, length(stage_order)), stage_order),
    round_pairs_target = round_pairs_target,
    round_committed = 0L,
    per_round_item_uses = stats::setNames(rep.int(0L, length(ids)), ids),
    repeat_in_round_budget = as.integer(defaults$repeat_in_round_budget),
    repeat_in_round_used = 0L,
    star_override_budget_per_round = as.integer(controller$star_override_budget_per_round),
    star_override_used = 0L,
    anchor_ids = character(),
    anchor_refresh_source = NA_character_,
    anchor_refit_round_id = 0L,
    anchor_round_id = as.integer(round_id),
    committed_total = 0L,
    global_identified = isTRUE(quota_meta$global_identified %||% FALSE),
    long_quota_raw = as.integer(quota_meta$long_quota_raw %||% NA_integer_),
    long_quota_effective = as.integer(quota_meta$long_quota_effective %||% NA_integer_),
    long_quota_removed = as.integer(quota_meta$long_quota_removed %||% NA_integer_),
    realloc_to_mid = as.integer(quota_meta$realloc_to_mid %||% NA_integer_),
    realloc_to_local = as.integer(quota_meta$realloc_to_local %||% NA_integer_),
    link_stage_shortfalls_by_refit_spoke = list(),
    link_stage_exhausted_by_refit_spoke = list()
  )
}

#' @keywords internal
#' @noRd
new_adaptive_state <- function(items, now_fn = function() Sys.time()) {
  force(now_fn)
  if (!is.function(now_fn)) {
    rlang::abort("`now_fn` must be a function.")
  }
  items <- .adaptive_state_normalize_items(items)
  item_ids <- as.character(items$item_id)
  set_ids <- as.integer(items$set_id)
  global_item_ids <- as.character(items$global_item_id)
  item_index <- stats::setNames(seq_along(item_ids), item_ids)
  history_pairs <- tibble::tibble(
    A_id = character(),
    B_id = character()
  )

  state <- structure(
    list(
      item_ids = item_ids,
      global_item_ids = global_item_ids,
      set_ids = set_ids,
      item_index = item_index,
      n_items = as.integer(length(item_ids)),
      items = items,
      history_pairs = history_pairs,
      step_log = new_step_log(now_fn = now_fn),
      round_log = new_round_log(),
      item_log = list(),
      link_stage_log = new_link_stage_log(),
      item_step_log = new_item_step_log(items),
      trueskill_state = new_trueskill_state(items),
      warm_start_pairs = tibble::tibble(i_id = character(), j_id = character()),
      warm_start_idx = 1L,
      warm_start_done = TRUE,
      controller = .adaptive_controller_defaults(length(item_ids)),
      round = .adaptive_new_round_state(
        item_ids,
        round_id = 1L,
        staged_active = FALSE,
        controller = .adaptive_controller_defaults(length(item_ids))
      ),
      btl_fit = NULL,
      stop_metrics = NULL,
      refit_meta = list(
        last_refit_M_done = 0L,
        last_refit_step = 0L,
        last_refit_M_done_by_phase_a_set = list(),
        last_refit_step_by_phase_a_set = list(),
        last_refit_round_id = 0L,
        theta_mean_history = list(),
        theta_mean_history_by_phase_a_set = list(),
        phase_a_lag_domain_last_set_id = NA_integer_,
        phase_a_lag_domain_reset_refit_id_by_set = list(),
        near_stop = FALSE,
        link_stage_shortfalls_by_refit_spoke = list(),
        link_stage_exhausted_by_refit_spoke = list(),
        last_completed_round_summary = list(
          round_id = NA_integer_,
          global_identified = NA,
          long_quota_raw = NA_integer_,
          long_quota_effective = NA_integer_,
          long_quota_removed = NA_integer_,
          realloc_to_mid = NA_integer_,
          realloc_to_local = NA_integer_
        )
      ),
      config = list(),
      linking = list(
        run_mode = "within_set",
        hub_id = 1L,
        spoke_ids = integer(),
        is_multi_set = length(unique(set_ids)) > 1L,
        probe = .adaptive_link_probe_empty_state(),
        phase_a = list(
          set_status = .adaptive_phase_a_empty_state(unique(set_ids)),
          artifacts = list(),
          ready_for_phase_b = FALSE,
          strict_ready_for_phase_b = FALSE,
          required_sets = as.integer(sort(unique(set_ids))),
          set_stop_pass_by_set = list(),
          phase = "phase_a",
          phase_b_started_at_step = NA_integer_
        )
      ),
      meta = list(
        schema_version = "adaptive-session",
        now_fn = now_fn,
        seed = 1L,
        stop_decision = FALSE,
        stop_reason = NA_character_,
        stop_boundary_refit_id = NA_integer_,
        stop_boundary_step_id = NA_integer_,
        pairs_committed_after_stop = 0L
      )
    ),
    class = "adaptive_state"
  )

  state
}
