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
.adaptive_stage_order <- function() {
  c("anchor_link", "long_link", "mid_link", "local_link")
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
    link_transform_mode = "auto",
    link_refit_mode = "shift_only",
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
    cross_set_ppc_mae_max = 0.07,
    link_transform_escalation_refits_required = 2L,
    link_transform_escalation_is_one_way = TRUE,
    spoke_quantile_coverage_bins = 3L,
    spoke_quantile_coverage_min_per_bin_per_refit = 1L,
    multi_spoke_mode = "independent",
    min_cross_set_pairs_per_spoke_per_refit = 5L,
    cross_set_utility = "p_times_1_minus_p",
    reliability_EAP = NA_real_,
    ts_btl_rank_spearman = NA_real_
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
    "link_transform_mode",
    "link_refit_mode",
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
    "cross_set_ppc_mae_max",
    "link_transform_escalation_refits_required",
    "link_transform_escalation_is_one_way",
    "spoke_quantile_coverage_bins",
    "spoke_quantile_coverage_min_per_bin_per_refit",
    "multi_spoke_mode",
    "min_cross_set_pairs_per_spoke_per_refit",
    "cross_set_utility"
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
  out$link_transform_mode <- read_choice("link_transform_mode", c("auto", "shift_only", "shift_scale"))
  out$link_refit_mode <- read_choice("link_refit_mode", c("shift_only", "joint_refit"))
  out$judge_param_mode <- read_choice("judge_param_mode", c("global_shared", "phase_specific"))
  out$hub_lock_mode <- read_choice("hub_lock_mode", c("hard_lock", "soft_lock", "free"))
  out$hub_lock_kappa <- read_double("hub_lock_kappa", 0, 1)
  out$link_identified_reliability_min <- read_double("link_identified_reliability_min", 0, 1)
  out$link_stop_reliability_min <- read_double("link_stop_reliability_min", 0, 1)
  out$link_rank_corr_min <- read_double("link_rank_corr_min", 0, 1)
  out$delta_sd_max <- read_double("delta_sd_max", 0, Inf)
  out$delta_change_max <- read_double("delta_change_max", 0, Inf)
  out$log_alpha_sd_max <- read_double("log_alpha_sd_max", 0, Inf)
  out$log_alpha_change_max <- read_double("log_alpha_change_max", 0, Inf)
  out$cross_set_ppc_mae_max <- read_double("cross_set_ppc_mae_max", 0, 1)
  out$link_transform_escalation_refits_required <- read_integer(
    "link_transform_escalation_refits_required",
    1L,
    Inf
  )
  out$link_transform_escalation_is_one_way <- read_logical("link_transform_escalation_is_one_way")
  out$spoke_quantile_coverage_bins <- read_integer("spoke_quantile_coverage_bins", 1L, Inf)
  out$spoke_quantile_coverage_min_per_bin_per_refit <- read_integer(
    "spoke_quantile_coverage_min_per_bin_per_refit",
    1L,
    Inf
  )
  out$multi_spoke_mode <- read_choice("multi_spoke_mode", c("independent", "concurrent"))
  out$min_cross_set_pairs_per_spoke_per_refit <- read_integer(
    "min_cross_set_pairs_per_spoke_per_refit",
    1L,
    Inf
  )
  out$cross_set_utility <- read_choice("cross_set_utility", "p_times_1_minus_p")

  if (!is.null(out$p_long_low) &&
    !is.null(out$p_long_high) &&
    out$p_long_low >= out$p_long_high) {
    rlang::abort("`adaptive_config$p_long_low` must be strictly less than `adaptive_config$p_long_high`.")
  }

  resolved <- utils::modifyList(.adaptive_controller_defaults(n_items), out)
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
  round$star_override_budget_per_round <- as.integer(controller$star_override_budget_per_round)

  can_refresh_quotas <- as.integer(round$round_committed %||% 0L) == 0L &&
    as.integer(round$stage_index %||% 1L) == 1L &&
    all(as.integer(round$stage_committed %||% integer()) == 0L)

  if (isTRUE(can_refresh_quotas)) {
    stage_quotas <- .adaptive_round_compute_quotas(
      round_id = as.integer(round$round_id %||% 1L),
      n_items = as.integer(out$n_items),
      controller = controller
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
  set_ids <- sort(unique(as.integer(out$items$set_id)))
  hub_id <- as.integer(controller$hub_id %||% 1L)
  spoke_ids <- setdiff(set_ids, hub_id)
  out$linking <- list(
    run_mode = as.character(controller$run_mode),
    hub_id = hub_id,
    spoke_ids = as.integer(spoke_ids),
    is_multi_set = length(set_ids) > 1L
  )
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
  defaults <- .adaptive_controller_defaults(n_items)
  utils::modifyList(defaults, controller)
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
  round_pairs_target <- as.integer(defaults$round_pairs_target)

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
  defaults <- adaptive_defaults(length(ids))
  controller <- utils::modifyList(.adaptive_controller_defaults(length(ids)), controller %||% list())
  stage_order <- .adaptive_stage_order()
  stage_quotas <- .adaptive_round_compute_quotas(
    round_id = round_id,
    n_items = length(ids),
    controller = controller
  )
  quota_meta <- attr(stage_quotas, "quota_meta") %||% list()
  stage_committed <- stats::setNames(rep.int(0L, length(stage_order)), stage_order)

  list(
    round_id = round_id,
    staged_active = isTRUE(staged_active),
    stage_order = stage_order,
    stage_index = 1L,
    stage_quotas = stage_quotas,
    stage_committed = stage_committed,
    stage_shortfalls = stats::setNames(rep.int(0L, length(stage_order)), stage_order),
    round_pairs_target = as.integer(defaults$round_pairs_target),
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
    realloc_to_local = as.integer(quota_meta$realloc_to_local %||% NA_integer_)
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
        last_refit_round_id = 0L,
        theta_mean_history = list(),
        near_stop = FALSE,
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
        is_multi_set = length(unique(set_ids)) > 1L
      ),
      meta = list(
        schema_version = "adaptive-session",
        now_fn = now_fn,
        seed = 1L,
        stop_decision = FALSE,
        stop_reason = NA_character_
      )
    ),
    class = "adaptive_state"
  )

  state
}
