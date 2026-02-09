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
    reliability_EAP = NA_real_,
    ts_btl_rank_spearman = NA_real_
  )
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
  item_index <- stats::setNames(seq_along(item_ids), item_ids)
  history_pairs <- tibble::tibble(
    A_id = character(),
    B_id = character()
  )

  state <- structure(
    list(
      item_ids = item_ids,
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
