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
.adaptive_round_compute_quotas <- function(round_id, n_items) {
  round_id <- as.integer(round_id %||% 1L)
  defaults <- adaptive_defaults(n_items)
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
  long_quota <- as.integer(ceiling(long_frac * round_pairs_target))
  mid_quota <- as.integer(ceiling(mid_frac * round_pairs_target))
  local_quota <- as.integer(round_pairs_target - (anchor_quota + long_quota + mid_quota))

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

  quotas
}

#' @keywords internal
#' @noRd
.adaptive_new_round_state <- function(item_ids, round_id = 1L, staged_active = FALSE) {
  ids <- as.character(item_ids)
  round_id <- as.integer(round_id %||% 1L)
  defaults <- adaptive_defaults(length(ids))
  stage_order <- .adaptive_stage_order()
  stage_quotas <- .adaptive_round_compute_quotas(round_id = round_id, n_items = length(ids))
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
    anchor_ids = character(),
    anchor_refresh_source = NA_character_,
    anchor_refit_round_id = 0L,
    anchor_round_id = as.integer(round_id),
    committed_total = 0L
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
      round = .adaptive_new_round_state(item_ids, round_id = 1L, staged_active = FALSE),
      btl_fit = NULL,
      stop_metrics = NULL,
      refit_meta = list(
        last_refit_M_done = 0L,
        last_refit_step = 0L,
        last_refit_round_id = 0L,
        theta_mean_history = list(),
        near_stop = FALSE
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
