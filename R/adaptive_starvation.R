# Observational diagnostics: these helpers never generate or select candidates.

.adaptive_starvation_scope <- function(state) {
  controller <- .adaptive_controller_resolve(state)
  phase <- .adaptive_link_phase_context(state, controller = controller)
  ids <- as.character(state$item_ids)
  set_id <- NA_integer_
  if (.adaptive_link_mode_active(controller)) {
    set_id <- as.integer(phase$active_phase_a_set %||% NA_integer_)
    ids <- as.character(state$items$item_id[!is.na(state$items$set_id) &
      !is.na(set_id) & state$items$set_id %in% set_id])
  }
  list(item_ids = ids, set_id = set_id)
}

.adaptive_starvation_capacity <- function(state, scope) {
  ids <- scope$item_ids
  if (length(ids) < 2L) {
    return(list(max_observations_per_pair = NA_integer_,
      remaining_capacity_upper_bound = NA_real_, capacity_domain = "unavailable"))
  }
  if (.adaptive_reservoir_active(state)) {
    return(list(max_observations_per_pair = 1L,
      remaining_capacity_upper_bound = as.double(nrow(.adaptive_reservoir_unused(state))),
      capacity_domain = "replay_reservoir"))
  }
  limit <- .adaptive_controller_resolve(state)$dup_max_obs_relaxed
  history <- state$history_pairs
  history <- history[history$A_id %in% ids & history$B_id %in% ids, , drop = FALSE]
  counts <- table(make_unordered_key(history$A_id, history$B_id))
  remaining <- limit * choose(length(ids), 2) - sum(pmin(limit, as.double(counts)))
  list(max_observations_per_pair = as.integer(limit),
    remaining_capacity_upper_bound = as.double(remaining),
    capacity_domain = "all_unordered_pairs_in_scope")
}

.adaptive_starvation_attempt <- function(stage_out, step_id, round_stage, stage) {
  counts <- stage_out$counts
  generation <- stage_out$diagnostic_generation %||% list()
  before_exposure <- stage_out$n_candidates_before_exposure_filters %||% NA_integer_
  # Canonical logs historically combine hard and exposure filtering. Record the
  # additional boundary here without changing those existing log columns.
  boundaries <- c(
    generation = counts$n_candidates_generated,
    route = counts$n_candidates_after_route_filters,
    active_domain = counts$n_candidates_after_active_domain,
    stage = counts$n_candidates_after_stage_filters,
    hard = before_exposure,
    exposure = counts$n_candidates_after_exposure_filters,
    duplicates = counts$n_candidates_after_duplicates,
    star_caps = counts$n_candidates_after_star_caps,
    scoring = counts$n_candidates_scored
  )
  collapsed <- names(boundaries)[!is.na(boundaries) & boundaries <= 0L]
  admissible <- as.integer(nrow(stage_out$selected) %||% 0L)
  tibble::as_tibble(c(list(
    step_id = as.integer(step_id), round_stage = as.character(round_stage),
    fallback = as.character(stage$name), duplicate_policy = as.character(stage$dup_policy),
    bounded = as.logical(generation$bounded_direct_construction_used %||% NA),
    n_candidates_legal_domain_total = as.double(generation$n_candidates_legal_domain_total %||% NA_real_),
    n_candidates_before_exposure_filters = as.integer(before_exposure),
    n_admissible_candidates = admissible,
    exhaustion_filter = if (admissible > 0L) NA_character_ else
      if (length(collapsed) > 0L) collapsed[[1L]] else "unknown"
  ), counts))
}

.adaptive_starvation_class <- function(attempts, capacity) {
  if (any(attempts$n_admissible_candidates > 0L)) return("selection_inconsistency")
  if (isTRUE(capacity == 0)) return("pair_capacity_exhausted")
  filters <- unique(attempts$exhaustion_filter)
  groups <- ifelse(filters == "duplicates", "duplicate_policy_exhausted",
    ifelse(filters %in% c("exposure", "star_caps"), "exposure_star_cap_exhausted",
      "other_filter_exhausted"))
  groups <- unique(groups)
  if (length(groups) == 0L || anyNA(filters) || "unknown" %in% filters) return("unknown")
  if (length(groups) > 1L) return("mixed_filter_exhaustion")
  groups[[1L]]
}

.adaptive_record_starvation <- function(state, selection, attempts, step_id) {
  previous <- state$meta$starvation_diagnostic
  state$meta$starvation_diagnostic <- NULL
  if (!isTRUE(selection$candidate_starved) || is.null(attempts)) return(state)
  scope <- .adaptive_starvation_scope(state)
  committed <- nrow(state$history_pairs)
  if (!is.null(previous) && identical(previous$step_id, as.integer(step_id - 1L)) &&
    identical(previous$scope, scope) && identical(previous$committed_pairs, committed)) {
    attempts <- dplyr::bind_rows(previous$attempts, attempts)
  }
  capacity <- .adaptive_starvation_capacity(state, scope)
  state$meta$starvation_diagnostic <- c(list(
    step_id = as.integer(step_id), committed_pairs = committed, scope = scope,
    classification = .adaptive_starvation_class(attempts, capacity$remaining_capacity_upper_bound),
    admissibility_scope = "examined_pools", attempts = attempts
  ), capacity)
  state
}

.adaptive_terminal_starvation <- function(state) {
  diagnostic <- state$meta$starvation_diagnostic
  if (is.null(diagnostic) || !isTRUE(state$meta$stop_decision) ||
    !state$meta$stop_reason %in% c("candidate_starvation", "phase_a_set_unresolved") ||
    !identical(diagnostic$step_id, as.integer(nrow(state$step_log))) ||
    !identical(diagnostic$committed_pairs, nrow(state$history_pairs)) ||
    !identical(diagnostic$scope, .adaptive_starvation_scope(state)) ||
    !isTRUE(utils::tail(state$step_log$candidate_starved, 1L))) return(NULL)
  diagnostic
}

.adaptive_starvation_print <- function(state) {
  diagnostic <- .adaptive_terminal_starvation(state)
  if (is.null(diagnostic)) return(character())
  explanation <- switch(diagnostic$classification,
    pair_capacity_exhausted = "all allowed pair observations have been used",
    duplicate_policy_exhausted = "repeat rules excluded the examined pairs",
    exposure_star_cap_exhausted = "item exposure limits excluded the examined pairs",
    mixed_filter_exhaustion = "different pairing restrictions exhausted the examined pools",
    other_filter_exhausted = "other pairing restrictions exhausted the examined pools",
    selection_inconsistency = "selection stopped despite surviving candidates; inspect the attempt report",
    "the recorded evidence does not identify the restriction")
  paste0("pair availability: ", explanation, "; remaining arithmetic upper bound: ",
    format(diagnostic$remaining_capacity_upper_bound, scientific = FALSE, trim = TRUE),
    " (not a guarantee of eligible comparisons)")
}
