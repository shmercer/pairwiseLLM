# -------------------------------------------------------------------------
# Adaptive schemas and validation helpers
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
as_pairs_tbl <- function(...) {
  pairs <- tibble::tibble(...)
  pairs <- validate_pairs_tbl(pairs)
  required <- c(
    "pair_uid", "unordered_key", "ordered_key",
    "A_id", "B_id", "A_text", "B_text",
    "phase", "iter", "created_at"
  )
  pairs[, c(required, setdiff(names(pairs), required)), drop = FALSE]
}

.adaptive_required_cols <- function(tbl, name, required) {
  missing <- setdiff(required, names(tbl))
  if (length(missing) > 0L) {
    rlang::abort(paste0(
      "`", name, "` is missing required columns: ",
      paste(missing, collapse = ", "),
      "."
    ))
  }
}

.adaptive_check_type <- function(x, name, type_label, predicate, ...) {
  if (!predicate(x, ...)) {
    rlang::abort(paste0("`", name, "` must be ", type_label, "."))
  }
}

.adaptive_check_phase <- function(phase, name) {
  allowed <- c("phase1", "phase2", "phase3")
  bad <- !is.na(phase) & !phase %in% allowed
  if (any(bad)) {
    rlang::abort(paste0(
      "`", name, "` must be one of: ",
      paste(allowed, collapse = ", "),
      "."
    ))
  }
}

.adaptive_fallback_used_levels <- function() {
  c(
    "base",
    "expand_locality",
    "uncertainty_pool",
    "dup_relax",
    "global_safe",
    "warm_start",
    "FAILED"
  )
}

.adaptive_starvation_reason_levels <- function() {
  c(
    "few_candidates_generated",
    "filtered_by_duplicates",
    "filtered_by_paircount",
    "filtered_by_other_filters",
    "unknown"
  )
}

#' @keywords internal
#' @noRd
validate_pairs_tbl <- function(pairs) {
  if (!is.data.frame(pairs)) {
    rlang::abort("`pairs` must be a data frame or tibble.")
  }
  pairs <- tibble::as_tibble(pairs)
  required <- c(
    "pair_uid", "unordered_key", "ordered_key",
    "A_id", "B_id", "A_text", "B_text",
    "phase", "iter", "created_at"
  )
  .adaptive_required_cols(pairs, "pairs", required)
  .adaptive_check_type(pairs$pair_uid, "pairs$pair_uid", "character", is.character)
  .adaptive_check_type(pairs$unordered_key, "pairs$unordered_key", "character", is.character)
  .adaptive_check_type(pairs$ordered_key, "pairs$ordered_key", "character", is.character)
  .adaptive_check_type(pairs$A_id, "pairs$A_id", "character", is.character)
  .adaptive_check_type(pairs$B_id, "pairs$B_id", "character", is.character)
  .adaptive_check_type(pairs$A_text, "pairs$A_text", "character", is.character)
  .adaptive_check_type(pairs$B_text, "pairs$B_text", "character", is.character)
  .adaptive_check_type(pairs$phase, "pairs$phase", "character", is.character)
  .adaptive_check_type(pairs$iter, "pairs$iter", "integer", is.integer)
  .adaptive_check_type(pairs$created_at, "pairs$created_at", "POSIXct", inherits, "POSIXct")
  .adaptive_check_phase(pairs$phase, "pairs$phase")

  optional <- list(
    batch_id = is.character,
    backend = is.character,
    model = is.character,
    utility = is.double,
    utility_raw = is.double,
    deg_A = is.integer,
    deg_B = is.integer,
    imb_A = is.integer,
    imb_B = is.integer,
    wc_A = is.integer,
    wc_B = is.integer,
    bin_A = is.integer,
    bin_B = is.integer
  )
  for (nm in names(optional)) {
    if (nm %in% names(pairs)) {
      type_label <- if (identical(optional[[nm]], is.double)) {
        "double"
      } else if (identical(optional[[nm]], is.integer)) {
        "integer"
      } else {
        "character"
      }
      .adaptive_check_type(
        pairs[[nm]],
        paste0("pairs$", nm),
        type_label,
        optional[[nm]]
      )
    }
  }

  invisible(pairs)
}

#' @keywords internal
#' @noRd
validate_results_tbl <- function(results) {
  if (!is.data.frame(results)) {
    rlang::abort("`results` must be a data frame or tibble.")
  }
  results <- tibble::as_tibble(results)
  required <- c(
    "pair_uid", "unordered_key", "ordered_key",
    "A_id", "B_id", "better_id", "winner_pos",
    "phase", "iter", "received_at", "backend", "model"
  )
  .adaptive_required_cols(results, "results", required)
  .adaptive_check_type(results$pair_uid, "results$pair_uid", "character", is.character)
  .adaptive_check_type(results$unordered_key, "results$unordered_key", "character", is.character)
  .adaptive_check_type(results$ordered_key, "results$ordered_key", "character", is.character)
  .adaptive_check_type(results$A_id, "results$A_id", "character", is.character)
  .adaptive_check_type(results$B_id, "results$B_id", "character", is.character)
  .adaptive_check_type(results$better_id, "results$better_id", "character", is.character)
  .adaptive_check_type(results$winner_pos, "results$winner_pos", "integer", is.integer)
  .adaptive_check_type(results$phase, "results$phase", "character", is.character)
  .adaptive_check_type(results$iter, "results$iter", "integer", is.integer)
  .adaptive_check_type(results$received_at, "results$received_at", "POSIXct", inherits, "POSIXct")
  .adaptive_check_type(results$backend, "results$backend", "character", is.character)
  .adaptive_check_type(results$model, "results$model", "character", is.character)
  .adaptive_check_phase(results$phase, "results$phase")

  has_self_pair <- !is.na(results$A_id) & !is.na(results$B_id) & results$A_id == results$B_id
  if (any(has_self_pair)) {
    rlang::abort("`results` must not contain self-pairs (`A_id == B_id`).")
  }

  missing_better <- is.na(results$better_id)
  if (any(missing_better)) {
    rlang::abort("`results$better_id` must be non-missing and match `A_id` or `B_id`.")
  }
  bad_better <- !(results$better_id == results$A_id | results$better_id == results$B_id)
  if (any(bad_better)) {
    rlang::abort("`results$better_id` must match `A_id` or `B_id` for every row.")
  }

  bad_pos <- !is.na(results$winner_pos) & !results$winner_pos %in% c(1L, 2L)
  if (any(bad_pos)) {
    rlang::abort("`results$winner_pos` must be 1 or 2.")
  }
  missing_pos <- is.na(results$winner_pos)
  if (any(missing_pos)) {
    rlang::abort("`results$winner_pos` must be 1 or 2.")
  }
  pos_mismatch <- !is.na(results$winner_pos) &
    !((results$winner_pos == 1L & results$better_id == results$A_id) |
      (results$winner_pos == 2L & results$better_id == results$B_id))
  if (any(pos_mismatch)) {
    rlang::abort("`results$winner_pos` must align with `better_id`.")
  }

  invisible(results)
}

#' @keywords internal
#' @noRd
validate_failed_attempts_tbl <- function(failed_attempts) {
  if (!is.data.frame(failed_attempts)) {
    rlang::abort("`failed_attempts` must be a data frame or tibble.")
  }
  failed_attempts <- tibble::as_tibble(failed_attempts)
  required <- c(
    "pair_uid", "unordered_key", "ordered_key",
    "A_id", "B_id", "phase", "iter",
    "attempted_at", "backend", "model",
    "error_code", "error_detail"
  )
  .adaptive_required_cols(failed_attempts, "failed_attempts", required)
  .adaptive_check_type(failed_attempts$pair_uid, "failed_attempts$pair_uid", "character", is.character)
  .adaptive_check_type(failed_attempts$unordered_key, "failed_attempts$unordered_key", "character", is.character)
  .adaptive_check_type(failed_attempts$ordered_key, "failed_attempts$ordered_key", "character", is.character)
  .adaptive_check_type(failed_attempts$A_id, "failed_attempts$A_id", "character", is.character)
  .adaptive_check_type(failed_attempts$B_id, "failed_attempts$B_id", "character", is.character)
  .adaptive_check_type(failed_attempts$phase, "failed_attempts$phase", "character", is.character)
  .adaptive_check_type(failed_attempts$iter, "failed_attempts$iter", "integer", is.integer)
  .adaptive_check_type(failed_attempts$attempted_at, "failed_attempts$attempted_at", "POSIXct", inherits, "POSIXct")
  .adaptive_check_type(failed_attempts$backend, "failed_attempts$backend", "character", is.character)
  .adaptive_check_type(failed_attempts$model, "failed_attempts$model", "character", is.character)
  .adaptive_check_type(failed_attempts$error_code, "failed_attempts$error_code", "character", is.character)
  .adaptive_check_type(failed_attempts$error_detail, "failed_attempts$error_detail", "character", is.character)
  .adaptive_check_phase(failed_attempts$phase, "failed_attempts$phase")

  base_codes <- c("invalid_winner", "parse_error", "backend_missing_fields", "http_error", "timeout")
  extra_codes <- getOption("pairwiseLLM.allowed_error_codes")
  if (!is.null(extra_codes)) {
    extra_codes <- as.character(extra_codes)
  }
  allowed <- unique(c(base_codes, extra_codes))
  bad_code <- is.na(failed_attempts$error_code) | failed_attempts$error_code == "" |
    !failed_attempts$error_code %in% allowed
  if (any(bad_code)) {
    rlang::abort("`failed_attempts$error_code` must be a supported non-empty value.")
  }

  invisible(failed_attempts)
}

#' @keywords internal
#' @noRd
validate_btl_mcmc_state <- function(state) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  is_canonical_runtime <- !is.null(state$item_ids) &&
    !is.null(state$step_log) &&
    !is.null(state$round_log)
  if (isTRUE(is_canonical_runtime)) {
    rlang::abort("Canonical adaptive runtime state is not supported by this validator.")
  }
  if ("fast_fit" %in% names(state)) {
    rlang::abort("`state$fast_fit` is no longer supported; use `state$fit`.")
  }
  if (!"fit" %in% names(state)) {
    rlang::abort("`state$fit` must be present (NULL or a BTL fit contract).")
  }
  if (!is.integer(state$schema_version) || length(state$schema_version) != 1L) {
    rlang::abort("`state$schema_version` must be a length-1 integer.")
  }
  if (!is.character(state$ids)) {
    rlang::abort("`state$ids` must be character.")
  }
  if (!is.integer(state$N) || length(state$N) != 1L) {
    rlang::abort("`state$N` must be a length-1 integer.")
  }
  if (state$N != length(state$ids)) {
    rlang::abort("`state$N` must equal length of `state$ids`.")
  }
  if (!is.character(state$texts) || is.null(names(state$texts))) {
    rlang::abort("`state$texts` must be a named character vector.")
  }
  if (!identical(names(state$texts), state$ids)) {
    rlang::abort("`state$texts` names must match `state$ids`.")
  }
  if (!is.null(state$fit)) {
    validate_btl_fit_contract(state$fit, ids = state$ids)
  }

  counts <- c("deg", "pos1", "pos2", "imb", "pos_count")
  for (nm in counts) {
    vec <- state[[nm]]
    if (!is.integer(vec) || is.null(names(vec)) || !identical(names(vec), state$ids)) {
      rlang::abort(paste0("`state$", nm, "` must be a named integer vector over `ids`."))
    }
  }

  if (!all(state$deg == state$pos1 + state$pos2)) {
    rlang::abort("`state$deg` must equal `state$pos1 + state$pos2`.")
  }
  if (!all(state$imb == state$pos1 - state$pos2)) {
    rlang::abort("`state$imb` must equal `state$pos1 - state$pos2`.")
  }

  if (!is.integer(state$unordered_count)) {
    rlang::abort("`state$unordered_count` must be an integer vector.")
  }

  if (!is.integer(state$pair_count)) {
    rlang::abort("`state$pair_count` must be an integer vector.")
  }
  if (!is.null(state$pair_count) && length(state$pair_count) > 0L && is.null(names(state$pair_count))) {
    rlang::abort("`state$pair_count` must be named when non-empty.")
  }
  if (any(state$pair_count < 0L, na.rm = TRUE)) {
    rlang::abort("`state$pair_count` must be non-negative.")
  }
  .btl_mcmc_validate_pair_keys(names(state$pair_count), state$ids, ordered = FALSE, "state$pair_count")

  if (!is.integer(state$pair_ordered_count)) {
    rlang::abort("`state$pair_ordered_count` must be an integer vector.")
  }
  if (!is.null(state$pair_ordered_count) &&
    length(state$pair_ordered_count) > 0L &&
    is.null(names(state$pair_ordered_count))) {
    rlang::abort("`state$pair_ordered_count` must be named when non-empty.")
  }
  if (any(state$pair_ordered_count < 0L, na.rm = TRUE)) {
    rlang::abort("`state$pair_ordered_count` must be non-negative.")
  }
  .btl_mcmc_validate_pair_keys(
    names(state$pair_ordered_count),
    state$ids,
    ordered = TRUE,
    "state$pair_ordered_count"
  )

  if (!(is.logical(state$ordered_seen) || is.environment(state$ordered_seen))) {
    rlang::abort("`state$ordered_seen` must be a named logical vector or environment.")
  }

  validate_pairs_tbl(state$history_pairs)
  validate_results_tbl(state$history_results)
  validate_failed_attempts_tbl(state$failed_attempts)
  if (!is.null(state$results_seen)) {
    if (!(is.logical(state$results_seen) || is.environment(state$results_seen))) {
      rlang::abort("`state$results_seen` must be a named logical vector or environment.")
    }
    if (is.logical(state$results_seen) &&
      length(state$results_seen) > 0L &&
      is.null(names(state$results_seen))) {
      rlang::abort("`state$results_seen` must be named when non-empty.")
    }
  }

  if (!is.integer(state$comparisons_scheduled) || length(state$comparisons_scheduled) != 1L) {
    rlang::abort("`state$comparisons_scheduled` must be a length-1 integer.")
  }
  if (!is.integer(state$comparisons_observed) || length(state$comparisons_observed) != 1L) {
    rlang::abort("`state$comparisons_observed` must be a length-1 integer.")
  }
  if (state$comparisons_scheduled < 0L) {
    rlang::abort("`state$comparisons_scheduled` must be non-negative.")
  }
  if (state$comparisons_observed < 0L) {
    rlang::abort("`state$comparisons_observed` must be non-negative.")
  }
  if (state$comparisons_scheduled != nrow(state$history_pairs)) {
    rlang::abort("`state$comparisons_scheduled` must equal nrow(history_pairs).")
  }
  if (state$comparisons_observed > state$comparisons_scheduled) {
    rlang::abort("`state$comparisons_observed` cannot exceed `comparisons_scheduled`.")
  }
  if (nrow(state$history_results) != state$comparisons_observed) {
    rlang::abort("`state$history_results` must have rows equal to comparisons_observed.")
  }
  total_pairs <- as.integer(state$N * (state$N - 1L) / 2L)
  n_unique_pairs_seen <- sum(state$pair_count >= 1L)
  if (n_unique_pairs_seen > total_pairs) {
    rlang::abort("`state$pair_count` implies more unique pairs than possible for `state$ids`.")
  }

  if (!is.character(state$phase) || length(state$phase) != 1L) {
    rlang::abort("`state$phase` must be a length-1 character value.")
  }
  .adaptive_check_phase(state$phase, "state$phase")
  if (!is.integer(state$iter) || length(state$iter) != 1L) {
    rlang::abort("`state$iter` must be a length-1 integer.")
  }

  if (!is.integer(state$budget_max) || length(state$budget_max) != 1L) {
    rlang::abort("`state$budget_max` must be a length-1 integer.")
  }
  if (!is.integer(state$M1_target) || length(state$M1_target) != 1L) {
    rlang::abort("`state$M1_target` must be a length-1 integer.")
  }
  if (!is.integer(state$last_check_at) || length(state$last_check_at) != 1L) {
    rlang::abort("`state$last_check_at` must be a length-1 integer.")
  }
  if (!is.integer(state$new_since_refit) || length(state$new_since_refit) != 1L) {
    rlang::abort("`state$new_since_refit` must be a length-1 integer.")
  }
  if (state$new_since_refit < 0L) {
    rlang::abort("`state$new_since_refit` must be non-negative.")
  }
  if (!is.integer(state$last_refit_at) || length(state$last_refit_at) != 1L) {
    rlang::abort("`state$last_refit_at` must be a length-1 integer.")
  }
  if (state$last_refit_at < 0L) {
    rlang::abort("`state$last_refit_at` must be non-negative.")
  }
  if (state$last_refit_at > state$comparisons_observed) {
    rlang::abort("`state$last_refit_at` cannot exceed `comparisons_observed`.")
  }
  if (state$new_since_refit > state$comparisons_observed) {
    rlang::abort("`state$new_since_refit` cannot exceed `comparisons_observed`.")
  }
  expected_new_since <- as.integer(state$comparisons_observed - state$last_refit_at)
  if (state$new_since_refit != expected_new_since) {
    rlang::abort("`state$new_since_refit` must equal `comparisons_observed - last_refit_at`.")
  }

  if (!is.list(state$posterior)) {
    rlang::abort("`state$posterior` must be a list.")
  }
  if (is.null(state$posterior$U_dup_threshold)) {
    rlang::abort("`state$posterior$U_dup_threshold` must be present.")
  }
  if (!is.numeric(state$posterior$U_dup_threshold) ||
    length(state$posterior$U_dup_threshold) != 1L) {
    rlang::abort("`state$posterior$U_dup_threshold` must be numeric length 1.")
  }

  allowed_modes <- c("warm_start", "adaptive", "repair", "stopped")
  if (!is.character(state$mode) || length(state$mode) != 1L) {
    rlang::abort("`state$mode` must be a length-1 character value.")
  }
  if (!state$mode %in% allowed_modes) {
    rlang::abort(paste0("`state$mode` must be one of: ", paste(allowed_modes, collapse = ", "), "."))
  }
  if (!is.integer(state$repair_attempts) || length(state$repair_attempts) != 1L) {
    rlang::abort("`state$repair_attempts` must be a length-1 integer.")
  }
  if (state$repair_attempts < 0L) {
    rlang::abort("`state$repair_attempts` must be non-negative.")
  }
  if (!is.null(state$stop_reason)) {
    if (!is.character(state$stop_reason) || length(state$stop_reason) != 1L) {
      rlang::abort("`state$stop_reason` must be a length-1 character value or NULL.")
    }
  }

  invisible(state)
}

#' @keywords internal
#' @noRd
validate_state <- function(state) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  schema_version <- state$meta$schema_version %||% NA_character_
  if (!is.character(schema_version) || length(schema_version) != 1L || is.na(schema_version) || schema_version == "") {
    rlang::abort("`state$meta$schema_version` must be a non-empty string.")
  }
  if (!is.character(state$item_ids) || length(state$item_ids) < 2L) {
    rlang::abort("`state$item_ids` must include at least two item ids.")
  }
  if (!is.integer(state$n_items) || state$n_items != length(state$item_ids)) {
    rlang::abort("`state$n_items` must match `state$item_ids`.")
  }
  if (!is.data.frame(state$items)) {
    rlang::abort("`state$items` must be a data frame.")
  }
  items <- tibble::as_tibble(state$items)
  req_item_cols <- c("item_id", "set_id", "global_item_id")
  missing_item_cols <- setdiff(req_item_cols, names(items))
  if (length(missing_item_cols) > 0L) {
    rlang::abort(paste0(
      "`state$items` must include columns: ",
      paste(req_item_cols, collapse = ", "),
      ". Missing: ",
      paste(missing_item_cols, collapse = ", "),
      "."
    ))
  }
  if (!is.character(items$item_id)) {
    rlang::abort("`state$items$item_id` must be character.")
  }
  if (!.adaptive_is_integerish(items$set_id) || any(is.na(items$set_id))) {
    rlang::abort("`state$items$set_id` must be non-missing integer-like values.")
  }
  if (!is.character(items$global_item_id) || any(is.na(items$global_item_id) | items$global_item_id == "")) {
    rlang::abort("`state$items$global_item_id` must be non-missing character values.")
  }
  if (anyDuplicated(items$global_item_id)) {
    rlang::abort("`state$items$global_item_id` must be unique.")
  }
  global_item_ids <- state$global_item_ids %||% as.character(items$global_item_id)
  if (!is.character(global_item_ids) || length(global_item_ids) != length(state$item_ids)) {
    rlang::abort("`state$global_item_ids` must be character with one value per item.")
  }
  set_ids_state <- state$set_ids %||% as.integer(items$set_id)
  if (!.adaptive_is_integerish(set_ids_state) || length(set_ids_state) != length(state$item_ids)) {
    rlang::abort("`state$set_ids` must be integer with one value per item.")
  }
  linking <- state$linking %||% list(
    run_mode = "within_set",
    hub_id = 1L,
    spoke_ids = setdiff(unique(as.integer(items$set_id)), 1L),
    is_multi_set = length(unique(as.integer(items$set_id))) > 1L
  )
  if (!is.list(linking)) {
    rlang::abort("`state$linking` must be a list.")
  }
  run_mode <- as.character(linking$run_mode %||% NA_character_)
  if (!run_mode %in% c("within_set", "link_one_spoke", "link_multi_spoke")) {
    rlang::abort("`state$linking$run_mode` must be within_set, link_one_spoke, or link_multi_spoke.")
  }
  if (!.adaptive_is_integerish(linking$hub_id) || length(linking$hub_id) != 1L || is.na(linking$hub_id)) {
    rlang::abort("`state$linking$hub_id` must be a non-missing integer value.")
  }
  set_ids <- unique(as.integer(items$set_id))
  is_link_mode <- run_mode %in% c("link_one_spoke", "link_multi_spoke")
  if (isTRUE(is_link_mode) && length(set_ids) < 2L) {
    rlang::abort("Linking run modes require at least two unique `set_id` values.")
  }
  if (isTRUE(is_link_mode) && !as.integer(linking$hub_id) %in% set_ids) {
    rlang::abort("`state$linking$hub_id` must match one observed `state$items$set_id` in linking mode.")
  }
  if (identical(run_mode, "link_one_spoke")) {
    spoke_ids <- setdiff(set_ids, as.integer(linking$hub_id))
    if (length(spoke_ids) != 1L) {
      rlang::abort("`state$linking$run_mode = \"link_one_spoke\"` requires exactly one spoke set.")
    }
  }
  controller <- .adaptive_controller_resolve(state)
  if (isTRUE(is_link_mode) &&
    identical(controller$multi_spoke_mode, "concurrent") &&
    !controller$hub_lock_mode %in% c("hard_lock", "soft_lock")) {
    rlang::abort(
      "`state$controller$hub_lock_mode` must be hard_lock or soft_lock when multi_spoke_mode is concurrent."
    )
  }
  if (!is.null(state$trueskill_state)) {
    validate_trueskill_state(state$trueskill_state)
  }
  if (!is.data.frame(state$history_pairs)) {
    rlang::abort("`state$history_pairs` must be a data frame.")
  }

  invisible(state)
}
