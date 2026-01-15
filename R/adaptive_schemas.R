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
      .adaptive_check_type(
        pairs[[nm]],
        paste0("pairs$", nm),
        if (identical(optional[[nm]], is.double)) "double" else if (identical(optional[[nm]], is.integer)) "integer" else "character",
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
validate_state <- function(state) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
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

  counts <- c("deg", "pos1", "pos2", "imb")
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

  if (!(is.logical(state$ordered_seen) || is.environment(state$ordered_seen))) {
    rlang::abort("`state$ordered_seen` must be a named logical vector or environment.")
  }

  validate_pairs_tbl(state$history_pairs)
  validate_results_tbl(state$history_results)
  validate_failed_attempts_tbl(state$failed_attempts)

  if (!is.integer(state$comparisons_scheduled) || length(state$comparisons_scheduled) != 1L) {
    rlang::abort("`state$comparisons_scheduled` must be a length-1 integer.")
  }
  if (!is.integer(state$comparisons_observed) || length(state$comparisons_observed) != 1L) {
    rlang::abort("`state$comparisons_observed` must be a length-1 integer.")
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
  if (!is.numeric(state$U0) || length(state$U0) != 1L) {
    rlang::abort("`state$U0` must be a length-1 numeric value.")
  }
  if (!is.integer(state$last_check_at) || length(state$last_check_at) != 1L) {
    rlang::abort("`state$last_check_at` must be a length-1 integer.")
  }
  if (!is.logical(state$stop_candidate) || length(state$stop_candidate) != 1L) {
    rlang::abort("`state$stop_candidate` must be a length-1 logical.")
  }
  if (!is.integer(state$checks_passed_in_row) || length(state$checks_passed_in_row) != 1L) {
    rlang::abort("`state$checks_passed_in_row` must be a length-1 integer.")
  }

  invisible(state)
}
