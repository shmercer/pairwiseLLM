# -------------------------------------------------------------------------
# Adaptive v2 canonical log schemas and append helpers.
# -------------------------------------------------------------------------

schema_step_log <- c(
  step_id = "integer",
  timestamp = "POSIXct",
  pair_id = "integer",
  i = "integer",
  j = "integer",
  A = "integer",
  B = "integer",
  Y = "integer",
  status = "character",
  is_explore_step = "logical",
  explore_mode = "character",
  explore_reason = "character",
  candidate_starved = "logical",
  fallback_used = "character",
  fallback_path = "character",
  starvation_reason = "character",
  n_candidates_generated = "integer",
  n_candidates_after_hard_filters = "integer",
  n_candidates_after_duplicates = "integer",
  n_candidates_after_star_caps = "integer",
  n_candidates_scored = "integer",
  deg_i = "integer",
  deg_j = "integer",
  recent_deg_i = "integer",
  recent_deg_j = "integer",
  mu_i = "double",
  mu_j = "double",
  sigma_i = "double",
  sigma_j = "double",
  p_ij = "double",
  U0_ij = "double",
  star_cap_rejects = "integer",
  star_cap_reject_items = "character"
)

schema_round_log <- c(
  round_id = "integer",
  step_id_at_refit = "integer",
  timestamp = "POSIXct",
  model_variant = "character",
  n_items = "integer",
  total_pairs_done = "integer",
  new_pairs_since_last_refit = "integer",
  n_unique_pairs_seen = "integer",
  proposed_pairs_mode = "double",
  starve_rate_since_last_refit = "double",
  fallback_rate_since_last_refit = "double",
  fallback_used_mode = "character",
  starvation_reason_mode = "character",
  mean_degree = "double",
  min_degree = "integer",
  pos_balance_sd = "double",
  epsilon_mean = "double",
  epsilon_p2.5 = "double",
  epsilon_p5 = "double",
  epsilon_p50 = "double",
  epsilon_p95 = "double",
  epsilon_p97.5 = "double",
  b_mean = "double",
  b_p2.5 = "double",
  b_p5 = "double",
  b_p50 = "double",
  b_p95 = "double",
  b_p97.5 = "double",
  diagnostics_pass = "logical",
  divergences = "integer",
  max_rhat = "double",
  min_ess_bulk = "double",
  ess_bulk_required = "double",
  reliability_EAP = "double",
  theta_sd_eap = "double",
  rho_theta = "double",
  delta_sd_theta = "double",
  rho_rank = "double",
  rho_rank_pass = "logical",
  stop_decision = "logical",
  stop_reason = "character"
)

schema_item_log <- c(
  step_id = "integer",
  timestamp = "POSIXct",
  item_id = "integer",
  mu = "double",
  sigma = "double",
  degree = "integer"
)

.adaptive_schema_empty_col <- function(type) {
  if (identical(type, "integer")) {
    return(integer())
  }
  if (identical(type, "double")) {
    return(double())
  }
  if (identical(type, "logical")) {
    return(logical())
  }
  if (identical(type, "character")) {
    return(character())
  }
  if (identical(type, "POSIXct")) {
    return(as.POSIXct(character(), tz = "UTC"))
  }
  rlang::abort("Unknown schema column type.")
}

.adaptive_schema_typed_na <- function(type) {
  if (identical(type, "integer")) {
    return(NA_integer_)
  }
  if (identical(type, "double")) {
    return(NA_real_)
  }
  if (identical(type, "logical")) {
    return(NA)
  }
  if (identical(type, "character")) {
    return(NA_character_)
  }
  if (identical(type, "POSIXct")) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  rlang::abort("Unknown schema column type.")
}

.adaptive_schema_empty_tbl <- function(schema) {
  if (is.null(names(schema)) || any(names(schema) == "")) {
    rlang::abort("`schema` must be a named list of column types.")
  }
  cols <- lapply(schema, .adaptive_schema_empty_col)
  tibble::as_tibble(cols)
}

.adaptive_is_integerish <- function(x) {
  if (is.integer(x)) {
    return(TRUE)
  }
  if (!is.numeric(x)) {
    return(FALSE)
  }
  if (any(is.na(x))) {
    return(FALSE)
  }
  all(x == as.integer(x))
}

#' @keywords internal
#' @noRd
append_canonical_row <- function(log_tbl, row, schema, allow_multirow = FALSE) {
  if (!is.data.frame(log_tbl)) {
    rlang::abort("`log_tbl` must be a data frame.")
  }
  log_tbl <- tibble::as_tibble(log_tbl)
  schema_names <- names(schema)
  if (is.null(schema_names) || any(schema_names == "")) {
    rlang::abort("`schema` must be a named list of column types.")
  }
  missing_cols <- setdiff(schema_names, names(log_tbl))
  extra_cols <- setdiff(names(log_tbl), schema_names)
  if (length(missing_cols) > 0L || length(extra_cols) > 0L) {
    rlang::abort("`log_tbl` must have exactly the canonical columns.")
  }
  log_tbl <- log_tbl[, schema_names, drop = FALSE]

  if (is.list(row) && !is.data.frame(row)) {
    row <- tibble::as_tibble(row)
  } else if (is.data.frame(row)) {
    row <- tibble::as_tibble(row)
  } else {
    rlang::abort("`row` must be a named list or data frame.")
  }

  n_rows <- nrow(row)
  if (!allow_multirow && n_rows != 1L) {
    rlang::abort("`row` must have exactly one row.")
  }
  if (allow_multirow && n_rows < 1L) {
    rlang::abort("`row` must have at least one row.")
  }

  unknown <- setdiff(names(row), schema_names)
  if (length(unknown) > 0L) {
    rlang::abort(paste0(
      "`row` has unknown columns: ",
      paste(unknown, collapse = ", "),
      "."
    ))
  }

  missing <- setdiff(schema_names, names(row))
  if (length(missing) > 0L) {
    for (col in missing) {
      row[[col]] <- rep_len(.adaptive_schema_typed_na(schema[[col]]), n_rows)
    }
  }

  for (col in schema_names) {
    type <- schema[[col]]
    value <- row[[col]]

    if (identical(type, "POSIXct")) {
      if (!inherits(value, "POSIXct")) {
        rlang::abort(paste0("`row$", col, "` must be POSIXct."))
      }
      next
    }

    before_na <- is.na(value)
    if (identical(type, "integer")) {
      if (!all(is.na(value)) && !.adaptive_is_integerish(value[!is.na(value)])) {
        rlang::abort(paste0("`row$", col, "` must be integer-like."))
      }
      value <- suppressWarnings(as.integer(value))
    } else if (identical(type, "double")) {
      value <- suppressWarnings(as.double(value))
    } else if (identical(type, "logical")) {
      value <- suppressWarnings(as.logical(value))
    } else if (identical(type, "character")) {
      value <- suppressWarnings(as.character(value))
    } else {
      rlang::abort("Unknown schema column type.")
    }

    if (any(!before_na & is.na(value))) {
      rlang::abort(paste0(
        "`row$", col, "` could not be coerced to ",
        type,
        " without introducing NA."
      ))
    }
    row[[col]] <- value
  }

  row <- row[, schema_names, drop = FALSE]
  dplyr::bind_rows(log_tbl, row)
}

#' @keywords internal
#' @noRd
new_step_log <- function(now_fn = function() Sys.time()) {
  force(now_fn)
  .adaptive_schema_empty_tbl(schema_step_log)
}

#' @keywords internal
#' @noRd
append_step_log <- function(step_log, row) {
  append_canonical_row(step_log, row, schema_step_log, allow_multirow = FALSE)
}

#' @keywords internal
#' @noRd
new_round_log <- function() {
  .adaptive_schema_empty_tbl(schema_round_log)
}

#' @keywords internal
#' @noRd
append_round_log <- function(round_log, row) {
  append_canonical_row(round_log, row, schema_round_log, allow_multirow = FALSE)
}

#' @keywords internal
#' @noRd
new_item_log <- function(items) {
  if (!is.data.frame(items)) {
    rlang::abort("`items` must be a data frame.")
  }
  items <- tibble::as_tibble(items)
  if (!"item_id" %in% names(items)) {
    rlang::abort("`items` must include `item_id`.")
  }
  .adaptive_schema_empty_tbl(schema_item_log)
}

#' @keywords internal
#' @noRd
append_item_log <- function(item_log, rows) {
  append_canonical_row(item_log, rows, schema_item_log, allow_multirow = TRUE)
}
