# -------------------------------------------------------------------------
# Adaptive state object
# -------------------------------------------------------------------------

.adaptive_empty_pairs_tbl <- function() {
  tibble::tibble(
    pair_uid = character(),
    unordered_key = character(),
    ordered_key = character(),
    A_id = character(),
    B_id = character(),
    A_text = character(),
    B_text = character(),
    phase = character(),
    iter = integer(),
    created_at = as.POSIXct(character(), tz = "UTC")
  )
}

.adaptive_empty_results_tbl <- function() {
  tibble::tibble(
    pair_uid = character(),
    unordered_key = character(),
    ordered_key = character(),
    A_id = character(),
    B_id = character(),
    better_id = character(),
    winner_pos = integer(),
    phase = character(),
    iter = integer(),
    received_at = as.POSIXct(character(), tz = "UTC"),
    backend = character(),
    model = character()
  )
}

.adaptive_empty_failed_attempts_tbl <- function() {
  tibble::tibble(
    pair_uid = character(),
    unordered_key = character(),
    ordered_key = character(),
    A_id = character(),
    B_id = character(),
    phase = character(),
    iter = integer(),
    attempted_at = as.POSIXct(character(), tz = "UTC"),
    backend = character(),
    model = character(),
    error_code = character(),
    error_detail = character()
  )
}

.adaptive_unordered_keys <- function(ids) {
  ids <- as.character(ids)
  if (length(ids) < 2L) return(character())
  combos <- utils::combn(ids, 2)
  paste(pmin(combos[1L, ], combos[2L, ]), pmax(combos[1L, ], combos[2L, ]), sep = ":")
}

.adaptive_log_default_value <- function(col) {
  if (is.integer(col)) {
    return(NA_integer_)
  }
  if (inherits(col, "POSIXct")) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  if (is.double(col)) {
    return(NA_real_)
  }
  if (is.logical(col)) {
    return(NA)
  }
  if (is.character(col)) {
    return(NA_character_)
  }
  NA
}

.adaptive_align_log_schema <- function(log, schema) {
  if (is.null(log) || !is.data.frame(log)) {
    log <- schema
  }
  log <- tibble::as_tibble(log)
  schema <- tibble::as_tibble(schema)
  missing <- setdiff(names(schema), names(log))
  if (length(missing) > 0L) {
    for (col in missing) {
      default_val <- .adaptive_log_default_value(schema[[col]])
      log[[col]] <- rep_len(default_val, nrow(log))
    }
  }
  ordered <- c(names(schema), setdiff(names(log), names(schema)))
  log[, ordered, drop = FALSE]
}

.adaptive_state_init_logs <- function(state) {
  state$config <- state$config %||% list()

  round_log <- state$config$round_log %||% round_log_schema()
  state$config$round_log <- .adaptive_align_log_schema(round_log, round_log_schema())

  batch_log <- state$batch_log %||% batch_log_schema()
  state$batch_log <- .adaptive_align_log_schema(batch_log, batch_log_schema())

  posterior <- state$posterior %||% list()
  posterior$stop_metrics <- .adaptive_stop_metrics_align(posterior$stop_metrics %||% NULL)
  state$posterior <- posterior

  counters <- state$log_counters %||% list()
  if (!is.list(counters)) {
    counters <- list()
  }
  counters$comparisons_observed <- as.integer(counters$comparisons_observed %||%
    state$comparisons_observed %||% 0L)
  counters$failed_attempts <- as.integer(counters$failed_attempts %||%
    nrow(state$failed_attempts %||% tibble::tibble()))
  state$log_counters <- counters

  state
}

#' @keywords internal
#' @noRd
btl_mcmc_state_new <- function(samples, config, seed = NULL, schema_version = 1L) {
  rlang::abort(
    "Legacy scaffold state constructors are disabled. Use `adaptive_rank_start()` for adaptive runs."
  )
}

#' @keywords internal
#' @noRd
btl_mcmc_state_save <- function(state, path) {
  rlang::abort(
    "Legacy scaffold state serialization is disabled."
  )
}

#' @keywords internal
#' @noRd
btl_mcmc_state_load <- function(path) {
  rlang::abort(
    "Legacy scaffold state serialization is disabled."
  )
}
