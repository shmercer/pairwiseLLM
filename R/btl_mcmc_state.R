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
  samples <- tibble::as_tibble(samples)
  if (!all(c("ID", "text") %in% names(samples))) {
    rlang::abort("`samples` must contain columns 'ID' and 'text'.")
  }
  ids <- as.character(samples$ID)
  if (anyDuplicated(ids)) {
    rlang::abort("`samples$ID` must be unique.")
  }
  texts <- as.character(samples$text)
  names(texts) <- ids
  N <- length(ids)

  config <- if (is.null(config)) list() else config
  d1 <- config$d1
  if (is.null(d1)) d1 <- 8
  budget_max <- config$budget_max
  if (is.null(budget_max)) budget_max <- floor(0.40 * choose(N, 2))
  M1_target <- config$M1_target
  if (is.null(M1_target)) M1_target <- floor(N * d1 / 2)

  counts <- stats::setNames(rep.int(0L, N), ids)
  unordered_keys <- .adaptive_unordered_keys(ids)
  unordered_count <- stats::setNames(rep.int(0L, length(unordered_keys)), unordered_keys)
  pair_count <- stats::setNames(rep.int(0L, length(unordered_keys)), unordered_keys)
  ordered_seen <- stats::setNames(logical(), character())
  pair_ordered_count <- stats::setNames(integer(), character())

  epsilon_prior_mean <- 2 / 22
  state <- structure(
    list(
      schema_version = as.integer(schema_version),
      ids = ids,
      texts = texts,
      N = as.integer(N),
      deg = counts,
      pos1 = counts,
      pos2 = counts,
      imb = counts,
      pos_count = counts,
      unordered_count = unordered_count,
      ordered_seen = ordered_seen,
      pair_count = pair_count,
      pair_ordered_count = pair_ordered_count,
      history_pairs = .adaptive_empty_pairs_tbl(),
      history_results = .adaptive_empty_results_tbl(),
      failed_attempts = .adaptive_empty_failed_attempts_tbl(),
      results_seen = logical(),
      fit = NULL,
      budget_max = as.integer(budget_max),
      M1_target = as.integer(M1_target),
      comparisons_scheduled = 0L,
      comparisons_observed = 0L,
      phase = "phase1",
      iter = 0L,
      wc = NULL,
      bin = NULL,
      bin_edges = NULL,
      last_check_at = 0L,
      new_since_refit = 0L,
      last_refit_at = 0L,
      posterior = list(
        U_dup_threshold = NA_real_,
        epsilon_mean = epsilon_prior_mean,
        stop_metrics = .adaptive_stop_metrics_defaults()
      ),
      mode = "warm_start",
      repair_attempts = 0L,
      stop_reason = NA_character_,
      seed = seed,
      config = config,
      batch_log = batch_log_schema(),
      log_counters = list(comparisons_observed = 0L, failed_attempts = 0L)
    ),
    class = "adaptive_state"
  )

  state <- .adaptive_state_init_logs(state)
  validate_btl_mcmc_state(state)
  state
}

#' @keywords internal
#' @noRd
btl_mcmc_state_save <- function(state, path) {
  validate_btl_mcmc_state(state)
  saveRDS(state, path)
  invisible(path)
}

#' @keywords internal
#' @noRd
btl_mcmc_state_load <- function(path) {
  state <- readRDS(path)
  state <- .adaptive_state_init_logs(state)
  validate_btl_mcmc_state(state)
  state
}
