# -------------------------------------------------------------------------
# Adaptive entrypoints.
# -------------------------------------------------------------------------

.adaptive_build_warm_start_pairs <- function(item_ids, seed) {
  item_ids <- as.character(item_ids)
  if (length(item_ids) < 2L) {
    return(tibble::tibble(i_id = character(), j_id = character()))
  }
  seed <- .adaptive_validate_seed(seed)
  shuffled <- withr::with_seed(seed, sample(item_ids, size = length(item_ids)))
  i_ids <- shuffled[-length(shuffled)]
  j_ids <- shuffled[-1L]
  tibble::tibble(i_id = i_ids, j_id = j_ids)
}

#' Adaptive ranking
#'
#' @description
#' Creates an adaptive state object with canonical logs. Pair selection and
#' stepwise execution are implemented in later steps.
#'
#' @param items A vector or data frame of items. Data frames must include an
#'   `item_id` column (or `id`/`ID`). Item IDs may be character; internal logs
#'   use integer indices derived from these IDs.
#' @param seed Integer seed used for deterministic warm-start shuffling and
#'   selection randomness.
#' @param session_dir Optional directory for saving session artifacts.
#' @param persist_item_log Logical; when TRUE, write per-refit item logs to disk.
#' @param ... Internal/testing only. Supply `now_fn` to override the clock used
#'   for timestamps.
#'
#' @return An adaptive state object containing `step_log`, `round_log`, and
#'   `item_log`.
#' @export
adaptive_rank_start <- function(items,
                                seed = 1L,
                                session_dir = NULL,
                                persist_item_log = FALSE,
                                ...) {
  dots <- list(...)
  if (length(dots) > 0L) {
    dot_names <- names(dots)
    if (is.null(dot_names) || any(dot_names == "")) {
      rlang::abort("Only named `now_fn` is supported in `...` for now.")
    }
    bad <- setdiff(dot_names, "now_fn")
    if (length(bad) > 0L) {
      rlang::abort("Only `now_fn` is supported in `...` for now.")
    }
  }
  if (!is.null(session_dir) &&
    (!is.character(session_dir) || length(session_dir) != 1L)) {
    rlang::abort("`session_dir` must be a single string.")
  }
  if (!is.logical(persist_item_log) ||
    length(persist_item_log) != 1L ||
    is.na(persist_item_log)) {
    rlang::abort("`persist_item_log` must be TRUE or FALSE.")
  }
  seed <- .adaptive_validate_seed(seed)
  now_fn <- dots$now_fn %||% function() Sys.time()
  state <- new_adaptive_state(items, now_fn = now_fn)
  state$meta$seed <- seed
  state$warm_start_pairs <- .adaptive_build_warm_start_pairs(state$item_ids, seed)
  state$warm_start_idx <- 1L
  state$warm_start_done <- nrow(state$warm_start_pairs) == 0L
  state$config$session_dir <- session_dir %||% NULL
  state$config$persist_item_log <- isTRUE(persist_item_log)
  if (!is.null(session_dir)) {
    save_adaptive_session(state, session_dir = session_dir, overwrite = FALSE)
  }
  state
}

#' Adaptive ranking live runner
#'
#' @description
#' Executes adaptive ranking steps using an injected judge function.
#'
#' @param state An adaptive state object created by [adaptive_rank_start()].
#' @param judge A function called as `judge(A, B, state, ...)` that returns a
#'   list with `is_valid = TRUE` and `Y` in `0/1`, or `is_valid = FALSE` with
#'   `invalid_reason`.
#' @param n_steps Number of steps to execute.
#' @param fit_fn Optional BTL fit function for deterministic testing; defaults
#'   to `default_btl_fit_fn()` when a refit is due.
#' @param btl_config Optional list overriding BTL refit/stop defaults.
#' @param session_dir Optional directory for saving session artifacts.
#' @param persist_item_log Logical; when TRUE, write per-refit item logs to disk.
#' @param progress Progress output: "all", "refits", "steps", or "none".
#' @param progress_redraw_every Redraw progress bar every N steps.
#' @param progress_show_events Logical; when TRUE, print notable step events.
#' @param progress_errors Logical; when TRUE, include invalid-step events.
#' @param ... Additional arguments passed through to `judge()`.
#'
#' @export
adaptive_rank_run_live <- function(state,
                                   judge,
                                   n_steps = 1L,
                                   fit_fn = NULL,
                                   btl_config = NULL,
                                   session_dir = NULL,
                                   persist_item_log = NULL,
                                   progress = c("all", "refits", "steps", "none"),
                                   progress_redraw_every = 10L,
                                   progress_show_events = TRUE,
                                   progress_errors = TRUE,
                                   ...) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  if (!is.function(judge)) {
    rlang::abort("`judge` must be a function.")
  }
  n_steps <- as.integer(n_steps)
  if (length(n_steps) != 1L || is.na(n_steps) || n_steps < 1L) {
    rlang::abort("`n_steps` must be a positive integer.")
  }
  if (!is.null(session_dir) &&
    (!is.character(session_dir) || length(session_dir) != 1L)) {
    rlang::abort("`session_dir` must be a single string.")
  }
  if (!is.null(persist_item_log) &&
    (!is.logical(persist_item_log) || length(persist_item_log) != 1L)) {
    rlang::abort("`persist_item_log` must be TRUE or FALSE.")
  }

  if (!is.null(session_dir)) {
    state$config$session_dir <- session_dir
  }
  if (!is.null(persist_item_log)) {
    state$config$persist_item_log <- isTRUE(persist_item_log)
  }

  cfg <- .adaptive_progress_config(
    progress = progress,
    progress_redraw_every = progress_redraw_every,
    progress_show_events = progress_show_events,
    progress_errors = progress_errors
  )
  btl_cfg <- .adaptive_btl_resolve_config(state, btl_config)
  btl_cfg$refit_pairs_target <- .adaptive_refit_pairs_target(state, btl_cfg)
  cfg$refit_pairs_target <- btl_cfg$refit_pairs_target
  cfg$stop_thresholds <- btl_cfg

  progress_handle <- adaptive_progress_init(state, cfg)
  on.exit(adaptive_progress_finish(progress_handle), add = TRUE)

  remaining <- n_steps
  while (remaining > 0L) {
    state <- run_one_step(state, judge, ...)
    step_row <- tibble::as_tibble(state$step_log)[nrow(state$step_log), , drop = FALSE]
    event <- adaptive_progress_step_event(step_row, cfg)
    if (!is.null(event)) {
      cli::cli_inform(event)
    }
    if (isTRUE(step_row$candidate_starved[[1L]])) {
      state$meta$stop_decision <- TRUE
      state$meta$stop_reason <- "candidate_starvation"
      if (!is.null(state$config$session_dir)) {
        save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
      }
      return(state)
    }

    refit_out <- maybe_refit_btl(state, config = btl_cfg, fit_fn = fit_fn)
    state <- refit_out$state
    if (isTRUE(refit_out$refit_performed)) {
      cfg$stop_thresholds <- refit_out$config
      metrics <- compute_stop_metrics(state, config = refit_out$config)
      state$stop_metrics <- metrics
      state <- .adaptive_maybe_enter_phase3(state, metrics, refit_out$config)
      stop_decision <- should_stop(metrics, config = refit_out$config)
      stop_reason <- if (isTRUE(stop_decision)) "btl_converged" else NA_character_

      round_row <- .adaptive_round_log_row(
        state = state,
        metrics = metrics,
        stop_decision = stop_decision,
        stop_reason = stop_reason,
        refit_context = refit_out$refit_context,
        config = refit_out$config
      )
      state$round_log <- append_round_log(state$round_log, round_row)
      item_log_tbl <- .adaptive_build_item_log_refit(
        state,
        refit_id = round_row$round_id
      )
      state <- .adaptive_append_item_log(state, item_log_tbl)
      if (!is.null(state$config$session_dir) &&
        isTRUE(state$config$persist_item_log)) {
        paths <- .adaptive_session_paths(state$config$session_dir)
        .adaptive_write_item_log_files(state$item_log, paths$item_log_dir)
      }
      if (cfg$progress %in% c("all", "refits")) {
        block <- adaptive_progress_refit_block(
          tibble::as_tibble(round_row),
          cfg
        )
        if (length(block) > 0L) {
          cat(paste(block, collapse = "\n"), "\n")
        }
      }
      if (isTRUE(stop_decision)) {
        state$meta$stop_decision <- TRUE
        state$meta$stop_reason <- stop_reason
        if (!is.null(state$config$session_dir)) {
          save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
        }
        return(state)
      }
    }
    if (!is.null(state$config$session_dir)) {
      save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
    }
    progress_handle <- adaptive_progress_update(progress_handle, state, cfg)
    remaining <- remaining - 1L
  }

  state
}

#' Adaptive ranking resume
#'
#' @description
#' Resume an adaptive session from disk.
#'
#' @param session_dir Directory containing session artifacts.
#' @param ... Reserved for future extensions; currently unused.
#'
#' @export
adaptive_rank_resume <- function(session_dir, ...) {
  if (missing(session_dir) || is.null(session_dir)) {
    rlang::abort("`session_dir` must be provided.")
  }
  load_adaptive_session(session_dir)
}
