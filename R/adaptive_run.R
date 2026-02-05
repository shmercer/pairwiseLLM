# -------------------------------------------------------------------------
# Adaptive entrypoints (stubs for v2 migration).
# -------------------------------------------------------------------------

#' Adaptive ranking (v2 scaffold)
#'
#' @description
#' Creates an Adaptive v2 state object with canonical logs. Pair selection and
#' stepwise execution are implemented in later steps.
#'
#' @param items A vector or data frame of items. Data frames must include an
#'   `item_id` column (or `id`/`ID`). Item IDs may be character; internal logs
#'   use integer indices derived from these IDs.
#' @param ... Internal/testing only. Supply `now_fn` to override the clock used
#'   for timestamps.
#'
#' @return An Adaptive v2 state object containing `step_log`, `round_log`, and
#'   `item_log`.
#' @export
adaptive_rank_start <- function(items, ...) {
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
  now_fn <- dots$now_fn %||% function() Sys.time()
  new_adaptive_state(items, now_fn = now_fn)
}

#' Adaptive ranking live runner (v2)
#'
#' @description
#' Executes adaptive ranking steps using an injected judge function.
#'
#' @param state An Adaptive v2 state object created by [adaptive_rank_start()].
#' @param judge A function called as `judge(A, B, state, ...)` that returns a
#'   list with `is_valid = TRUE` and `Y` in `0/1`, or `is_valid = FALSE` with
#'   `invalid_reason`.
#' @param n_steps Number of steps to execute.
#' @param fit_fn Optional BTL fit function for deterministic testing; defaults
#'   to `default_btl_fit_fn()` when a refit is due.
#' @param btl_config Optional list overriding BTL refit/stop defaults.
#' @param ... Additional arguments passed through to `judge()`.
#'
#' @export
adaptive_rank_run_live <- function(state, judge, n_steps = 1L, fit_fn = NULL, btl_config = NULL, ...) {
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

  remaining <- n_steps
  while (remaining > 0L) {
    state <- run_one_step(state, judge, ...)
    refit_out <- maybe_refit_btl(state, config = btl_config, fit_fn = fit_fn)
    state <- refit_out$state
    if (isTRUE(refit_out$refit_performed)) {
      metrics <- compute_stop_metrics(state, config = refit_out$config)
      state$stop_metrics <- metrics
      stop_decision <- should_stop(metrics, config = refit_out$config)
      stop_reason <- if (isTRUE(stop_decision)) "btl_converged" else NA_character_

      round_row <- .adaptive_round_log_row(
        state = state,
        metrics = metrics,
        stop_decision = stop_decision,
        stop_reason = stop_reason,
        refit_context = refit_out$refit_context
      )
      state$round_log <- append_round_log(state$round_log, round_row)
      if (isTRUE(stop_decision)) {
        return(state)
      }
    }
    remaining <- remaining - 1L
  }

  state
}

#' Adaptive ranking resume (v2 scaffold)
#'
#' @description
#' Resume support is not implemented yet.
#'
#' @param ... Reserved for future extensions; currently unused.
#'
#' @export
adaptive_rank_resume <- function(...) {
  rlang::abort("Adaptive: stepwise execution not implemented yet (see roadmap Step 5/6).")
}
