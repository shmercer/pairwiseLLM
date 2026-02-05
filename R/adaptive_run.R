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

#' Adaptive ranking live runner (v2 scaffold)
#'
#' @description
#' Stepwise execution is not implemented yet.
#'
#' @param ... Reserved for future extensions; currently unused.
#'
#' @export
adaptive_rank_run_live <- function(...) {
  rlang::abort("Adaptive: stepwise execution not implemented yet (see roadmap Step 5/6).")
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
