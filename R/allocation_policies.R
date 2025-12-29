#' Built-in allocation policies for adaptive core-linking
#'
#' These helpers return an `allocation_fun` compatible with
#' [bt_run_core_linking()] and [bt_run_adaptive_core_linking()].
#'
#' An `allocation_fun` is called once per round with a `state` list and may
#' return `NULL` (no change) or a list with updated `within_batch_frac` and/or
#' `core_audit_frac`. Values are validated and clamped by the runners.
#'
#' @name allocation_policies
NULL

#' Compose multiple allocation policies
#'
#' Combines multiple `allocation_fun` functions into a single function.
#'
#' The returned function calls each policy in order with the same `state`
#' object and merges their outputs. If multiple policies set the same
#' field (e.g., `within_batch_frac`), later policies take precedence.
#'
#' This is useful when you want to combine, for example,
#' [allocation_precision_ramp()] and [allocation_audit_on_drift()].
#'
#' @param ... One or more allocation functions (each a function or `NULL`).
#'
#' @return A function suitable for passing as `allocation_fun`.
#'
#' @examples
#' alloc <- allocation_compose(
#'   allocation_precision_ramp(step = 0.2, max_within = 0.8),
#'   allocation_audit_on_drift(drift_threshold = 0.2, step = 0.05)
#' )
#' state <- list(
#'   within_batch_frac = 0.1,
#'   core_audit_frac = 0.1,
#'   metrics = data.frame(rel_se_p90 = 0.5, linking_max_abs_shift = 0.3),
#'   prev_metrics = data.frame(rel_se_p90 = 0.8)
#' )
#' alloc(state)
#'
#' @export
allocation_compose <- function(...) {
  fns <- list(...)
  fns <- fns[!vapply(fns, is.null, logical(1))]
  if (length(fns) == 0L) {
    stop("At least one non-NULL allocation function must be provided.", call. = FALSE)
  }
  ok <- vapply(fns, is.function, logical(1))
  if (!all(ok)) {
    stop("All arguments to `allocation_compose()` must be functions (or NULL).", call. = FALSE)
  }

  function(state) {
    out <- NULL
    for (fn in fns) {
      upd <- fn(state)
      if (is.null(upd)) next
      if (!is.list(upd)) {
        stop("`allocation_fun` must return NULL or a list.", call. = FALSE)
      }
      if (is.null(out)) out <- upd else out <- utils::modifyList(out, upd)
      # Update state so downstream policies see the latest values.
      state <- utils::modifyList(state, out)
    }
    out
  }
}

#' Allocation policy: increase within-batch comparisons as precision improves
#'
#' Returns an `allocation_fun` that increases `within_batch_frac` by `step`
#' when the selected precision metric improves (decreases) compared to the
#' previous round.
#'
#' @param step Amount to increase `within_batch_frac` when improvement is
#'   detected.
#' @param max_within Maximum allowed `within_batch_frac`.
#' @param metric Name of a column in `state$metrics` and `state$prev_metrics`
#'   to use as the precision metric. Defaults to `"rel_se_p90"`.
#' @param min_improve Minimum required decrease in the metric to count as an
#'   improvement.
#'
#' @return A function suitable for passing as `allocation_fun`.
#'
#' @examples
#' alloc <- allocation_precision_ramp(step = 0.2, max_within = 0.8)
#'
#' # Improvement (metric decreases): within_batch_frac increases by step
#' state <- list(
#'   within_batch_frac = 0.1,
#'   core_audit_frac = 0.1,
#'   metrics = data.frame(rel_se_p90 = 0.50),
#'   prev_metrics = data.frame(rel_se_p90 = 0.80)
#' )
#' alloc(state)
#'
#' # No improvement: returns NULL (no change)
#' state$prev_metrics <- data.frame(rel_se_p90 = 0.45)
#' alloc(state)
#'
#' @export
allocation_precision_ramp <- function(step = 0.05,
                                      max_within = 0.80,
                                      metric = "rel_se_p90",
                                      min_improve = 0) {
  stopifnot(is.numeric(step), length(step) == 1L)
  stopifnot(is.numeric(max_within), length(max_within) == 1L)
  stopifnot(is.character(metric), length(metric) == 1L)
  stopifnot(is.numeric(min_improve), length(min_improve) == 1L)

  function(state) {
    m <- state$metrics
    pm <- state$prev_metrics
    if (is.null(m) || is.null(pm)) {
      return(NULL)
    }
    if (!(metric %in% names(m)) || !(metric %in% names(pm))) {
      return(NULL)
    }

    cur <- m[[metric]][[1]]
    prev <- pm[[metric]][[1]]
    if (!is.finite(cur) || !is.finite(prev)) {
      return(NULL)
    }

    # Improvement = decrease in metric
    if ((prev - cur) < min_improve) {
      return(NULL)
    }

    within <- state$within_batch_frac
    if (!is.finite(within)) {
      return(NULL)
    }

    list(within_batch_frac = min(max_within, within + step))
  }
}

#' Allocation policy: increase core auditing when drift is high
#'
#' Returns an `allocation_fun` that increases `core_audit_frac` by `step` when
#' the selected drift metric exceeds `drift_threshold`, and otherwise moves
#' `core_audit_frac` back toward `base_core_audit` by `step`.
#'
#' @param drift_metric Name of the drift metric column in `state$metrics`.
#'   Defaults to `"linking_max_abs_shift"`.
#' @param drift_threshold Threshold above which drift is considered high.
#' @param step Amount to adjust `core_audit_frac`.
#' @param base_core_audit Target (baseline) `core_audit_frac` when drift is low.
#' @param max_core_audit Maximum allowed `core_audit_frac`.
#'
#' @return A function suitable for passing as `allocation_fun`.
#'
#' @examples
#' alloc <- allocation_audit_on_drift(drift_threshold = 0.2, step = 0.05)
#'
#' # High drift: core_audit_frac increases by step
#' state <- list(
#'   within_batch_frac = 0.2,
#'   core_audit_frac = 0.10,
#'   metrics = data.frame(linking_max_abs_shift = 0.30)
#' )
#' alloc(state)
#'
#' # Low drift: core_audit_frac moves back toward baseline
#' state$core_audit_frac <- 0.25
#' state$metrics <- data.frame(linking_max_abs_shift = 0.05)
#' alloc(state)
#'
#' @export
allocation_audit_on_drift <- function(drift_metric = "linking_max_abs_shift",
                                      drift_threshold = 0.20,
                                      step = 0.05,
                                      base_core_audit = 0.10,
                                      max_core_audit = 0.40) {
  stopifnot(is.character(drift_metric), length(drift_metric) == 1L)
  stopifnot(is.numeric(drift_threshold), length(drift_threshold) == 1L)
  stopifnot(is.numeric(step), length(step) == 1L)
  stopifnot(is.numeric(base_core_audit), length(base_core_audit) == 1L)
  stopifnot(is.numeric(max_core_audit), length(max_core_audit) == 1L)

  function(state) {
    m <- state$metrics
    if (is.null(m) || !(drift_metric %in% names(m))) {
      return(NULL)
    }

    drift <- m[[drift_metric]][[1]]
    if (!is.finite(drift)) {
      return(NULL)
    }

    audit <- state$core_audit_frac
    if (!is.finite(audit)) {
      return(NULL)
    }

    if (drift > drift_threshold) {
      audit_new <- min(max_core_audit, audit + step)
    } else {
      # Move back toward baseline
      if (audit > base_core_audit) {
        audit_new <- max(base_core_audit, audit - step)
      } else if (audit < base_core_audit) {
        audit_new <- min(base_core_audit, audit + step)
      } else {
        return(NULL)
      }
    }

    list(core_audit_frac = audit_new)
  }
}
