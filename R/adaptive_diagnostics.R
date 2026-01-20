# -------------------------------------------------------------------------
# Diagnostics gating for adaptive v3.
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
diagnostics_gate <- function(fit, config, near_stop = FALSE) {
  diagnostics <- NULL
  if (is.list(fit) && !is.null(fit$diagnostics)) {
    diagnostics <- fit$diagnostics
  } else if (is.list(fit) &&
    all(c("divergences", "max_rhat", "min_ess_bulk") %in% names(fit))) {
    diagnostics <- fit
  }
  if (is.null(diagnostics) || !is.list(diagnostics)) {
    return(FALSE)
  }
  validate_config(config)
  if (!is.logical(near_stop) || length(near_stop) != 1L || is.na(near_stop)) {
    rlang::abort("`near_stop` must be TRUE or FALSE.")
  }

  divergences <- diagnostics$divergences
  max_rhat <- diagnostics$max_rhat
  min_ess_bulk <- diagnostics$min_ess_bulk
  if (is.null(divergences) || is.null(max_rhat) || is.null(min_ess_bulk)) {
    return(FALSE)
  }

  divergences_ok <- isTRUE(as.integer(divergences) == 0L)
  rhat_ok <- is.finite(max_rhat) && max_rhat <= config$max_rhat

  threshold <- if (isTRUE(near_stop)) {
    config$min_ess_bulk_near_stop
  } else {
    config$min_ess_bulk
  }
  ess_ok <- is.finite(min_ess_bulk) && min_ess_bulk >= threshold

  isTRUE(divergences_ok && rhat_ok && ess_ok)
}
