# -------------------------------------------------------------------------
# Stopping checks for adaptive refinement (diagnostics + reliability +
# lagged theta/rank stability with refit-only stop passes).
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
near_stop_from_state <- function(state) {
  rlang::abort(
    "Legacy scaffold stopping helpers are disabled in the canonical adaptive path."
  )
}

#' @keywords internal
#' @noRd
btl_mcmc_compute_stop_metrics <- function(state, fit, candidates_with_utility, config) {
  rlang::abort(
    "Legacy scaffold stopping helpers are disabled in the canonical adaptive path."
  )
}

#' @keywords internal
#' @noRd
.adaptive_update_theta_history <- function(state, theta_summary = NULL, fit = NULL) {
  rlang::abort(
    "Legacy scaffold stopping helpers are disabled in the canonical adaptive path."
  )
}

#' @keywords internal
#' @noRd
btl_mcmc_should_stop <- function(metrics, state, config, theta_summary = NULL, fit = NULL) {
  rlang::abort(
    "Legacy scaffold stopping helpers are disabled in the canonical adaptive path."
  )
}
