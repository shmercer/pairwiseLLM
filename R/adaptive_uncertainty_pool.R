# -------------------------------------------------------------------------
# Adaptive uncertainty pool anchors
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
select_uncertainty_anchors <- function(state, config, theta_summary = NULL) {
  validate_state(state)
  if (!is.list(config)) {
    rlang::abort("`config` must be a list.")
  }
  if (is.null(theta_summary)) {
    rlang::abort("`theta_summary` must be provided for uncertainty anchor selection.")
  }

  theta_summary <- .adaptive_v3_theta_summary(theta_summary, state)
  N <- as.integer(state$N)
  if (is.na(N) || N < 1L) {
    rlang::abort("`state$N` must be a positive integer.")
  }

  M <- .adaptive_v3_clamp(50L, 400L, .adaptive_v3_round_int(6 * sqrt(N)))
  M <- min(as.integer(M), nrow(theta_summary))
  if (M < 1L) {
    return(character())
  }

  ordered <- theta_summary[order(-theta_summary$theta_sd, theta_summary$item_id), , drop = FALSE]
  as.character(utils::head(ordered$item_id, M))
}
