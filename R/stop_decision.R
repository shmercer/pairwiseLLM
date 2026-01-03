# -------------------------------------------------------------------------
# Stop decision helper (single source of truth)
# -------------------------------------------------------------------------

#' Internal: decide whether to stop and why
#'
#' Applies a deterministic priority order to select a single stop reason.
#' Designed to be called once per round after metrics are computed.
#'
#' Priority (first match wins):
#'  1) round < min_rounds -> continue
#'  2) no_new_pairs -> stop ("no_new_pairs")
#'  3) budget_exhausted -> stop ("pair_budget_exhausted")
#'  4) max_rounds_reached -> stop ("max_rounds_reached")
#'  5) graph unhealthy -> continue (details record; optionally stop if unrecoverable)
#'  6) stability reached AND graph healthy -> stop ("stability_reached")
#'  7) precision reached -> stop ("precision_reached")
#'
#' @param round Integer round index (1-based).
#' @param min_rounds Integer minimum rounds before any stopping other than exhaustion reasons.
#' @param no_new_pairs Logical; TRUE when no additional pairs can be proposed.
#' @param budget_exhausted Logical; TRUE when a user-specified pair budget is exhausted.
#' @param max_rounds_reached Logical; TRUE when `round >= max_rounds`.
#' @param graph_healthy Logical; TRUE when graph thresholds pass.
#' @param stability_reached Logical; TRUE when stability thresholds pass.
#' @param precision_reached Logical; TRUE when precision tier / thresholds pass.
#' @param stop_reason_priority Optional character vector of reason priority. When supplied,
#'   it overrides the default priority among the stop-causes that are eligible after
#'   the min_rounds gate.
#'
#' @return A list with `stop` (scalar logical), `reason` (scalar character or NA),
#'   and `details` (named list).
#'
#' @keywords internal
.stop_decision <- function(round,
                           min_rounds = 2L,
                           no_new_pairs = FALSE,
                           budget_exhausted = FALSE,
                           max_rounds_reached = FALSE,
                           graph_healthy = TRUE,
                           stability_reached = FALSE,
                           precision_reached = FALSE,
                           stop_reason_priority = NULL) {
  round <- as.integer(round)
  min_rounds <- as.integer(min_rounds)

  if (is.na(round) || round < 1L) {
    stop("`round` must be a positive integer.", call. = FALSE)
  }
  if (is.na(min_rounds) || min_rounds < 1L) {
    stop("`min_rounds` must be a positive integer.", call. = FALSE)
  }

  details <- list(
    round = round,
    min_rounds = min_rounds,
    no_new_pairs = isTRUE(no_new_pairs),
    budget_exhausted = isTRUE(budget_exhausted),
    max_rounds_reached = isTRUE(max_rounds_reached),
    graph_healthy = isTRUE(graph_healthy),
    stability_reached = isTRUE(stability_reached),
    precision_reached = isTRUE(precision_reached)
  )

  if (round < min_rounds) {
    return(list(stop = FALSE, reason = NA_character_, details = details))
  }

  # Hard-stop reasons (always take precedence)
  if (isTRUE(no_new_pairs)) {
    return(list(stop = TRUE, reason = "no_new_pairs", details = details))
  }
  if (isTRUE(budget_exhausted)) {
    return(list(stop = TRUE, reason = "pair_budget_exhausted", details = details))
  }
  if (isTRUE(max_rounds_reached)) {
    return(list(stop = TRUE, reason = "max_rounds_reached", details = details))
  }

  # If graph is unhealthy, we do not allow stability stops.
  if (!isTRUE(graph_healthy)) {
    return(list(stop = FALSE, reason = NA_character_, details = details))
  }

  # Eligible stop causes after health gating
  candidates <- c(
    stability_reached = isTRUE(stability_reached),
    precision_reached = isTRUE(precision_reached)
  )
  active <- names(candidates)[candidates]

  if (length(active) == 0L) {
    return(list(stop = FALSE, reason = NA_character_, details = details))
  }

  if (!is.null(stop_reason_priority)) {
    stop_reason_priority <- as.character(stop_reason_priority)
    active <- active[order(match(active, stop_reason_priority, nomatch = Inf))]
  }

  reason <- active[[1]]
  list(stop = TRUE, reason = reason, details = details)
}
