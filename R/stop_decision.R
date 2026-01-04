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
#'  4) precision_reached -> stop ("precision_reached")
#'  5) stability_reached AND graph_healthy -> stop ("stability_reached")
#'  6) max_rounds_reached -> stop ("max_rounds_reached")
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
  # Coerce / normalize to scalars (defensive; internal helper)
  round <- as.integer(round)
  min_rounds <- as.integer(min_rounds)

  graph_healthy <- isTRUE(graph_healthy)
  stability_reached <- isTRUE(stability_reached)
  precision_reached <- isTRUE(precision_reached)

  candidates <- c(
    no_new_pairs = isTRUE(no_new_pairs),
    pair_budget_exhausted = isTRUE(budget_exhausted),
    precision_reached = precision_reached,
    stability_reached = stability_reached && graph_healthy,
    max_rounds_reached = isTRUE(max_rounds_reached)
  )

  # Gate stability/precision by min_rounds (but allow hard stops).
  if (isTRUE(round < min_rounds)) {
    candidates["stability_reached"] <- FALSE
    candidates["precision_reached"] <- FALSE
  }

  # Priority order: user-supplied (validated) or default.
  default_priority <- names(candidates)
  if (is.null(stop_reason_priority)) {
    priority <- default_priority
  } else {
    if (!is.character(stop_reason_priority)) {
      stop("`stop_reason_priority` must be a character vector of stop reasons.", call. = FALSE)
    }
    unknown <- setdiff(stop_reason_priority, default_priority)
    if (length(unknown) > 0L) {
      stop("`stop_reason_priority` contains unknown stop reasons: ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    # Keep provided order; append any reasons not mentioned to ensure completeness.
    priority <- c(stop_reason_priority, setdiff(default_priority, stop_reason_priority))
  }

  stop <- FALSE
  reason <- NA_character_
  for (nm in priority) {
    if (isTRUE(candidates[[nm]])) {
      stop <- TRUE
      reason <- nm
      break
    }
  }

  details <- list(
    round = round,
    min_rounds = min_rounds,
    priority = priority,
    candidates = candidates,
    graph_healthy = graph_healthy,
    stability_reached = stability_reached,
    precision_reached = precision_reached
  )

  list(stop = stop, reason = reason, details = details)
}
