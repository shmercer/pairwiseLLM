# Internal helper: resolve legacy / edge-case stop flags into a single stop_reason.
#
# Notes:
# - PR7 introduced explicit, testable stop reasons returned by `.stop_decision()`.
# - This helper remains for early-return / legacy branches that are still expressed
#   as boolean flags (e.g., max_rounds==0, round_size==0, no results returned).
# - We map legacy names to the standardized PR7 taxonomy where possible.

.bt_resolve_stop_reason <- function(
  no_new_results = FALSE,
  max_rounds_is_zero = FALSE,
  reached_max_rounds = FALSE,
  stopped = FALSE,
  round_size_zero = FALSE,
  no_pairs = FALSE,
  no_results = FALSE,
  no_new_ids = FALSE
) {
  if (isTRUE(no_pairs)) {
    return("no_new_pairs")
  }

  if (isTRUE(no_new_ids)) {
    # Used by linking runners: no more IDs to add from the requested batches.
    return("no_new_ids")
  }

  if (isTRUE(round_size_zero)) {
    return("pair_budget_exhausted")
  }

  if (isTRUE(max_rounds_is_zero) || isTRUE(reached_max_rounds)) {
    return("max_rounds_reached")
  }

  if (isTRUE(stopped)) {
    # Legacy "stopped" generally meant an accuracy/precision rule triggered.
    return("precision_reached")
  }


  if (isTRUE(no_new_results) || isTRUE(no_results)) {
    return("no_new_pairs")
  }

  NA_character_
}

# Internal: stable list of all stop_reason values that bt_run_adaptive may emit.
# Useful for tests and schema validation.
known_stop_reasons <- function() {
  c(
    "precision_reached",
    "stability_reached",
    "no_new_pairs",
    "pair_budget_exhausted",
    "max_rounds_reached",
    "no_new_ids",
    "graph_unhealthy",
    "blocked_by_graph_health",
    "precision_blocked_by_graph_health"
  )
}
