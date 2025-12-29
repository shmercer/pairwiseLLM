# ---- stop reasons (internal) ----
# Centralize stop_reason vocabulary + precedence so all runners report consistent values.

#' Internal: resolve a stop reason according to a shared precedence order
#'
#' Precedence (first TRUE wins):
#'   1) stopped
#'   2) max_rounds (either max_rounds_is_zero or reached_max_rounds)
#'   3) round_size_zero
#'   4) no_pairs
#'   5) no_new_ids
#'   6) no_new_results
#'   7) no_results
#'
#' @keywords internal
.bt_resolve_stop_reason <- function(stopped = FALSE,
                                    reached_max_rounds = FALSE,
                                    max_rounds_is_zero = FALSE,
                                    round_size_zero = FALSE,
                                    no_pairs = FALSE,
                                    no_new_ids = FALSE,
                                    no_new_results = FALSE,
                                    no_results = FALSE) {
  if (isTRUE(stopped)) {
    return("stopped")
  }
  if (isTRUE(max_rounds_is_zero) || isTRUE(reached_max_rounds)) {
    return("max_rounds")
  }
  if (isTRUE(round_size_zero)) {
    return("round_size_zero")
  }
  if (isTRUE(no_pairs)) {
    return("no_pairs")
  }
  if (isTRUE(no_new_ids)) {
    return("no_new_ids")
  }
  if (isTRUE(no_new_results)) {
    return("no_new_results")
  }
  if (isTRUE(no_results)) {
    return("no_results")
  }
  NA_character_
}
