# Reservoir selection uses only edge membership, presentation and committed use.

.adaptive_reservoir_unused <- function(state) {
  edges <- state$replay_reservoir$edges
  keys <- .adaptive_reservoir_key(edges$A_id, edges$B_id)
  history <- state$history_pairs
  used <- .adaptive_reservoir_key(history$A_id, history$B_id)
  edges[!keys %in% used, , drop = FALSE]
}

.adaptive_reservoir_filter <- function(state, candidates) {
  if (!.adaptive_reservoir_active(state)) return(candidates)
  unused <- .adaptive_reservoir_unused(state)
  keys <- .adaptive_reservoir_key(candidates$i, candidates$j)
  candidates[keys %in% .adaptive_reservoir_key(unused$A_id, unused$B_id), , drop = FALSE]
}

.adaptive_reservoir_starvation <- function(state) {
  if (nrow(.adaptive_reservoir_unused(state)) == 0L) "reservoir_exhausted" else
    "reservoir_constraints_exhausted"
}

.adaptive_assign_order_for_state <- function(state, pair, posA, posB, pair_last_order, seed_base = 1L) {
  if (!.adaptive_reservoir_active(state)) {
    return(.adaptive_assign_order(pair, posA, posB, pair_last_order, seed_base))
  }
  edges <- state$replay_reservoir$edges
  row <- match(.adaptive_reservoir_key(pair$i[[1L]], pair$j[[1L]]),
    .adaptive_reservoir_key(edges$A_id, edges$B_id))
  if (is.na(row)) rlang::abort("Selected unordered edge is outside the replay reservoir.")
  c(A_id = edges$A_id[[row]], B_id = edges$B_id[[row]])
}

.adaptive_reservoir_stage_pairs <- function(state, ids, anchor_ids, rank_index,
                                             stratum_map, stage_name, bounds, C_max, seed) {
  inputs <- .adaptive_within_set_stage_sorted_inputs(ids, anchor_ids, rank_index,
    stratum_map, stage_name)
  edges <- .adaptive_reservoir_unused(state)
  keep <- edges$A_id %in% inputs$sorted_ids & edges$B_id %in% inputs$sorted_ids
  edges <- edges[keep, , drop = FALSE]
  distance <- abs(as.integer(stratum_map[edges$A_id]) - as.integer(stratum_map[edges$B_id]))
  keep <- if (identical(stage_name, "anchor_link")) {
    xor(edges$A_id %in% anchor_ids, edges$B_id %in% anchor_ids)
  } else {
    distance >= bounds$min & distance <= bounds$max
  }
  candidates <- tibble::tibble(i = pmin(edges$A_id[keep], edges$B_id[keep]),
    j = pmax(edges$A_id[keep], edges$B_id[keep]), dist_stratum_global = distance[keep])
  candidates <- candidates[order(candidates$i, candidates$j, method = "radix"), , drop = FALSE]
  total <- nrow(candidates)
  bounded <- total > C_max
  if (bounded) {
    rows <- withr::with_seed(seed, sort(sample.int(total, C_max)))
    candidates <- candidates[rows, , drop = FALSE]
  }
  list(candidates = candidates, total_legal = total, bounded_used = bounded)
}
