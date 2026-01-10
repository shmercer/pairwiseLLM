# Internal component-bridging helpers for adaptive pairing.

#' Construct deterministic component-bridge candidates
#'
#' When the observed comparison graph is disconnected, exploration should
#' explicitly propose cross-component edges rather than relying on chance.
#' This helper generates a schema-stable candidate tibble of bridge pairs
#' between the largest connected component (LCC) and other components.
#'
#' @param graph_state A graph state list from \.graph_state_from_pairs() or
#'   graph_metrics(), containing named vectors `degree` and `component_id`.
#' @param id_vec_theta Character vector of IDs in theta order.
#' @param n_bridge Integer number of bridge pairs desired (upper bound).
#' @param max_lcc Integer; max LCC endpoints to consider per component.
#' @param max_other Integer; max non-LCC endpoints to consider per component.
#'
#' @return A tibble with at least:
#'   - i_idx, j_idx, anchor_idx, pair_key
#'   - source, embed_hit, embed_rank, directed
#'   - score_info, score_need, score_embed, score_total
#'   plus bridge metadata columns.
#'
#' @keywords internal
.ap_make_component_bridge_pairs <- function(graph_state,
                                            id_vec_theta,
                                            n_bridge,
                                            max_lcc = 5L,
                                            max_other = 5L) {
  id_vec_theta <- as.character(id_vec_theta)
  n_bridge <- as.integer(n_bridge)
  max_lcc <- as.integer(max_lcc)
  max_other <- as.integer(max_other)

  empty_out <- function() {
    tibble::tibble(
      i_idx = integer(),
      j_idx = integer(),
      anchor_idx = integer(),
      pair_key = character(),
      source = character(),
      embed_hit = logical(),
      embed_rank = integer(),
      directed = logical(),
      score_info = numeric(),
      score_need = numeric(),
      score_embed = numeric(),
      score_total = numeric(),
      ID1 = character(),
      ID2 = character(),
      pair_type = character(),
      reason = character(),
      component_id_1 = integer(),
      component_id_2 = integer(),
      target_component_id = integer(),
      target_component_size = integer()
    )
  }

  if (length(id_vec_theta) < 2L || is.na(n_bridge) || n_bridge <= 0L) {
    return(empty_out())
  }

  if (is.null(graph_state) || is.null(graph_state$component_id) || is.null(graph_state$degree)) {
    return(empty_out())
  }

  comp_all <- graph_state$component_id
  deg_all <- graph_state$degree
  if (is.null(names(comp_all)) || is.null(names(deg_all))) {
    return(empty_out())
  }

  theta_rank <- seq_along(id_vec_theta)
  names(theta_rank) <- id_vec_theta

  comp <- comp_all[id_vec_theta]
  deg <- deg_all[id_vec_theta]
  deg[is.na(deg)] <- 0

  if (all(is.na(comp))) {
    return(empty_out())
  }

  comp_tbl <- tibble::tibble(
    ID = id_vec_theta,
    component_id = as.integer(comp)
  )
  comp_tbl <- dplyr::filter(comp_tbl, !is.na(.data$component_id))

  comp_sizes <- dplyr::count(comp_tbl, .data$component_id, name = "n")
  if (nrow(comp_sizes) <= 1L) {
    return(empty_out())
  }

  # Largest connected component id; tie-break by smallest numeric label.
  comp_sizes <- dplyr::arrange(comp_sizes, dplyr::desc(.data$n), .data$component_id)
  lcc_id <- as.integer(comp_sizes$component_id[[1]])

  lcc_ids <- comp_tbl$ID[comp_tbl$component_id == lcc_id]
  if (length(lcc_ids) == 0L) {
    return(empty_out())
  }

  lcc_pool <- tibble::tibble(
    ID = lcc_ids,
    degree = as.integer(deg[lcc_ids]),
    theta_rank = as.integer(theta_rank[lcc_ids])
  )
  lcc_pool <- dplyr::arrange(lcc_pool, .data$degree, .data$theta_rank, .data$ID)
  lcc_take <- min(nrow(lcc_pool), max(max_lcc, n_bridge))
  lcc_pool <- dplyr::slice_head(lcc_pool, n = lcc_take)

  other_sizes <- dplyr::filter(comp_sizes, .data$component_id != lcc_id)
  # Prefer to bridge smaller components first.
  other_sizes <- dplyr::arrange(other_sizes, .data$n, .data$component_id)

  max_other <- max(1L, max_other)

  out <- list()
  out_i <- 0L

  for (k in seq_len(nrow(other_sizes))) {
    comp_id <- as.integer(other_sizes$component_id[[k]])
    comp_n <- as.integer(other_sizes$n[[k]])

    other_ids <- comp_tbl$ID[comp_tbl$component_id == comp_id]
    if (length(other_ids) == 0L) {
      next
    }

    other_pool <- tibble::tibble(
      ID = other_ids,
      degree = as.integer(deg[other_ids]),
      theta_rank = as.integer(theta_rank[other_ids])
    )
    other_pool <- dplyr::arrange(other_pool, .data$degree, .data$theta_rank, .data$ID)
    other_pool <- dplyr::slice_head(other_pool, n = min(nrow(other_pool), max_other))

    grid <- tidyr::crossing(
      lcc_id = lcc_pool$ID,
      other_id = other_pool$ID
    )

    if (nrow(grid) == 0L) {
      next
    }

    grid <- dplyr::mutate(
      grid,
      lcc_component_id = lcc_id,
      other_component_id = comp_id,
      target_component_id = comp_id,
      target_component_size = comp_n,
      degree_lcc = as.integer(deg[.data$lcc_id]),
      degree_other = as.integer(deg[.data$other_id]),
      theta_rank_lcc = as.integer(theta_rank[.data$lcc_id]),
      theta_rank_other = as.integer(theta_rank[.data$other_id])
    )

    # Stable priority within a component: smaller degree endpoints first, then theta order.
    grid <- dplyr::arrange(
      grid,
      .data$target_component_size,
      .data$degree_other,
      .data$degree_lcc,
      .data$theta_rank_other,
      .data$theta_rank_lcc,
      .data$other_id,
      .data$lcc_id
    )

    # Map IDs to theta indices.
    i_raw <- match(grid$lcc_id, id_vec_theta)
    j_raw <- match(grid$other_id, id_vec_theta)
    ok <- !is.na(i_raw) & !is.na(j_raw) & i_raw != j_raw
    if (!any(ok)) {
      next
    }

    i_raw <- i_raw[ok]
    j_raw <- j_raw[ok]
    grid <- grid[ok, , drop = FALSE]

    lo <- pmin(i_raw, j_raw)
    hi <- pmax(i_raw, j_raw)
    id1 <- id_vec_theta[lo]
    id2 <- id_vec_theta[hi]

    out_i <- out_i + 1L
    out[[out_i]] <- tibble::tibble(
      i_idx = as.integer(lo),
      j_idx = as.integer(hi),
      anchor_idx = as.integer(match(grid$lcc_id, id_vec_theta)),
      pair_key = .unordered_pair_key(id1, id2),
      source = "bridge",
      embed_hit = FALSE,
      embed_rank = NA_integer_,
      directed = FALSE,
      score_info = NA_real_,
      score_need = NA_real_,
      score_embed = 0,
      score_total = Inf,
      ID1 = as.character(id1),
      ID2 = as.character(id2),
      pair_type = "component_bridge",
      reason = "graph_disconnected",
      component_id_1 = as.integer(comp_all[id1]),
      component_id_2 = as.integer(comp_all[id2]),
      target_component_id = as.integer(grid$target_component_id),
      target_component_size = as.integer(grid$target_component_size)
    )
  }

  if (length(out) == 0L) {
    return(empty_out())
  }

  out_tbl <- dplyr::bind_rows(out)

  if (nrow(out_tbl) == 0L) {
    return(empty_out())
  }

  # Deduplicate pairs across components (rare, but possible when theta order interleaves).
  out_tbl <- dplyr::distinct(out_tbl, .data$pair_key, .keep_all = TRUE)

  # Limit total candidates to keep downstream work bounded.
  max_keep <- max(10L, n_bridge * 10L)
  out_tbl <- dplyr::slice_head(out_tbl, n = min(nrow(out_tbl), max_keep))

  out_tbl
}
