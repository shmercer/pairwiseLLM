# -------------------------------------------------------------------------
# Graph state helpers (shared across diagnostics / guards)
# -------------------------------------------------------------------------

#' Summarize comparison-graph state from a pairs table
#'
#' This helper summarizes the undirected comparison graph induced by a set of
#' pairs. It is intentionally lightweight and avoids dense adjacency.
#'
#' @param pairs A data frame with columns giving pair endpoints.
#' @param ids Character vector of all node IDs to include.
#' @param id1_col,id2_col Column names in `pairs` containing endpoints.
#'
#' @return A list with elements:
#' \describe{
#'   \item{metrics}{One-row tibble with counts and summary degree/component metrics.}
#'   \item{degree}{Named numeric vector of degrees (one per `ids`).}
#'   \item{component_id}{Named integer vector of connected-component labels (one per `ids`).}
#' }
#'
#' @keywords internal
.graph_state_from_pairs <- function(pairs,
                                    ids,
                                    id1_col = "ID1",
                                    id2_col = "ID2") {
  ids <- as.character(ids)
  ids <- ids[!is.na(ids) & nzchar(ids)]
  ids <- unique(ids)

  if (length(ids) == 0L) {
    stop("`ids` must contain at least one non-missing ID.", call. = FALSE)
  }

  n <- length(ids)

  if (is.null(pairs) || (is.data.frame(pairs) && nrow(pairs) == 0L)) {
    degree <- rep(0, n)
    names(degree) <- ids
    component_id <- seq_len(n)
    names(component_id) <- ids

    metrics <- tibble::tibble(
      n_nodes = as.integer(n),
      n_edges = as.integer(0),
      n_components = as.integer(n),
      largest_component_frac = if (n > 0) 1 / n else NA_real_,
      degree_min = 0,
      degree_median = 0,
      degree_max = 0
    )

    return(list(metrics = metrics, degree = degree, component_id = component_id))
  }

  pairs <- tibble::as_tibble(pairs)

  if (!all(c(id1_col, id2_col) %in% names(pairs))) {
    stop("`pairs` must contain columns: `", id1_col, "` and `", id2_col, "`.", call. = FALSE)
  }

  id1 <- as.character(pairs[[id1_col]])
  id2 <- as.character(pairs[[id2_col]])

  ok <- !is.na(id1) & nzchar(id1) & !is.na(id2) & nzchar(id2) & id1 != id2
  if (any(ok)) {
    id1 <- id1[ok]
    id2 <- id2[ok]
  } else {
    id1 <- character(0)
    id2 <- character(0)
  }

  keep <- (id1 %in% ids) & (id2 %in% ids)
  id1 <- id1[keep]
  id2 <- id2[keep]

  if (length(id1) == 0L) {
    degree <- rep(0, n)
    names(degree) <- ids
    component_id <- seq_len(n)
    names(component_id) <- ids

    metrics <- tibble::tibble(
      n_nodes = as.integer(n),
      n_edges = as.integer(0),
      n_components = as.integer(n),
      largest_component_frac = if (n > 0) 1 / n else NA_real_,
      degree_min = 0,
      degree_median = 0,
      degree_max = 0
    )

    return(list(metrics = metrics, degree = degree, component_id = component_id))
  }

  # Deduplicate to unique unordered edges
  key <- .unordered_pair_key(id1, id2)
  keep_edge <- !duplicated(key)
  id1 <- id1[keep_edge]
  id2 <- id2[keep_edge]

  # Map to indices and construct unordered endpoints
  i <- match(id1, ids)
  j <- match(id2, ids)

  a <- pmin(i, j)
  b <- pmax(i, j)

  n_edges <- length(a)

  # Degree from undirected edges
  degree <- tabulate(a, nbins = n) + tabulate(b, nbins = n)

  # Connected components via union-find
  parent <- seq_len(n)

  .find <- function(x) {
    while (parent[[x]] != x) {
      parent[[x]] <<- parent[[parent[[x]]]]
      x <- parent[[x]]
    }
    x
  }

  .union <- function(x, y) {
    rx <- .find(x)
    ry <- .find(y)
    if (rx != ry) parent[[ry]] <<- rx
    invisible(NULL)
  }

  for (k in seq_len(n_edges)) {
    .union(a[[k]], b[[k]])
  }

  roots <- vapply(seq_len(n), .find, integer(1))
  root_levels <- sort(unique(roots))
  component_id <- match(roots, root_levels)

  tab_comp <- table(component_id)
  n_components <- length(tab_comp)
  largest_component_frac <- if (n > 0) max(tab_comp) / n else NA_real_

  metrics <- tibble::tibble(
    n_nodes = as.integer(n),
    n_edges = as.integer(n_edges),
    n_components = as.integer(n_components),
    largest_component_frac = as.double(largest_component_frac),
    degree_min = as.double(min(degree)),
    degree_median = as.double(stats::median(degree)),
    degree_max = as.double(max(degree))
  )

  degree <- as.double(degree)
  component_id <- as.integer(component_id)
  names(degree) <- ids
  names(component_id) <- ids

  list(metrics = metrics, degree = degree, component_id = component_id)
}
