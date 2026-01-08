# Internal helper: summarize round state from accumulated results
#' @keywords internal
.bt_round_state <- function(results, ids = NULL, judge_col = NULL, prefix = "") {
  results <- tibble::as_tibble(results)

  ids_for_graph <- NULL
  if (!is.null(ids)) {
    ids_for_graph <- as.character(ids)
    ids_for_graph <- ids_for_graph[!is.na(ids_for_graph) & nzchar(ids_for_graph)]
    ids_for_graph <- unique(ids_for_graph)
    if (length(ids_for_graph) == 0L) {
      ids_for_graph <- NULL
    }
  }

  graph_vals <- list(
    n_components = NA_integer_,
    largest_component_frac = NA_real_,
    degree_min = NA_real_,
    degree_median = NA_real_,
    pct_nodes_with_degree_gt0 = NA_real_
  )

  if (!is.null(ids_for_graph)) {
    gm <- .graph_state_from_pairs(results, ids = ids_for_graph)$metrics
    graph_vals <- list(
      n_components = gm$n_components[[1]],
      largest_component_frac = gm$largest_component_frac[[1]],
      degree_min = gm$degree_min[[1]],
      degree_median = gm$degree_median[[1]],
      pct_nodes_with_degree_gt0 = gm$pct_nodes_with_degree_gt0[[1]]
    )
  }

  if (nrow(results) == 0L) {
    out <- tibble::tibble(
      n_results = 0L,
      n_unique_unordered_pairs = 0L,
      n_unique_unordered_pairs_in_ids = NA_integer_,
      n_ids = if (is.null(ids)) NA_integer_ else as.integer(length(ids)),
      n_ids_seen = 0L,
      n_components = graph_vals$n_components,
      largest_component_frac = graph_vals$largest_component_frac,
      degree_min = graph_vals$degree_min,
      degree_median = graph_vals$degree_median,
      pct_nodes_with_degree_gt0 = graph_vals$pct_nodes_with_degree_gt0,
      min_appearances = 0L,
      p10_appearances = 0L,
      median_appearances = 0L,
      p90_appearances = 0L,
      max_appearances = 0L,
      pos_imbalance_max = 0L,
      n_self_pairs = 0L,
      n_missing_better_id = 0L,
      n_judges = NA_integer_
    )
    if (!is.null(prefix) && nzchar(prefix)) {
      names(out) <- paste0(prefix, names(out))
    }
    return(out)
  }

  if (!all(c("ID1", "ID2") %in% names(results))) {
    stop("`results` must have columns ID1 and ID2.", call. = FALSE)
  }

  id1 <- as.character(results$ID1)
  id2 <- as.character(results$ID2)

  # Unique unordered pairs across all results
  keys_all <- .unordered_pair_key(id1, id2)
  n_unique_all <- length(unique(keys_all))

  n_self <- sum(!is.na(id1) & !is.na(id2) & (id1 == id2))

  n_missing_better <- 0L
  if ("better_id" %in% names(results)) {
    b <- results$better_id
    n_missing_better <- sum(is.na(b) | (as.character(b) == ""))
  }

  # Judge counts
  n_judges <- NA_integer_
  if (!is.null(judge_col) && judge_col %in% names(results)) {
    j <- results[[judge_col]]
    j <- j[!is.na(j) & as.character(j) != ""]
    n_judges <- as.integer(length(unique(as.character(j))))
  }

  # Universe of ids for appearance + imbalance
  if (is.null(ids)) {
    ids_u <- sort(unique(c(id1, id2)))
    ids_u <- ids_u[!is.na(ids_u) & ids_u != ""]
  } else {
    ids_u <- as.character(ids)
    ids_u <- ids_u[!is.na(ids_u) & ids_u != ""]
  }

  n_ids <- as.integer(length(ids_u))

  # Appearances: count occurrences as either ID1 or ID2 (including cross-pairs)
  app <- integer(length(ids_u))
  pos1 <- integer(length(ids_u))
  pos2 <- integer(length(ids_u))

  if (length(ids_u) > 0L) {
    pos1 <- as.integer(table(factor(id1, levels = ids_u)))
    pos2 <- as.integer(table(factor(id2, levels = ids_u)))
    app <- pos1 + pos2
  }

  n_ids_seen <- as.integer(sum(app > 0L))

  # Appearance quantiles: for robustness, compute on full vector (including zeros)
  safe_q <- function(x, p) {
    if (length(x) == 0L) {
      return(0L)
    }
    as.integer(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE, type = 7))
  }

  min_app <- if (length(app) == 0L) 0L else as.integer(min(app))
  max_app <- if (length(app) == 0L) 0L else as.integer(max(app))
  p10_app <- safe_q(app, 0.10)
  med_app <- safe_q(app, 0.50)
  p90_app <- safe_q(app, 0.90)

  imb <- if (length(app) == 0L) integer() else (pos1 - pos2)
  pos_imb_max <- if (length(imb) == 0L) 0L else as.integer(max(abs(imb)))

  # Unordered pairs restricted to ids (both endpoints in ids_u)
  n_unique_in_ids <- NA_integer_
  if (!is.null(ids)) {
    keep <- (!is.na(id1) & !is.na(id2) & id1 %in% ids_u & id2 %in% ids_u)
    if (any(keep)) {
      n_unique_in_ids <- as.integer(length(unique(keys_all[keep])))
    } else {
      n_unique_in_ids <- 0L
    }
  }

  out <- tibble::tibble(
    n_results = as.integer(nrow(results)),
    n_unique_unordered_pairs = as.integer(n_unique_all),
    n_unique_unordered_pairs_in_ids = n_unique_in_ids,
    n_ids = n_ids,
    n_ids_seen = n_ids_seen,
    n_components = graph_vals$n_components,
    largest_component_frac = graph_vals$largest_component_frac,
    degree_min = graph_vals$degree_min,
    degree_median = graph_vals$degree_median,
    pct_nodes_with_degree_gt0 = graph_vals$pct_nodes_with_degree_gt0,
    min_appearances = min_app,
    p10_appearances = p10_app,
    median_appearances = med_app,
    p90_appearances = p90_app,
    max_appearances = max_app,
    pos_imbalance_max = pos_imb_max,
    n_self_pairs = as.integer(n_self),
    n_missing_better_id = as.integer(n_missing_better),
    n_judges = n_judges
  )

  if (!is.null(prefix) && nzchar(prefix)) {
    names(out) <- paste0(prefix, names(out))
  }

  out
}
