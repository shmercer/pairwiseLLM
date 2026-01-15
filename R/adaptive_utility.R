# -------------------------------------------------------------------------
# Adaptive utility computation helpers
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
logistic <- function(x) {
  stats::plogis(x)
}

#' @keywords internal
#' @noRd
compute_pair_utility <- function(theta_draws, candidates) {
  if (!is.matrix(theta_draws) || !is.numeric(theta_draws)) {
    rlang::abort("`theta_draws` must be a numeric matrix.")
  }
  if (nrow(theta_draws) < 2L) {
    rlang::abort("`theta_draws` must have at least two draws.")
  }
  ids <- colnames(theta_draws)
  if (is.null(ids) || any(is.na(ids)) || any(ids == "")) {
    rlang::abort("`theta_draws` must have non-empty column names.")
  }

  if (!is.data.frame(candidates)) {
    rlang::abort("`candidates` must be a data frame or tibble.")
  }
  candidates <- tibble::as_tibble(candidates)
  required <- c("i_id", "j_id")
  .adaptive_required_cols(candidates, "candidates", required)

  i_id <- as.character(candidates$i_id)
  j_id <- as.character(candidates$j_id)
  if (anyNA(i_id) || anyNA(j_id)) {
    rlang::abort("`candidates` ids must be non-missing.")
  }

  missing_ids <- setdiff(unique(c(i_id, j_id)), ids)
  if (length(missing_ids) > 0L) {
    rlang::abort("`candidates` ids must be present in `theta_draws`.")
  }

  i_idx <- match(i_id, ids)
  j_idx <- match(j_id, ids)

  diffs <- theta_draws[, i_idx, drop = FALSE] - theta_draws[, j_idx, drop = FALSE]
  probs <- logistic(diffs)
  utility_raw <- apply(probs, 2, stats::var)

  unordered_key <- if ("unordered_key" %in% names(candidates)) {
    as.character(candidates$unordered_key)
  } else {
    make_unordered_key(i_id, j_id)
  }

  tibble::tibble(
    unordered_key = unordered_key,
    i_id = i_id,
    j_id = j_id,
    utility_raw = as.double(utility_raw)
  )
}

#' @keywords internal
#' @noRd
apply_degree_penalty <- function(utility_tbl, state) {
  validate_state(state)
  if (!is.data.frame(utility_tbl)) {
    rlang::abort("`utility_tbl` must be a data frame or tibble.")
  }
  utility_tbl <- tibble::as_tibble(utility_tbl)
  required <- c("i_id", "j_id", "utility_raw")
  .adaptive_required_cols(utility_tbl, "utility_tbl", required)
  if (!is.numeric(utility_tbl$utility_raw)) {
    rlang::abort("`utility_tbl$utility_raw` must be numeric.")
  }

  i_id <- as.character(utility_tbl$i_id)
  j_id <- as.character(utility_tbl$j_id)
  missing_ids <- setdiff(unique(c(i_id, j_id)), state$ids)
  if (length(missing_ids) > 0L) {
    rlang::abort("`utility_tbl` ids must exist in `state$ids`.")
  }

  deg_i <- state$deg[i_id]
  deg_j <- state$deg[j_id]
  denom <- sqrt((deg_i + 1) * (deg_j + 1))
  utility <- utility_tbl$utility_raw / denom
  utility_tbl$utility <- as.double(utility)
  utility_tbl
}
