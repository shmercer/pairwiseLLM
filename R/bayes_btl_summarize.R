#' @include bayes_btl_mcmc_adaptive.R
NULL

# -------------------------------------------------------------------------
# MCMC-only summaries for adaptive outputs.
# -------------------------------------------------------------------------

.btl_mcmc_validate_draws <- function(theta_draws) {
  if (!is.matrix(theta_draws) || !is.numeric(theta_draws)) {
    rlang::abort("`theta_draws` must be a numeric matrix.")
  }
  if (nrow(theta_draws) < 2L) {
    rlang::abort("`theta_draws` must contain at least two draws.")
  }
  ids <- colnames(theta_draws)
  if (is.null(ids) || any(is.na(ids)) || any(ids == "")) {
    rlang::abort("`theta_draws` must have non-empty column names.")
  }
  ids
}

#' @keywords internal
#' @noRd
summarize_theta <- function(theta_draws) {
  ids <- .btl_mcmc_validate_draws(theta_draws)
  theta_draws <- .pairwiseLLM_sanitize_draws_matrix(theta_draws, name = "theta_draws")
  tibble::tibble(
    ID = ids,
    mean = as.double(colMeans(theta_draws)),
    sd = as.double(apply(theta_draws, 2, stats::sd)),
    median = as.double(apply(theta_draws, 2, stats::median)),
    q05 = as.double(apply(theta_draws, 2, stats::quantile, probs = 0.05, names = FALSE)),
    q95 = as.double(apply(theta_draws, 2, stats::quantile, probs = 0.95, names = FALSE)),
    q025 = as.double(apply(theta_draws, 2, stats::quantile, probs = 0.025, names = FALSE)),
    q975 = as.double(apply(theta_draws, 2, stats::quantile, probs = 0.975, names = FALSE))
  )
}

#' @keywords internal
#' @noRd
summarize_ranks <- function(theta_draws) {
  ids <- .btl_mcmc_validate_draws(theta_draws)
  theta_draws <- .pairwiseLLM_sanitize_draws_matrix(theta_draws, name = "theta_draws")
  rank_mat <- t(apply(theta_draws, 1, function(row) rank(-row, ties.method = "average")))
  colnames(rank_mat) <- ids

  tibble::tibble(
    ID = ids,
    rank_mean = as.double(colMeans(rank_mat)),
    rank_sd = as.double(apply(rank_mat, 2, stats::sd)),
    rank_median = as.double(apply(rank_mat, 2, stats::median)),
    rank_q05 = as.double(apply(rank_mat, 2, stats::quantile, probs = 0.05, names = FALSE)),
    rank_q95 = as.double(apply(rank_mat, 2, stats::quantile, probs = 0.95, names = FALSE)),
    rank_q025 = as.double(apply(rank_mat, 2, stats::quantile, probs = 0.025, names = FALSE)),
    rank_q975 = as.double(apply(rank_mat, 2, stats::quantile, probs = 0.975, names = FALSE))
  )
}

#' @keywords internal
#' @noRd
compute_adjacent_win_probs <- function(theta_draws, ranking_ids) {
  ids <- .btl_mcmc_validate_draws(theta_draws)
  theta_draws <- .pairwiseLLM_sanitize_draws_matrix(theta_draws, name = "theta_draws")
  ranking_ids <- as.character(ranking_ids)
  if (length(ranking_ids) < 2L) {
    rlang::abort("`ranking_ids` must contain at least two ids.")
  }
  if (anyDuplicated(ranking_ids)) {
    rlang::abort("`ranking_ids` must not contain duplicates.")
  }
  if (!setequal(ranking_ids, ids) || length(ranking_ids) != length(ids)) {
    rlang::abort("`ranking_ids` must match `theta_draws` column names.")
  }

  idx <- match(ranking_ids, ids)
  win_prob <- numeric(length(idx) - 1L)
  win_prob_btl <- numeric(length(idx) - 1L)
  for (k in seq_along(win_prob)) {
    lhs <- theta_draws[, idx[k], drop = TRUE]
    rhs <- theta_draws[, idx[k + 1L], drop = TRUE]
    win_prob[k] <- mean(lhs > rhs)
    win_prob_btl[k] <- mean(stats::plogis(lhs - rhs))
  }

  tibble::tibble(
    A_id = ranking_ids[-length(ranking_ids)],
    B_id = ranking_ids[-1L],
    win_prob = as.double(win_prob),
    win_prob_btl = as.double(win_prob_btl)
  )
}

#' @keywords internal
#' @noRd
finalize_adaptive_ranking <- function(state, mcmc_fit) {
  validate_state(state)
  if (!is.list(mcmc_fit)) {
    rlang::abort("`mcmc_fit` must be a list.")
  }
  if (!is.null(mcmc_fit$theta_draws)) {
    theta_draws <- mcmc_fit$theta_draws
  } else if (!is.null(mcmc_fit$draws)) {
    theta_draws <- .btl_mcmc_v3_theta_draws(mcmc_fit$draws, item_id = state$ids)
  } else {
    rlang::abort("`mcmc_fit` must contain `theta_draws` or `draws`.")
  }
  ids <- .btl_mcmc_validate_draws(theta_draws)
  theta_draws <- .pairwiseLLM_sanitize_draws_matrix(theta_draws, name = "theta_draws")

  theta_mean <- colMeans(theta_draws)
  ranking_ids <- names(sort(theta_mean, decreasing = TRUE))

  list(
    theta_summary = summarize_theta(theta_draws),
    rank_summary = summarize_ranks(theta_draws),
    adjacent_win_probs = compute_adjacent_win_probs(theta_draws, ranking_ids),
    diagnostics = list(
      degree = tibble::tibble(
        ID = state$ids,
        deg = as.integer(state$deg),
        imb = as.integer(state$imb)
      ),
      duplicates = tibble::tibble(
        unordered_key = names(state$unordered_count),
        count = as.integer(state$unordered_count)
      ),
      comparisons_scheduled = state$comparisons_scheduled,
      comparisons_observed = state$comparisons_observed
    )
  )
}
