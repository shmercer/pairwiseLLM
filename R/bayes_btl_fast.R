# -------------------------------------------------------------------------
# Fast selection-grade BTL inference.
# Fast inference is used only for adaptive selection. Final inference and
# reporting use MCMC (PR-A6).
#
# Notes
# - This is an approximation intended for adaptive selection only.
# - We enforce identifiability (sum-to-zero) and a fixed SD scale to keep
#   Var(p_ij) comparable across iterations.
# - Standard errors are derived from observed comparison counts when
#   available; otherwise a deterministic fallback is used.
# -------------------------------------------------------------------------

.btl_fast_validate_ids <- function(ids) {
  ids <- as.character(ids)
  if (length(ids) < 1L) {
    rlang::abort("`ids` must contain at least one id.")
  }
  if (anyNA(ids) || any(ids == "")) {
    rlang::abort("`ids` must be non-missing, non-empty character values.")
  }
  if (anyDuplicated(ids)) {
    rlang::abort("`ids` must be unique.")
  }
  ids
}

.btl_fast_prepare_edges <- function(results) {
  winner <- as.character(results$better_id)
  loser <- ifelse(results$better_id == results$A_id, results$B_id, results$A_id)
  win_tbl <- tibble::tibble(winner = winner, loser = loser)
  win_tbl <- dplyr::count(win_tbl, .data$winner, .data$loser, name = "wins")

  if (nrow(win_tbl) == 0L) {
    return(tibble::tibble(
      i_id = character(),
      j_id = character(),
      win_i = integer(),
      win_j = integer(),
      n_ij = integer()
    ))
  }

  win_tbl <- dplyr::mutate(
    win_tbl,
    i_id = pmin(.data$winner, .data$loser),
    j_id = pmax(.data$winner, .data$loser),
    win_i = ifelse(.data$winner == .data$i_id, .data$wins, 0L),
    win_j = ifelse(.data$winner == .data$j_id, .data$wins, 0L)
  )

  dplyr::summarise(
    dplyr::group_by(win_tbl, .data$i_id, .data$j_id),
    win_i = sum(.data$win_i),
    win_j = sum(.data$win_j),
    n_ij = .data$win_i + .data$win_j,
    .groups = "drop"
  )
}

.btl_fast_fit <- function(edges, ids, max_iter = 200L, tol = 1e-6, win_prior = 0.5) {
  sum_by_index <- function(values, idx, n) {
    out <- rep.int(0, n)
    if (length(values) == 0L) {
      return(out)
    }
    sums <- tapply(values, idx, sum)
    out[as.integer(names(sums))] <- as.double(sums)
    out
  }

  N <- length(ids)
  theta_raw <- stats::setNames(rep.int(0, N), ids)
  se_raw <- stats::setNames(rep.int(1, N), ids)
  converged <- TRUE

  if (nrow(edges) == 0L) {
    return(list(theta_raw = theta_raw, se_raw = se_raw, converged = TRUE))
  }

  i_idx <- match(edges$i_id, ids)
  j_idx <- match(edges$j_id, ids)
  total_wins <- sum_by_index(edges$win_i, i_idx, N) +
    sum_by_index(edges$win_j, j_idx, N)
  total_comp <- sum_by_index(edges$n_ij, i_idx, N) +
    sum_by_index(edges$n_ij, j_idx, N)

  w <- rep.int(1, N)
  converged <- FALSE

  for (iter in seq_len(max_iter)) {
    denom <- edges$n_ij / (w[i_idx] + w[j_idx])
    denom_sum <- sum_by_index(denom, i_idx, N) +
      sum_by_index(denom, j_idx, N)
    denom_sum[!is.finite(denom_sum)] <- 0
    wins_adj <- total_wins + win_prior
    w_new <- w
    update_mask <- denom_sum > 0 & is.finite(wins_adj)
    w_new[update_mask] <- w[update_mask] * (wins_adj[update_mask] / denom_sum[update_mask])
    bad_w <- !is.finite(w_new)
    if (any(bad_w)) {
      w_new[bad_w] <- w[bad_w]
    }

    delta <- max(abs(w_new - w), na.rm = TRUE)
    if (is.finite(delta) && delta < tol) {
      converged <- TRUE
      w <- w_new
      break
    }
    w <- w_new
  }

  theta_raw <- log(w)
  se_raw <- ifelse(total_comp > 0, 1 / sqrt(total_comp), 1)
  se_raw <- stats::setNames(as.double(se_raw), ids)

  list(theta_raw = theta_raw, se_raw = se_raw, converged = converged)
}

#' Fast Bayesian BTL approximation for adaptive selection
#'
#' Selection-grade approximation used only for adaptive selection and stopping
#' checks. Results from this function are not final and must not be reported as
#' uncertainty-quantified rankings.
#'
#' @param results Canonical \code{results_tbl} with \code{A_id}, \code{B_id}, and
#'   \code{better_id}.
#' @param ids Character vector of all sample ids (length \code{N}).
#' @param method Approximation method. Only \code{"laplace_sirt"} is supported.
#' @param n_draws Number of posterior draws to generate.
#' @param seed Optional integer seed for determinism.
#'
#' @return A list with:
#' \describe{
#'   \item{theta_mean}{Named numeric vector of posterior means (names == ids).}
#'   \item{theta_draws}{Matrix of posterior draws \code{[n_draws, N]} (colnames == ids).}
#'   \item{fit_meta}{Metadata about the approximation run.}
#' }
#'
#' @examples
#' results <- tibble::tibble(
#'   pair_uid = "A:B#1",
#'   unordered_key = "A:B",
#'   ordered_key = "A:B",
#'   A_id = "A",
#'   B_id = "B",
#'   better_id = "A",
#'   winner_pos = 1L,
#'   phase = "phase2",
#'   iter = 1L,
#'   received_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
#'   backend = "openai",
#'   model = "gpt-test"
#' )
#' fit <- fit_bayes_btl_fast(results, ids = c("A", "B"), n_draws = 10, seed = 1)
#'
#' \dontrun{
#' # Full workflow shape (after adaptive run has completed):
#' # 1) Read observed results (results_tbl) and ids from state
#' # 2) Compute fast draws for selection or stopping checks
#' results_tbl <- state$history_results
#' ids <- state$ids
#' fast_fit <- fit_bayes_btl_fast(results_tbl, ids, n_draws = 400, seed = 123)
#' }
#'
#' @export
fit_bayes_btl_fast <- function(
    results,
    ids,
    method = c("laplace_sirt"),
    n_draws = 400,
    seed = NULL
) {
  method <- match.arg(method)

  if (!is.data.frame(results)) {
    rlang::abort("`results` must be a data frame or tibble.")
  }
  results <- tibble::as_tibble(results)
  required <- c("A_id", "B_id", "better_id")
  .adaptive_required_cols(results, "results", required)
  validate_results_tbl(results)

  ids <- .btl_fast_validate_ids(ids)

  result_ids <- unique(c(results$A_id, results$B_id, results$better_id))
  missing_ids <- setdiff(result_ids, ids)
  if (length(missing_ids) > 0L) {
    rlang::abort("All ids in `results` must be contained in `ids`.")
  }

  bad_better <- !(results$better_id == results$A_id | results$better_id == results$B_id)
  if (any(bad_better)) {
    rlang::abort("`results$better_id` must match `A_id` or `B_id` for every row.")
  }

  start_time <- proc.time()[["elapsed"]]

  edges <- .btl_fast_prepare_edges(results)
  fit <- .btl_fast_fit(edges, ids)

  theta_centered <- fit$theta_raw - mean(fit$theta_raw)
  scale_sd <- stats::sd(theta_centered)
  if (!is.finite(scale_sd) || scale_sd <= 0) {
    scale_sd <- 1
  }
  theta_mean <- theta_centered / scale_sd
  names(theta_mean) <- ids

  se_scaled <- fit$se_raw / scale_sd
  se_scaled[!is.finite(se_scaled) | se_scaled <= 0] <- 1
  names(se_scaled) <- ids

  n_draws <- as.integer(n_draws)
  if (is.na(n_draws) || n_draws < 2L) {
    rlang::abort("`n_draws` must be an integer >= 2.")
  }

  theta_draws <- .pairwiseLLM_with_seed(seed, function() {
    N <- length(ids)
    base <- matrix(stats::rnorm(n_draws * N), nrow = n_draws, ncol = N)
    scaled <- sweep(base, 2, se_scaled, `*`)
    sweep(scaled, 2, theta_mean, `+`)
  })

  theta_draws <- theta_draws - rowMeans(theta_draws)
  row_sd <- apply(theta_draws, 1, stats::sd)
  row_sd[!is.finite(row_sd) | row_sd <= 0] <- 1
  theta_draws <- theta_draws / row_sd
  colnames(theta_draws) <- ids

  end_time <- proc.time()[["elapsed"]]
  fit_time <- as.double(end_time - start_time)

  list(
    theta_mean = theta_mean,
    theta_draws = theta_draws,
    fit_meta = list(
      method = method,
      n_draws = n_draws,
      converged = fit$converged,
      message = if (nrow(edges) == 0L) "No comparisons available; defaults applied." else NULL,
      fit_time_sec = fit_time
    )
  )
}
