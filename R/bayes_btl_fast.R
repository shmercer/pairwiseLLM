# -------------------------------------------------------------------------
# Fast selection-grade BTL inference.
# Fast inference is used only for adaptive selection. Final inference and
# reporting use MCMC (PR-A6).
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
    return(tibble::tibble(i_id = character(), j_id = character(), win_i = integer(), win_j = integer(), n_ij = integer()))
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

#' @keywords internal
#' @noRd
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

  theta_mean <- fit$theta_raw - mean(fit$theta_raw)
  scale_sd <- stats::sd(theta_mean)
  if (!is.finite(scale_sd) || scale_sd <= 0) {
    scale_sd <- 1
  }
  theta_mean <- theta_mean / scale_sd
  names(theta_mean) <- ids

  n_draws <- as.integer(n_draws)
  if (is.na(n_draws) || n_draws < 2L) {
    rlang::abort("`n_draws` must be an integer >= 2.")
  }

  theta_draws <- .pairwiseLLM_with_seed(seed, function() {
    N <- length(ids)
    base <- matrix(stats::rnorm(n_draws * N), nrow = n_draws, ncol = N)
    scaled <- sweep(base, 2, fit$se_raw, `*`)
    sweep(scaled, 2, fit$theta_raw, `+`)
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
