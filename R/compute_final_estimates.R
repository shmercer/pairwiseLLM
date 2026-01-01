#' Compute final ranking estimates from pairwise results
#'
#' This helper computes a comprehensive set of final ranking outputs from a
#' set of pairwise comparison results. It is intended for "end of run" use,
#' where the full set of comparisons is available.
#'
#' The function fits:
#' \itemize{
#'   \item A Bradley--Terry model via \pkg{BradleyTerry2}. If bias reduction is
#'   enabled (default), the function attempts a bias-reduced fit (often called
#'   "Firth" correction). If the bias-reduced fit is unavailable (e.g., missing
#'   optional dependencies), the function falls back to a standard maximum
#'   likelihood Bradley--Terry fit.
#'   \item A Rank Centrality model (spectral / random-walk based) via
#'   \code{\link{fit_rank_centrality}}.
#' }
#'
#' In addition to model-based estimates, the returned table includes
#' graph-derived summaries such as win/loss/tie counts, appearance counts
#' (degree proxies), and connectivity diagnostics from Rank Centrality.
#'
#' @param results A data frame with columns \code{ID1}, \code{ID2}, and
#'   \code{better_id}. This is typically the \code{$results} output from an
#'   adaptive run.
#' @param ids Optional character vector of all item IDs. If \code{NULL}
#'   (default), IDs are inferred from \code{results}.
#' @param bt_bias_reduction Logical; attempt a bias-reduced Bradley--Terry fit
#'   via \pkg{BradleyTerry2} when available (default \code{TRUE}). If the
#'   bias-reduced fit fails, the function falls back to standard MLE.
#' @param bt_verbose Logical; if \code{TRUE}, allow model-fitting warnings to be
#'   printed. If \code{FALSE} (default), suppress common fitting warnings.
#' @param rc_smoothing Numeric scalar \code{>= 0}. Passed to
#'   \code{\link{fit_rank_centrality}}.
#' @param rc_damping Numeric scalar in \code{[0,1)}. Passed to
#'   \code{\link{fit_rank_centrality}}.
#' @param ... Additional arguments forwarded to \code{\link{fit_rank_centrality}}.
#'
#' @return A list with elements:
#' \describe{
#'   \item{estimates}{A tibble with one row per \code{ID}, including:
#'     \itemize{
#'       \item \code{theta_bt_firth}: Bradley--Terry ability estimate (centered)
#'       \item \code{se_bt_firth}: standard error for \code{theta_bt_firth}
#'       \item \code{rank_bt_firth}: rank of \code{theta_bt_firth} (1 = highest)
#'       \item \code{pi_rc}: Rank Centrality stationary probability
#'       \item \code{theta_rc}: centered \code{log(pi_rc)}
#'       \item \code{rank_rc}: rank of \code{theta_rc} (1 = highest)
#'       \item \code{wins}, \code{losses}, \code{ties}: counts from \code{results}
#'       \item \code{n_appear}, \code{n_pos1}, \code{n_pos2}: appearance counts
#'       \item \code{component_id}: connected-component label from Rank Centrality
#'     }
#'   }
#'   \item{bt_fit}{The full Bradley--Terry fit object (list) as returned internally.}
#'   \item{rc_fit}{The Rank Centrality fit list returned by \code{\link{fit_rank_centrality}}.}
#'   \item{diagnostics}{A list of run-level diagnostics including whether bias
#'     reduction was used and basic graph stats.}
#' }
#'
#' @examples
#' data("example_writing_pairs")
#' out <- compute_final_estimates(example_writing_pairs)
#' out$estimates
#'
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr mutate transmute left_join arrange desc coalesce
#' @export
compute_final_estimates <- function(
  results,
  ids = NULL,
  bt_bias_reduction = TRUE,
  bt_verbose = FALSE,
  rc_smoothing = 0.5,
  rc_damping = 0,
  ...
) {
  results <- tibble::as_tibble(results)

  required_cols <- c("ID1", "ID2", "better_id")
  if (!all(required_cols %in% names(results))) {
    stop(
      "`results` must contain columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(ids)) {
    ids <- sort(unique(c(as.character(results$ID1), as.character(results$ID2))))
  } else {
    if (!is.character(ids) || length(ids) == 0L || anyNA(ids)) {
      stop("`ids` must be a non-empty character vector with no NA values.", call. = FALSE)
    }
    ids <- as.character(ids)
  }

  # ---- Graph summaries (wins/losses/ties + appearance counts) ----
  gl <- .summarize_results_graph(results, ids)
  wlt <- gl$wlt
  deg <- gl$degree

  # ---- Bradley--Terry fit (bias-reduced if possible) ----
  bt_data <- build_bt_data(results)
  bt_fit <- .fit_bt_bradleyterry2(
    bt_data,
    ids = ids,
    bias_reduction = isTRUE(bt_bias_reduction),
    verbose = isTRUE(bt_verbose)
  )

  bt_theta_tbl <- tibble::as_tibble(bt_fit$theta)
  idx_bt <- match(ids, bt_theta_tbl$ID)
  theta_bt_firth <- bt_theta_tbl$theta[idx_bt]
  se_bt_firth <- bt_theta_tbl$se[idx_bt]
  theta_bt_firth <- theta_bt_firth - mean(theta_bt_firth, na.rm = TRUE)

  # ---- Rank Centrality ----
  rc_fit <- fit_rank_centrality(
    bt_data,
    ids = ids,
    smoothing = rc_smoothing,
    damping = rc_damping,
    verbose = FALSE,
    ...
  )

  rc_theta_tbl <- tibble::as_tibble(rc_fit$theta)
  idx_rc <- match(ids, rc_theta_tbl$ID)
  pi_rc <- rc_theta_tbl$pi[idx_rc]
  theta_rc <- rc_theta_tbl$theta[idx_rc]

  component_id <- rep(NA_integer_, length(ids))
  cid <- rc_fit$diagnostics$component_id
  if (!is.null(cid)) {
    component_id <- as.integer(unname(cid[ids]))
  }

  # ---- Assemble estimates (stable schema) ----
  est <- .make_estimates_tbl(
    ids = ids,
    theta_bt_firth = theta_bt_firth,
    se_bt_firth = se_bt_firth,
    theta_rc = theta_rc,
    pi_rc = pi_rc,
    wins = wlt$wins,
    losses = wlt$losses,
    ties = wlt$ties,
    n_appear = deg$n_appear,
    n_pos1 = deg$n_pos1,
    n_pos2 = deg$n_pos2,
    component_id = component_id
  )
  est <- dplyr::arrange(est, dplyr::desc(.data$theta_bt_firth))

  diagnostics <- list(
    bt_engine = "BradleyTerry2",
    bt_bias_reduced = isTRUE(bt_fit$bias_reduced),
    rc_engine = "rank_centrality",
    n_nodes = rc_fit$diagnostics$n_nodes %||% length(ids),
    n_edges = rc_fit$diagnostics$n_edges %||% NA_integer_,
    n_components = rc_fit$diagnostics$n_components %||% NA_integer_,
    largest_component_frac = rc_fit$diagnostics$largest_component_frac %||% NA_real_,
    spectral_gap = rc_fit$diagnostics$spectral_gap %||% NA_real_
  )

  list(
    estimates = est,
    bt_fit = bt_fit,
    rc_fit = rc_fit,
    diagnostics = diagnostics
  )
}


# ---- Internal helpers ----

.summarize_results_graph <- function(results, ids) {
  r <- tibble::as_tibble(results)
  r <- dplyr::mutate(
    r,
    ID1 = as.character(.data$ID1),
    ID2 = as.character(.data$ID2),
    better_id = as.character(.data$better_id)
  )

  # Appearance counts
  n_pos1 <- as.integer(table(factor(r$ID1, levels = ids)))
  n_pos2 <- as.integer(table(factor(r$ID2, levels = ids)))
  degree <- tibble::tibble(
    ID = ids,
    n_appear = n_pos1 + n_pos2,
    n_pos1 = n_pos1,
    n_pos2 = n_pos2
  )

  wins <- stats::setNames(integer(length(ids)), ids)
  losses <- stats::setNames(integer(length(ids)), ids)
  ties <- stats::setNames(integer(length(ids)), ids)

  ok <- !is.na(r$better_id) & nzchar(r$better_id)
  rr <- r[ok, , drop = FALSE]
  for (i in seq_len(nrow(rr))) {
    a <- rr$ID1[i]
    b <- rr$ID2[i]
    w <- rr$better_id[i]
    if (w == a && w != b) {
      wins[a] <- wins[a] + 1L
      losses[b] <- losses[b] + 1L
    } else if (w == b && w != a) {
      wins[b] <- wins[b] + 1L
      losses[a] <- losses[a] + 1L
    } else {
      # Treat unknown/other as tie
      ties[a] <- ties[a] + 1L
      ties[b] <- ties[b] + 1L
    }
  }

  wlt <- tibble::tibble(
    ID = ids,
    wins = as.integer(unname(wins)),
    losses = as.integer(unname(losses)),
    ties = as.integer(unname(ties))
  )

  list(wlt = wlt, degree = degree)
}


.rank_desc_numeric <- function(x) {
  # 1 = highest. Missing/non-finite remain NA.
  x <- as.double(unname(x))
  out <- rep(NA_integer_, length(x))
  ok <- which(is.finite(x))
  if (length(ok) == 0L) {
    return(out)
  }
  ord <- order(x[ok], decreasing = TRUE)
  out[ok[ord]] <- seq_along(ord)
  out
}


.fit_bt_bradleyterry2 <- function(bt_data, ids, bias_reduction = TRUE, verbose = FALSE) {
  if (!.require_ns("BradleyTerry2", quietly = TRUE)) {
    stop(
      "Package 'BradleyTerry2' must be installed to fit Bradley--Terry models.\n",
      "Install it with: install.packages('BradleyTerry2')",
      call. = FALSE
    )
  }

  dat <- as.data.frame(bt_data)
  dat <- dat[, 1:3, drop = FALSE]
  names(dat)[1:3] <- c("object1", "object2", "result")

  # Aggregate wins for object1 vs object2
  wins1 <- stats::aggregate(I(result == 1) ~ object1 + object2, data = dat, sum)
  wins0 <- stats::aggregate(I(result == 0) ~ object1 + object2, data = dat, sum)
  agg <- merge(wins1, wins0, by = c("object1", "object2"), all = TRUE)
  agg[is.na(agg)] <- 0
  names(agg)[3:4] <- c("win1", "win2")

  players <- sort(unique(c(agg$object1, agg$object2, ids)))
  agg$object1 <- factor(agg$object1, levels = players)
  agg$object2 <- factor(agg$object2, levels = players)

  fit_one <- function(br) {
    if (isTRUE(verbose)) {
      BradleyTerry2::BTm(
        outcome = cbind(agg$win1, agg$win2),
        player1 = agg$object1,
        player2 = agg$object2,
        data = agg,
        br = br
      )
    } else {
      suppressWarnings(
        BradleyTerry2::BTm(
          outcome = cbind(agg$win1, agg$win2),
          player1 = agg$object1,
          player2 = agg$object2,
          data = agg,
          br = br
        )
      )
    }
  }

  fit <- NULL
  bias_used <- FALSE

  if (isTRUE(bias_reduction)) {
    fit <- tryCatch(fit_one(TRUE), error = function(e) e)
    if (!inherits(fit, "error")) {
      bias_used <- TRUE
    } else {
      # Fall back to MLE
      fit <- fit_one(FALSE)
      bias_used <- FALSE
    }
  } else {
    fit <- fit_one(FALSE)
    bias_used <- FALSE
  }

  abil <- BradleyTerry2::BTabilities(fit)
  theta <- tibble::tibble(
    ID = rownames(abil),
    theta = abil[, 1],
    se = abil[, 2]
  )

  list(
    engine = "BradleyTerry2",
    fit = fit,
    theta = theta,
    reliability = NA_real_,
    diagnostics = NULL,
    bias_reduced = bias_used
  )
}
