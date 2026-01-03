# -------------------------------------------------------------------------
# Stability metrics helpers (theta drift + Top-K stability)
# -------------------------------------------------------------------------

#' Internal: compute stability metrics between successive theta estimates
#'
#' Computes scale-aligned stability summaries between a prior and current theta
#' table. Thetas are centered within matched IDs each call; if the correlation
#' between rounds is negative, the current thetas are sign-flipped before RMS and
#' Top-K calculations.
#'
#' @param prev_theta_tbl Prior theta table with columns `ID` and `theta`.
#' @param curr_theta_tbl Current theta table with columns `ID` and `theta`.
#' @param topk Integer K for Top-K overlap.
#' @param topk_ties Character; tie-breaking for Top-K. `"id"` sorts by `(theta desc, ID asc)`
#'   and is deterministic. `"random"` uses a seed and randomizes within exact-theta ties.
#' @param seed Optional integer seed used only when `topk_ties = "random"`.
#'
#' @return One-row tibble with `n_matched`, `rms_theta_delta`, `topk_overlap`,
#'   and `rank_corr` (Spearman on ranks; NA if not enough matched IDs).
#'
#' @keywords internal
.stability_metrics <- function(prev_theta_tbl,
                               curr_theta_tbl,
                               topk = 50L,
                               topk_ties = c("id", "random"),
                               seed = NULL) {
  topk <- as.integer(topk)
  if (is.na(topk) || topk < 1L) {
    stop("`topk` must be a positive integer.", call. = FALSE)
  }
  topk_ties <- match.arg(topk_ties)

  prev_theta_tbl <- tibble::as_tibble(prev_theta_tbl)
  curr_theta_tbl <- tibble::as_tibble(curr_theta_tbl)

  if (!all(c("ID", "theta") %in% names(prev_theta_tbl))) {
    stop("`prev_theta_tbl` must contain columns `ID` and `theta`.", call. = FALSE)
  }
  if (!all(c("ID", "theta") %in% names(curr_theta_tbl))) {
    stop("`curr_theta_tbl` must contain columns `ID` and `theta`.", call. = FALSE)
  }

  prev_theta_tbl <- dplyr::transmute(
    prev_theta_tbl,
    ID = as.character(.data$ID),
    theta_prev = as.double(.data$theta)
  )
  curr_theta_tbl <- dplyr::transmute(
    curr_theta_tbl,
    ID = as.character(.data$ID),
    theta_curr = as.double(.data$theta)
  )

  joined <- dplyr::inner_join(prev_theta_tbl, curr_theta_tbl, by = "ID")
  n_matched <- nrow(joined)

  if (n_matched == 0L) {
    return(tibble::tibble(
      n_matched = 0L,
      rms_theta_delta = NA_real_,
      topk_overlap = NA_real_,
      rank_corr = NA_real_
    ))
  }

  joined <- dplyr::mutate(
    joined,
    theta_prev = .data$theta_prev - mean(.data$theta_prev, na.rm = TRUE),
    theta_curr = .data$theta_curr - mean(.data$theta_curr, na.rm = TRUE)
  )

  # Sign-flip alignment (handles global invariance)
  cor_pc <- suppressWarnings(stats::cor(joined$theta_prev, joined$theta_curr, use = "pairwise.complete.obs"))
  if (is.finite(cor_pc) && cor_pc < 0) {
    joined <- dplyr::mutate(joined, theta_curr = -.data$theta_curr)
  }

  rms_theta_delta <- sqrt(mean((joined$theta_curr - joined$theta_prev)^2, na.rm = TRUE))

  # Rank correlation (optional diagnostic)
  rank_corr <- NA_real_
  if (n_matched >= 2L) {
    rank_corr <- suppressWarnings(stats::cor(
      rank(-joined$theta_prev, ties.method = "average"),
      rank(-joined$theta_curr, ties.method = "average"),
      method = "spearman",
      use = "pairwise.complete.obs"
    ))
  }

  # Deterministic Top-K sets (or seeded randomized tie-breaking)
  K <- min(topk, n_matched)

  top_ids <- function(df, theta_col) {
    df <- dplyr::mutate(df, theta = .data[[theta_col]])
    if (identical(topk_ties, "random")) {
      # Randomize within exact-theta ties while keeping theta order.
      df <- dplyr::arrange(df, dplyr::desc(.data$theta), .data$ID)
      df <- dplyr::group_by(df, .data$theta)
      df <- dplyr::group_modify(df, function(.x, .y) {
        if (nrow(.x) <= 1L) {
          return(.x)
        }
        idx <- .with_seed_restore(seed, function() sample.int(nrow(.x)), arg_name = "stability_seed")
        .x[idx, , drop = FALSE]
      })
      df <- dplyr::ungroup(df)
      df <- dplyr::arrange(df, dplyr::desc(.data$theta))
    } else {
      df <- dplyr::arrange(df, dplyr::desc(.data$theta), .data$ID)
    }
    head(df$ID, K)
  }

  prev_top <- top_ids(joined, "theta_prev")
  curr_top <- top_ids(joined, "theta_curr")
  topk_overlap <- length(intersect(prev_top, curr_top)) / K

  tibble::tibble(
    n_matched = as.integer(n_matched),
    rms_theta_delta = as.double(rms_theta_delta),
    topk_overlap = as.double(topk_overlap),
    rank_corr = as.double(rank_corr)
  )
}
