# Prepare and stabilize reference theta scales used for linking.
#
# These helpers are intentionally internal (not exported). They are used by the
# core-linking runners to make linking and drift diagnostics robust in the
# presence of early-round separation and arbitrary sign orientation.

# Compute per-ID "win score" from a BT data table.
.bt_win_scores <- function(bt_data) {
  bt_data <- tibble::as_tibble(bt_data)
  if (nrow(bt_data) == 0L) {
    return(stats::setNames(numeric(0), character(0)))
  }
  if (!all(c("object1", "object2") %in% names(bt_data))) {
    return(stats::setNames(numeric(0), character(0)))
  }

  ids <- sort(unique(c(as.character(bt_data$object1), as.character(bt_data$object2))))
  wins <- stats::setNames(rep(0, length(ids)), ids)

  if ("winner" %in% names(bt_data)) {
    w <- as.character(bt_data$winner)
    w <- w[!is.na(w) & nzchar(w)]
    tab <- table(w)
    wins[names(tab)] <- wins[names(tab)] + as.numeric(tab)
    return(wins)
  }

  # btm-style: result in {1, 0, 0.5}, where 1 = object1 wins, 0 = object2 wins
  if ("result" %in% names(bt_data)) {
    res <- as.numeric(bt_data$result)
    o1 <- as.character(bt_data$object1)
    o2 <- as.character(bt_data$object2)
    ok1 <- is.finite(res) & res == 1
    ok0 <- is.finite(res) & res == 0
    okt <- is.finite(res) & res == 0.5
    if (any(ok1)) {
      tab <- table(o1[ok1])
      wins[names(tab)] <- wins[names(tab)] + as.numeric(tab)
    }
    if (any(ok0)) {
      tab <- table(o2[ok0])
      wins[names(tab)] <- wins[names(tab)] + as.numeric(tab)
    }
    if (any(okt)) {
      # ties: half-win to each side
      tab1 <- table(o1[okt])
      tab2 <- table(o2[okt])
      wins[names(tab1)] <- wins[names(tab1)] + 0.5 * as.numeric(tab1)
      wins[names(tab2)] <- wins[names(tab2)] + 0.5 * as.numeric(tab2)
    }
    return(wins)
  }

  wins
}


# Orient theta so that it has non-negative correlation with win scores.
.bt_orient_theta_by_wins <- function(theta_tbl, bt_data) {
  theta_tbl <- tibble::as_tibble(theta_tbl)
  if (nrow(theta_tbl) == 0L || !all(c("ID", "theta") %in% names(theta_tbl))) {
    return(list(theta = theta_tbl, flipped = FALSE, cor = NA_real_))
  }
  wins <- .bt_win_scores(bt_data)
  if (length(wins) < 2L) {
    return(list(theta = theta_tbl, flipped = FALSE, cor = NA_real_))
  }
  idx <- match(theta_tbl$ID, names(wins))
  w <- wins[idx]
  ok <- stats::complete.cases(theta_tbl$theta, w)
  if (sum(ok) < 2L) {
    return(list(theta = theta_tbl, flipped = FALSE, cor = NA_real_))
  }
  cor_val <- suppressWarnings(stats::cor(theta_tbl$theta[ok], w[ok], method = "pearson"))
  flipped <- FALSE
  if (is.finite(cor_val) && cor_val < 0) {
    theta_tbl$theta <- -theta_tbl$theta
    if ("theta_linked" %in% names(theta_tbl)) theta_tbl$theta_linked <- -theta_tbl$theta_linked
    flipped <- TRUE
    cor_val <- -cor_val
  }
  list(theta = theta_tbl, flipped = flipped, cor = as.numeric(cor_val))
}


# Robust center/scale and clamp theta to avoid reference blow-ups.
.bt_normalize_theta_tbl <- function(theta_tbl,
                                    ids = NULL,
                                    center = TRUE,
                                    scale = TRUE,
                                    scale_method = c("median_iqr", "median_mad", "mean_sd"),
                                    max_abs = 6) {
  scale_method <- match.arg(scale_method)
  theta_tbl <- tibble::as_tibble(theta_tbl)
  if (nrow(theta_tbl) == 0L || !all(c("ID", "theta") %in% names(theta_tbl))) {
    return(list(theta = theta_tbl, center = 0, scale = 1))
  }

  use <- theta_tbl
  if (!is.null(ids)) {
    use <- use[use$ID %in% ids, , drop = FALSE]
  }
  x <- as.numeric(use$theta)
  x <- x[is.finite(x)]
  if (length(x) < 2L) {
    return(list(theta = theta_tbl, center = 0, scale = 1))
  }

  a <- 0
  b <- 1
  if (isTRUE(center)) {
    if (scale_method %in% c("median_iqr", "median_mad")) {
      a <- stats::median(x, na.rm = TRUE)
    } else {
      a <- mean(x, na.rm = TRUE)
    }
  }

  if (isTRUE(scale)) {
    if (scale_method == "median_iqr") {
      i <- stats::IQR(x, na.rm = TRUE, type = 7)
      s <- as.numeric(i) / 1.349
      if (!is.finite(s) || s <= 1e-8) s <- stats::sd(x, na.rm = TRUE)
      if (!is.finite(s) || s <= 1e-8) s <- 1
      b <- s
    } else if (scale_method == "median_mad") {
      s <- stats::mad(x, constant = 1, na.rm = TRUE)
      s <- as.numeric(s) / 0.6745
      if (!is.finite(s) || s <= 1e-8) s <- stats::sd(x, na.rm = TRUE)
      if (!is.finite(s) || s <= 1e-8) s <- 1
      b <- s
    } else {
      s <- stats::sd(x, na.rm = TRUE)
      if (!is.finite(s) || s <= 1e-8) s <- 1
      b <- s
    }
  }

  # Transform: (theta - a) / b
  theta_tbl$theta <- (theta_tbl$theta - a) / b
  if ("se" %in% names(theta_tbl)) {
    theta_tbl$se <- theta_tbl$se / abs(b)
  }
  if ("theta_linked" %in% names(theta_tbl)) {
    theta_tbl$theta_linked <- (theta_tbl$theta_linked - a) / b
  }
  if ("se_linked" %in% names(theta_tbl)) {
    theta_tbl$se_linked <- theta_tbl$se_linked / abs(b)
  }

  # Clamp extreme values (useful under separation).
  if (is.finite(max_abs) && max_abs > 0) {
    theta_tbl$theta <- pmax(-max_abs, pmin(max_abs, theta_tbl$theta))
    if ("theta_linked" %in% names(theta_tbl)) {
      theta_tbl$theta_linked <- pmax(-max_abs, pmin(max_abs, theta_tbl$theta_linked))
    }
  }

  list(theta = theta_tbl, center = as.numeric(a), scale = as.numeric(b))
}


# Prepare reference fit for linking/drift: orient by wins, then robust normalize.
.bt_prepare_reference_fit <- function(fit,
                                      bt_data,
                                      core_ids,
                                      scale_method = c("median_iqr", "median_mad", "mean_sd"),
                                      max_abs = 6) {
  scale_method <- match.arg(scale_method)
  if (is.null(fit) || is.null(fit$theta) || !inherits(fit$theta, "data.frame")) {
    return(fit)
  }
  theta_tbl <- tibble::as_tibble(fit$theta)

  oriented <- .bt_orient_theta_by_wins(theta_tbl, bt_data)
  normed <- .bt_normalize_theta_tbl(
    oriented$theta,
    ids = core_ids,
    center = TRUE,
    scale = TRUE,
    scale_method = scale_method,
    max_abs = max_abs
  )
  fit$theta <- normed$theta
  fit$reference_norm <- list(
    oriented_by = "wins",
    wins_cor = oriented$cor,
    flipped = isTRUE(oriented$flipped),
    center = normed$center,
    scale = normed$scale,
    scale_method = scale_method,
    max_abs = max_abs
  )
  fit
}
