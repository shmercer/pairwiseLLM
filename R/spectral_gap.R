# Internal helpers for an optional end-of-run spectral gap check.
#
# NOTE: This file intentionally contains only internal (non-exported) helpers.
# The goal is to provide a deterministic, lightweight, end-of-run diagnostic.

#' @keywords internal
#' @noRd
.sg_n_components_from_sparse_adj <- function(W) {
  # W is a symmetric sparse adjacency (dgCMatrix).
  n <- nrow(W)
  if (is.null(n) || n == 0L) {
    return(0L)
  }

  # Build adjacency lists from sparse triplets.
  s <- Matrix::summary(W)
  if (nrow(s) == 0L) {
    return(as.integer(n))
  }

  s <- s[s$i != s$j, , drop = FALSE]
  adj <- vector("list", n)
  if (nrow(s) > 0L) {
    for (k in seq_len(nrow(s))) {
      i <- s$i[[k]]
      j <- s$j[[k]]
      adj[[i]] <- c(adj[[i]], j)
    }
  }

  visited <- rep(FALSE, n)
  n_comp <- 0L

  for (start in seq_len(n)) {
    if (!visited[[start]]) {
      n_comp <- n_comp + 1L
      # Simple queue-based BFS (deterministic).
      q <- start
      visited[[start]] <- TRUE

      while (length(q) > 0L) {
        v <- q[[1]]
        q <- q[-1]
        nbrs <- adj[[v]]
        if (length(nbrs) > 0L) {
          for (u in nbrs) {
            if (!visited[[u]]) {
              visited[[u]] <- TRUE
              q <- c(q, u)
            }
          }
        }
      }
    }
  }

  as.integer(n_comp)
}

#' Estimate spectral gap of a lazy random walk on an undirected graph
#'
#' This is a deterministic, lightweight end-of-run diagnostic; it is not
#' used for per-round decisions by default.
#'
#' @param pairs Tibble/data.frame with pair columns.
#' @param ids Character vector of node IDs.
#' @param weights How to treat repeated edges: "count" uses multiplicity,
#'   "binary" uses an unweighted simple graph.
#' @param max_iter Maximum number of iterations for the power method.
#' @param tol Convergence tolerance on the Rayleigh quotient.
#' @param id1_col,id2_col Column names for IDs in `pairs`.
#'
#' @return A one-row tibble with columns:
#'   * spectral_gap_est: 1 - |lambda2| (lazy RW)
#'   * lambda2_est: estimated second-largest eigenvalue magnitude
#'   * iters: iterations used
#'   * converged: logical
#'
#' @keywords internal
#' @noRd
.estimate_spectral_gap_lazy_rw <- function(pairs,
                                           ids,
                                           weights = c("count", "binary"),
                                           max_iter = 200L,
                                           tol = 1e-6,
                                           id1_col = "ID1",
                                           id2_col = "ID2",
                                           init_vec = NULL) {
  weights <- match.arg(weights)
  max_iter <- as.integer(max_iter)
  tol <- as.double(tol)

  ids <- sort(unique(as.character(ids)))
  n <- length(ids)

  out <- tibble::tibble(
    spectral_gap_est = NA_real_,
    lambda2_est = NA_real_,
    iters = 0L,
    converged = FALSE
  )

  if (n < 2L) {
    out$converged <- TRUE
    return(out)
  }

  if (is.null(pairs) || nrow(pairs) == 0L) {
    out$spectral_gap_est <- 0
    out$lambda2_est <- 1
    out$iters <- 0L
    out$converged <- TRUE
    return(out)
  }

  pairs <- tibble::as_tibble(pairs)
  if (!all(c(id1_col, id2_col) %in% names(pairs))) {
    rlang::abort(paste0("pairs must contain columns ", sQuote(id1_col), " and ", sQuote(id2_col), "."))
  }

  e1 <- as.character(pairs[[id1_col]])
  e2 <- as.character(pairs[[id2_col]])

  ok <- !is.na(e1) & !is.na(e2) & e1 %in% ids & e2 %in% ids & e1 != e2
  e1 <- e1[ok]
  e2 <- e2[ok]

  if (length(e1) == 0L) {
    out$spectral_gap_est <- 0
    out$lambda2_est <- 1
    out$iters <- 0L
    out$converged <- TRUE
    return(out)
  }

  # setNames() lives in the stats namespace; qualify explicitly for R CMD check.
  id_to_index <- stats::setNames(seq_len(n), ids)
  i <- as.integer(id_to_index[e1])
  j <- as.integer(id_to_index[e2])

  if (weights == "binary") {
    a <- pmin(i, j)
    b <- pmax(i, j)
    key <- paste(a, b, sep = "_")
    keep <- !duplicated(key)
    i <- a[keep]
    j <- b[keep]
  }

  W <- Matrix::sparseMatrix(
    i = c(i, j),
    j = c(j, i),
    x = rep(1, 2L * length(i)),
    dims = c(n, n)
  )
  W <- Matrix::drop0(W)

  # Disconnected graphs (including isolated nodes) yield spectral gap 0 for the
  # whole-chain lazy RW (lambda2 magnitude = 1).
  deg <- as.numeric(Matrix::rowSums(W))
  n_comp <- .sg_n_components_from_sparse_adj(W)
  if (n_comp > 1L || any(deg <= 0)) {
    out$spectral_gap_est <- 0
    out$lambda2_est <- 1
    out$iters <- 0L
    out$converged <- TRUE
    return(out)
  }

  inv_sqrt_deg <- 1 / sqrt(deg)

  # Leading eigenvector for the symmetric similarity transform.
  u <- sqrt(deg)
  u <- u / sqrt(sum(u^2))

  # Deterministic initialization, orthogonalized to u.
  v <- if (is.null(init_vec)) {
    as.double(seq_len(n))
  } else {
    as.double(init_vec)
  }
  v <- v - u * sum(u * v)
  v_norm <- sqrt(sum(v^2))

  if (isTRUE(v_norm == 0)) {
    v <- rep(0, n)
    v[1] <- 1
    v[2] <- -1
    v <- v - u * sum(u * v)
    v_norm <- sqrt(sum(v^2))
  }

  v <- v / v_norm

  mult_S_lazy <- function(vec) {
    # S = D^{-1/2} W D^{-1/2}, S_lazy = (I + S) / 2
    x <- inv_sqrt_deg * vec
    y <- as.numeric(W %*% x)
    z <- inv_sqrt_deg * y
    0.5 * vec + 0.5 * z
  }

  lambda_prev <- NA_real_
  iters_used <- 0L
  converged <- FALSE

  for (it in seq_len(max_iter)) {
    iters_used <- it

    w <- mult_S_lazy(v)

    # Deflate the principal eigenvector.
    w <- w - u * sum(u * w)

    w_norm <- sqrt(sum(w^2))
    if (w_norm == 0) {
      lambda_prev <- 0
      converged <- TRUE
      break
    }

    v <- w / w_norm

    Sv <- mult_S_lazy(v)
    lambda <- sum(v * Sv)

    if (!is.na(lambda_prev) && abs(lambda - lambda_prev) < tol) {
      converged <- TRUE
      lambda_prev <- lambda
      break
    }

    lambda_prev <- lambda
  }

  if (!is.na(lambda_prev)) {
    lambda2 <- abs(as.double(lambda_prev))
    out$lambda2_est <- lambda2
    out$spectral_gap_est <- 1 - lambda2
    out$iters <- as.integer(iters_used)
    out$converged <- converged
    return(out)
  }

  out$iters <- as.integer(iters_used)
  out$converged <- converged
  out
}

#' @keywords internal
.spectral_gap_checks_template <- function() {
  tibble::tibble(
    when = character(),
    round = integer(),
    spectral_gap_est = double(),
    lambda2_est = double(),
    iters = integer(),
    converged = logical(),
    spectral_gap_warn = logical()
  )
}
