#' Fit Rank Centrality scores from pairwise comparison data
#'
#' Rank Centrality is a spectral / random-walk based estimator for ranking items
#' from pairwise comparisons. It is particularly useful on sparse comparison
#' graphs and can be used as a stable running estimator during adaptive data
#' collection.
#'
#' The method constructs a Markov chain on the comparison graph. For each
#' compared pair \eqn{(i, j)}, let \eqn{\hat{p}_{ij}} be the empirical probability
#' that item \eqn{i} wins against item \eqn{j}. Rank Centrality defines transition
#' probabilities from \eqn{i} to neighbor \eqn{j} proportional to
#' \eqn{1 - \hat{p}_{ij}} (the probability that \eqn{j} beats \eqn{i}), then
#' normalizes by the degree of \eqn{i}. The stationary distribution of the chain
#' is used as a score; \code{theta} is reported as \code{log(pi)} centered to have
#' mean zero.
#'
#' This function accepts Bradley-Terry style comparison data (as produced by
#' \code{\link{build_bt_data}}). It is designed to be robust to sparse graphs
#' and extreme win/loss patterns by using optional smoothing and an optional
#' teleport/damping term (similar to PageRank) to ensure a unique stationary
#' distribution.
#'
#' @param bt_data A data frame or tibble with columns \code{object1},
#'   \code{object2}, and \code{result} (numeric 0/1), as produced by
#'   \code{\link{build_bt_data}}. A 4th column (e.g., \code{judge}) is allowed and
#'   ignored.
#' @param ids Optional character vector of item IDs to include. If \code{NULL}
#'   (default), uses the union of \code{object1} and \code{object2} in
#'   \code{bt_data}.
#' @param smoothing Non-negative numeric scalar. Adds a symmetric pseudo-count
#'   to each pair to avoid empirical probabilities of exactly 0 or 1. For each
#'   unordered pair, the smoothed estimate is
#'   \deqn{\hat{p}_{ij} = (w_{ij} + s) / (n_{ij} + 2s),}
#'   where \eqn{w_{ij}} is the number of wins by \eqn{i} over \eqn{j},
#'   \eqn{n_{ij}} is the total number of comparisons, and \eqn{s} is
#'   \code{smoothing}. Defaults to \code{0.5}.
#' @param damping Numeric scalar in \eqn{[0, 1)}. If greater than 0, mixes the
#'   transition matrix with a uniform teleport term to ensure ergodicity:
#'   \deqn{P \leftarrow (1 - \alpha) P + \alpha \mathbf{1}\mathbf{1}^T / n,}
#'   where \eqn{\alpha} is \code{damping}. Defaults to \code{0}.
#' @param max_iter Maximum number of iterations for the power method used to
#'   compute the stationary distribution. Defaults to \code{5000}.
#' @param tol Convergence tolerance for the power method. Defaults to
#'   \code{1e-10}.
#' @param return_transition Logical. If \code{TRUE}, include the transition
#'   matrix \code{P} in the returned object. Defaults to \code{FALSE}.
#' @param verbose Logical. If \code{TRUE}, print basic convergence information.
#'   Defaults to \code{FALSE}.
#'
#' @return A list with components:
#' \itemize{
#'   \item \code{engine}: \code{"rank_centrality"}.
#'   \item \code{reliability}: \code{NA_real_} (Rank Centrality does not provide
#'     an MLE reliability measure).
#'   \item \code{theta}: a tibble with columns \code{ID}, \code{theta},
#'     and \code{pi}. \code{theta} is \code{log(pi)} centered to have mean zero.
#'   \item \code{diagnostics}: a list containing graph and numerical diagnostics
#'     (degree statistics, number of connected components, spectral gap if
#'     computable, power-method iterations, and convergence error).
#'   \item \code{P}: (optional) the transition matrix if
#'     \code{return_transition = TRUE}.
#' }
#'
#' @examples
#' # A simple transitive toy example: A > B > C
#' bt_data <- tibble::tibble(
#'   object1 = c("A", "A", "B"),
#'   object2 = c("B", "C", "C"),
#'   result  = c(1, 1, 1)
#' )
#'
#' fit <- fit_rank_centrality(bt_data)
#' fit$theta
#'
#' # Disconnected graph (two components)
#' bt_data2 <- tibble::tibble(
#'   object1 = c("A", "C"),
#'   object2 = c("B", "D"),
#'   result  = c(1, 1)
#' )
#' fit2 <- fit_rank_centrality(bt_data2, damping = 0.1)
#' fit2$diagnostics$n_components
#'
#' @export
fit_rank_centrality <- function(
  bt_data,
  ids = NULL,
  smoothing = 0.5,
  damping = 0,
  max_iter = 5000,
  tol = 1e-10,
  return_transition = FALSE,
  verbose = FALSE
) {
  bt_data <- tibble::as_tibble(bt_data)

  required_cols <- c("object1", "object2", "result")
  if (!all(required_cols %in% names(bt_data))) {
    stop("`bt_data` must contain columns: object1, object2, result.", call. = FALSE)
  }

  if (!is.null(ids)) {
    if (!is.character(ids)) stop("`ids` must be a character vector.", call. = FALSE)
    ids <- unique(as.character(ids))
    if (length(ids) < 2L) stop("`ids` must contain at least 2 unique IDs.", call. = FALSE)
  }

  if (!is.numeric(smoothing) || length(smoothing) != 1L || is.na(smoothing) || smoothing < 0) {
    stop("`smoothing` must be a non-negative numeric scalar.", call. = FALSE)
  }

  if (!is.numeric(damping) || length(damping) != 1L || is.na(damping) || damping < 0 || damping >= 1) {
    stop("`damping` must be a numeric scalar in [0, 1).", call. = FALSE)
  }

  if (!is.numeric(max_iter) || length(max_iter) != 1L || is.na(max_iter) || max_iter < 1) {
    stop("`max_iter` must be a positive integer-like scalar.", call. = FALSE)
  }
  max_iter <- as.integer(max_iter)

  if (!is.numeric(tol) || length(tol) != 1L || is.na(tol) || tol <= 0) {
    stop("`tol` must be a positive numeric scalar.", call. = FALSE)
  }

  bt_data <- dplyr::mutate(
    bt_data,
    object1 = as.character(.data$object1),
    object2 = as.character(.data$object2)
  )

  # Normalise result to numeric 0/1 where possible
  bt_data$result <- as.numeric(bt_data$result)
  bt_data <- dplyr::filter(bt_data, !is.na(.data$result))
  bt_data <- dplyr::filter(bt_data, .data$result %in% c(0, 1))

  if (is.null(ids)) {
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
  } else {
    bt_data <- dplyr::filter(bt_data, .data$object1 %in% ids & .data$object2 %in% ids)
  }

  n <- length(ids)
  if (n < 2L) stop("Need at least 2 unique IDs to fit Rank Centrality.", call. = FALSE)

  idx <- stats::setNames(seq_len(n), ids)

  # Build win/loss counts per unordered pair
  # We'll store wins for directed i -> j in a sparse-like map via matrices for simplicity (n is small here).
  W <- matrix(0, n, n, dimnames = list(ids, ids))
  N <- matrix(0, n, n, dimnames = list(ids, ids))

  o1 <- bt_data$object1
  o2 <- bt_data$object2
  r <- bt_data$result

  for (k in seq_along(r)) {
    i <- idx[[o1[k]]]
    j <- idx[[o2[k]]]
    if (is.na(i) || is.na(j) || i == j) next

    # n_ij counts both directions (unordered)
    N[i, j] <- N[i, j] + 1
    N[j, i] <- N[j, i] + 1

    if (r[k] == 1) {
      # object1 wins
      W[i, j] <- W[i, j] + 1
    } else {
      # object2 wins
      W[j, i] <- W[j, i] + 1
    }
  }

  # Adjacency (undirected) where at least one comparison exists
  adj <- (N > 0)
  diag(adj) <- FALSE

  degree <- rowSums(adj)
  n_edges <- sum(adj[upper.tri(adj)]) # undirected count

  # Connected components on the undirected graph
  .components_bfs <- function(adj_mat) {
    n0 <- nrow(adj_mat)
    comp <- rep(NA_integer_, n0)
    cid <- 0L
    for (i in seq_len(n0)) {
      if (!is.na(comp[i])) next
      cid <- cid + 1L
      queue <- i
      comp[i] <- cid
      while (length(queue) > 0) {
        v <- queue[[1]]
        queue <- queue[-1]
        nbrs <- which(adj_mat[v, ])
        for (u in nbrs) {
          if (is.na(comp[u])) {
            comp[u] <- cid
            queue <- c(queue, u)
          }
        }
      }
    }
    comp
  }

  component_id <- .components_bfs(adj)
  tab_comp <- table(component_id)
  n_components <- length(tab_comp)
  largest_component_frac <- if (n > 0) max(tab_comp) / n else NA_real_

  # Construct transition matrix P
  P <- matrix(0, n, n, dimnames = list(ids, ids))

  for (i in seq_len(n)) {
    nbrs <- which(adj[i, ])
    di <- length(nbrs)
    if (di == 0L) {
      P[i, i] <- 1
      next
    }

    for (j in nbrs) {
      nij <- N[i, j]
      # wins by i over j
      wij <- W[i, j]
      # Smoothed win probability for i beating j
      p_ij <- (wij + smoothing) / (nij + 2 * smoothing)
      # Transition i -> j proportional to prob j beats i
      P[i, j] <- (1 / di) * (1 - p_ij)
    }
    P[i, i] <- 1 - sum(P[i, -i])
    # Numerical guard
    if (P[i, i] < 0) {
      P[i, i] <- 0
      s <- sum(P[i, ])
      if (s > 0) P[i, ] <- P[i, ] / s else P[i, i] <- 1
    }
  }

  if (damping > 0) {
    U <- matrix(1 / n, n, n)
    P <- (1 - damping) * P + damping * U
  }

  # Power method for stationary distribution of a row-stochastic matrix:
  # pi^T = pi^T P, equivalently pi = t(P) %*% pi.
  pi <- rep(1 / n, n)
  err <- NA_real_
  it <- 0L

  for (it0 in seq_len(max_iter)) {
    pi_next <- as.numeric(crossprod(P, pi)) # t(P) %*% pi
    s <- sum(pi_next)
    if (!is.finite(s) || s <= 0) {
      stop("Rank Centrality failed: non-finite stationary mass.", call. = FALSE)
    }
    pi_next <- pi_next / s
    err <- max(abs(pi_next - pi))
    pi <- pi_next
    it <- it0
    if (err < tol) break
  }

  if (verbose) {
    message(sprintf("Rank Centrality: n=%d, it=%d, err=%.3g", n, it, err))
  }

  # Ensure strictly positive (numerical)
  pi <- pmax(pi, .Machine$double.xmin)
  pi <- pi / sum(pi)

  theta <- log(pi)
  theta <- theta - mean(theta)

  theta_tbl <- tibble::tibble(
    ID = ids,
    theta = as.numeric(theta),
    pi = as.numeric(pi)
  )

  # Spectral gap (1 - |lambda2|) when computable
  spectral_gap <- NA_real_
  lambda2 <- NA_real_
  if (n <= 200) {
    ev <- tryCatch(eigen(P, only.values = TRUE)$values, error = function(e) NULL)
    if (!is.null(ev)) {
      # Largest eigenvalue should be 1; take second largest by modulus
      ord <- order(Mod(ev), decreasing = TRUE)
      if (length(ord) >= 2) {
        lambda2 <- Mod(ev[[ord[[2]]]])
        spectral_gap <- 1 - lambda2
      }
    }
  }

  diagnostics <- list(
    n_nodes = n,
    n_edges = as.integer(n_edges),
    degree = as.numeric(degree),
    degree_min = as.numeric(min(degree)),
    degree_median = as.numeric(stats::median(degree)),
    degree_p90 = as.numeric(stats::quantile(degree, probs = 0.90, names = FALSE)),
    n_components = as.integer(n_components),
    largest_component_frac = as.numeric(largest_component_frac),
    component_id = stats::setNames(as.integer(component_id), ids),
    damping = damping,
    smoothing = smoothing,
    power_iter = as.integer(it),
    power_err = as.numeric(err),
    lambda2 = as.numeric(lambda2),
    spectral_gap = as.numeric(spectral_gap)
  )

  out <- list(
    engine = "rank_centrality",
    reliability = NA_real_,
    theta = theta_tbl,
    diagnostics = diagnostics
  )

  if (return_transition) out$P <- P
  out
}
