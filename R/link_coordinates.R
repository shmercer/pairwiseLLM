# Deterministic, item-bound sum-to-zero coordinates used by every new linker.

.link_basis <- function(item_ids) {
  ids <- .link_ids(item_ids, "basis item IDs")
  ids <- ids[order(ids, method = "radix")]
  n <- length(ids)
  H <- matrix(0, n, n - 1L)
  if (n > 1L) {
    for (j in seq_len(n - 1L)) {
      H[seq_len(j), j] <- -1 / sqrt(j * (j + 1))
      H[j + 1L, j] <- j / sqrt(j * (j + 1))
    }
  }
  dimnames(H) <- list(ids, if (n > 1L) paste0("u", seq_len(n - 1L)) else character())
  out <- list(version = 1L, method = "orthonormal_helmert", item_ids = ids, H = H)
  .link_validate_basis(out)
  out
}

.link_validate_basis <- function(basis) {
  .link_check(is.list(basis) && identical(basis$version, 1L) &&
    identical(basis$method, "orthonormal_helmert"), "Invalid basis metadata.")
  ids <- .link_ids(basis$item_ids, "basis item IDs")
  H <- basis$H
  .link_check(is.matrix(H) && is.numeric(H) && all(is.finite(H)) &&
    identical(dim(H), c(length(ids), length(ids) - 1L)) &&
    identical(rownames(H), ids), "Basis dimensions or item ordering do not match.")
  .link_check(all(abs(colSums(H)) < 1e-10) &&
    all(abs(crossprod(H) - diag(ncol(H))) < 1e-10), "Basis must be orthonormal and sum to zero.")
  invisible(TRUE)
}

.link_align_numeric <- function(x, ids, label) {
  matrix_input <- is.matrix(x)
  .link_check(is.numeric(x) && (matrix_input || is.null(dim(x))) && all(is.finite(x)),
    paste0(label, " must be finite numeric coordinates."))
  nms <- if (matrix_input) colnames(x) else names(x)
  .link_check(length(nms) == length(ids) && !anyDuplicated(nms) && setequal(nms, ids) &&
    (if (matrix_input) ncol(x) else length(x)) == length(ids),
    paste0(label, " names must match coordinate identities."))
  if (matrix_input) x[, match(ids, nms), drop = FALSE] else x[match(ids, nms)]
}

.link_to_reduced <- function(x, basis) {
  .link_validate_basis(basis)
  x <- .link_align_numeric(x, basis$item_ids, "Item")
  sums <- if (is.matrix(x)) rowSums(x) else sum(x)
  scale <- max(1, abs(x))
  .link_check(all(abs(sums) <= 1e-10 * scale * length(basis$item_ids)),
    "Item coordinates must already be centered; no hidden centering is performed.")
  if (is.matrix(x)) return(x %*% basis$H)
  stats::setNames(as.double(crossprod(basis$H, x)), colnames(basis$H))
}

.link_from_reduced <- function(x, basis) {
  .link_validate_basis(basis)
  x <- .link_align_numeric(x, colnames(basis$H), "Reduced")
  if (is.matrix(x)) return(x %*% t(basis$H))
  stats::setNames(as.double(basis$H %*% x), basis$item_ids)
}

.link_covariance <- function(x, ids) {
  .link_check(is.matrix(x) && is.numeric(x) &&
    identical(dim(x), c(length(ids), length(ids))) && all(is.finite(x)),
    "Covariance must be a finite square matrix matching its coordinates.")
  .link_check(length(rownames(x)) == length(ids) && length(colnames(x)) == length(ids) &&
    !anyDuplicated(rownames(x)) && !anyDuplicated(colnames(x)) &&
    setequal(rownames(x), ids) && setequal(colnames(x), ids), "Covariance names must match coordinates.")
  x <- x[match(ids, rownames(x)), match(ids, colnames(x)), drop = FALSE]
  tol <- 1e-10 * max(1, abs(x))
  .link_check(all(abs(x - t(x)) <= tol), "Covariance must be symmetric.")
  .link_check(length(ids) == 0L || min(eigen(x, symmetric = TRUE, only.values = TRUE)$values) >= -tol,
    "Covariance must be positive semidefinite.")
  x
}

.link_cov_to_reduced <- function(x, basis) {
  .link_validate_basis(basis)
  x <- .link_covariance(x, basis$item_ids)
  .link_check(all(abs(rowSums(x)) <= 1e-10 * max(1, abs(x)) * nrow(x)),
    "Item covariance must be for centered shapes.")
  crossprod(basis$H, x %*% basis$H)
}

.link_cov_to_items <- function(x, basis) {
  .link_validate_basis(basis)
  x <- .link_covariance(x, colnames(basis$H))
  basis$H %*% x %*% t(basis$H)
}

.link_item_transform <- function(basis, estimator) {
  .link_validate_basis(basis$hub)
  .link_validate_basis(basis$spoke)
  nh <- nrow(basis$hub$H)
  ns <- nrow(basis$spoke$H)
  fixed <- identical(estimator, "fixed_shape_offset")
  coordinates <- "delta"
  if (!fixed) coordinates <- c("delta",
    if (nh > 1L) paste0("hub_", colnames(basis$hub$H)),
    if (ns > 1L) paste0("spoke_", colnames(basis$spoke$H)))
  out <- matrix(0, nh + ns, length(coordinates), dimnames = list(NULL, coordinates))
  out[nh + seq_len(ns), 1L] <- 1
  if (!fixed) {
    if (nh > 1L) out[seq_len(nh), 1L + seq_len(nh - 1L)] <- basis$hub$H
    if (ns > 1L) out[nh + seq_len(ns), nh + seq_len(ns - 1L)] <- basis$spoke$H
  }
  out
}
