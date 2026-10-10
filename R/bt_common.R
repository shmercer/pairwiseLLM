# Shared binary BT data, centered uncertainty and plug-in prediction contracts.
.bt_prepare_items <- function(ids, all_pairs = FALSE, complete = FALSE, binary = TRUE) {
  ids <- sort(as.character(ids), method = "radix")
  # Reference-coordinate coefficients are contrasts to the last sorted item.
  # Centering the full coordinate map gives item estimates AND their covariance
  # under the same deterministic sum-to-zero constraint.
  transform <- rbind(diag(length(ids) - 1L), 0)
  transform <- sweep(transform, 2L, colMeans(transform))
  rownames(transform) <- ids
  colnames(transform) <- paste0("contrast", seq_len(ncol(transform)))
  out <- list(ids = ids, transform = transform)
  if (all_pairs) {
    n <- length(ids)
    first <- rep(seq_len(n - 1L), n - seq_len(n - 1L))
    second <- sequence(n - seq_len(n - 1L), from = seq_len(n - 1L) + 1L)
    # merge(all.x = TRUE) historically sorts complete graphs by pasted keys,
    # but incomplete graphs by numeric columns. Retain BOTH solver row orders.
    if (complete) {
      order <- sort.list(paste(first, second, sep = "\r"))
      first <- first[order]
      second <- second[order]
    }
    design <- transform[first, , drop = FALSE] - transform[second, , drop = FALSE]
    out$alpha <- list(first = first, second = second, design = design,
      binary_design = if (binary) design[rep(seq_along(first), 2L), , drop = FALSE] else NULL,
      pair_index = .bt_pair_index(first, second, n), complete = complete)
  }
  out
}

# Lexicographic index in all unordered pairs, independent of observed outcomes.
.bt_pair_index <- function(first, second, n) {
  (first - 1) * (2 * n - first) / 2 + second - first
}

# An ordinary list owned by one bootstrap invocation, never a global cache.
# Only graph structure is retained; every call below recomputes outcome counts.
.bt_prepare_schedule <- function(dat, items = NULL) {
  object1 <- as.character(dat[[1L]])
  object2 <- as.character(dat[[2L]])
  if (is.null(items)) items <- .bt_prepare_items(unique(c(object1, object2)))
  first <- match(object1, items$ids)
  second <- match(object2, items$ids)
  lo <- pmin(first, second)
  hi <- pmax(first, second)
  key <- .bt_pair_index(lo, hi, length(items$ids))
  keys <- sort(unique(key), method = "radix")
  index <- match(key, keys)
  observed <- match(keys, key)
  counts <- data.frame(first = lo[observed], second = hi[observed])
  complete <- nrow(counts) == length(items$ids) * (length(items$ids) - 1) / 2
  if (!is.null(items$alpha) && !identical(items$alpha$complete, complete)) {
    items <- .bt_prepare_items(items$ids, all_pairs = TRUE, complete = complete,
      binary = !is.null(items$alpha$binary_design))
  }
  # Preserve aggregate()'s historical row names as well as its pair ordering.
  rownames(counts) <- order(order(counts$second, counts$first))
  list(ids = items$ids, transform = items$transform, alpha = items$alpha,
    counts = counts, design = items$transform[counts$first, , drop = FALSE] -
      items$transform[counts$second, , drop = FALSE],
    object1 = object1, object2 = object2, forward = first < second,
    index = index, totals = tabulate(index, nrow(counts)), all_pair_index = keys)
}

.bt_binary_design <- function(dat, prepared = NULL) {
  if (is.null(prepared)) prepared <- .bt_prepare_schedule(dat)
  if (!identical(as.character(dat[[1L]]), prepared$object1) ||
      !identical(as.character(dat[[2L]]), prepared$object2)) {
    .bt_abort("Prepared BT schedule does not match the supplied comparison rows.")
  }
  y <- dat[[3L]]
  if (!.bt_real_vector(y) || length(y) != length(prepared$index) ||
      anyNA(y) || any(!y %in% c(0, 0.5, 1))) {
    .bt_abort("Comparison results must be finite numeric 0, 0.5 (tie), or 1.")
  }
  wins <- ifelse(prepared$forward, y, 1 - y)
  counts <- prepared$counts
  counts$wins <- as.double(tabulate(prepared$index[wins == 1], nrow(counts)))
  if (any(wins == 0.5)) {
    counts$wins <- counts$wins + 0.5 * tabulate(prepared$index[wins == 0.5], nrow(counts))
  }
  counts$losses <- prepared$totals - counts$wins
  list(ids = prepared$ids, counts = counts, transform = prepared$transform,
    design = prepared$design, alpha = prepared$alpha, all_pair_index = prepared$all_pair_index)
}

.bt_item_covariance <- function(covariance, transform, ids, method) {
  size <- ncol(transform)
  if (!is.matrix(covariance) || !is.numeric(covariance) || is.complex(covariance) ||
      !identical(dim(covariance), c(size, size)) || any(!is.finite(covariance)) ||
      !isSymmetric(covariance, tol = 1e-10)) {
    .bt_abort(paste(method, "covariance must be finite and symmetric in identifiable coordinates."))
  }
  # Test positive definiteness before mapping to the necessarily singular,
  # sum-to-zero item covariance. Do not repair an unidentified covariance.
  tryCatch(chol(covariance), error = function(e) .bt_abort(paste(method, "covariance is not positive definite.")))
  covariance <- transform %*% covariance %*% t(transform)
  dimnames(covariance) <- list(ids, ids)
  if (any(!is.finite(covariance)) || any(diag(covariance) <= 0)) {
    .bt_abort(paste(method, "item covariance must be finite with positive variances."))
  }
  covariance
}

.bt_centered_ssr <- function(theta, se, method) {
  .bt_validate_estimates(theta, se)
  observed <- stats::var(theta)
  error <- mean(se^2)
  if (!is.finite(observed) || !is.finite(error)) {
    .bt_abort(paste(method, "SSR requires finite score variance and mean squared SE."))
  }
  if (observed == 0) {
    return(list(observed_variance = observed, mean_squared_se = error,
                true_score_variance = -error, ssr = NA_real_, n_items = length(theta),
                n_finite = length(theta), valid = FALSE, status = "zero_score_variance"))
  }
  scale_separation_reliability(theta, se)
}

.bt_predict_pairs <- function(object, newdata, dots, method) {
  if (length(dots)) .bt_abort(paste(method, "prediction does not accept additional arguments."))
  if (is.null(newdata)) newdata <- object$comparisons
  if (!is.data.frame(newdata) || !all(c("object1", "object2") %in% names(newdata))) {
    .bt_abort("`newdata` must be a data frame containing `object1` and `object2`.")
  }
  for (name in c("object1", "object2")) {
    ids <- newdata[[name]]
    if (!is.atomic(ids) || anyNA(ids) || any(!nzchar(as.character(ids))) ||
        any(!as.character(ids) %in% object$theta$ID)) {
      .bt_abort(paste("Prediction IDs must be nonmissing item labels present in the", method, "fit."))
    }
  }
  first <- match(as.character(newdata$object1), object$theta$ID)
  second <- match(as.character(newdata$object2), object$theta$ID)
  stats::plogis(object$theta$theta[first] - object$theta$theta[second])
}
