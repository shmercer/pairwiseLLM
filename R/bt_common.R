# Shared binary BT data, centered uncertainty and plug-in prediction contracts.
.bt_binary_design <- function(dat) {
  ids <- sort(unique(c(as.character(dat[[1L]]), as.character(dat[[2L]]))), method = "radix")
  first <- match(as.character(dat[[1L]]), ids)
  second <- match(as.character(dat[[2L]]), ids)
  pairs <- data.frame(first = pmin(first, second), second = pmax(first, second),
                      wins = ifelse(first < second, dat[[3L]], 1 - dat[[3L]]))
  pairs$losses <- 1 - pairs$wins
  counts <- stats::aggregate(cbind(wins, losses) ~ first + second, data = pairs, FUN = sum)
  counts <- counts[order(counts$first, counts$second), , drop = FALSE]

  # Reference-coordinate coefficients are contrasts to the last sorted item.
  # Centering the full coordinate map gives item estimates AND their covariance
  # under the same deterministic sum-to-zero constraint.
  transform <- rbind(diag(length(ids) - 1L), 0)
  transform <- sweep(transform, 2L, colMeans(transform))
  rownames(transform) <- ids
  colnames(transform) <- paste0("contrast", seq_len(ncol(transform)))
  design <- transform[counts$first, , drop = FALSE] - transform[counts$second, , drop = FALSE]
  list(ids = ids, counts = counts, transform = transform, design = design)
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
