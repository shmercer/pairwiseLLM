# Deterministic distribution matching on an accepted, unchanged CJ metric.

.rubric_fit_percentile <- function(object) {
  theta <- object$cj$items$theta
  requested <- stats::setNames(object$target_distribution %||% rep(1 / object$K, object$K),
    as.character(object$levels))
  effective <- requested / sum(requested)
  # Accepted inputs may sum to one only within tolerance. Clamp roundoff after normalization.
  cumulative_probs <- pmin(1, unname(cumsum(effective)[seq_len(object$K - 1L)]))
  cutpoints <- stats::quantile(theta, probs = cumulative_probs, type = 8, names = FALSE)
  category <- findInterval(theta, cutpoints) + 1L
  counts <- stats::setNames(tabulate(category, nbins = object$K), as.character(object$levels))
  object$target_distribution <- requested
  object$transformation <- list(center = 0, scale = 1)
  object$backend <- list(
    quantile_type = 8L, cutpoints = cutpoints, cumulative_probs = cumulative_probs,
    requested_proportions = requested, effective_proportions = effective,
    achieved_proportions = counts / length(theta), achieved_counts = counts,
    cutpoint_tie_counts = vapply(cutpoints, function(cutpoint) sum(theta == cutpoint), integer(1))
  )
  object$diagnostics$category_probabilities_available <- FALSE
  object$status <- "fitted"
  .rubric_validate_calibration(object)
  object
}

.rubric_validate_percentile <- function(object) {
  backend <- object$backend
  cutpoints <- if (is.list(backend)) backend$cutpoints else NULL
  if (!is.list(backend) || !identical(backend$quantile_type, 8L) ||
    !is.numeric(cutpoints) || !is.null(dim(cutpoints)) || length(cutpoints) != object$K - 1L ||
    any(!is.finite(cutpoints)) || is.unsorted(cutpoints) ||
    !identical(object$transformation, list(center = 0, scale = 1))) {
    rlang::abort("Invalid fitted percentile cutpoints or transformation.")
  }
  invisible(object)
}

.rubric_predict_percentile <- function(object, newdata) {
  items <- .rubric_same_set_prediction_items(object, newdata)
  category <- findInterval(items$theta, object$backend$cutpoints) + 1L
  tibble::tibble(item_id = items$item_id, theta = items$theta, category = category,
    rubric_score = object$levels[category],
    extrapolated = items$theta < object$calibration_range[[1L]] |
      items$theta > object$calibration_range[[2L]])
}
