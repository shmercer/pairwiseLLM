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

.rubric_percentile_fit_evidence <- function(cj) {
  contract <- cj$fit_contract
  # A completed fit may be supplied with its item axis reordered.
  for (field in grep("^theta_", names(contract), value = TRUE)) {
    value <- contract[[field]]
    if (is.numeric(value) && is.null(dim(value)) && !is.null(names(value))) {
      contract[[field]] <- value[order(names(value))]
    }
  }
  list(model_variant = cj$model_variant, estimation_mode = cj$estimation_mode,
    orientation = cj$orientation, fit_contract = contract, fit_contract_hash = cj$fit_contract_hash,
    reference = cj$reference, provenance = cj$provenance[setdiff(names(cj$provenance), "collection")])
}

.rubric_percentile_prediction_items <- function(object, newdata) {
  if (is.null(newdata)) return(object$cj$items)
  cj <- .rubric_normalize_cj(newdata, object$trait, scale_status = object$cj$scale_status,
    include_draws = FALSE)
  source <- object$cj$items
  items <- cj$items
  # Configuration hashes alone do not identify a CJ metric. Require the same
  # item domain, exact accepted locations/uncertainty, and original fit evidence.
  if (!setequal(items$item_id, source$item_id) ||
    !identical(items[order(items$item_id), ], source[order(source$item_id), ]) ||
    !identical(.rubric_percentile_fit_evidence(cj), .rubric_percentile_fit_evidence(object$cj))) {
    rlang::abort(paste0("Percentile `newdata` must reuse the original completed CJ result with unchanged ",
      "items, accepted scores, and fit evidence. Independent cohorts and refits are not supported."))
  }
  items
}

.rubric_predict_percentile <- function(object, newdata) {
  items <- .rubric_percentile_prediction_items(object, newdata)
  category <- findInterval(items$theta, object$backend$cutpoints) + 1L
  tibble::tibble(item_id = items$item_id, theta = items$theta, category = category,
    rubric_score = object$levels[category],
    extrapolated = items$theta < object$calibration_range[[1L]] |
      items$theta > object$calibration_range[[2L]])
}
