# Common probability validation and evaluation on internal ordered indices.

.rubric_check_probabilities <- function(probabilities, levels, n = nrow(probabilities)) {
  if (!is.matrix(probabilities) || !is.numeric(probabilities) ||
    !identical(dim(probabilities), as.integer(c(n, length(levels)))) ||
    !identical(colnames(probabilities), as.character(levels)) ||
    any(!is.finite(probabilities)) || any(probabilities < -1e-12 | probabilities > 1 + 1e-12) ||
    any(abs(rowSums(probabilities) - 1) > 1e-12)) {
    rlang::abort("Invalid category probabilities: check dimensions, level order, bounds, and row sums.")
  }
  invisible(probabilities)
}

.rubric_probability_copy <- function(probabilities, levels) {
  .rubric_check_probabilities(probabilities, levels)
  # Correct tolerated roundoff only in this calculation copy, never the prediction.
  probabilities[probabilities < 0] <- 0
  probabilities[probabilities > 1] <- 1
  probabilities
}

.rubric_cumulative <- function(probabilities) {
  cumulative <- probabilities[, -ncol(probabilities), drop = FALSE]
  for (k in seq_len(ncol(cumulative))[-1L]) {
    cumulative[, k] <- cumulative[, k - 1L] + cumulative[, k]
  }
  cumulative[cumulative < 0] <- 0
  cumulative[cumulative > 1] <- 1
  cumulative
}

.rubric_metric_values <- function(observed, predicted, K, probabilities = NULL) {
  n <- length(observed)
  distance <- abs(observed - predicted)
  table <- matrix(tabulate(observed + K * (predicted - 1L), nbins = K * K), K, K)
  weights <- outer(seq_len(K), seq_len(K), function(i, j) (i - j)^2 / (K - 1)^2)
  expected <- outer(rowSums(table), colSums(table)) / n
  denominator <- sum(weights * expected)
  kappa <- if (denominator == 0) NA_real_ else 1 - sum(weights * table) / denominator
  rps <- log_loss <- rep(NA_real_, n)
  floored <- rep(FALSE, n)
  if (!is.null(probabilities)) {
    cumulative <- .rubric_cumulative(probabilities)
    indicators <- outer(observed, seq_len(K - 1L), `<=`)
    rps <- rowSums((cumulative - indicators)^2) / (K - 1L)
    realized <- probabilities[cbind(seq_len(n), observed)]
    floored <- realized < .Machine$double.xmin
    log_loss <- -log(pmax(realized, .Machine$double.xmin))
  }
  list(metrics = tibble::tibble(n = n, rps = mean(rps), log_loss = mean(log_loss),
    exact_accuracy = mean(distance == 0L), within_one_accuracy = mean(distance <= 1L),
    mae = mean(distance), quadratic_weighted_kappa = kappa),
    rps = rps, log_loss = log_loss, log_loss_floored = floored,
    kappa_reason = if (denominator == 0) "Expected weighted disagreement is zero." else NULL)
}

.rubric_positive_integer <- function(x, name, minimum = 1L) {
  if (!is.numeric(x) || !is.null(dim(x)) || length(x) != 1L || !is.finite(x) ||
    x < minimum || x > .Machine$integer.max || x != as.integer(x)) {
    rlang::abort(paste0("`", name, "` must be a single integer >= ", minimum, "."))
  }
  as.integer(x)
}

.rubric_calibration_summary <- function(probabilities, observed, theta, levels, bins) {
  cumulative <- .rubric_cumulative(probabilities)
  # Scale first to avoid overflowing the span for large finite theta values.
  scaled <- theta / max(1, abs(theta))
  span <- diff(range(scaled))
  theta_group <- if (span == 0) rep(1L, length(theta)) else
    pmin(bins, 1L + floor(bins * (scaled - min(scaled)) / span))
  rows <- list()
  for (k in seq_len(ncol(cumulative))) {
    for (group_by in c("probability", "theta")) {
      group <- if (group_by == "theta") theta_group else pmin(bins, 1L + floor(bins * cumulative[, k]))
      for (bin in sort(unique(group))) {
        keep <- group == bin
        predicted <- mean(cumulative[keep, k])
        actual <- mean(observed[keep] <= k)
        rows[[length(rows) + 1L]] <- tibble::tibble(group_by = group_by, boundary = k,
          rubric_level = levels[k], bin = as.integer(bin), n = sum(keep),
          theta_min = min(theta[keep]), theta_max = max(theta[keep]),
          probability_min = min(cumulative[keep, k]), probability_max = max(cumulative[keep, k]),
          predicted = predicted, observed = actual, residual = actual - predicted)
      }
    }
  }
  dplyr::bind_rows(rows)
}

.rubric_evaluation_labels <- function(rubric, object, ids) {
  if (!is.data.frame(rubric) || !all(c("item_id", "rubric_score") %in% names(rubric))) {
    rlang::abort("`rubric` must contain `item_id` and `rubric_score`.")
  }
  rubric$item_id <- .rubric_ids(rubric$item_id, "rubric item_id")
  if (!all(rubric$item_id %in% ids)) rlang::abort("Every evaluation rubric item ID must have a prediction.")
  .rubric_trait(object$trait, rubric[["trait"]])
  .rubric_levels(rubric$rubric_score, levels = object$levels)
  category <- match(as.character(rubric$rubric_score), as.character(object$levels))
  if (any(!is.na(rubric$rubric_score) & is.na(category))) {
    rlang::abort("Evaluation rubric scores contain labels outside the fitted levels.")
  }
  if (all(is.na(category))) rlang::abort("Evaluation requires at least one observed rubric label.")
  list(category = category[match(ids, rubric$item_id)], missing = sum(is.na(category)))
}

.rubric_evaluate_table <- function(object, rubric, predictions, hard_score, bins) {
  ids <- .rubric_ids(predictions$item_id, "prediction item_id")
  labels <- .rubric_evaluation_labels(rubric, object, ids)
  keep <- !is.na(labels$category)
  probabilities <- NULL
  if (object$method != "percentile") {
    vectors <- predictions$probabilities
    if (!is.list(vectors) || length(vectors) != length(ids) ||
      any(!vapply(vectors, function(x) {
        is.numeric(x) && is.null(dim(x)) && length(x) == object$K &&
          identical(names(x), as.character(object$levels))
      }, logical(1L)))) {
      rlang::abort("Prediction probability vectors must retain the fitted level order.")
    }
    probabilities <- .rubric_probability_copy(do.call(rbind, vectors), object$levels)
    decisions <- .rubric_ordinal_decisions(probabilities)
    if (!identical(predictions$median_category, decisions$median) ||
      !identical(predictions$modal_category, decisions$mode) ||
      !identical(predictions$category, decisions[[hard_score]]) ||
      !isTRUE(all.equal(predictions$expected_level, decisions$expected_level, tolerance = 1e-12))) {
      rlang::abort("Ordinal decisions must be derived from the stored category probabilities.")
    }
    probabilities <- probabilities[keep, , drop = FALSE]
  }
  if (!identical(as.character(predictions$rubric_score), as.character(object$levels[predictions$category]))) {
    rlang::abort("Predicted rubric labels must agree with their ordered category indices.")
  }
  per_item <- predictions[keep, ]
  per_item$observed_category <- labels$category[keep]
  per_item$observed_rubric_score <- object$levels[per_item$observed_category]
  per_item$training_label_overlap <- per_item$item_id %in%
    object$calibration_data$item_id[!is.na(object$calibration_data$category)]
  if (!"theta_sd" %in% names(per_item)) {
    per_item$theta_sd <- object$cj$items$theta_sd[match(per_item$item_id, object$cj$items$item_id)]
  }
  values <- .rubric_metric_values(per_item$observed_category, per_item$category, object$K, probabilities)
  per_item$rps <- values$rps
  per_item$log_loss <- values$log_loss
  per_item$log_loss_floored <- values$log_loss_floored
  list(metrics = values$metrics, per_item = per_item,
    calibration = if (is.null(probabilities)) NULL else .rubric_calibration_summary(
      probabilities, per_item$observed_category, per_item$theta, object$levels, bins),
    diagnostics = list(status = "not_requested"),
    metadata = list(method = object$method, calibration_design = object$calibration_design,
      trait = object$trait, levels = object$levels, hard_score = hard_score,
      conditional_on_cj = TRUE, primary_metric = if (is.null(probabilities)) NA_character_ else "rps",
      probabilities_available = !is.null(probabilities),
      probability_reason = if (is.null(probabilities)) "Percentile scoring provides no probabilities." else NULL,
      n_predictions = length(ids), n_unscored_predictions = sum(!keep), n_missing_labels = labels$missing,
      n_training_label_overlap = sum(per_item$training_label_overlap),
      n_extrapolated = sum(per_item$extrapolated), bins = bins, rps_normalized = TRUE,
      log_loss_floor = .Machine$double.xmin, n_log_loss_floored = sum(values$log_loss_floored),
      kappa_reason = values$kappa_reason, linking = attr(predictions, "linking")))
}

#' Evaluate rubric predictions on observed ordered labels
#'
#' Compute rubric prediction metrics conditional on completed CJ point scores.
#' Normalized ranked probability score (RPS) is the primary ordinal probability
#' metric. Additional assumption diagnostics run only when requested here.
#'
#' @param object A fitted `pairwiseLLM_rubric_calibration` object.
#' @param rubric Data frame with unique `item_id` and `rubric_score`, optionally
#'   `trait`. Labels align by ID and use the fitted level order. Missing labels
#'   are excluded and counted. Evaluation need not represent every category.
#' @param newdata As in [predict.pairwiseLLM_rubric_calibration()]: `NULL` predicts
#'   original items; linked targets require accepted Phase B results.
#' @param hard_score Hard category rule, default `"median"`, optionally `"mode"`.
#' @param bins Positive integer number of equal-width bins (default 10) for
#'   cumulative calibration summaries. Empty bins are omitted; ties stay together.
#' @param diagnostics Logical; request additional training-model diagnostics.
#'   Default `FALSE` avoids diagnostic refits. Evaluation labels never refit the
#'   production model or enter its training-model diagnostics.
#'
#' @details
#' For K levels, RPS for item i is
#' `sum((F[i, k] - I(Y[i] <= k))^2) / (K - 1)` over boundaries `k = 1, ..., K-1`.
#' This is normalized RPS, not the unnormalized sum; lower is better and zero is
#' perfect. Log loss is `-log(p[i, Y[i]])`. Observed-category probabilities are
#' floored at `.Machine$double.xmin` before taking logs, solely during evaluation
#' to handle zero/underflow probabilities. Stored
#' predictions are unchanged. Both metrics are averaged over observed labels.
#'
#' Hard metrics are exact accuracy, within-one accuracy, mean absolute category
#' error, and quadratic weighted kappa. Distances use internal indices `1:K`,
#' not numeric gaps between user labels. Kappa is one minus observed divided by
#' expected weighted disagreement, with weights `(i-j)^2/(K-1)^2` and independent
#' empirical marginals. Zero expected disagreement returns `NA` with a reason.
#' Percentile scoring supplies only hard metrics; no probabilities are invented.
#'
#' Cumulative summaries compare `P(Y <= k)` with observed cumulative frequencies
#' within equal-width probability bins on `[0, 1]` and theta bins over the evaluated
#' range. Internal edges enter the higher bin; the upper endpoint stays in the
#' last bin. A constant theta range occupies one bin. These descriptive tables
#' require no plotting dependency and do not constitute formal goodness-of-fit
#' tests. Training-label overlap is reported; apparent performance is not
#' out-of-sample validation.
#'
#' Requested diagnostics expose stored numerical/uncertainty information and
#' training-only cumulative summaries. For linear fits, an explicit
#' threshold-varying `ordinal::clm` alternative supplies a proportional-odds
#' likelihood-ratio diagnostic. Convergence/identification failures produce an
#' unavailable status. A p-value below .05 triggers an exploratory review warning,
#' not automatic selection. This test is distinct from functional-form assessment.
#' Monotone common-effect checks are descriptive boundary-specific binomial fits
#' against the estimated latent effect (expected cumulative slope -1), with no
#' omnibus p-value or automatic adequacy threshold. Diagnostic failures preserve
#' the fitted calibration. Optional backends are required only when used.
#'
#' Internal rubric-label cross-validation holds out labels while keeping the
#' completed CJ evidence and theta fixed. Each fold re-estimates calibration
#' scaling and parameters from training labels alone. Linked-reference CV assesses
#' reference calibration; externally labeled Phase B targets assess transport.
#' Invalid training folds are reported without collapsing categories, and prevent
#' an overall CV estimate. If resamples are used to choose methods or tuning,
#' unbiased post-selection assessment requires an outer validation layer.
#' No automatic method selection is supplied. Core fits condition on point theta;
#' retained CJ uncertainty is not propagated through a posterior/bootstrap engine.
#'
#' @return A list with `metrics` (one-row tibble), `per_item` (predictions, observed
#'   labels, uncertainty, overlap flags and individual losses), `calibration`
#'   (cumulative summaries, or `NULL` for percentile), `diagnostics`, and `metadata`.
#'   Metrics are `n`, `rps`, `log_loss`, `exact_accuracy`, `within_one_accuracy`,
#'   `mae`, and `quadratic_weighted_kappa`. Unavailable probability metrics are NA.
#'   Summary columns are `group_by`, `boundary`, `rubric_level`, `bin`, `n`,
#'   `theta_min`, `theta_max`, `probability_min`, `probability_max`, `predicted`,
#'   `observed`, and `residual` (observed minus predicted).
#'   Metadata records exclusions, training overlap, extrapolation, metric
#'   conventions, unavailable reasons, and original linking metadata when present.
#'   Requested diagnostics contain `status`, `stored`, `functional_form`, and
#'   `common_effect`; their statuses distinguish formal, descriptive and unavailable
#'   results. No input object or upstream CJ state is modified.
#' @seealso [fit_rubric_calibration()], [predict.pairwiseLLM_rubric_calibration()]
#' @examples
#' \dontrun{
#' # Rubric labels align with items in an already completed CJ result.
#' if (requireNamespace("ordinal", quietly = TRUE)) {
#'   fit <- fit_rubric_calibration(completed_cj, training_labels,
#'     trait = "organization", levels = c("developing", "proficient", "advanced"))
#'   assessment <- evaluate_rubric_predictions(fit, evaluation_labels, diagnostics = TRUE)
#'   assessment$metrics
#'   assessment$calibration
#'   assessment$metadata$n_training_label_overlap
#'   assessment$diagnostics$common_effect
#' }
#' }
#' @export
evaluate_rubric_predictions <- function(object, rubric, newdata = NULL,
                                       hard_score = c("median", "mode"), bins = 10L, diagnostics = FALSE) {
  .rubric_validate_calibration(object)
  if (object$status != "fitted") rlang::abort("Evaluation requires a fitted rubric calibration.")
  hard_score <- match.arg(hard_score)
  bins <- .rubric_positive_integer(bins, "bins")
  if (!is.logical(diagnostics) || length(diagnostics) != 1L || is.na(diagnostics)) {
    rlang::abort("`diagnostics` must be TRUE or FALSE.")
  }
  predictions <- stats::predict(object, newdata = newdata, hard_score = hard_score)
  out <- .rubric_evaluate_table(object, rubric, predictions, hard_score, bins)
  out$metadata$evaluation_design <- if (object$calibration_design == "linked_anchors") {
    if (is.null(newdata)) "reference_calibration" else "linked_target_transport"
  } else {
    "same_set"
  }
  if (diagnostics) out$diagnostics <- .rubric_model_diagnostics(object, bins)
  out
}
