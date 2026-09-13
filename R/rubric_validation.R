# Internal validation of prespecified ordinal mappings conditional on completed CJ.

.rubric_cv_assignments <- function(data, folds, fold_id, seed) {
  n <- nrow(data)
  folds <- .rubric_positive_integer(folds, "folds", 2L)
  if (folds > n) rlang::abort("`folds` cannot exceed the number of observed rubric labels.")
  if (!is.null(fold_id)) {
    if (!is.data.frame(fold_id) || !all(c("item_id", "fold") %in% names(fold_id))) {
      rlang::abort("`fold_id` must be a data frame with `item_id` and `fold`.")
    }
    ids <- .rubric_ids(fold_id$item_id, "fold item_id")
    if (!setequal(ids, data$item_id)) rlang::abort("Fold IDs must match all and only labeled item IDs.")
    fold <- fold_id$fold[match(data$item_id, ids)]
    if (!is.numeric(fold) || !is.null(dim(fold)) || any(!is.finite(fold)) ||
      any(fold < 1 | fold > folds | fold != floor(fold)) || !setequal(fold, seq_len(folds))) {
      rlang::abort("Fold values must be integers 1 through `folds`, with every fold represented.")
    }
    return(as.integer(fold))
  }
  seed <- .rubric_positive_integer(seed, "seed", 0L)
  if (!requireNamespace("withr", quietly = TRUE)) {
    rlang::abort("Generated rubric CV folds require 'withr'; install.packages(\"withr\").",
      class = "pairwiseLLM_rubric_dependency_missing")
  }
  .pairwiseLLM_with_seed(seed, function() {
    fold <- integer(n)
    offset <- 0L
    for (category in sort(unique(data$category))) {
      indices <- which(data$category == category)
      indices <- indices[sample.int(length(indices))]
      fold[indices] <- as.integer((seq_along(indices) - 1L + offset) %% folds + 1L)
      offset <- (offset + length(indices)) %% folds
    }
    fold
  })
}

.rubric_refit_labels <- function(cj, rubric, method, calibration_design, levels, controls) {
  aligned <- .rubric_align_labels(rubric, cj, levels = levels)
  object <- .rubric_new_calibration(cj, method, calibration_design, levels, aligned)
  if (method == "ordinal_linear") .rubric_fit_ordinal_linear(object) else
    .rubric_fit_ordinal_monotone(object, controls)
}

# Returns assignments, per-fold status/metrics/transformation, held-out predictions
# and pooled (not mean-fold) metrics. Any invalid fold invalidates aggregate metrics.
.rubric_cross_validate <- function(object, folds = 5L, fold_id = NULL, seed = 1L) {
  .rubric_validate_calibration(object)
  if (object$status != "fitted" || object$method == "percentile") {
    rlang::abort("Rubric-label CV requires a fitted, prespecified ordinal calibration.")
  }
  data <- object$calibration_data[!is.na(object$calibration_data$category), ]
  data <- data[order(data$item_id), ]
  assignment <- .rubric_cv_assignments(data, folds, fold_id, seed)
  controls <- if (object$method == "ordinal_monotone") {
    list(k = object$backend$basis$k_requested, sp = object$backend$smoothing$sp_requested)
  } else {
    NULL
  }
  records <- predictions <- vector("list", folds)
  for (fold in seq_len(folds)) {
    held <- assignment == fold
    training <- data[!held, c("item_id", "rubric_score")]
    result <- .rubric_capture(function() {
      fit <- .rubric_refit_labels(object$cj, training, object$method,
        object$calibration_design, object$levels, controls)
      if (!isTRUE(fit$diagnostics$ordinal$converged)) {
        rlang::abort("Fold calibration did not converge or is unidentified; CV predictions are invalid.")
      }
      prediction <- stats::predict(fit, hard_score = "median")
      evaluation <- .rubric_evaluate_table(fit, data[held, c("item_id", "rubric_score")],
        prediction, "median", 10L)
      list(evaluation = evaluation, transformation = fit$transformation, calibration_range = fit$calibration_range)
    })
    valid <- !is.null(result$value)
    metrics <- if (valid) result$value$evaluation$metrics else tibble::tibble(
      n = sum(held), rps = NA_real_, log_loss = NA_real_, exact_accuracy = NA_real_,
      within_one_accuracy = NA_real_, mae = NA_real_, quadratic_weighted_kappa = NA_real_)
    records[[fold]] <- tibble::tibble(fold = fold, status = if (valid) "valid" else "invalid",
      reason = result$reason %||% NA_character_, warnings = list(result$warnings),
      n_train = nrow(training), transformation = list(result$value$transformation),
      calibration_range = list(result$value$calibration_range)) |> dplyr::bind_cols(metrics)
    if (valid) {
      predictions[[fold]] <- result$value$evaluation$per_item
      predictions[[fold]]$fold <- fold
    }
  }
  records <- dplyr::bind_rows(records)
  predictions <- dplyr::bind_rows(predictions)
  complete <- all(records$status == "valid")
  aggregate <- NULL
  calibration <- NULL
  if (complete) {
    probabilities <- .rubric_probability_copy(do.call(rbind, predictions$probabilities), object$levels)
    aggregate <- .rubric_metric_values(predictions$observed_category, predictions$category,
      object$K, probabilities)
    calibration <- .rubric_calibration_summary(probabilities, predictions$observed_category,
      predictions$theta, object$levels, 10L)
  }
  list(status = if (complete) "complete" else "invalid", assignments = tibble::tibble(
    item_id = data$item_id, fold = assignment), folds = records, per_item = predictions,
    metrics = aggregate$metrics, calibration = calibration,
    metadata = list(method = object$method, calibration_design = object$calibration_design,
      validation_estimand = if (object$calibration_design == "linked_anchors") "reference_calibration" else "same_set",
      levels = object$levels, trait = object$trait, hard_score = "median", conditional_on_cj = TRUE,
      primary_metric = "rps", rps_normalized = TRUE, log_loss_floor = .Machine$double.xmin,
      n_log_loss_floored = if (complete) sum(aggregate$log_loss_floored) else NA_integer_,
      kappa_reason = aggregate$kappa_reason, seed = if (is.null(fold_id)) seed else NULL,
      aggregate_reason = if (complete) NULL else "At least one fold is invalid; no overall CV estimate is available.",
      selection_performed = FALSE, controls = controls))
}
