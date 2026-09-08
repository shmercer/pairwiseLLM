#' Train a task-specific warm-start model with nested validation
#'
#' A warm-start model predicts relative writing quality before pairwise comparisons.
#' One fitting call represents one assessment; do not pool unlinked BT/BTL scales.
#'
#' @param ids Unique item IDs, aligned positionally with `theta` and `texts`.
#' @param theta Finite numeric BT/BTL scores from one assessment, one per ID.
#' @param task_id One nonblank assessment label, stored as provenance. It does not
#'   group rows, affect fitting, or identify a registered model.
#' @param texts Optional character vector of texts. Supply exactly one of `texts`
#'   and `features`. Extraction runs once before resampling.
#' @param features Optional precomputed feature table accepted by
#'   [predict.pairwiseLLM_warm_model()], aligned to `ids` by its `item_id` column.
#' @param schema Frozen feature schema identifier.
#' @param python Existing Python interpreter path for text extraction only.
#' @param seed Integer from zero through `.Machine$integer.max`. Local randomization
#'   preserves the caller's RNG state. Defaults to 1.
#' @param outer_folds,inner_folds Integer fold counts, at least two; defaults are
#'   five each. Every fitting split needs three rows and a nonconstant outcome.
#'   Requested fold counts are never reduced automatically.
#' @param alpha_grid Unique finite alpha candidates in `[0, 1]`, sorted internally.
#'   Defaults to 41 values from ridge to lasso in increments of 0.025.
#' @param lambda_rule Deployment rule: `"lambda.1se"` (default) or expert override
#'   `"lambda.min"`. The rule applies to outer models and the final model alike.
#'
#' @details
#' Folds balance outcome ranges using consecutive outcome-ranked blocks, with
#' randomized ties and distinct randomized fold labels within each block.
#' Predictor missingness filtering, median imputation, near-zero-variance removal
#' and sample-SD scaling are learned separately in every inner training split.
#' Each outer training set defines its outcome mean and sample SD; both its inner
#' fits and its held-out outcomes use that scale. See [pairwiseLLM_warm_model]
#' for preprocessing defaults. No PCA or outcome-based feature screening is used.
#'
#' For each alpha, glmnet constructs a reference path on the current tuning dataset
#' with its own preprocessing: 100 requested lambdas, minimum ratio 0.01 if retained
#' predictors outnumber rows, otherwise 0.0001. The actual returned path is stored.
#' Each inner training split fits those exact penalties with its own preprocessing;
#' reference preprocessing is not used to transform inner rows. Candidate penalties
#' use the entire tuning dataset, including its inner holdouts. No interpolation,
#' extrapolation, incomplete path, or omitted fold is accepted.
#'
#' CV error is the observation-count-weighted mean of fold mean squared errors.
#' Its SE is `sqrt(weighted.mean((fold_mse - cvm)^2, fold_sizes) / (K - 1))`.
#' Errors within `1e-10 * max(1, abs(a), abs(b))` are numerical ties. Lambda minimum
#' ties favor the largest penalty. Alpha selection compares CV error at each
#' alpha's lambda minimum and favors smaller alpha in a tie. The default selected
#' penalty is the largest lambda within one SE of that alpha's minimum error.
#'
#' OOF (out-of-fold) predictions are predictions for rows excluded from their
#' corresponding coefficient fit. Calibration regresses standardized outcomes on
#' selected-hyperparameter OOF predictions using ordinary least squares. Those same
#' folds select hyperparameters: calibration fit statistics are not independent
#' performance estimates. Only untouched outer predictions define validation metrics.
#' Calibration needs at least three rows and full rank at QR tolerance 1e-7;
#' constant/degenerate predictions or nonfinite coefficients cause an explicit
#' error. Finite negative slopes are allowed. Undefined validation diagnostics
#' are recorded as NA with reasons, without an identity-calibration fallback.
#'
#' After outer validation, full-data tuning and OOF calibration precede the final
#' all-row coefficient refit. Raw and calibrated predictions use within-task
#' standardized units, not original BTL units. They are not Bayesian prior SDs.
#' glmnet and withr are required only for model development; Python is required only
#' for the text-input path. This function never installs software or downloads data.
#'
#' @return A portable [pairwiseLLM_warm_model] with deployment preprocessing,
#'   coefficients and OOF calibration, plus tuning traces, fold assignments,
#'   outer raw/calibrated predictions, transformed held-out outcomes, validation
#'   metrics and warnings. Audit records include item IDs and outcomes but no raw
#'   training texts. Each outer record includes its tuning, scaling, preprocessing,
#'   calibration, hyperparameters and nonzero coefficient count. Final tuning
#'   metadata is separate from outer validation. No glmnet fit is retained.
#' @export
fit_warm_start_model <- function(ids, theta, task_id, texts = NULL, features = NULL,
                                 schema = "writing_features_v1", python = NULL, seed = 1L,
                                 outer_folds = 5L, inner_folds = 5L,
                                 alpha_grid = seq(0, 1, by = 0.025),
                                 lambda_rule = c("lambda.1se", "lambda.min")) {
  ids <- .warm_start_ids(ids)
  .warm_start_outcome_values(theta)
  .warm_start_task_id(task_id)
  warm_start_feature_schema(schema)
  if (length(theta) != length(ids)) rlang::abort("Supply one theta value per ID in the supplied ID order.")
  .warm_start_outcome_fit(theta)
  if (is.null(texts) == is.null(features)) rlang::abort("Supply exactly one of `texts` or `features`.")
  if (!is.null(features) && !is.null(python)) rlang::abort("`python` is only used with `texts`.")
  if (!.warm_start_number(seed, 0, .Machine$integer.max) || seed != floor(seed)) {
    rlang::abort("`seed` must be an integer from zero through .Machine$integer.max.")
  }
  .warm_start_fold_count(outer_folds, length(ids))
  .warm_start_fold_count(inner_folds, length(ids) - ceiling(length(ids) / outer_folds))
  if (!is.numeric(alpha_grid) || is.object(alpha_grid) || !is.null(dim(alpha_grid)) ||
      !length(alpha_grid) || any(!is.finite(alpha_grid)) || any(alpha_grid < 0 | alpha_grid > 1) ||
      anyDuplicated(alpha_grid)) rlang::abort("`alpha_grid` must contain unique finite values in [0, 1].")
  alpha_grid <- sort(as.numeric(alpha_grid))
  lambda_rule <- match.arg(lambda_rule)
  .warm_start_require_glmnet()
  if (!requireNamespace("withr", quietly = TRUE)) {
    rlang::abort("Model development requires optional package 'withr'. Install it explicitly first.")
  }
  warnings <- character()
  model <- withCallingHandlers(.pairwiseLLM_with_seed(seed, function() {
    if (!is.null(texts)) features <- extract_warm_start_features(ids, texts, schema, python)
    features <- .validate_warm_start_features(features, ids, schema)
    x <- as.matrix(features[, -1, drop = FALSE])
    outer_id <- .warm_start_folds(theta, outer_folds)
    records <- vector("list", outer_folds)
    predictions <- data.frame(item_id = ids, fold = outer_id, observed = NA_real_,
      raw_prediction = NA_real_, calibrated_prediction = NA_real_)
    for (fold in seq_len(outer_folds)) {
      records[[fold]] <- .warm_start_cv_context(paste("Outer fold", fold), function() {
        train <- which(outer_id != fold)
        test <- which(outer_id == fold)
        result <- .warm_start_train_cv(x[train, , drop = FALSE], theta[train], inner_folds,
          alpha_grid, lambda_rule)
        result$train_ids <- ids[train]
        result$test_ids <- ids[test]
        scaled <- .warm_start_preprocess_apply(x[test, , drop = FALSE], result$preprocessing)
        raw <- as.numeric(result$intercept + scaled %*% result$coefficients)
        result$predictions <- data.frame(item_id = ids[test],
          observed = .warm_start_outcome_apply(theta[test], result$outcome), raw_prediction = raw,
          calibrated_prediction = .warm_start_calibration_apply(raw, result$calibration))
        result
      })
      test <- which(outer_id == fold)
      predictions[test, c("observed", "raw_prediction", "calibrated_prediction")] <-
        records[[fold]]$predictions[, c("observed", "raw_prediction", "calibrated_prediction")]
    }
    metrics <- .warm_start_validation_metrics(predictions$calibrated_prediction, predictions$observed)
    final <- .warm_start_cv_context("Final full-data fit", function() {
      .warm_start_train_cv(x, theta, inner_folds, alpha_grid, lambda_rule)
    })
    training <- list(task_id = task_id, n = length(ids), alpha = final$tuning$selected$alpha,
      lambda = final$tuning$selected$lambda, n_nonzero = sum(final$coefficients != 0), engine = "glmnet",
      engine_version = as.character(utils::packageVersion("glmnet")),
      package_version = as.character(utils::packageVersion("pairwiseLLM")))
    final$tuning$ids <- ids
    final$tuning$seed <- as.integer(seed)
    .new_warm_start_model(schema, final$preprocessing, final$coefficients, final$intercept,
      final$outcome, training, calibration = final$calibration, tuning = final$tuning,
      validation = list(method = "nested_cv", outer_folds = as.integer(outer_folds),
        inner_folds = as.integer(inner_folds), predictions = predictions, folds = records,
        metrics = metrics, warnings = warnings))
  }), warning = function(w) warnings <<- c(warnings, conditionMessage(w)))
  model$validation$warnings <- warnings
  .validate_warm_start_model(model)
  model
}

.warm_start_fold_count <- function(k, n) {
  if (!.warm_start_number(k, 2, n) || k != floor(k) || n - ceiling(n / k) < 3L) {
    rlang::abort("Requested folds require nonempty holdouts and at least three rows in every training split.")
  }
  invisible(k)
}

# Called only inside locally seeded training, or locally seeded tests.
.warm_start_folds <- function(theta, k) {
  .warm_start_outcome_values(theta)
  .warm_start_fold_count(k, length(theta))
  order <- order(theta, stats::runif(length(theta)))
  labels <- integer(length(theta))
  for (start in seq.int(1L, length(theta), by = k)) {
    block <- seq.int(start, min(start + k - 1L, length(theta)))
    labels[order[block]] <- sample.int(k, length(block))
  }
  labels
}

.warm_start_train_cv <- function(x, theta, inner_folds, alpha_grid, lambda_rule) {
  outcome <- .warm_start_outcome_fit(theta)
  z <- .warm_start_outcome_apply(theta, outcome)
  foldid <- .warm_start_folds(theta, inner_folds)
  tuning <- .warm_start_tune(x, z, foldid, alpha_grid, lambda_rule)
  calibration <- .warm_start_calibration_fit(tuning$selected$oof, z)
  preprocessing <- tuning$reference_preprocessing
  fit <- .warm_start_glmnet_fit(.warm_start_preprocess_apply(x, preprocessing), z,
    tuning$selected$alpha, tuning$selected$lambda)
  coefficients <- as.matrix(fit$beta)[preprocessing$retained, 1]
  coefficients <- stats::setNames(as.numeric(coefficients), preprocessing$retained)
  list(outcome = outcome, tuning = tuning, calibration = calibration, preprocessing = preprocessing,
    coefficients = coefficients, intercept = unname(fit$a0[1]), n_nonzero = sum(coefficients != 0))
}
