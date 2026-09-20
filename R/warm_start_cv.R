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
#' @param engine Fitting engine: `"glmnet"` (default), `"pls"`, or `"svr_rbf"`.
#'   Explicit `alpha_grid` or `lambda_rule`
#'   arguments are accepted only for glmnet.
#' @param cv_plan Optional [make_warm_start_cv_plan()] object with exactly matching
#'   task, ordered IDs and outcomes. Omitted seed/fold arguments defer to the plan;
#'   explicitly conflicting arguments fail. A supplied plan is never regenerated.
#' @param engine_control For PLS, an optional named list containing `ncomp`, a
#'   vector of unique positive integer candidate component counts, at most 10.
#'   Every candidate must be legal in every required inner fit and context refit.
#'   The default uses all counts from one through the common legal maximum in
#'   each tuning context. For SVR, only `cost` and `gamma_multiplier` are accepted:
#'   unique finite positive candidate vectors, defaulting independently to
#'   `2^(-2:4)` and `2^(-2:2)`. Epsilon is fixed at 0.10. Unknown controls fail.
#'   For glmnet use `alpha_grid` and `lambda_rule` instead.
#'
#' @details
#' New public fits use engine-neutral format 3 with explicit full audit status,
#' a reusable CV plan, and a numeric deployment payload. Legacy formats 1/2 remain
#' supported. Construct a plan once to reuse partitions across feature schemas.
#'
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
#' PLS uses the optional `pls` package with explicit `method = "kernelpls"`,
#' `scale = FALSE`, `validation = "none"`, and centering. Preprocessing remains
#' owned by this package. Each tuning context uses a common component grid bounded
#' by every inner training matrix and the context refit: centered QR rank at
#' tolerance 1e-7, retained predictor count, training row count minus one, and 10.
#' Explicit candidates are never silently dropped. Weighted MSE and SE follow
#' the rules above; minimum-error ties and eligible 1-SE choices favor fewer
#' components. Nonfinite or degenerate fits fail with context, without fallback.
#' All candidate OOF predictions, fold losses, rank bounds and selection evidence
#' are retained. Stored rank bounds are checked for consistency; recomputing rank
#' itself requires the original feature table, which is not stored in the model.
#'
#' RBF-SVR uses optional `e1071`, with `type = "eps-regression"`, `kernel = "radial"`,
#' `scale = FALSE`, `cross = 0`, `probability = FALSE`, and fixed `epsilon = 0.10`.
#' Every fit divides its gamma multiplier by its own retained predictor count.
#' The complete Cartesian grid is tuned with the weighted MSE/SE above. Both
#' minimum-error ties and eligible 1-SE choices favor lower cost, then lower
#' gamma multiplier. Full audits retain candidate OOF values, fold losses,
#' retained counts and actual gammas. Failed candidates are never omitted.
#'
#' OOF (out-of-fold) predictions are predictions for rows excluded from their
#' corresponding engine fit. Calibration regresses standardized outcomes on
#' selected-hyperparameter OOF predictions using ordinary least squares. Those same
#' folds select hyperparameters: calibration fit statistics are not independent
#' performance estimates. Only untouched outer predictions define validation metrics.
#' Calibration needs at least three rows and full rank at QR tolerance 1e-7;
#' constant/degenerate predictions or nonfinite coefficients cause an explicit
#' error. Finite negative slopes are allowed. Undefined validation diagnostics
#' are recorded as NA with reasons, without an identity-calibration fallback.
#'
#' After outer validation, full-data tuning and OOF calibration precede the final
#' all-row engine refit. Raw and calibrated predictions use within-task
#' standardized units, not original BTL units. They are not Bayesian prior SDs.
#' The selected engine package and withr are required only for model development; Python is required only
#' for the text-input path. This function never installs software or downloads data.
#'
#' @return A portable [pairwiseLLM_warm_model] with deployment preprocessing,
#'   numeric engine parameters and OOF calibration, plus tuning traces, fold assignments,
#'   outer raw/calibrated predictions, transformed held-out outcomes, validation
#'   metrics and warnings. Audit records include item IDs and outcomes but no raw
#'   training texts. Each outer record includes its tuning, scaling, preprocessing,
#'   calibration and hyperparameters (linear models also record nonzero counts). Final tuning
#'   metadata is separate from outer validation. No backend fit is retained.
#'   PLS coefficients and `Ymeans - Xmeans %*% beta` reproduce backend predictions
#'   on the stored preprocessed predictor scale without requiring `pls` at deployment.
#'   SVR stores numeric support vectors, dual coefficients, rho and actual gamma;
#'   prediction needs no `e1071`. Its linear coefficients/intercept are NULL.
#' @family adaptive warm start
#' @seealso [predict.pairwiseLLM_warm_model()], [ensemble_warm_start_models()],
#'   [save_warm_start_model()]
#' @examples
#' if (requireNamespace("glmnet", quietly = TRUE) &&
#'     requireNamespace("withr", quietly = TRUE)) {
#'   local({
#'     # Synthetic features illustrate the interface, not predictive validity.
#'     example_features <- function(seed) {
#'       withr::local_seed(seed)
#'       fields <- warm_start_feature_schema()$feature
#'       x <- as.data.frame(matrix(runif(15 * length(fields)), nrow = 15))
#'       names(x) <- fields
#'       x$n_tokens <- 11:25
#'       x$token_length_mean <- 2 + 10 * x$token_length_mean
#'       x$token_length_std <- 0.2 + x$token_length_std
#'       x$dale_chall_readability_score <- 5 + 20 * x$dale_chall_readability_score
#'       x <- data.frame(item_id = as.character(1:15), x)
#'       attr(x, "warm_start_schema") <- "writing_features_v1"
#'       x
#'     }
#'     features <- example_features(3103)
#'     theta <- 10 + 0.4 * features$n_tokens - 2 * features$token_length_mean
#'     # A small alpha grid keeps this example fast; the default has 41 values.
#'     model <- fit_warm_start_model(features$item_id, theta, "synthetic-a",
#'       features = features, alpha_grid = c(0, 1))
#'     summary(model)
#'     model$validation$metrics
#'   })
#' }
#' @export
fit_warm_start_model <- function(ids, theta, task_id, texts = NULL, features = NULL,
                                 schema = "writing_features_v1", python = NULL, seed = 1L,
                                 outer_folds = 5L, inner_folds = 5L,
                                 alpha_grid = seq(0, 1, by = 0.025),
                                 lambda_rule = c("lambda.1se", "lambda.min"),
                                 engine = c("glmnet", "pls", "svr_rbf"),
                                 cv_plan = NULL, engine_control = NULL) {
  ids <- .warm_start_ids(ids)
  .warm_start_outcome_values(theta)
  .warm_start_task_id(task_id)
  engine <- match.arg(engine)
  control <- .warm_start_engine_control(engine, engine_control)
  if (engine != "glmnet" && (!missing(alpha_grid) || !missing(lambda_rule))) {
    rlang::abort("Explicit `alpha_grid` and `lambda_rule` are glmnet-only controls.")
  }
  warm_start_feature_schema(schema)
  if (length(theta) != length(ids)) rlang::abort("Supply one theta value per ID in the supplied ID order.")
  .warm_start_outcome_fit(theta)
  if (is.null(texts) == is.null(features)) rlang::abort("Supply exactly one of `texts` or `features`.")
  if (!is.null(features) && !is.null(python)) rlang::abort("`python` is only used with `texts`.")
  if (!is.null(cv_plan)) {
    .validate_warm_start_cv_plan(cv_plan, ids, theta, task_id)
    for (field in c("seed", "outer_folds", "inner_folds")) {
      explicit <- switch(field, seed = !missing(seed), outer_folds = !missing(outer_folds),
        inner_folds = !missing(inner_folds))
      if (explicit && (!.warm_start_number(get(field), 0) ||
          !identical(as.numeric(get(field)), as.numeric(cv_plan[[field]])))) {
        rlang::abort(paste0("Explicit `", field, "` conflicts with the supplied CV plan."))
      }
    }
    seed <- cv_plan$seed
    outer_folds <- cv_plan$outer_folds
    inner_folds <- cv_plan$inner_folds
  }
  .warm_start_plan_seed(seed)
  .warm_start_fold_count(outer_folds, length(ids))
  .warm_start_fold_count(inner_folds, length(ids) - ceiling(length(ids) / outer_folds))
  if (!is.numeric(alpha_grid) || is.object(alpha_grid) || !is.null(dim(alpha_grid)) ||
      !length(alpha_grid) || any(!is.finite(alpha_grid)) || any(alpha_grid < 0 | alpha_grid > 1) ||
      anyDuplicated(alpha_grid)) rlang::abort("`alpha_grid` must contain unique finite values in [0, 1].")
  alpha_grid <- sort(as.numeric(alpha_grid))
  lambda_rule <- match.arg(lambda_rule)
  switch(engine, glmnet = .warm_start_require_glmnet(), pls = .warm_start_require_pls(),
    svr_rbf = .warm_start_require_svr())
  if (!requireNamespace("withr", quietly = TRUE)) {
    rlang::abort("Model development requires optional package 'withr'. Install it explicitly first.")
  }
  if (is.null(cv_plan)) {
    cv_plan <- make_warm_start_cv_plan(ids, theta, task_id, seed, outer_folds, inner_folds)
  }
  warnings <- character()
  model <- withCallingHandlers(.pairwiseLLM_with_seed(seed, function() {
    if (!is.null(texts)) features <- extract_warm_start_features(ids, texts, schema, python)
    features <- .validate_warm_start_features(features, ids, schema)
    x <- as.matrix(features[, -1, drop = FALSE])
    outer_id <- unname(cv_plan$outer_foldid)
    records <- vector("list", outer_folds)
    predictions <- data.frame(item_id = ids, fold = outer_id, observed = NA_real_,
      raw_prediction = NA_real_, calibrated_prediction = NA_real_)
    for (fold in seq_len(outer_folds)) {
      records[[fold]] <- .warm_start_cv_context(paste("Outer fold", fold), function() {
        train <- which(outer_id != fold)
        test <- which(outer_id == fold)
        result <- .warm_start_train_cv(x[train, , drop = FALSE], theta[train], inner_folds,
          alpha_grid, lambda_rule, unname(cv_plan$outer_inner_foldid[[fold]]), engine, control)
        result$train_ids <- ids[train]
        result$test_ids <- ids[test]
        scaled <- .warm_start_preprocess_apply(x[test, , drop = FALSE], result$preprocessing)
        raw <- .warm_start_engine_predict(result$engine_payload, scaled)
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
      .warm_start_train_cv(x, theta, inner_folds, alpha_grid, lambda_rule,
        unname(cv_plan$full_inner_foldid), engine, control)
    })
    training <- list(task_id = task_id, n = length(ids), alpha = final$tuning$selected$alpha,
      lambda = final$tuning$selected$lambda, n_nonzero = sum(final$coefficients != 0), engine = engine,
      engine_version = as.character(utils::packageVersion(if (engine == "svr_rbf") "e1071" else engine)),
      package_version = as.character(utils::packageVersion("pairwiseLLM")))
    if (engine == "pls") {
      training[c("alpha", "lambda")] <- NULL
      training$hyperparameters <- list(ncomp = final$tuning$selected$ncomp)
    }
    if (engine == "svr_rbf") {
      training[c("alpha", "lambda", "n_nonzero")] <- NULL
      training$hyperparameters <- .warm_start_svr_hyperparameters(final$tuning$selected,
        length(final$preprocessing$retained))
    }
    final$tuning$ids <- ids
    final$tuning$seed <- as.integer(seed)
    fitted <- .new_warm_start_model(schema, final$preprocessing, final$coefficients, final$intercept,
      final$outcome, training, calibration = final$calibration, tuning = final$tuning,
      validation = list(method = "nested_cv", outer_folds = as.integer(outer_folds),
        inner_folds = as.integer(inner_folds), predictions = predictions, folds = records,
        metrics = metrics, warnings = warnings),
      format_version = if (engine == "glmnet") 1L else 3L,
      cv_plan = cv_plan, engine_payload = final$engine_payload)
    if (engine == "glmnet") .warm_start_format3(fitted, cv_plan, final$engine_payload) else fitted
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

.warm_start_train_cv <- function(x, theta, inner_folds, alpha_grid, lambda_rule,
                                  foldid = NULL, engine = "glmnet", control = NULL) {
  outcome <- .warm_start_outcome_fit(theta)
  z <- .warm_start_outcome_apply(theta, outcome)
  if (is.null(foldid)) foldid <- .warm_start_folds(theta, inner_folds)
  tuning <- .warm_start_engine_tune(engine, x, z, foldid, alpha_grid, lambda_rule, control)
  calibration <- .warm_start_calibration_fit(tuning$selected$oof, z)
  preprocessing <- tuning$reference_preprocessing
  payload <- .warm_start_engine_refit(engine, .warm_start_preprocess_apply(x, preprocessing), z,
    tuning$selected)
  out <- list(outcome = outcome, tuning = tuning, calibration = calibration, preprocessing = preprocessing,
    coefficients = payload$coefficients, intercept = payload$intercept,
    n_nonzero = sum(payload$coefficients != 0), engine_payload = payload)
  if (engine == "svr_rbf") out$n_nonzero <- NULL
  out
}
