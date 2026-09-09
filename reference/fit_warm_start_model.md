# Train a task-specific warm-start model with nested validation

A warm-start model predicts relative writing quality before pairwise
comparisons. One fitting call represents one assessment; do not pool
unlinked BT/BTL scales.

## Usage

``` r
fit_warm_start_model(
  ids,
  theta,
  task_id,
  texts = NULL,
  features = NULL,
  schema = "writing_features_v1",
  python = NULL,
  seed = 1L,
  outer_folds = 5L,
  inner_folds = 5L,
  alpha_grid = seq(0, 1, by = 0.025),
  lambda_rule = c("lambda.1se", "lambda.min")
)
```

## Arguments

- ids:

  Unique item IDs, aligned positionally with `theta` and `texts`.

- theta:

  Finite numeric BT/BTL scores from one assessment, one per ID.

- task_id:

  One nonblank assessment label, stored as provenance. It does not group
  rows, affect fitting, or identify a registered model.

- texts:

  Optional character vector of texts. Supply exactly one of `texts` and
  `features`. Extraction runs once before resampling.

- features:

  Optional precomputed feature table accepted by
  [`predict.pairwiseLLM_warm_model()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_model.md),
  aligned to `ids` by its `item_id` column.

- schema:

  Frozen feature schema identifier.

- python:

  Existing Python interpreter path for text extraction only.

- seed:

  Integer from zero through `.Machine$integer.max`. Local randomization
  preserves the caller's RNG state. Defaults to 1.

- outer_folds, inner_folds:

  Integer fold counts, at least two; defaults are five each. Every
  fitting split needs three rows and a nonconstant outcome. Requested
  fold counts are never reduced automatically.

- alpha_grid:

  Unique finite alpha candidates in `[0, 1]`, sorted internally.
  Defaults to 41 values from ridge to lasso in increments of 0.025.

- lambda_rule:

  Deployment rule: `"lambda.1se"` (default) or expert override
  `"lambda.min"`. The rule applies to outer models and the final model
  alike.

## Value

A portable
[pairwiseLLM_warm_model](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM_warm_model.md)
with deployment preprocessing, coefficients and OOF calibration, plus
tuning traces, fold assignments, outer raw/calibrated predictions,
transformed held-out outcomes, validation metrics and warnings. Audit
records include item IDs and outcomes but no raw training texts. Each
outer record includes its tuning, scaling, preprocessing, calibration,
hyperparameters and nonzero coefficient count. Final tuning metadata is
separate from outer validation. No glmnet fit is retained.

## Details

Folds balance outcome ranges using consecutive outcome-ranked blocks,
with randomized ties and distinct randomized fold labels within each
block. Predictor missingness filtering, median imputation,
near-zero-variance removal and sample-SD scaling are learned separately
in every inner training split. Each outer training set defines its
outcome mean and sample SD; both its inner fits and its held-out
outcomes use that scale. See
[pairwiseLLM_warm_model](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM_warm_model.md)
for preprocessing defaults. No PCA or outcome-based feature screening is
used.

For each alpha, glmnet constructs a reference path on the current tuning
dataset with its own preprocessing: 100 requested lambdas, minimum ratio
0.01 if retained predictors outnumber rows, otherwise 0.0001. The actual
returned path is stored. Each inner training split fits those exact
penalties with its own preprocessing; reference preprocessing is not
used to transform inner rows. Candidate penalties use the entire tuning
dataset, including its inner holdouts. No interpolation, extrapolation,
incomplete path, or omitted fold is accepted.

CV error is the observation-count-weighted mean of fold mean squared
errors. Its SE is
`sqrt(weighted.mean((fold_mse - cvm)^2, fold_sizes) / (K - 1))`. Errors
within `1e-10 * max(1, abs(a), abs(b))` are numerical ties. Lambda
minimum ties favor the largest penalty. Alpha selection compares CV
error at each alpha's lambda minimum and favors smaller alpha in a tie.
The default selected penalty is the largest lambda within one SE of that
alpha's minimum error.

OOF (out-of-fold) predictions are predictions for rows excluded from
their corresponding coefficient fit. Calibration regresses standardized
outcomes on selected-hyperparameter OOF predictions using ordinary least
squares. Those same folds select hyperparameters: calibration fit
statistics are not independent performance estimates. Only untouched
outer predictions define validation metrics. Calibration needs at least
three rows and full rank at QR tolerance 1e-7; constant/degenerate
predictions or nonfinite coefficients cause an explicit error. Finite
negative slopes are allowed. Undefined validation diagnostics are
recorded as NA with reasons, without an identity-calibration fallback.

After outer validation, full-data tuning and OOF calibration precede the
final all-row coefficient refit. Raw and calibrated predictions use
within-task standardized units, not original BTL units. They are not
Bayesian prior SDs. glmnet and withr are required only for model
development; Python is required only for the text-input path. This
function never installs software or downloads data.

## See also

[`predict.pairwiseLLM_warm_model()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_model.md),
[`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md),
[`save_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/save_warm_start_model.md)

Other adaptive warm start:
[`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md),
[`extract_warm_start_features()`](https://shmercer.github.io/pairwiseLLM/reference/extract_warm_start_features.md),
[`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md),
[`pairwiseLLM_warm_model`](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM_warm_model.md),
[`predict.pairwiseLLM_warm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_ensemble.md),
[`predict.pairwiseLLM_warm_model()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_model.md),
[`prepare_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_warm_start_model.md),
[`register_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/register_warm_start_model.md),
[`save_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/save_warm_start_model.md),
[`summary.pairwiseLLM_warm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_warm_ensemble.md),
[`summary.pairwiseLLM_warm_predictions()`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_warm_predictions.md),
[`warm_start_feature_schema()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_feature_schema.md),
[`warm_start_python_status()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_python_status.md)

## Examples

``` r
if (requireNamespace("glmnet", quietly = TRUE) &&
    requireNamespace("withr", quietly = TRUE)) {
  local({
    # Synthetic features illustrate the interface, not predictive validity.
    example_features <- function(seed) {
      withr::local_seed(seed)
      fields <- warm_start_feature_schema()$feature
      x <- as.data.frame(matrix(runif(15 * length(fields)), nrow = 15))
      names(x) <- fields
      x$n_tokens <- 11:25
      x$token_length_mean <- 2 + 10 * x$token_length_mean
      x$token_length_std <- 0.2 + x$token_length_std
      x$dale_chall_readability_score <- 5 + 20 * x$dale_chall_readability_score
      x <- data.frame(item_id = as.character(1:15), x)
      attr(x, "warm_start_schema") <- "writing_features_v1"
      x
    }
    features <- example_features(3103)
    theta <- 10 + 0.4 * features$n_tokens - 2 * features$token_length_mean
    # A small alpha grid keeps this example fast; the default has 41 values.
    model <- fit_warm_start_model(features$item_id, theta, "synthetic-a",
      features = features, alpha_grid = c(0, 1))
    summary(model)
    model$validation$metrics
  })
}
#> $pearson_r
#> [1] 0.9897764
#> 
#> $squared_pearson_r
#> [1] 0.9796574
#> 
#> $spearman_rho
#> [1] 0.9535714
#> 
#> $rmse
#> [1] 0.1600619
#> 
#> $mae
#> [1] 0.08329452
#> 
#> $calibration_intercept
#> [1] -0.04764372
#> 
#> $calibration_slope
#> [1] 0.9501558
#> 
#> $undefined_reasons
#> character(0)
#> 
```
