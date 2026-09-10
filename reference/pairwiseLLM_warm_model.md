# Portable task-specific warm-start models

A warm-start model predicts relative quality from frozen writing
features before pairwise comparisons are collected. Each model
represents one task or assessment; unlinked BT/BTL scores from different
tasks must not be pooled.

## Usage

``` r
# S3 method for class 'pairwiseLLM_warm_model'
summary(object, ...)

# S3 method for class 'pairwiseLLM_warm_model'
print(x, ...)
```

## Arguments

- object:

  A `pairwiseLLM_warm_model` object.

- ...:

  Reserved for future extensions; must be empty.

- x:

  A `pairwiseLLM_warm_model` object.

## Value

[`print()`](https://rdrr.io/r/base/print.html) invisibly returns the
model. [`summary()`](https://rdrr.io/r/base/summary.html) returns a
named list describing the task, target, preprocessing, hyperparameters,
and calibration status and stored nested-validation metrics when
available.

## Details

The fixed-hyperparameter core is internal. It does not tune
hyperparameters, run cross-validation, or learn calibration.
[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md)
adds nested validation and OOF calibration. Core models remain
explicitly uncalibrated.

Format version 1 is an S3 list with these deployment fields:

- `format_version`: integer `1L`.

- `schema` and `features`: frozen schema identity and original feature
  order, including predictors removed during training.

- `preprocessing`: original/retained feature names, named removal
  reasons, training missing fractions, retained medians, centers, sample
  SDs, thresholds, training row count, and SD convention.

- `coefficients` and `intercept`: finite linear coefficients on the
  stored preprocessed predictor scale, in retained feature order, and
  intercept.

- `outcome`: `definition = "within_task_z"`, original training mean and
  sample SD, and `sd_convention = "sample"`. The target is
  `(theta - mean) / sd`.

- `calibration`: `status = "uncalibrated"`, with `intercept = NULL` and
  `slope = NULL` for core fits. Public fits store
  `status = "oof_linear"`, learned intercept/slope, calibration row
  count, method, QR tolerance and OOF source.

- `training`: task ID, training row count, requested alpha/lambda,
  nonzero coefficient count, engine/version, and package version.

- `tuning` and `validation`: NULL for core fits; public fits retain
  exact alpha/lambda traces, fold preprocessing, OOF calibration inputs,
  outer held-out predictions and metrics, and warning messages. See
  [`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md).

Explicit
[`prepare_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_warm_start_model.md)
audit omission creates format 2 with `audit_status = "summary_only"`. It
preserves deployment parameters and validation summaries but omits
row-level evidence. Those summaries cannot be recomputed from the
reduced artifact. Format 1 remains fully audited.

Preprocessing is fitted only on the supplied training rows. It first
removes all-missing columns and columns with missing fraction strictly
above 0.20, then median-imputes remaining columns. It removes constants
and columns with unique-value fraction at most 0.10 AND
most-common/second-most-common frequency ratio strictly above 19.
Frequencies are calculated after imputation. Retained columns are
centered and divided by sample SD (denominator `n - 1`). Internal
controls permit changing these thresholds; fitted values are stored. No
surviving predictors is an error. With one survivor, an excluded zero
column satisfies glmnet's two-column input requirement only during
fitting; it is never a schema feature or deployment coefficient.

The internal fixed fit requires at least three observations, explicit
alpha in `[0, 1]`, and finite lambda greater than or equal to zero. It
uses Gaussian glmnet with an intercept and `standardize = FALSE`, with
solver threshold `1e-12` and maximum `100000` iterations, directly at
the requested lambda. Outcomes are standardized before fitting. No PCA
or feature screening based on outcomes is used, including when
predictors outnumber observations.

Prediction uses only the stored preprocessing and linear coefficients,
never a serialized glmnet object. Python is optional for extraction, and
glmnet is optional for development; neither is needed to inspect or
predict from a deployment object with precomputed features. Schema
metadata is an input contract, not verified extraction provenance or
evidence of predictive validity.

## See also

[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md),
[`predict.pairwiseLLM_warm_model()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_model.md),
[`warm_start_coefficients()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_coefficients.md),
[`prepare_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_warm_start_model.md)

Other adaptive warm start:
[`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md),
[`extract_warm_start_features()`](https://shmercer.github.io/pairwiseLLM/reference/extract_warm_start_features.md),
[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md),
[`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md),
[`predict.pairwiseLLM_warm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_ensemble.md),
[`predict.pairwiseLLM_warm_model()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_model.md),
[`prepare_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_warm_start_model.md),
[`register_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/register_warm_start_model.md),
[`save_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/save_warm_start_model.md),
[`summary.pairwiseLLM_warm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_warm_ensemble.md),
[`summary.pairwiseLLM_warm_predictions()`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_warm_predictions.md),
[`warm_start_coefficients()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_coefficients.md),
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
    print(model)
    summary(model)
  })
}
#> Task-specific warm-start model: synthetic-a 
#> Target: within-task standardized BT/BTL theta (sample SD)
#> Training rows: 15 | Retained predictors: 20 | Nonzero coefficients: 2 
#> Alpha: 1 | Lambda: 0.02274379 
#> Calibration: oof_linear | Audit: full 
#> Nested validation: Pearson r = 0.9897764 | RMSE = 0.1600619 | MAE = 0.08329452 
#> $task_id
#> [1] "synthetic-a"
#> 
#> $target
#> $target$definition
#> [1] "within_task_z"
#> 
#> $target$mean
#> [1] 4.784082
#> 
#> $target$sd
#> [1] 6.397207
#> 
#> $target$sd_convention
#> [1] "sample"
#> 
#> 
#> $n
#> [1] 15
#> 
#> $schema
#> [1] "writing_features_v1"
#> 
#> $retained_predictors
#> [1] 20
#> 
#> $removed_predictors
#> named character(0)
#> 
#> $nonzero_coefficients
#> [1] 2
#> 
#> $alpha
#> [1] 1
#> 
#> $lambda
#> [1] 0.02274379
#> 
#> $calibration
#> [1] "oof_linear"
#> 
#> $audit_status
#> [1] "full"
#> 
#> $validation
#> $validation$pearson_r
#> [1] 0.9897764
#> 
#> $validation$squared_pearson_r
#> [1] 0.9796574
#> 
#> $validation$spearman_rho
#> [1] 0.9535714
#> 
#> $validation$rmse
#> [1] 0.1600619
#> 
#> $validation$mae
#> [1] 0.08329452
#> 
#> $validation$calibration_intercept
#> [1] -0.04764372
#> 
#> $validation$calibration_slope
#> [1] 0.9501558
#> 
#> $validation$undefined_reasons
#> character(0)
#> 
#> 
```
