# Predict relative quality from a portable warm-start model

Predict relative quality from a portable warm-start model

## Usage

``` r
# S3 method for class 'pairwiseLLM_warm_model'
predict(object, newdata, ...)
```

## Arguments

- object:

  A valid `pairwiseLLM_warm_model` object.

- newdata:

  Precomputed feature rows with `item_id`, all original frozen schema
  columns, and matching `warm_start_schema` metadata. IDs must be
  unique, nonmissing and nonblank. Numeric IDs normalize to character.
  Extra columns are ignored; required columns cannot be omitted even if
  training removed them.

- ...:

  Reserved for future extensions; must be empty.

## Value

A tibble in `newdata` row order with character `item_id`, numeric
`raw_prediction`, and numeric `calibrated_prediction`. Raw predictions
are on the within-task standardized outcome scale, not the original
BT/BTL scale. Public fits apply the stored OOF calibration intercept and
slope to raw values. For uncalibrated core fits, calibrated predictions
are `NA_real_`, never identity-calibrated substitutes. Attributes
`warm_start_schema` and `warm_start_model` record schema identity and
model metadata (format version, task ID, outcome definition, and
calibration status), respectively.

## Details

Prediction applies training medians, centers, and sample SDs, followed
by the stored intercept and coefficients. It does not recompute
preprocessing, train a model, load glmnet, initialize Python, or check a
Python environment. Predictions are not calibrated Bayesian prior means
or prior standard deviations. See
[pairwiseLLM_warm_model](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM_warm_model.md)
for the portable model contract.

## See also

[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md),
[`predict.pairwiseLLM_warm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_ensemble.md),
[`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md)

Other adaptive warm start:
[`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md),
[`extract_warm_start_features()`](https://shmercer.github.io/pairwiseLLM/reference/extract_warm_start_features.md),
[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md),
[`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md),
[`pairwiseLLM_warm_model`](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM_warm_model.md),
[`predict.pairwiseLLM_warm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_ensemble.md),
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
    predictions <- predict(model, features)
    head(predictions)
  })
}
#> # A tibble: 6 × 3
#>   item_id raw_prediction calibrated_prediction
#>   <chr>            <dbl>                 <dbl>
#> 1 1                0.529                 0.553
#> 2 2                0.188                 0.199
#> 3 3                0.125                 0.134
#> 4 4               -1.67                 -1.73 
#> 5 5                0.790                 0.825
#> 6 6               -1.76                 -1.82 
```
