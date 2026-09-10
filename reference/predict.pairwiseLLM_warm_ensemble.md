# Predict with an equal-weight warm-start ensemble

Predict with an equal-weight warm-start ensemble

## Usage

``` r
# S3 method for class 'pairwiseLLM_warm_ensemble'
predict(object, newdata = NULL, ..., texts = NULL, ids = NULL, python = NULL)
```

## Arguments

- object:

  A valid
  [`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md)
  ensemble.

- newdata:

  Precomputed frozen feature rows, including `item_id` and
  `warm_start_schema` metadata. Mutually exclusive with `texts`.

- ...:

  Reserved; must be empty.

- texts:

  Character texts, paired positionally with explicit `ids`.

- ids:

  Item IDs for text input only.

- python:

  Optional explicitly selected Python interpreter for text extraction
  only; see
  [`extract_warm_start_features()`](https://shmercer.github.io/pairwiseLLM/reference/extract_warm_start_features.md).

## Value

A `pairwiseLLM_warm_predictions` tibble in input order with `item_id`,
one `component_<name>` calibrated prediction column per component,
`ensemble_mean`, and `ensemble_sd`. SD is the sample SD (denominator
k - 1) across all k calibrated predictions, including for exactly two
components. Attributes `warm_start_schema` and `warm_start_model` retain
schema and ensemble/component identity and format metadata.
`component_predictions` retains complete named single-model results,
including raw predictions; `component_columns` maps component names to
output columns.

## Details

Text features are extracted once. Each model independently applies its
stored preprocessing and OOF calibration to the same raw feature rows.
Original BTL scales never enter the average; calibration is not applied
a second time. Every component must return exactly the input IDs in
order and finite numeric predictions. A failure names the component; no
model or row is dropped. Precomputed prediction needs neither Python nor
glmnet.

Ensemble SD measures between-model disagreement. It is diagnostic, not a
Bayesian prior SD or a calibrated uncertainty estimate. These
predictions do not perform BTL prior conversion. No ensemble validation
metric is inferred from component validation metrics.

## See also

[`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md),
[`summary.pairwiseLLM_warm_predictions()`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_warm_predictions.md),
[`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md)

Other adaptive warm start:
[`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md),
[`extract_warm_start_features()`](https://shmercer.github.io/pairwiseLLM/reference/extract_warm_start_features.md),
[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md),
[`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md),
[`pairwiseLLM_warm_model`](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM_warm_model.md),
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
    features_b <- example_features(3104)
    theta_b <- 30 + features_b$n_tokens - 3 * features_b$token_length_mean
    model_b <- fit_warm_start_model(features_b$item_id, theta_b, "synthetic-b",
      features = features_b, alpha_grid = c(0, 1))
    ensemble <- ensemble_warm_start_models(assessment_a = model, assessment_b = model_b)
    predictions <- predict(ensemble, features)
    head(predictions)
  })
}
#> Warm-start ensemble predictions; sample SD is diagnostic, not Bayesian prior SD.
#> # A tibble: 6 × 5
#>   item_id component_assessment_a component_assessment_b ensemble_mean
#>   <chr>                    <dbl>                  <dbl>         <dbl>
#> 1 1                        0.553                  0.848         0.700
#> 2 2                        0.199                  0.563         0.381
#> 3 3                        0.134                  0.543         0.338
#> 4 4                       -1.73                  -1.13         -1.43 
#> 5 5                        0.825                  1.26          1.04 
#> 6 6                       -1.82                  -1.13         -1.48 
#> # ℹ 1 more variable: ensemble_sd <dbl>
```
