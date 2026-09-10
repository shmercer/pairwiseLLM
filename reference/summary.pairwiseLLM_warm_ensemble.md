# Inspect a warm-start ensemble

Inspect a warm-start ensemble

## Usage

``` r
# S3 method for class 'pairwiseLLM_warm_ensemble'
summary(object, ...)

# S3 method for class 'pairwiseLLM_warm_ensemble'
print(x, ...)
```

## Arguments

- object, x:

  A `pairwiseLLM_warm_ensemble`.

- ...:

  Reserved; must be empty.

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns component
summaries and ensemble contract details.
[`print()`](https://rdrr.io/r/base/print.html) returns its input
invisibly. Component metrics do not validate the ensemble. Between-model
SD is diagnostic disagreement, not Bayesian prior SD.

## See also

[`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md),
[`predict.pairwiseLLM_warm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_ensemble.md)

Other adaptive warm start:
[`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md),
[`extract_warm_start_features()`](https://shmercer.github.io/pairwiseLLM/reference/extract_warm_start_features.md),
[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md),
[`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md),
[`pairwiseLLM_warm_model`](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM_warm_model.md),
[`predict.pairwiseLLM_warm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_ensemble.md),
[`predict.pairwiseLLM_warm_model()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_model.md),
[`prepare_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_warm_start_model.md),
[`register_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/register_warm_start_model.md),
[`save_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/save_warm_start_model.md),
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
    summary(ensemble)
    print(ensemble)
  })
}
#> Warm-start ensemble: 2 equally weighted task models
#> Components: assessment_a, assessment_b 
#> Target: within_task_z; audit: full 
#> Between-model sample SD is diagnostic, not Bayesian prior SD. 
```
