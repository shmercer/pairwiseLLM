# Combine independently trained warm-start models

An ensemble averages calibrated standardized predictions from separate
task models. Each task standardizes its own outcome, so original BT/BTL
scales need not be linked. Callers are responsible for independently
sourced training data; task labels cannot establish independence.

## Usage

``` r
ensemble_warm_start_models(...)
```

## Arguments

- ...:

  Individual calibrated models, path strings, or reference lists with
  `path`, or `name` and optional `source`, as in
  [`load_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/save_warm_start_model.md).
  Supply argument names for explicit component identities. Unnamed
  arguments receive `component_<position>`; duplicate final names and
  blank supplied names are rejected. Order and supplied names are
  preserved. Nested ensembles fail.

## Value

A portable `pairwiseLLM_warm_ensemble` list with ensemble format version
1, named `components`, common `schema`, original `features`,
standardized `outcome` definition/sample-SD convention, and
`weighting = "equal"`.

## Details

Components must have learned OOF calibration and compatible frozen
schemas. Full-audit model format 1 and summary-only model format 2 can
be mixed. Each component keeps its own preprocessing, calibration, and
training metadata. References are loaded once; prediction uses stored
components. Character references always mean paths, never implicit
registry names. Registry names retain existing normalization and source
ambiguity rules; component names do not use registry normalization.
Custom or learned weights are not supported.

[`prepare_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_warm_start_model.md)
can add ensemble metadata or explicitly reduce all component audits.
Ordinary save/load is lossless. Ensemble format 1 is independent of
component formats and remains 1 after reduction.

## See also

[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md),
[`predict.pairwiseLLM_warm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_ensemble.md),
[`warm_start_coefficients()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_coefficients.md),
[`summary.pairwiseLLM_warm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_warm_ensemble.md)

Other adaptive warm start:
[`extract_warm_start_features()`](https://shmercer.github.io/pairwiseLLM/reference/extract_warm_start_features.md),
[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md),
[`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md),
[`pairwiseLLM_warm_model`](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM_warm_model.md),
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
    features_b <- example_features(3104)
    theta_b <- 30 + features_b$n_tokens - 3 * features_b$token_length_mean
    model_b <- fit_warm_start_model(features_b$item_id, theta_b, "synthetic-b",
      features = features_b, alpha_grid = c(0, 1))
    ensemble <- ensemble_warm_start_models(assessment_a = model, assessment_b = model_b)
    print(ensemble)
  })
}
#> Warm-start ensemble: 2 equally weighted task models
#> Components: assessment_a, assessment_b 
#> Target: within_task_z; audit: full 
#> Between-model sample SD is diagnostic, not Bayesian prior SD. 
```
