# Inspect calibrated standardized warm-start coefficients

`warm_start_coefficients()` reports the fitted elastic-net coefficients
after applying the model's learned OOF linear calibration. It supports
individual calibrated warm-start models and equal-weight warm-start
ensembles.

## Usage

``` r
warm_start_coefficients(object, ...)

# S3 method for class 'pairwiseLLM_warm_model'
warm_start_coefficients(object, ...)

# S3 method for class 'pairwiseLLM_warm_ensemble'
warm_start_coefficients(object, ...)
```

## Arguments

- object:

  A calibrated `pairwiseLLM_warm_model` or `pairwiseLLM_warm_ensemble`
  object.

- ...:

  Reserved for future extensions; must be empty.

## Value

For an individual model, a tibble with exactly `feature`, `retained`,
and `calibrated_std_coefficient`, in the model's frozen feature order.
For an ensemble, a tibble with `feature` followed by one
`<component>_std_coefficient` column per component, in component order.

## Details

Predictors use each component model's fitted training medians, centers,
and sample SDs. The fitted target is within-task standardized BT/BTL
quality. For each retained feature, the reported value is the stored
elastic-net coefficient multiplied by the learned OOF calibration slope.
Thus, holding the other included predictors fixed, it is the change in
calibrated within-task standardized prediction for a one-training-SD
increase in that feature.

`retained = FALSE` with an `NA` coefficient means preprocessing removed
the feature and no fitted standardized coefficient exists.
`retained = TRUE` with coefficient zero means the feature survived
preprocessing but elastic net assigned it zero calibrated weight at the
selected alpha and lambda.

Positive and negative signs describe fitted direction conditional on the
other included predictors. Correlated predictors can share or trade
fitted weight, so coefficient magnitude is not a unique measure of
predictive importance, causal influence, or explained variance.

Ensemble columns show component coefficients side by side. Each
component standardized predictors using its own training distribution,
so columns do not imply one common raw-feature SD. The table exposes
fitted direction, magnitude, and stability across task models; it
neither defines an aggregate coefficient nor changes equal prediction
weighting. Inspection from an existing portable artifact needs neither
Python nor glmnet.

## See also

[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md),
[`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md),
[`predict.pairwiseLLM_warm_model()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_model.md)

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
[`summary.pairwiseLLM_warm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_warm_ensemble.md),
[`summary.pairwiseLLM_warm_predictions()`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_warm_predictions.md),
[`warm_start_feature_schema()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_feature_schema.md),
[`warm_start_python_status()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_python_status.md)

## Examples

``` r
if (requireNamespace("glmnet", quietly = TRUE) &&
    requireNamespace("withr", quietly = TRUE)) {
  local({
    withr::local_seed(3103)
    fields <- warm_start_feature_schema()$feature
    x <- as.data.frame(matrix(runif(15 * length(fields)), nrow = 15))
    names(x) <- fields
    x$n_tokens <- 11:25
    x$token_length_mean <- 2 + 10 * x$token_length_mean
    x$token_length_std <- 0.2 + x$token_length_std
    x$dale_chall_readability_score <- 5 + 20 * x$dale_chall_readability_score
    x <- data.frame(item_id = as.character(1:15), x)
    attr(x, "warm_start_schema") <- "writing_features_v1"
    theta <- 10 + 0.4 * x$n_tokens - 2 * x$token_length_mean
    model <- fit_warm_start_model(x$item_id, theta, "synthetic-example",
      features = x, alpha_grid = c(0, 1))
    warm_start_coefficients(model)
  })
}
#> # A tibble: 20 × 3
#>    feature                                retained calibrated_std_coefficient
#>    <chr>                                  <lgl>                         <dbl>
#>  1 n_tokens                               TRUE                          0.260
#>  2 proportion_unique_tokens               TRUE                          0    
#>  3 token_length_mean                      TRUE                         -1.01 
#>  4 token_length_std                       TRUE                          0    
#>  5 sentence_length_mean                   TRUE                          0    
#>  6 sentence_length_std                    TRUE                          0    
#>  7 pos_prop_noun                          TRUE                          0    
#>  8 pos_prop_verb                          TRUE                          0    
#>  9 pos_prop_adj                           TRUE                          0    
#> 10 pos_prop_adv                           TRUE                          0    
#> 11 pos_prop_pron                          TRUE                          0    
#> 12 pos_prop_adp                           TRUE                          0    
#> 13 pos_prop_cconj                         TRUE                          0    
#> 14 pos_prop_sconj                         TRUE                          0    
#> 15 dependency_distance_mean               TRUE                          0    
#> 16 dependency_distance_std                TRUE                          0    
#> 17 prop_adjacent_dependency_relation_mean TRUE                          0    
#> 18 upstream_entropy_per_token             TRUE                          0    
#> 19 first_order_coherence                  TRUE                          0    
#> 20 dale_chall_readability_score           TRUE                          0    
```
