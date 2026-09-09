# Prepare metadata or a summary-only warm-start artifact

Prepare metadata or a summary-only warm-start artifact

## Usage

``` r
prepare_warm_start_model(model, metadata = list(), omit_audit = FALSE)
```

## Arguments

- model:

  A valid
  [pairwiseLLM_warm_model](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM_warm_model.md)
  or
  [`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md)
  ensemble.

- metadata:

  Named list with optional scalar character `name`, `version`, `domain`,
  `notes`, `license`, `prepared_at`, `preparation_package_version`, and
  `extraction_provenance` (a named character vector). Supplied fields
  replace existing fields. Timestamps use UTC `YYYY-MM-DDTHH:MM:SSZ`
  strings.

- omit_audit:

  Explicitly omit row-level development evidence. Default FALSE.

## Value

A prepared copy using the same model class and prediction method.

## Details

Preparation adds a preparation timestamp/package version when absent,
not a training date. Extraction provenance defaults to
`c(status = "unavailable")`; user-supplied records are not independently
verified. Schema identity or a later Python status check is not
extraction provenance.

Ordinary preparation preserves audit records. Explicit omission creates
format 2 with summary-only audit status, retaining deployment
parameters, tuning settings and nested-validation summaries. IDs, OOF
rows, fold records, tuning traces and contextual warning messages are
omitted; warning counts remain. Summaries cannot be recomputed without
the original evidence. An already reduced artifact cannot recover its
audit through this function.

Ensembles retain ensemble format 1; audit omission recursively reduces
each component to model format 2, preserving existing component
metadata. Supplied preparation metadata applies to the ensemble only.

No raw texts are added. Review task labels, notes, domain, and
provenance for restricted information before bundling; this is not a
general anonymizer. User models do not need complete publication
metadata. Task-specific outcome scales remain standardized
independently; storage does not link BTL scales.

## See also

[`save_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/save_warm_start_model.md),
[`register_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/register_warm_start_model.md)

Other adaptive warm start:
[`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md),
[`extract_warm_start_features()`](https://shmercer.github.io/pairwiseLLM/reference/extract_warm_start_features.md),
[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md),
[`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md),
[`pairwiseLLM_warm_model`](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM_warm_model.md),
[`predict.pairwiseLLM_warm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_ensemble.md),
[`predict.pairwiseLLM_warm_model()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_model.md),
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
    deployment <- prepare_warm_start_model(model,
      metadata = list(name = "example", domain = "Synthetic demonstration"),
      omit_audit = TRUE)
    predict(deployment, features)
  })
}
#> # A tibble: 15 × 3
#>    item_id raw_prediction calibrated_prediction
#>    <chr>            <dbl>                 <dbl>
#>  1 1                0.529                 0.553
#>  2 2                0.188                 0.199
#>  3 3                0.125                 0.134
#>  4 4               -1.67                 -1.73 
#>  5 5                0.790                 0.825
#>  6 6               -1.76                 -1.82 
#>  7 7                0.859                 0.896
#>  8 8                0.838                 0.874
#>  9 9                0.922                 0.961
#> 10 10              -1.20                 -1.25 
#> 11 11              -0.194                -0.198
#> 12 12              -0.709                -0.733
#> 13 13              -0.344                -0.354
#> 14 14               0.259                 0.273
#> 15 15               1.36                  1.42 
```
