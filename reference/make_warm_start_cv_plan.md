# Construct reusable warm-start cross-validation partitions

Construct reusable warm-start cross-validation partitions

## Usage

``` r
make_warm_start_cv_plan(
  ids,
  theta,
  task_id,
  seed = 1L,
  outer_folds = 5L,
  inner_folds = 5L
)
```

## Arguments

- ids:

  Unique item IDs in the same order as `theta`.

- theta:

  Finite numeric scores from one assessment.

- task_id:

  One nonblank assessment label.

- seed:

  Integer random seed, default 1.

- outer_folds, inner_folds:

  Fold counts, each at least two. Counts are never reduced
  automatically; every training split needs three nonconstant outcomes.

## Value

A portable `pairwiseLLM_warm_cv_plan` list, format 1. It contains exact
ordered IDs and outcomes, task identity, seed/RNG provenance, named
outer and inner assignments, and an integrity digest. Save with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html).

## Details

Partitions use outcome-ranked blocks with randomized ties and fold
labels. Draw order is outer folds, inner folds for outer training sets
in fold order, then full-data inner folds. The caller's RNG kind and
seed state are preserved. Plans are independent of feature schema and
engine. A supplied plan is checked before extraction or fitting and is
never regenerated or silently realigned. Its digest detects accidental
changes, not authorship. Plans contain outcomes and item IDs; they are
development evidence, not anonymized artifacts.

## See also

[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md)

Other adaptive warm start:
[`ensemble_warm_start_algorithms()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_algorithms.md),
[`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md),
[`extract_warm_start_features()`](https://shmercer.github.io/pairwiseLLM/reference/extract_warm_start_features.md),
[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md),
[`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md),
[`pairwiseLLM_warm_model`](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM_warm_model.md),
[`predict.pairwiseLLM_warm_algorithm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_algorithm_ensemble.md),
[`predict.pairwiseLLM_warm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_ensemble.md),
[`predict.pairwiseLLM_warm_model()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_model.md),
[`prepare_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_warm_start_model.md),
[`register_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/register_warm_start_model.md),
[`save_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/save_warm_start_model.md),
[`summary.pairwiseLLM_warm_algorithm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_warm_algorithm_ensemble.md),
[`summary.pairwiseLLM_warm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_warm_ensemble.md),
[`summary.pairwiseLLM_warm_predictions()`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_warm_predictions.md),
[`warm_start_coefficients()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_coefficients.md),
[`warm_start_feature_schema()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_feature_schema.md),
[`warm_start_python_status()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_python_status.md)

## Examples

``` r
if (requireNamespace("withr", quietly = TRUE)) {
  plan <- make_warm_start_cv_plan(as.character(1:20), seq_len(20), "example")
  plan$outer_foldid
}
#>  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
#>  1  3  5  4  2  1  2  5  3  4  2  5  1  4  3  1  4  3  2  5 
```
