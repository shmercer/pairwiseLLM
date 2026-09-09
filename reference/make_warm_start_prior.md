# Convert warm-start predictions to Bayesian BTL priors

Convert warm-start predictions to Bayesian BTL priors

## Usage

``` r
make_warm_start_prior(predictions, ids = NULL, prior_sd = 0.5)
```

## Arguments

- predictions:

  Calibrated single-model or ensemble predictions, or finite numeric
  scores with names or explicit `ids`. Numeric input is an expert choice
  of relative prior location, not an automatic calibration method.

- ids:

  Active item IDs. For prediction tables and named scores these must
  match the input ID set exactly and determine output order. For unnamed
  scores they identify input positions. Defaults to the IDs in the
  input.

- prior_sd:

  Positive finite scalar or vector. An unnamed vector follows input
  order; a named vector aligns by ID. Defaults to 0.5.

## Value

A version-1 `pairwiseLLM_warm_prior` list containing `item_id`,
`scores`, `prior_mean`, `prior_sd`, compact `diagnostics`, `provenance`,
and an integrity `digest`. Pass this object to
[`fit_bayes_btl_mcmc()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bayes_btl_mcmc.md)
or
[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md).

## Details

Single-model values must have learned OOF calibration. Ensembles use
their equal-weight calibrated mean. Scores are centered over the active
items in R; adaptive scoped refits subset the saved scores and center
again in that scope. Calibration is not applied twice and original
training BTL units are not used. Ensemble component predictions and
sample disagreement SD remain diagnostics; disagreement never supplies
the Bayesian prior SD automatically.

The normal prior applies to `theta_raw`; Stan centers this to obtain
`theta`. Centering induces dependence, so the supplied SD is not the
marginal SD of centered theta. Without predictive input, BTL retains raw
prior mean 0 and SD 1. This estimation prior is separate from initial
warm-start pairing schedules.

## See also

[`predict.pairwiseLLM_warm_model()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_model.md),
[`predict.pairwiseLLM_warm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_ensemble.md),
[`fit_bayes_btl_mcmc()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bayes_btl_mcmc.md),
[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md)

Other adaptive warm start:
[`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md),
[`extract_warm_start_features()`](https://shmercer.github.io/pairwiseLLM/reference/extract_warm_start_features.md),
[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md),
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
prior <- make_warm_start_prior(c(a = -1, b = 0, c = 1))
prior$prior_mean
#> [1] -1  0  1
```
