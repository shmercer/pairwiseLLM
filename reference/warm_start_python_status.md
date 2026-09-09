# Check the optional warm-start feature environment

Check the optional warm-start feature environment

## Usage

``` r
warm_start_python_status(schema = "writing_features_v1", python = NULL)
```

## Arguments

- schema:

  The frozen schema identifier, currently `"writing_features_v1"`.

- python:

  Optional path to an existing Python interpreter. With `NULL`, use an
  existing environment selected through reticulate. Automatic
  environment creation is disabled. Conflicting interpreter selections
  require correction or a fresh R session; this function never switches
  an initialized interpreter.

## Value

A list with `available`, `python`, `expected`, `observed`, and
`problems`. Availability requires the pinned versions, resource
contents, and pipeline capabilities. Missing prerequisites are reported
without throwing an error; malformed arguments or missing installed
metadata are errors.

## Details

This explicitly invoked check may initialize reticulate's Python
interpreter and load the spaCy model to verify its capabilities. It
never installs software or downloads resources. Once Python is
initialized, selecting a different interpreter requires restarting R.
Use
[`reticulate::use_virtualenv()`](https://rstudio.github.io/reticulate/reference/use_python.html)
or
[`reticulate::use_condaenv()`](https://rstudio.github.io/reticulate/reference/use_python.html)
with `required = TRUE` before this call, or supply `python` directly.
See the installed `python/README.md` for explicit setup.

## See also

[`extract_warm_start_features()`](https://shmercer.github.io/pairwiseLLM/reference/extract_warm_start_features.md),
[`warm_start_feature_schema()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_feature_schema.md)

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
[`warm_start_feature_schema()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_feature_schema.md)

## Examples

``` r
if (FALSE) { # \dontrun{
warm_start_python_status(python = "/path/to/venv/bin/python")
} # }
```
