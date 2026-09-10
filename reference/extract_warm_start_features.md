# Extract frozen writing features for warm-start prediction

Warm-start prediction uses text features to predict relative writing
quality before collecting pairwise comparisons. This function extracts
the frozen English writing features; it does not train a model or
estimate BTL scores.

## Usage

``` r
extract_warm_start_features(
  ids,
  texts,
  schema = "writing_features_v1",
  python = NULL
)
```

## Arguments

- ids:

  Unique, nonmissing character or finite numeric item IDs. IDs are
  returned as character strings; blank IDs are not allowed.

- texts:

  A nonempty character vector of the same length as `ids`, without
  missing values. Empty strings are allowed. Text is never trimmed or
  normalized.

- schema:

  The frozen schema identifier, currently `"writing_features_v1"`.

- python:

  Optional path to an existing Python interpreter. With `NULL`, use an
  existing environment selected through reticulate. Automatic
  environment creation is disabled. Conflicting interpreter selections
  require correction or a fresh R session; this function never switches
  an initialized interpreter.

## Value

A tibble with character `item_id` and the 20 numeric features in schema
order, in the requested ID order. The `warm_start_schema` attribute
records the schema identifier. Document-level undefined values remain
`NA`.

## Details

Extraction requires optional reticulate and the audited Python 3.12.3
stack. See
[`warm_start_python_status()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_python_status.md)
and the installed setup instructions located by
`system.file("python", "README.md", package = "pairwiseLLM")`. Software
and resources must be installed explicitly before extraction. Package
loading, schema inspection, and later prediction from precomputed
features do not require Python. The tested environment is Linux x86_64;
Windows and macOS have not been validated. Features are not evidence of
predictive validity.

The default English spaCy model, resource contents and package versions
are checked before extraction. Missing resources fail without downloads.
Entropy is divided by all spaCy tokens, including punctuation and
whitespace, rather than the filtered `n_tokens` feature. Undefined
values and valid zeros follow
[`warm_start_feature_schema()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_feature_schema.md).
Zero-vector coherence preserves upstream values and warnings. No feature
is imputed or replaced with zero.

## See also

[`warm_start_python_status()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_python_status.md),
[`warm_start_feature_schema()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_feature_schema.md),
[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md)

Other adaptive warm start:
[`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md),
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
if (FALSE) { # \dontrun{
# Explicitly provision the documented environment first.
extract_warm_start_features(c("a", "b"), c("A short text.", "Another text."),
  python = "/path/to/venv/bin/python")
} # }
```
