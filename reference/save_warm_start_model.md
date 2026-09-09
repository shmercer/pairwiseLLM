# Save or load a portable warm-start model

Save or load a portable warm-start model

## Usage

``` r
save_warm_start_model(model, path, overwrite = FALSE)

load_warm_start_model(
  path = NULL,
  name = NULL,
  source = c("auto", "user", "bundled")
)
```

## Arguments

- model:

  A valid
  [pairwiseLLM_warm_model](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM_warm_model.md)
  or
  [`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md)
  ensemble.

- path:

  Explicit file path. Its parent must already exist when saving.

- overwrite:

  Allow replacement of an existing artifact. Default FALSE.

- name:

  Registered model name, mutually exclusive with `path`.

- source:

  Registry to search. `auto` errors if user and bundled names collide.

## Value

Saving invisibly returns the normalized destination path. Loading
returns the validated model, unchanged from its serialized
representation.

## Details

Artifacts are compressed RDS objects, without an envelope or serialized
glmnet engine. Format versions 1 (full audit) and 2 (explicit
summary-only) are supported, independently of package version. Ensembles
use their own format 1 and may contain either supported single-model
format. Use
[`prepare_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_warm_start_model.md)
to add metadata or explicitly omit audit records before saving. Saving
never strips records or adds timestamps. Neither loading nor prediction
from precomputed features needs glmnet or Python.

Exactly one of `path` or `name` is required for loading. Positional
input means a path; a missing file never falls back to a registry
search. Explicit paths require `source = "auto"`. Registry names follow
[`register_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/register_warm_start_model.md).
Named bundled lookup verifies the installed manifest and artifact
checksum. Explicit file paths use ordinary artifact validation without a
manifest. Load only trusted RDS files; contract validation is not a
serialization sandbox.

Writes are staged in the destination directory and validated before
publishing. Failed writes clean up staging files. Replacement uses
filesystem rename; if the platform cannot replace an existing file this
way, the operation fails and leaves that file intact. No persistent
backup history is created.

## See also

[`prepare_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_warm_start_model.md),
[`register_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/register_warm_start_model.md),
[`list_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/register_warm_start_model.md)

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
    path <- tempfile(fileext = ".rds")
    on.exit(unlink(path), add = TRUE)
    save_warm_start_model(model, path = path)
    restored <- load_warm_start_model(path = path)
    predict(restored, features)
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
