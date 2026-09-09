# Register, inspect, or remove user warm-start models

Register, inspect, or remove user warm-start models

## Usage

``` r
register_warm_start_model(model, name, overwrite = FALSE)

remove_warm_start_model(name)

list_warm_start_models(source = c("all", "user", "bundled"))
```

## Arguments

- model:

  A valid
  [pairwiseLLM_warm_model](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM_warm_model.md)
  or
  [`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md)
  ensemble.

- name:

  Model registry name, separate from the assessment task ID.

- overwrite:

  Explicitly replace an existing user entry. Default FALSE.

- source:

  Which registries to list.

## Value

Registration/removal invisibly return the entry path. Listing returns a
tibble with name, source, path, version, format_version, schema, target,
n, calibration, audit_status, size_bytes, metadata, and validation.
Metadata and validation are list columns; unspecified metadata versions
are NA character values. Additional columns `artifact_type` and
`component_count` distinguish ensembles. Ensemble n is NA (no pooled
sample size), calibration is component_oof_linear, and audit status is
full, summary_only, or mixed. Ensemble validation contains named
component metrics, not ensemble-performance estimates.

## Details

User models live in the `models` subdirectory of
`tools::R_user_dir("pairwiseLLM", "data")`. Only explicit registration
creates this directory. Names are trimmed, ASCII-lowercased, and
spaces/underscores become hyphens. The result must contain alphanumeric
segments separated by single hyphens. Dots, path separators, traversal,
and escaping symlinks are rejected. The normalized name determines
collisions and the `<name>.rds` file.

Installed bundled models are read-only `models/<name>.rds` resources.
Both sources use the same model validator and prediction method. Listing
reads and validates artifacts without glmnet or Python; corrupt entries
produce errors naming their paths. Missing registries return empty
results and are not created. Bundled lookup/listing additionally require
manifest version 1, matching file inventory, MD5 checksum, size, and
artifact metadata. Checksums detect changes; they do not authenticate
publishers. User artifacts do not require a manifest.

Registration preserves full audit evidence unless explicitly reduced
beforehand. Compressed files replace entries only with explicit
overwrite; no backup history accumulates. Use listing and removal to
manage obsolete user models. Models are not automatically removed based
on age. Removal never affects bundled models. Tests and examples must
redirect `R_USER_DATA_DIR` to a temporary directory. No user models are
written into the installed package tree.

## See also

[`save_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/save_warm_start_model.md),
[`load_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/save_warm_start_model.md),
[`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md)

Other adaptive warm start:
[`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md),
[`extract_warm_start_features()`](https://shmercer.github.io/pairwiseLLM/reference/extract_warm_start_features.md),
[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md),
[`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md),
[`pairwiseLLM_warm_model`](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM_warm_model.md),
[`predict.pairwiseLLM_warm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_ensemble.md),
[`predict.pairwiseLLM_warm_model()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_model.md),
[`prepare_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_warm_start_model.md),
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
    registry <- withr::local_tempdir()
    withr::local_envvar(c(R_USER_DATA_DIR = registry))
    register_warm_start_model(model, name = "example")
    list_warm_start_models(source = "user")
    restored <- load_warm_start_model(name = "example", source = "user")
    remove_warm_start_model("example")
  })
}
```
