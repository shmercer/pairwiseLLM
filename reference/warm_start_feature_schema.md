# Inspect the frozen warm-start writing feature schema

Warm-start prediction uses text features to predict relative writing
quality before collecting pairwise comparisons. This function lists the
frozen predictor definitions; it does not extract features or require
Python.

## Usage

``` r
warm_start_feature_schema(schema = "writing_features_v1")
```

## Arguments

- schema:

  A single schema identifier. Currently only `"writing_features_v1"` is
  supported.

## Value

A tibble with one row per feature in fixed predictor order. Columns:

- `schema`, `position`, `feature`: version, integer order, and canonical
  name.

- `source_package`, `source_version`, `component`, `upstream_field`:
  audited upstream mapping.

- `family`, `type`, `unit`, `interpretation`: meaning of the feature.

- `configuration`, `definition`, `requirements`, `missing_behavior`:
  fixed calculation settings, formula, capabilities, and undefined
  values.

- `source_url`: upstream implementation reference.

## Details

Version 1 describes English writing using TextDescriptives and a
supplementary textstat readability measure. The schema records
definitions, not evidence of predictive validity for any particular
writing population.

`upstream_entropy_per_token` divides TextDescriptives' entropy by the
number of all spaCy tokens, including punctuation and whitespace tokens.
This is an average of upstream probability-weighted contributions, not
normalized document Shannon entropy or language-model cross-entropy. Its
denominator differs from the filtered `n_tokens` feature. Zero tokens
give a missing value.

Definitions, settings, membership, and ordering are frozen. Changes
require a new schema identifier rather than silently modifying
version 1. Optional Python software is needed only for extraction, not
schema inspection or prediction from precomputed features.

## See also

[`extract_warm_start_features()`](https://shmercer.github.io/pairwiseLLM/reference/extract_warm_start_features.md),
[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md)

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
[`warm_start_coefficients()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_coefficients.md),
[`warm_start_python_status()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_python_status.md)

## Examples

``` r
features <- warm_start_feature_schema()
features[c("position", "feature", "family")]
#> # A tibble: 20 × 3
#>    position feature                                family                       
#>       <int> <chr>                                  <chr>                        
#>  1        1 n_tokens                               length_productivity          
#>  2        2 proportion_unique_tokens               lexical_diversity            
#>  3        3 token_length_mean                      lexical_surface_complexity   
#>  4        4 token_length_std                       lexical_surface_complexity   
#>  5        5 sentence_length_mean                   sentence_syntactic_complexity
#>  6        6 sentence_length_std                    sentence_syntactic_complexity
#>  7        7 pos_prop_noun                          pos_composition              
#>  8        8 pos_prop_verb                          pos_composition              
#>  9        9 pos_prop_adj                           pos_composition              
#> 10       10 pos_prop_adv                           pos_composition              
#> 11       11 pos_prop_pron                          pos_composition              
#> 12       12 pos_prop_adp                           pos_composition              
#> 13       13 pos_prop_cconj                         pos_composition              
#> 14       14 pos_prop_sconj                         pos_composition              
#> 15       15 dependency_distance_mean               dependency_characteristics   
#> 16       16 dependency_distance_std                dependency_characteristics   
#> 17       17 prop_adjacent_dependency_relation_mean dependency_characteristics   
#> 18       18 upstream_entropy_per_token             information_theory           
#> 19       19 first_order_coherence                  semantic_coherence           
#> 20       20 dale_chall_readability_score           readability                  
```
