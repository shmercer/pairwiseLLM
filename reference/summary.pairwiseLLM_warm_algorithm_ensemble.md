# Inspect a same-task algorithm ensemble

Inspect a same-task algorithm ensemble

## Usage

``` r
# S3 method for class 'pairwiseLLM_warm_algorithm_ensemble'
summary(object, ...)

# S3 method for class 'pairwiseLLM_warm_algorithm_ensemble'
print(x, ...)
```

## Arguments

- object, x:

  A full or summary-only
  [`ensemble_warm_start_algorithms()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_algorithms.md)
  artifact.

- ...:

  Reserved; must be empty.

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns shared
identity, component summaries, audit status and honest outer ensemble
validation metrics. Summary-only metrics are explicitly labeled and
cannot be recomputed without the original audit.
[`print()`](https://rdrr.io/r/base/print.html) returns its input
invisibly.

## See also

Other adaptive warm start:
[`ensemble_warm_start_algorithms()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_algorithms.md),
[`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md),
[`extract_warm_start_features()`](https://shmercer.github.io/pairwiseLLM/reference/extract_warm_start_features.md),
[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md),
[`make_warm_start_cv_plan()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_cv_plan.md),
[`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md),
[`pairwiseLLM_warm_model`](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM_warm_model.md),
[`predict.pairwiseLLM_warm_algorithm_ensemble()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_algorithm_ensemble.md),
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
    requireNamespace("pls", quietly = TRUE) && requireNamespace("withr", quietly = TRUE)) {
  local({
    withr::local_seed(259L)
    fields <- warm_start_feature_schema()$feature
    x <- as.data.frame(matrix(runif(20 * length(fields)), nrow = 20))
    names(x) <- fields
    x$n_tokens <- 11:30
    x <- data.frame(item_id = as.character(1:20), x)
    attr(x, "warm_start_schema") <- "writing_features_v1"
    theta <- x$n_tokens - 3 * x$token_length_mean
    plan <- make_warm_start_cv_plan(x$item_id, theta, "synthetic", seed = 259L)
    en <- fit_warm_start_model(x$item_id, theta, "synthetic", features = x,
      cv_plan = plan, alpha_grid = c(0, 1))
    pls <- fit_warm_start_model(x$item_id, theta, "synthetic", features = x,
      cv_plan = plan, engine = "pls", engine_control = list(ncomp = 1L))
    ensemble <- ensemble_warm_start_algorithms(elastic_net = en, pls = pls)
    print(ensemble)
    summary(ensemble)$validation$metrics
    predictions <- predict(ensemble, x)
    make_warm_start_prior(predictions, prior_sd = 0.5)
  })
}
#> Same-task warm-start algorithm ensemble: 2 equally weighted models
#> Task: synthetic ; n = 20 ; audit: full 
#> Components: elastic_net, pls 
#> Outer-held-out ensemble RMSE: 0.4236862 
#> Between-algorithm sample SD is diagnostic, not Bayesian prior SD. 
#> $format_version
#> [1] 1
#> 
#> $item_id
#>  [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "12" "13" "14" "15"
#> [16] "16" "17" "18" "19" "20"
#> 
#> $scores
#>  [1] -1.45616200 -1.21180598 -1.14499964 -1.03020497 -0.50359799 -0.53899534
#>  [7] -0.71798234 -0.50243125 -0.24201606  0.23480938  0.16479034 -0.05085892
#> [13]  0.14447884  0.63924707  0.36820665  1.01916790  0.59406721  0.91703461
#> [19]  1.36748266  1.38345459
#> 
#> $prior_mean
#>  [1] -1.42784624 -1.18349022 -1.11668388 -1.00188921 -0.47528223 -0.51067958
#>  [7] -0.68966657 -0.47411548 -0.21370029  0.26312514  0.19310610 -0.02254316
#> [13]  0.17279461  0.66756283  0.39652241  1.04748366  0.62238297  0.94535037
#> [19]  1.39579842  1.41177035
#> 
#> $prior_sd
#>  [1] 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5
#> [20] 0.5
#> 
#> $diagnostics
#>    component_elastic_net component_pls ensemble_sd
#> 1             -1.7995551    -1.1127689  0.48563122
#> 2             -1.3917568    -1.0318552  0.25448883
#> 3             -1.1699304    -1.1200689  0.03525743
#> 4             -1.2471186    -0.8132913  0.30676225
#> 5             -0.8471727    -0.1600233  0.48588801
#> 6             -0.6916026    -0.3863881  0.21581929
#> 7             -0.5304277    -0.9055370  0.26524230
#> 8             -0.4440361    -0.5608264  0.08258323
#> 9             -0.1777378    -0.3062943  0.09090321
#> 10            -0.2627908     0.7324096  0.70371299
#> 11             0.2240133     0.1055673  0.08375397
#> 12             0.1612560    -0.2629739  0.29997581
#> 13             0.4960284    -0.2070707  0.49716617
#> 14             0.7552469     0.5232472  0.16404857
#> 15             0.5955082     0.1409051  0.32145298
#> 16             0.8880054     1.1503304  0.18549183
#> 17             1.0155563     0.1725782  0.59607554
#> 18             1.4435228     0.3905464  0.74456673
#> 19             1.4715164     1.2634490  0.14712585
#> 20             1.4236725     1.3432367  0.05687673
#> 
#> $provenance
#> $provenance$source
#> [1] "predictions"
#> 
#> $provenance$schema
#> [1] "writing_features_v1"
#> 
#> $provenance$model
#> $provenance$model$artifact_type
#> [1] "algorithm_ensemble"
#> 
#> $provenance$model$format_version
#> [1] 1
#> 
#> $provenance$model$outcome_definition
#> [1] "within_task_z"
#> 
#> $provenance$model$weighting
#> [1] "equal"
#> 
#> $provenance$model$metadata
#> NULL
#> 
#> $provenance$model$components
#> $provenance$model$components$elastic_net
#> $provenance$model$components$elastic_net$format_version
#> [1] 3
#> 
#> $provenance$model$components$elastic_net$task_id
#> [1] "synthetic"
#> 
#> $provenance$model$components$elastic_net$outcome_definition
#> [1] "within_task_z"
#> 
#> $provenance$model$components$elastic_net$calibration_status
#> [1] "oof_linear"
#> 
#> $provenance$model$components$elastic_net$engine
#> [1] "glmnet"
#> 
#> $provenance$model$components$elastic_net$engine_version
#> [1] "5.0"
#> 
#> $provenance$model$components$elastic_net$cv_digest
#> [1] "d34bdd49b449179566091e9ca4e9b27b"
#> 
#> 
#> $provenance$model$components$pls
#> $provenance$model$components$pls$format_version
#> [1] 3
#> 
#> $provenance$model$components$pls$task_id
#> [1] "synthetic"
#> 
#> $provenance$model$components$pls$outcome_definition
#> [1] "within_task_z"
#> 
#> $provenance$model$components$pls$calibration_status
#> [1] "oof_linear"
#> 
#> $provenance$model$components$pls$engine
#> [1] "pls"
#> 
#> $provenance$model$components$pls$engine_version
#> [1] "2.9.0"
#> 
#> $provenance$model$components$pls$cv_digest
#> [1] "d34bdd49b449179566091e9ca4e9b27b"
#> 
#> 
#> 
#> $provenance$model$cv_identity
#> $provenance$model$cv_identity$format_version
#> [1] 1
#> 
#> $provenance$model$cv_identity$digest
#> [1] "d34bdd49b449179566091e9ca4e9b27b"
#> 
#> $provenance$model$cv_identity$task_id
#> [1] "synthetic"
#> 
#> $provenance$model$cv_identity$n
#> [1] 20
#> 
#> $provenance$model$cv_identity$outcome_digest
#> [1] "d3287c5bd3f09197a2e1dac8e8bd8f45"
#> 
#> $provenance$model$cv_identity$seed
#> [1] 259
#> 
#> $provenance$model$cv_identity$outer_folds
#> [1] 5
#> 
#> $provenance$model$cv_identity$inner_folds
#> [1] 5
#> 
#> $provenance$model$cv_identity$rng_kind
#> [1] "Mersenne-Twister" "Inversion"        "Rejection"       
#> 
#> 
#> 
#> $provenance$sd_rule
#> [1] "scalar"
#> 
#> 
#> $digest
#> [1] "5bae688cb1315d2106159b013b3e01ab"
#> 
#> attr(,"class")
#> [1] "pairwiseLLM_warm_prior"
```
