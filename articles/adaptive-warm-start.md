# Guide: Adaptive Warm Start

## What warm-start prediction does

A **warm-start prediction** estimates relative writing quality before
comparisons are collected. A **task-specific sub-model** learns from one
assessment’s texts or features and its Bradley–Terry–Luce (BTL) scores.
An **ensemble** averages calibrated predictions from two or more
separately trained task models. **Prior calibration** uses out-of-fold
(OOF) predictions to map model output to standardized quality;
[`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md)
then centers those values for Bayesian BTL estimation. The prior SD is a
separate, explicitly chosen quantity, defaulting to 0.5.

Each sub-model standardizes its own outcome using its training mean and
sample SD. Independent BT/BTL scales do not need linking: raw unlinked
scores are never pooled. Model compatibility cannot establish that
training assessments are independent or that predictions are valid in a
new population. Validate the intended domain.

Predictive priors do not change the adaptive pair-selection algorithm or
its initial pairing schedule. Without a predictive prior, existing
cold-start behavior is preserved. **No default predictive model is
bundled in version 1.3.2.** Real bundled models remain deferred; the
examples below use synthetic user models.

## Optional extraction environment and frozen features

Schema inspection is available without Python:

``` r

schema <- warm_start_feature_schema()
schema[c("position", "feature", "family")]
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
system.file("python", "README.md", package = "pairwiseLLM")
#> [1] "/home/runner/work/_temp/Library/pairwiseLLM/python/README.md"
```

The installed README gives explicit user-run setup and CMUdict resource
verification instructions. There is no public installer. Package
loading, examples and prediction never create environments or download
software. A Python installation needs `venv` and `pip`/`ensurepip`
support; maintainer bootstrap instructions are separate from runtime
extraction.

The tested stack is Python 3.12.3, TextDescriptives 2.8.4, textstat
0.7.13, spaCy 3.7.5 and `en_core_web_lg` 3.7.1. The full lock and
resource provenance are installed under `python/`. Runtime
version/resource checks reject incompatible stacks with an actionable
message. Linux testing does not establish Windows/macOS compatibility.

Select an existing interpreter explicitly before initialization. A
conflicting `RETICULATE_PYTHON` setting or already initialized
interpreter requires correcting the configuration or restarting R. A
status call can initialize Python and load the large language model; it
is not a prerequisite for precomputed prediction. These commands are
shown without execution because they require your environment:

``` r

python <- "/path/to/venv/bin/python"
status <- warm_start_python_status(python = python)
status$problems
features <- extract_warm_start_features(
  ids = c("a", "b"), texts = c("First response.", "Another response."), python = python
)
```

The returned table has character `item_id`, 20 numeric features in
frozen order, and
`attr(features, "warm_start_schema") == "writing_features_v1"`. Missing
or duplicate IDs, missing required columns, and incompatible schemas
fail explicitly. Use RDS to preserve attributes. If a cache format drops
attributes, restore the known schema explicitly after verifying its
origin; the attribute alone is not verified extraction provenance.
Required features are never replaced with zero.

The schema is curated from constructs and upstream definitions, not
training outcomes. See
[`warm_start_feature_schema()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_feature_schema.md)
for each feature’s definition and missingness rules. In particular,
`upstream_entropy_per_token` divides upstream probability-weighted
entropy by **all** spaCy tokens, including punctuation/whitespace. It is
neither per-word perplexity nor conventional Shannon entropy, and does
not use filtered `n_tokens` as its denominator. Tokenization,
zero-vector coherence and undefined values follow the frozen upstream
contract; upgrades cannot silently redefine v1.

## Develop one model per assessment

The following deterministic data are fabricated to illustrate the public
interface. They are not extracted student features. Chunks needing
glmnet/withr are skipped when those optional packages are unavailable.

``` r

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

summary(model)
#> $task_id
#> [1] "synthetic-a"
#> 
#> $target
#> $target$definition
#> [1] "within_task_z"
#> 
#> $target$mean
#> [1] 4.784082
#> 
#> $target$sd
#> [1] 6.397207
#> 
#> $target$sd_convention
#> [1] "sample"
#> 
#> 
#> $n
#> [1] 15
#> 
#> $schema
#> [1] "writing_features_v1"
#> 
#> $retained_predictors
#> [1] 20
#> 
#> $removed_predictors
#> named character(0)
#> 
#> $nonzero_coefficients
#> [1] 2
#> 
#> $alpha
#> [1] 1
#> 
#> $lambda
#> [1] 0.02274379
#> 
#> $calibration
#> [1] "oof_linear"
#> 
#> $audit_status
#> [1] "full"
#> 
#> $validation
#> $validation$pearson_r
#> [1] 0.9897764
#> 
#> $validation$squared_pearson_r
#> [1] 0.9796574
#> 
#> $validation$spearman_rho
#> [1] 0.9535714
#> 
#> $validation$rmse
#> [1] 0.1600619
#> 
#> $validation$mae
#> [1] 0.08329452
#> 
#> $validation$calibration_intercept
#> [1] -0.04764372
#> 
#> $validation$calibration_slope
#> [1] 0.9501558
#> 
#> $validation$undefined_reasons
#> character(0)
```

For real data, replace the synthetic table and `theta` with aligned item
IDs, precomputed features, and finite BT/BTL scores from **one**
assessment. Alternatively, pass `texts` and `python` instead of
`features`; extraction runs once before fitting. The `task_id` labels
assessment provenance, not a registry name or grouping column.

Default validation is nested five outer by five inner folds, with seed 1
and 41 alpha candidates `seq(0, 1, by = 0.025)` spanning ridge through
lasso. The example uses two alphas solely to shorten execution. Folds
are shared across alpha candidates. Insufficient data produces an error;
fold counts are not silently reduced.

Every applicable training fold learns missingness filtering (\>20%
missing or all missing), median imputation, constant/near-zero-variance
removal, and sample-SD scaling. Near-zero variance requires unique
fraction \<=10% and frequency ratio \>19. Outcome scaling uses the
corresponding outer training set throughout its inner fits and held-out
scoring. No PCA or univariate screening is introduced when p \> n; this
example has 20 candidate features and 15 observations. See
[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md)
for exact reference-path, weighted-loss, SE and tie rules. The default
lambda is the largest within one SE of the selected alpha’s minimum;
`lambda_rule = "lambda.min"` is an explicit expert override.

OOF calibration learns an intercept/slope from predictions whose
coefficient fits excluded those rows. These folds also select
hyperparameters, so calibration-fit statistics are not independent
validation. Calibration for an outer holdout uses only its outer
training data. Final deployment uses full-data OOF calibration and an
all-row coefficient refit. Degenerate calibration errors; finite
negative slopes are allowed, and undefined validation diagnostics have
NA values with reasons.

``` r

model$validation$metrics
#> $pearson_r
#> [1] 0.9897764
#> 
#> $squared_pearson_r
#> [1] 0.9796574
#> 
#> $spearman_rho
#> [1] 0.9535714
#> 
#> $rmse
#> [1] 0.1600619
#> 
#> $mae
#> [1] 0.08329452
#> 
#> $calibration_intercept
#> [1] -0.04764372
#> 
#> $calibration_slope
#> [1] 0.9501558
#> 
#> $undefined_reasons
#> character(0)
head(model$validation$predictions)
#>   item_id fold   observed raw_prediction calibrated_prediction
#> 1       1    2  0.5053469      0.7107407             1.0870880
#> 2       2    5  0.1117938      0.1428030             0.1646139
#> 3       3    3  0.1253086      0.1445398             0.1602008
#> 4       4    5 -1.8680692     -1.7816206            -1.9146367
#> 5       5    1  0.7511354      0.7418718             0.7727400
#> 6       6    2 -1.9762534     -1.2776439            -1.8804273
model$training[c("alpha", "lambda", "n_nonzero")]
#> $alpha
#> [1] 1
#> 
#> $lambda
#> [1] 0.02274379
#> 
#> $n_nonzero
#> [1] 2
```

Pearson r, squared Pearson r, Spearman rho, RMSE, MAE and diagnostic
calibration intercept/slope come from outer held-out predictions. Final
refit predictions are for deployment, not a replacement for those
validation results. Outer fold records retain their own selected alpha,
lambda and nonzero counts.

## Save, register and combine task models

Model use from precomputed features needs neither glmnet nor Python.
Store the portable object, not a training-engine fit. Explicit saving
and registration are separate operations; ordinary save/load preserves
the complete audit.

``` r

path <- tempfile(fileext = ".rds")
save_warm_start_model(model, path = path)
restored <- load_warm_start_model(path = path)
stopifnot(identical(predict(restored, features), predict(model, features)))
unlink(path)

local({
  # Keep this executable example out of the real user registry.
  withr::local_envvar(c(R_USER_DATA_DIR = withr::local_tempdir()))
  register_warm_start_model(model, name = "example")
  list_warm_start_models(source = "user")
  registered <- load_warm_start_model(name = "example", source = "user")
  remove_warm_start_model("example")
})
```

Real registrations use `tools::R_user_dir("pairwiseLLM", "data")` under
`models`. Names normalize to lowercase hyphen-separated identifiers;
collisions require explicit overwrite. Loading positionally always means
a path. Same-name user and bundled entries require an explicit source.
List/remove operations manage obsolete entries; no automatic backup
history accumulates. Bundles are read-only.

Fit another independently generated synthetic assessment on its own
scale, then combine the models with stable component names. Actual
training assessments must be independent; relabeling the same training
data does not establish independence.

``` r

features_b <- example_features(3104)
theta_b <- 30 + features_b$n_tokens - 3 * features_b$token_length_mean
model_b <- fit_warm_start_model(features_b$item_id, theta_b, "synthetic-b",
  features = features_b, alpha_grid = c(0, 1))
ensemble <- ensemble_warm_start_models(assessment_a = model, assessment_b = model_b)

predictions <- predict(ensemble, features)
predictions
#> Warm-start ensemble predictions; sample SD is diagnostic, not Bayesian prior SD.
#> # A tibble: 15 × 5
#>    item_id component_assessment_a component_assessment_b ensemble_mean
#>    <chr>                    <dbl>                  <dbl>         <dbl>
#>  1 1                        0.553                  0.848        0.700 
#>  2 2                        0.199                  0.563        0.381 
#>  3 3                        0.134                  0.543        0.338 
#>  4 4                       -1.73                  -1.13        -1.43  
#>  5 5                        0.825                  1.26         1.04  
#>  6 6                       -1.82                  -1.13        -1.48  
#>  7 7                        0.896                  1.40         1.15  
#>  8 8                        0.874                  1.43         1.15  
#>  9 9                        0.961                  1.55         1.25  
#> 10 10                      -1.25                  -0.446       -0.847 
#> 11 11                      -0.198                  0.560        0.181 
#> 12 12                      -0.733                  0.108       -0.313 
#> 13 13                      -0.354                  0.498        0.0719
#> 14 14                       0.273                  1.11         0.693 
#> 15 15                       1.42                   2.21         1.81  
#> # ℹ 1 more variable: ensemble_sd <dbl>
summary(predictions)
#> $n
#> [1] 15
#> 
#> $components
#> [1] "assessment_a" "assessment_b"
#> 
#> $ensemble_mean
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -1.4765 -0.1204  0.3810  0.3140  1.0959  1.8149 
#> 
#> $ensemble_sd
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.2085  0.3335  0.4237  0.4391  0.5627  0.6019 
#> 
#> $sd_interpretation
#> [1] "Between-model sample SD is diagnostic, not Bayesian prior SD."
```

For a named list of models, use
`do.call(ensemble_warm_start_models, models)`. Components must share a
frozen schema and standardized outcome definition, and have learned
calibration. Different preprocessing and full/reduced audit formats are
supported. Uncalibrated components, nested ensembles and failed
components error. No automatic stacking or silent component removal
occurs.

| Quantity | Meaning |
|----|----|
| `raw_prediction` | Linear prediction on the standardized target scale |
| `calibrated_prediction` | Single-model output after its stored OOF calibration |
| `component_<name>` | Calibrated output from that ensemble component |
| `ensemble_mean` | Equal-weight mean of calibrated component outputs |
| `ensemble_sd` | Between-model sample SD, denominator k - 1; diagnostic only |
| `prior_sd` | Chosen Normal SD for raw BTL theta, default 0.5 |

Complete component prediction tables remain in attributes, including raw
predictions. Their attributes do not automatically subset with tibble
rows. Use explicit ID alignment at prior boundaries; do not assume
generic slicing updates nested metadata. For text input, ensemble
prediction extracts once using `texts`, `ids`, and `python`.
Single-model [`predict()`](https://rdrr.io/r/stats/predict.html) accepts
precomputed features only.

``` r

# Empty in this release. Future reviewed bundles use the same public prediction path.
list_warm_start_models(source = "bundled")
#> # A tibble: 0 × 15
#> # ℹ 15 variables: name <chr>, source <chr>, path <chr>, version <chr>,
#> #   format_version <int>, schema <chr>, target <chr>, n <int>,
#> #   calibration <chr>, audit_status <chr>, size_bytes <dbl>,
#> #   artifact_type <chr>, component_count <int>, metadata <list>,
#> #   validation <list>
```

When bundles become available, load an explicit advertised name with
`load_warm_start_model(name = ..., source = "bundled")`. Named lookup
verifies manifest inventory, containment, size, checksum and metadata
agreement. An MD5 checksum detects changed bytes; it does not
authenticate a publisher.

## Convert predictions to BTL priors and resume

``` r

prior <- make_warm_start_prior(predictions, ids = features$item_id)
head(data.frame(item_id = prior$item_id, mean = prior$prior_mean, sd = prior$prior_sd))
#>   item_id        mean  sd
#> 1       1  0.38643390 0.5
#> 2       2  0.06693345 0.5
#> 3       3  0.02421720 0.5
#> 4       4 -1.74359311 0.5
#> 5       5  0.72788848 0.5
#> 6       6 -1.79050843 0.5
state <- adaptive_rank_start(features$item_id, seed = 1, warm_start_prior = prior)
```

Conversion uses calibrated single predictions or ensemble means and
centers them in R. Do not inverse-transform to an original training
scale or calibrate twice. Expert numeric scores require names or
explicit IDs. IDs must match exactly; scalar SDs recycle, named SDs
align by ID, and unnamed vectors follow input order. Invalid scores/SDs
or missing items error.

The SD is for `theta_raw`. Stan centers raw theta, inducing dependence
and changing centered-theta marginal SDs. All four active variants
(`btl`, `btl_e`, `btl_b`, `btl_e_b`) use supplied priors. Cold starts
retain raw means zero and SDs one. Default adaptive refits subset saved
scores to the active fitted IDs before centering, including fitted items
without comparisons. Downstream transform, anchored-joint and pooled
judge refits retain their existing priors; evidence is not injected
twice.

Standalone sampling requires your installed CmdStan toolchain and is not
run here:

``` r

results <- build_btl_results_data(data.frame(ID1 = "1", ID2 = "2", better_id = "1"))
fit <- fit_bayes_btl_mcmc(results, ids = features$item_id, warm_start_prior = prior)
```

Instead of supplying a prior, initialize with
`warm_start_model = ensemble` and `warm_start_features = features`.
Model objects, paths and explicit loader reference lists are accepted.
Without precomputed features, supply assessment texts and select
`warm_start_python`. Prediction runs once; initialization and resume
never train. Model-specific feature/Python/SD arguments cannot accompany
an already resolved prior. Custom fit functions must consume
`state$predictive_prior`; their signatures are unchanged.

``` r

local({
  directory <- withr::local_tempdir()
  artifact <- file.path(directory, "ensemble.rds")
  save_warm_start_model(ensemble, artifact)
  session <- file.path(directory, "session")
  state <- adaptive_rank_start(features$item_id, session_dir = session,
    warm_start_model = artifact, warm_start_features = features)
  unlink(artifact)
  resumed <- adaptive_rank_resume(session)
  stopifnot(identical(resumed$predictive_prior, state$predictive_prior))
})
```

Sessions store numeric priors and compact provenance, not trained
components or nested-CV audits. Resume uses these saved values even if
the original artifact is removed or replaced, with no Python/glmnet
requirement. On resumed
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md)
calls, omit **all** warm-start arguments. Older sessions retain cold
starts. See [Guide: Adaptive
Pairing](https://shmercer.github.io/pairwiseLLM/articles/adaptive-pairing.md)
for judging and continued runs, and [Standalone Bayesian BTL with
CmdStan](https://shmercer.github.io/pairwiseLLM/articles/bayesian-btl.md)
for sampling diagnostics.

## Model artifacts and the maintainer workflow

A portable model stores schema/version and original feature order,
fitted preprocessing, named coefficients/intercept, outcome mean/sample
SD, calibration, training metadata and validation evidence. Prediction
validates the schema before applying these parameters; no glmnet object
is required.

Full-audit model format 1 retains IDs, outcomes, tuning traces and fold
predictions. `prepare_warm_start_model(model, omit_audit = TRUE)`
creates model format 2 with summary-only evidence and unchanged numeric
predictions. Summary metrics cannot be recomputed from reduced
artifacts. Ensemble format 1 is separate from its component formats, R
serialization version, package version and manifest version.

Reduction is not anonymization. Task labels, notes, provenance and
diagnostic prose still need review. Preparation time is not training
time, and supplied provenance is not verified extraction history. Load
only trusted RDS files.

In the source repository, `data-raw/warm-start/README.md` describes the
explicit maintainer build/review/promotion workflow. It uses the public
APIs to train separate assessments, keep full audits privately, reduce
deployment components, and compare predictions before/after storage.
Text builds capture extraction versions and lock hashes; cached
provenance remains supplied or unavailable. Private recursive review
checks provenance, distribution rights, validation, retained sensitive
material and compressed size before promotion. Heuristics do not
guarantee privacy. Review reports can themselves contain restricted
material and are not installed.

Only reviewed deployment artifacts and their manifest belong in
installed `models`. Training texts, full audits, staged files,
environments and third-party model binaries stay outside the package. No
general predictive-performance or size threshold is inferred from the
synthetic examples or component validation metrics.

## Citation

> Mercer, S. H. (2026). *Guide: Adaptive Warm Start* \[R package
> vignette\]. Comprehensive R Archive Network.
> <https://doi.org/10.32614/CRAN.package.pairwiseLLM>
