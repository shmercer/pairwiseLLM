# Guide: Adaptive Warm Start

## Before you start

Warm start is optional; skip it for your first adaptive run. Return here
when you have defensible prior scores or a prediction model developed
from earlier assessments. Start with [Adaptive
Pairing](https://shmercer.github.io/pairwiseLLM/articles/adaptive-pairing.md)
for the ranking workflow itself.

**You get:** starting scores for Bayesian Bradley–Terry–Luce (BTL),
TrueSkill, or both. They give the models initial information about
relative writing quality. Observed comparisons still drive the
assessment; a warm start does not guarantee better rankings or fewer
comparisons.

The first example uses numeric scores and the optional `withr` package.
The longer example uses fabricated texts, already extracted features,
and invented outcomes. It runs locally when the optional `glmnet`,
`pls`, `e1071`, and `withr` packages are installed. Otherwise, its
training and dependent chunks are skipped. Python is needed only to
repeat text extraction; CmdStan is needed for later Bayesian fitting,
not initialization. No example installs software or contacts a
comparison provider.

**No pretrained predictive model is bundled.** The example data
demonstrate the workflow, not the accuracy of a model for students or a
new assessment.

## Choose where starting scores are used

[`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md)
converts predictions into ID-aligned starting information. It centers
the scores by subtracting their mean. The **prior standard deviation
(SD)** is a separate choice: smaller values put more confidence in the
starting scores. The default of 0.5 controls the raw BTL prior; it is
not the uncertainty of the final centered scores. It is not learned from
model accuracy or disagreement.

`warm_start_mode` chooses which model receives this information:

| Mode | BTL predictive prior | TrueSkill predictive `mu` | Default connected bootstrap |
|----|----|----|----|
| `cold` | no | no | seeded shuffled chain |
| `btl_only` | yes | no | same seeded shuffled chain |
| `trueskill_only` | no | yes | same seeded shuffled chain |
| `both` | yes | yes | same seeded shuffled chain |

The bootstrap is the initial connected set of observed comparisons. It
is the same for all modes under the default
`bootstrap_policy = "shuffled_connected"` when the items and seed are
the same. Later pair choices can differ as TrueSkill updates from its
starting values and the observed judgments.

With no predictive input, the default mode is `cold`. With predictive
input, the default is `btl_only`. Request `both` explicitly to
initialize both models. The names below connect each starting score to
its item:

``` r

locations <- c(a = -1, b = -0.25, c = 0.25, d = 1)
numeric_prior <- make_warm_start_prior(locations)
historical <- adaptive_rank_start(names(locations), seed = 17L,
  warm_start_prior = numeric_prior)
warmed <- adaptive_rank_start(names(locations), seed = 17L,
  warm_start_prior = numeric_prior, warm_start_mode = "both")
historical$meta$warm_start_mode
#> [1] "btl_only"
warmed$trueskill_state$items[, c("item_id", "mu", "sigma")]
#> # A tibble: 4 × 3
#>   item_id    mu sigma
#>   <chr>   <dbl> <dbl>
#> 1 a        16.7  8.33
#> 2 b        22.9  8.33
#> 3 c        27.1  8.33
#> 4 d        33.3  8.33
```

The output shows the saved mode and TrueSkill’s initial locations (`mu`)
and uncertainties (`sigma`). TrueSkill maps centered scores to its own
scale as `25 + (25/3) * prior_mean`. By default, its initial sigma
remains `25/3` (or the constructor-supplied value); BTL prior SD does
not set TrueSkill uncertainty. Both models can subsequently learn from
judgments.

### Opt in to predictive uncertainty

Use `warm_start_trueskill = "predictive_distribution"` with
`trueskill_only` or `both` to initialize the entire TrueSkill
distribution. For each matching item ID, `mu = 25 + (25/3) * prior_mean`
and `sigma = (25/3) * prior_sd`, with `beta = 25/6`. The mapping
overrides constructor means and sigmas and never clips SD.

``` r

# Illustrative synthetic SD values; this example does not establish calibration.
distribution_prior <- make_warm_start_prior(c(a = -1, b = 0, c = 1),
  prior_sd = c(c = 0.8, a = 0.2, b = 0.5))
distribution_state <- adaptive_rank_start(c("c", "a", "b"), seed = 11,
  warm_start_prior = distribution_prior, warm_start_mode = "both",
  warm_start_trueskill = "predictive_distribution")
distribution_state$trueskill_state$items[, c("item_id", "mu", "sigma")]
#> # A tibble: 3 × 3
#>   item_id    mu sigma
#>   <chr>   <dbl> <dbl>
#> 1 c        33.3  6.67
#> 2 a        16.7  1.67
#> 3 b        25    4.17
distribution_state$meta$trueskill_mapping
#> $format_version
#> [1] 1
#> 
#> $policy
#> [1] "predictive_distribution"
#> 
#> $warm_start_mode
#> [1] "both"
#> 
#> $mu_offset
#> [1] 25
#> 
#> $scale
#> [1] 8.333333
#> 
#> $beta
#> [1] 4.166667
#> 
#> $clipping
#> [1] "none"
#> 
#> $sd_source
#> [1] "prior_object"
#> 
#> $sd_rule
#> [1] "per_item"
#> 
#> $calibration
#> [1] "upstream_not_verified"
#> 
#> $predictive_prior_digest
#> [1] "99312d5deb3fc84cce0f49a3eeefefe6"
#> 
#> $item_id
#> [1] "c" "a" "b"
#> 
#> $distribution_digest
#> [1] "2833810099643bab99132e6f9814ce88"
#> 
#> $digest
#> [1] "1ac44a08bd262bb4029e128c07e40882"
```

With `warm_start_model`, this opt-in requires explicit
`warm_start_prior_sd`; it never silently substitutes the historical 0.5
default. Named vectors align by exact item ID; unnamed vectors follow
prediction-input order. Prebuilt priors use their stored SD. A scalar
remains available for sensitivity analyses but is not essay-specific
calibration. Legacy modes retain their existing 0.5 fallback.

The upstream predictive workflow must establish uncertainty calibration
using training data only. Explicit provision alone does not prove
calibration, and raw ensemble disagreement is not calibrated predictive
uncertainty. Saved metadata records the SD source, scalar/per-item rule,
mapping version and parameters, and integrity digests; calibration
remains marked as upstream, unverified.

This convention assumes transformed BTL prior SD and TrueSkill
uncertainty are on comparable latent scales. BTL places the prior on
`theta_raw`, then centers effects for identifiability; centering induces
dependence. The mapped sigma is therefore not a marginal SD of centered
BTL effects, and the two models’ posteriors are not equivalent. BTL
prior and identifiability rules remain unchanged. `trueskill_only` still
leaves BTL cold; `both` also warms BTL. The mapping does not depend on
the graph or later selection strategy. Changing the distribution alone
retains the default shuffled bootstrap.

Resume retains the saved mapping and evolved ratings without
initializing again. Omit `warm_start_trueskill` along with all other
warm-start arguments on resume.

## Choose the initial graph independently

`bootstrap_policy = "shuffled_connected"` preserves historical
initialization. Use `"predictive_connected"` to choose the initial
connected tree from the frozen predictive TrueSkill distribution. This
requires `warm_start_trueskill = "predictive_distribution"`, mode
`trueskill_only` or `both`, a replay reservoir, ordinary `within_set`
mode, and explicit `pairing_strategy = "trueskill_pollitt"`. Linking and
Phase B are unsupported for this graph policy.

Create the reservoir from **selectable primary observations only**.
Exclude held-out edges and separate reversal audits before calling
[`make_adaptive_replay_reservoir()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_replay_reservoir.md).
The package validates the supplied manifest; it cannot infer which
observations your study designated as held out.

The tree builder receives only allowed A/B endpoints, frozen initial
means/SDs, and a seed. It favors probabilities near 1/3 or 2/3, starts
with degree cap two, and doubles that cap only when needed to connect
the graph. Exact-score ties use a local seeded permutation. It builds
once before judging, preserves recorded A/B orientation, and never reads
outcomes or evolving ratings. After `N - 1` successful bootstrap
comparisons, the existing Pollitt selector continues using current
ratings and its existing degree rules.

These five configurations separate estimation, selection state, and
graph effects:

| Arm | `warm_start_mode` | Predictive prior | `warm_start_trueskill` | `bootstrap_policy` |
|----|----|----|----|----|
| Cold | `cold` | omitted | `NULL` | `shuffled_connected` |
| Estimation warm | `btl_only` | supplied | `NULL` | `shuffled_connected` |
| Legacy-graph coherent | `both` | supplied | `predictive_distribution` | `shuffled_connected` |
| Selection warm | `trueskill_only` | supplied | `predictive_distribution` | `predictive_connected` |
| Fully coherent | `both` | supplied | `predictive_distribution` | `predictive_connected` |

All five use the same selectable reservoir, seed, and Pollitt selector.
Legacy-graph coherent and fully coherent have identical predictive BTL
priors and initial TrueSkill means **and SDs**; only graph policy
changes. The word “legacy” here describes the graph, not historical
fixed-sigma initialization.

This small example uses invented scores, SDs, and outcomes. They
demonstrate the API and do not establish predictive calibration or
improved ranking accuracy.

``` r

bootstrap_ids <- letters[1:6]
bootstrap_observations <- data.frame(
  A_id = c("b", "c", "d", "e", "f", "a", "d", "b", "f"),
  B_id = c("a", "b", "c", "d", "e", "f", "a", "e", "c"),
  Y = c(1L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 1L))
bootstrap_reservoir <- make_adaptive_replay_reservoir(
  bootstrap_observations, bootstrap_ids)
bootstrap_prior <- make_warm_start_prior(
  setNames(c(-1, -0.6, -0.2, 0.2, 0.6, 1), bootstrap_ids),
  prior_sd = setNames(c(0.2, 0.7, 0.3, 0.8, 0.4, 0.6), bootstrap_ids))
bootstrap_modes <- c(cold = "cold", estimation = "btl_only",
  legacy_graph = "both", selection = "trueskill_only", full = "both")
bootstrap_states <- lapply(names(bootstrap_modes), function(arm) {
  adaptive_rank_start(bootstrap_ids, seed = 87L,
    replay_reservoir = bootstrap_reservoir,
    warm_start_mode = bootstrap_modes[[arm]],
    warm_start_prior = if (arm == "cold") NULL else bootstrap_prior,
    warm_start_trueskill = if (arm %in% c("cold", "estimation")) NULL else
      "predictive_distribution",
    bootstrap_policy = if (arm %in% c("selection", "full"))
      "predictive_connected" else "shuffled_connected",
    adaptive_config = list(pairing_strategy = "trueskill_pollitt"))
})
names(bootstrap_states) <- names(bootstrap_modes)
stopifnot(identical(bootstrap_states$selection$warm_start_pairs,
  bootstrap_states$full$warm_start_pairs))
stopifnot(identical(bootstrap_states$legacy_graph$trueskill_state,
  bootstrap_states$full$trueskill_state))
bootstrap_judge <- make_adaptive_judge_replay(bootstrap_reservoir)
bootstrap_partial <- adaptive_rank_run_live(bootstrap_states$full,
  bootstrap_judge, n_steps = 2L, progress = "none")
summarize_adaptive(bootstrap_partial, include_bootstrap = TRUE)$bootstrap[[1]]
#> $policy
#> [1] "predictive_connected"
#> 
#> $version
#> [1] 1
#> 
#> $seed
#> [1] 87
#> 
#> $digest
#> [1] "6710d0337e006fe5b0f7319e12877b3a3cb0084ca0860a40989e29f834a842fe"
#> 
#> $trueskill_mapping_digest
#> [1] "20c80bcbb6d2c827f54bc6ce51bbb33a"
#> 
#> $manifest_digest
#> [1] "5bd49db4fb30374d7e7f8804b01cdf33"
#> 
#> $diagnostics
#> $diagnostics$policy
#> $diagnostics$policy$version
#> [1] 1
#> 
#> $diagnostics$policy$targets
#> [1] 0.3333333 0.6666667
#> 
#> $diagnostics$policy$initial_degree_cap
#> [1] 2
#> 
#> $diagnostics$policy$degree_cap_multiplier
#> [1] 2
#> 
#> $diagnostics$policy$ties
#> [1] "canonical_seeded_permutation_v1"
#> 
#> 
#> $diagnostics$seed
#> [1] 87
#> 
#> $diagnostics$relaxations
#> # A tibble: 0 × 4
#> # ℹ 4 variables: old_cap <dbl>, new_cap <dbl>, selected_edges <int>,
#> #   components <int>
#> 
#> $diagnostics$degrees
#> # A tibble: 6 × 2
#>   item_id degree
#>   <chr>    <int>
#> 1 a            1
#> 2 b            2
#> 3 c            2
#> 4 d            2
#> 5 e            2
#> 6 f            1
#> 
#> $diagnostics$degree_histogram
#> # A tibble: 2 × 2
#>   degree n_items
#>    <int>   <int>
#> 1      1       2
#> 2      2       4
#> 
#> $diagnostics$operations
#> $diagnostics$operations$probability_evaluations
#> [1] 9
#> 
#> $diagnostics$operations$candidate_visits
#> [1] 5
#> 
#> $diagnostics$operations$component_checks
#> [1] 5
#> 
#> $diagnostics$operations$passes
#> [1] 1
```

An invalid judgment leaves queue progress and reservoir consumption
unchanged; retry uses the same ordered edge. `n_steps` counts attempts.
When comparing arms at equal budgets, count committed comparisons with
`summarize_adaptive(state)$committed_pairs`, not attempted requests.

Predictive sessions save their initial distribution, mapping identity,
tree policy/version, seed, ordered queue, diagnostics, and SHA-256
digest. Session metadata repeats the policy/version and digest. Resume
validates these frozen artifacts without predicting, initializing
ratings, or rebuilding the tree. Keep the original reservoir to recreate
its judge; changing its membership, orientation, or outcomes is
rejected.

``` r

bootstrap_session <- tempfile("predictive-bootstrap-")
save_adaptive_session(bootstrap_partial, bootstrap_session)
bootstrap_restored <- adaptive_rank_resume(bootstrap_session)
stopifnot(identical(bootstrap_restored$bootstrap, bootstrap_partial$bootstrap))
bootstrap_continued <- adaptive_rank_run_live(bootstrap_restored,
  make_adaptive_judge_replay(bootstrap_reservoir), n_steps = 1L, progress = "none")
unlink(bootstrap_session, recursive = TRUE)
```

On
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md)
wrapper resume, omit initialization arguments. An explicit bootstrap
policy must match the saved policy; an explicit predictive seed must
also match. Warm-start mapping/model/prior overrides are rejected. Older
saved sessions without bootstrap policy metadata retain their shuffled
queue, current ratings, and progress. Default summary columns remain
unchanged; use `include_bootstrap = TRUE` for the additional audit
list-column.

## A complete example with three algorithms

### Load example texts and inspect the features

A **feature** is a measured property of a text, such as its length or
vocabulary. `writing_features_v1` has 20 features and remains the
default. The expanded `writing_features_v2` keeps those 20 first and
adds 26, using the same extraction software. More features do not by
themselves establish better prediction.

The package includes features extracted from fabricated English texts,
so you can run the rest of this example without Python. The first 40
texts are used for training; eight different texts are reserved for
prediction. The example’s `theta` values are invented, not estimates
from a BTL analysis.

``` r

example <- readRDS(system.file("extdata", "warm-start-example.rds", package = "pairwiseLLM"))
training <- example$training
new_items <- example$new_items
features <- example$training_features
new_features <- example$new_features
schema <- "writing_features_v2"
head(training[c("item_id", "theta")])
#>      item_id     theta
#> 1 example-01 0.2611090
#> 2 example-02 0.7246159
#> 3 example-03 0.9424600
#> 4 example-04 1.0524932
#> 5 example-05 1.2741566
#> 6 example-06 1.7424118
head(warm_start_feature_schema(schema)[c("position", "feature", "family")])
#> # A tibble: 6 × 3
#>   position feature                  family                       
#>      <int> <chr>                    <chr>                        
#> 1        1 n_tokens                 length_productivity          
#> 2        2 proportion_unique_tokens lexical_diversity            
#> 3        3 token_length_mean        lexical_surface_complexity   
#> 4        4 token_length_std         lexical_surface_complexity   
#> 5        5 sentence_length_mean     sentence_syntactic_complexity
#> 6        6 sentence_length_std      sentence_syntactic_complexity
```

In your own assessment, use one row per item and completed BT/BTL scores
from that assessment. Keep IDs, texts, and scores aligned. Do not pool
raw scores from separately ranked assessments. For separate writing
traits, develop a model from the corresponding trait’s scores.

### Extract features when you have Python configured

These calls reproduce the example feature tables. They are shown without
execution in an ordinary render because they require your existing
Python environment. Substitute your own aligned texts and IDs when using
real data.

``` r

python <- "/path/to/venv/bin/python"
status <- warm_start_python_status(python = python, schema = schema)
status$problems
features <- extract_warm_start_features(
  ids = training$item_id, texts = training$text, schema = schema, python = python)
new_features <- extract_warm_start_features(
  ids = new_items$item_id, texts = new_items$text, schema = schema, python = python)
```

Each table contains `item_id` and 46 numeric features in a fixed order.
Undefined values remain missing; the training procedure handles
permitted missing values. Use
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) to cache features
without losing the `warm_start_schema` attribute. The schema attribute
identifies the expected columns; it does not prove where cached features
came from. Saved v1 models still require v1 features.

The installed setup instructions are available with:

``` r

system.file("python", "README.md", package = "pairwiseLLM")
#> [1] "/home/runner/work/_temp/Library/pairwiseLLM/python/README.md"
```

Extraction uses the pinned Python 3.12.3, spaCy 3.7.5, TextDescriptives
2.8.4, textstat 0.7.13, and English model/resources described there. No
environment or resource is installed automatically. A status check loads
the large language model and can take time. If R has already initialized
another interpreter, restart R before selecting this one. The pinned
environment has been tested on Linux; Windows/macOS setup paths are not
validation of those Python environments.

### Share the same training and validation splits

**Cross-validation (CV)** temporarily holds out some samples, trains on
the remaining samples, and predicts the held-out ones. Nested CV has two
levels: inner splits choose model settings; outer splits assess
predictions for samples kept outside that training and tuning process.

Make one plan and reuse it for all three algorithms. This gives each
algorithm the same training and held-out samples, making their
validation results comparable. The plan checks the task label, ordered
IDs, and outcomes; it cannot be reused with different scores or a
different order of items.

``` r

plan <- make_warm_start_cv_plan(training$item_id, training$theta,
  task_id = "example-assessment", seed = 259L, outer_folds = 5L, inner_folds = 5L)
```

During fitting, missing-value handling and feature scaling are learned
separately from each training split. Held-out samples do not determine
that preprocessing. Scores are standardized using the relevant training
set, so held-out predictions and observations are compared on the same
scale. Too few suitable samples gives an error; the requested fold
counts are not silently reduced.

### Fit the three algorithms

| Algorithm | What it allows | Optional R package |
|----|----|----|
| Elastic net | A weighted combination of features, with shrinkage to limit overfitting | `glmnet` |
| Partial least squares (PLS) | A few combinations of correlated features | `pls` |
| Radial-basis support-vector regression (RBF-SVR) | Nonlinear relationships between features and scores | `e1071` |

The examples use smaller explicit tuning grids to keep execution quick.
These are demonstration settings, not recommendations selected from the
results. The default grids and deterministic selection rules are
documented in
[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md).

``` r

model <- fit_warm_start_model(training$item_id, training$theta, "example-assessment",
  features = features, schema = schema, cv_plan = plan, alpha_grid = c(0, 0.5))
pls_model <- fit_warm_start_model(training$item_id, training$theta, "example-assessment",
  features = features, schema = schema, cv_plan = plan, engine = "pls",
  engine_control = list(ncomp = 1:2))
svr_model <- fit_warm_start_model(training$item_id, training$theta, "example-assessment",
  features = features, schema = schema, cv_plan = plan, engine = "svr_rbf",
  engine_control = list(cost = c(0.5, 2), gamma_multiplier = c(0.5, 1)))
```

Each model also learns a calibration step from **out-of-fold (OOF)**
predictions: training-side predictions made without fitting on the
corresponding rows. Calibration puts the model output onto the
standardized quality scale. It is learned within the outer training set
before predicting its held-out samples. The final model uses all
training samples only after outer validation is complete.

### Inspect validation before combining models

``` r

models <- list(elastic_net = model, pls = pls_model, svr_rbf = svr_model)
validation <- do.call(rbind, lapply(models, function(x) {
  unlist(x$validation$metrics[c("rmse", "mae", "pearson_r", "spearman_rho")])
}))
knitr::kable(validation, digits = 3)
```

|             |  rmse |   mae | pearson_r | spearman_rho |
|:------------|------:|------:|----------:|-------------:|
| elastic_net | 0.349 | 0.315 |     0.935 |        0.916 |
| pls         | 0.409 | 0.348 |     0.911 |        0.905 |
| svr_rbf     | 0.448 | 0.372 |     0.894 |        0.883 |

These metrics describe the outer held-out predictions. Smaller RMSE and
MAE mean smaller prediction errors; larger Pearson or Spearman
correlations mean stronger agreement in scores or ordering. Squared
Pearson correlation is the square of that correlation, not a separate
measure of unbiased prediction. Calibration intercept/slope describe the
validation predictions; they do not apply another calibration to the
deployed model. Undefined metrics are reported with reasons.

Do not replace these results with predictions on the training samples
from the final fitted model. The invented example outcomes make the
table useful for learning the interface only. It does not select an
algorithm automatically or establish performance on a new population.

### Combine algorithms trained on this assessment

A **same-task algorithm ensemble** averages the calibrated predictions
of models trained on the same assessment. Supply named, full model
objects with matching IDs, outcomes, feature schema, and CV plan:

``` r

algorithm_ensemble <- ensemble_warm_start_algorithms(
  elastic_net = model, pls = pls_model, svr_rbf = svr_model)
summary(algorithm_ensemble)$validation$metrics
#> $pearson_r
#> [1] 0.9182634
#> 
#> $squared_pearson_r
#> [1] 0.8432077
#> 
#> $spearman_rho
#> [1] 0.9054409
#> 
#> $rmse
#> [1] 0.391374
#> 
#> $mae
#> [1] 0.339891
#> 
#> $calibration_intercept
#> [1] -0.01665459
#> 
#> $calibration_slope
#> [1] 0.9842066
#> 
#> $undefined_reasons
#> character(0)
```

Each component receives equal weight after its own calibration. There
are no learned weights or additional ensemble calibration. The
ensemble’s validation averages the aligned outer held-out predictions
first, then compares those means with the corresponding held-out
observations. It does not average the three models’ error metrics.
Legacy or reduced models lack the full shared evidence needed to
construct a new same-task ensemble.

### Predict new items and initialize an assessment

``` r

algorithm_predictions <- predict(algorithm_ensemble, new_features)
algorithm_predictions
#> Warm-start ensemble predictions; sample SD is diagnostic, not Bayesian prior SD.
#> # A tibble: 8 × 6
#>   item_id    component_elastic_net component_pls component_svr_rbf ensemble_mean
#>   <chr>                      <dbl>         <dbl>             <dbl>         <dbl>
#> 1 example-41                0.840         0.695              0.969        0.835 
#> 2 example-42                1.35          1.35               1.36         1.35  
#> 3 example-43               -1.34         -1.59              -1.64        -1.52  
#> 4 example-44               -0.803        -0.877             -1.10        -0.926 
#> 5 example-45               -0.0986        0.0154            -0.190       -0.0912
#> 6 example-46                0.342         0.349              0.645        0.445 
#> 7 example-47                0.892         0.756              1.01         0.885 
#> 8 example-48                1.46          1.52               1.44         1.47  
#> # ℹ 1 more variable: ensemble_sd <dbl>
algorithm_prior <- make_warm_start_prior(algorithm_predictions,
  ids = new_items$item_id, prior_sd = 0.5)
head(data.frame(item_id = algorithm_prior$item_id,
  mean = algorithm_prior$prior_mean, sd = algorithm_prior$prior_sd))
#>      item_id       mean  sd
#> 1 example-41  0.5289074 0.5
#> 2 example-42  1.0479329 0.5
#> 3 example-43 -1.8308708 0.5
#> 4 example-44 -1.2316579 0.5
#> 5 example-45 -0.3970813 0.5
#> 6 example-46  0.1394400 0.5
```

`ensemble_mean` is the mean calibrated prediction; `ensemble_sd`
describes how much the algorithms disagree. That disagreement does
**not** set the prior SD. Here we explicitly choose 0.5. The prior
constructor centers the predictions for these new items; it does not
restore the old assessment’s raw BTL scale.

Initialize any of the three predictive modes using the same resolved
prior:

``` r

btl_state <- adaptive_rank_start(new_items$item_id, seed = 17L,
  warm_start_prior = algorithm_prior, warm_start_mode = "btl_only")
trueskill_state <- adaptive_rank_start(new_items$item_id, seed = 17L,
  warm_start_prior = algorithm_prior, warm_start_mode = "trueskill_only")
both_state <- adaptive_rank_start(new_items$item_id, seed = 17L,
  warm_start_prior = algorithm_prior, warm_start_mode = "both")
both_state$trueskill_state$items[, c("item_id", "mu", "sigma")]
#> # A tibble: 8 × 3
#>   item_id       mu sigma
#>   <chr>      <dbl> <dbl>
#> 1 example-41 29.4   8.33
#> 2 example-42 33.7   8.33
#> 3 example-43  9.74  8.33
#> 4 example-44 14.7   8.33
#> 5 example-45 21.7   8.33
#> 6 example-46 26.2   8.33
#> 7 example-47 29.8   8.33
#> 8 example-48 34.7   8.33
```

These calls initialize state without collecting judgments or running
Bayesian sampling. Continue with the judging and running steps in
[Adaptive
Pairing](https://shmercer.github.io/pairwiseLLM/articles/adaptive-pairing.md).
All four Bayesian BTL variants support predictive priors. In
`trueskill_only`, BTL keeps its cold prior.

## Save a model and resume a session

Prediction from precomputed features needs neither Python nor any
fitting engine. Ordinary save/load retains the full model and its
validation evidence:

``` r

local({
  directory <- withr::local_tempdir()
  path <- file.path(directory, "algorithm-ensemble.rds")
  save_warm_start_model(algorithm_ensemble, path)
  restored <- load_warm_start_model(path)
  stopifnot(identical(predict(restored, new_features), algorithm_predictions))

  # This example uses a temporary registry, leaving your own registry alone.
  withr::local_envvar(c(R_USER_DATA_DIR = directory))
  register_warm_start_model(restored, "example")
  list_warm_start_models(source = "user")
})
#> # A tibble: 1 × 19
#>   name    source path     version format_version schema target     n calibration
#>   <chr>   <chr>  <chr>    <chr>            <int> <chr>  <chr>  <int> <chr>      
#> 1 example user   /tmp/Rt… NA                   1 writi… withi…    40 component_…
#> # ℹ 10 more variables: audit_status <chr>, artifact_type <chr>, engine <chr>,
#> #   engine_version <chr>, component_engines <list>,
#> #   component_engine_versions <list>, component_count <int>, size_bytes <dbl>,
#> #   metadata <list>, validation <list>
```

Registered names are normalized to lowercase with hyphens. If a user
model and a bundled model share a name, select `source` explicitly. No
pretrained models are currently listed by
`list_warm_start_models(source = "bundled")`.

To save an adaptive session, set `session_dir` at initialization. Resume
uses the saved numeric prior even if the original model file is no
longer available:

``` r

local({
  directory <- withr::local_tempdir()
  state <- adaptive_rank_start(new_items$item_id, session_dir = directory,
    warm_start_prior = algorithm_prior, warm_start_mode = "both")
  resumed <- adaptive_rank_resume(directory)
  stopifnot(identical(resumed$predictive_prior, state$predictive_prior))
})
```

On resume, omit all warm-start arguments, including mode. The session
retains its prior, mode, TrueSkill state, and progress. To change the
starting information, start a new session. Model input and prior input
are alternatives: do not supply both. `warm_start_prior_sd` is available
with model input for BTL warming and required for
predictive-distribution initialization; a resolved prior already
contains its chosen SDs.

## Combining models from separate assessments

A **cross-task ensemble** has a different purpose: combining calibrated
models from separately developed assessments. Each model standardizes
its own outcome; raw unlinked BT/BTL scores are never pooled. Use
[`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md).
It retains component validation summaries, not a common ensemble
validation score.

The following second assessment is entirely fabricated, including its
feature values. It illustrates the separate API without suggesting that
relabeling the first assessment would create independent training data.

``` r

features_b <- local({
  withr::local_seed(3104L)
  x <- features
  # Reuse valid feature ranges, but independently shuffle each column.
  for (field in warm_start_feature_schema(schema)$feature) x[[field]] <- sample(x[[field]])
  x$item_id <- paste0("assessment-b-", seq_len(nrow(x)))
  x
})
theta_b <- withr::with_seed(3105L,
  0.1 * features_b$n_tokens + rnorm(nrow(features_b)))
model_b <- fit_warm_start_model(features_b$item_id, theta_b, "synthetic-b",
  features = features_b, schema = schema, alpha_grid = c(0, 0.5))
ensemble <- ensemble_warm_start_models(assessment_a = model, assessment_b = model_b)
summary(ensemble)
#> $format_version
#> [1] 1
#> 
#> $schema
#> [1] "writing_features_v2"
#> 
#> $target
#> $target$definition
#> [1] "within_task_z"
#> 
#> $target$sd_convention
#> [1] "sample"
#> 
#> 
#> $weighting
#> [1] "equal"
#> 
#> $audit_status
#> [1] "full"
#> 
#> $components
#> $components$assessment_a
#> $components$assessment_a$task_id
#> [1] "example-assessment"
#> 
#> $components$assessment_a$target
#> $components$assessment_a$target$definition
#> [1] "within_task_z"
#> 
#> $components$assessment_a$target$mean
#> [1] 0.965597
#> 
#> $components$assessment_a$target$sd
#> [1] 0.7317642
#> 
#> $components$assessment_a$target$sd_convention
#> [1] "sample"
#> 
#> 
#> $components$assessment_a$n
#> [1] 40
#> 
#> $components$assessment_a$schema
#> [1] "writing_features_v2"
#> 
#> $components$assessment_a$retained_predictors
#> [1] 45
#> 
#> $components$assessment_a$removed_predictors
#> syllables_per_token_median 
#>                 "constant" 
#> 
#> $components$assessment_a$nonzero_coefficients
#> [1] 12
#> 
#> $components$assessment_a$alpha
#> [1] 0.5
#> 
#> $components$assessment_a$lambda
#> [1] 0.2609714
#> 
#> $components$assessment_a$calibration
#> [1] "oof_linear"
#> 
#> $components$assessment_a$audit_status
#> [1] "full"
#> 
#> $components$assessment_a$engine
#> [1] "glmnet"
#> 
#> $components$assessment_a$engine_version
#> [1] "5.1"
#> 
#> $components$assessment_a$validation
#> $components$assessment_a$validation$pearson_r
#> [1] 0.9354842
#> 
#> $components$assessment_a$validation$squared_pearson_r
#> [1] 0.8751307
#> 
#> $components$assessment_a$validation$spearman_rho
#> [1] 0.9161351
#> 
#> $components$assessment_a$validation$rmse
#> [1] 0.349274
#> 
#> $components$assessment_a$validation$mae
#> [1] 0.3146212
#> 
#> $components$assessment_a$validation$calibration_intercept
#> [1] -0.007381862
#> 
#> $components$assessment_a$validation$calibration_slope
#> [1] 1.020659
#> 
#> $components$assessment_a$validation$undefined_reasons
#> character(0)
#> 
#> 
#> 
#> $components$assessment_b
#> $components$assessment_b$task_id
#> [1] "synthetic-b"
#> 
#> $components$assessment_b$target
#> $components$assessment_b$target$definition
#> [1] "within_task_z"
#> 
#> $components$assessment_b$target$mean
#> [1] 6.046153
#> 
#> $components$assessment_b$target$sd
#> [1] 2.152588
#> 
#> $components$assessment_b$target$sd_convention
#> [1] "sample"
#> 
#> 
#> $components$assessment_b$n
#> [1] 40
#> 
#> $components$assessment_b$schema
#> [1] "writing_features_v2"
#> 
#> $components$assessment_b$retained_predictors
#> [1] 45
#> 
#> $components$assessment_b$removed_predictors
#> syllables_per_token_median 
#>                 "constant" 
#> 
#> $components$assessment_b$nonzero_coefficients
#> [1] 4
#> 
#> $components$assessment_b$alpha
#> [1] 0.5
#> 
#> $components$assessment_b$lambda
#> [1] 0.400063
#> 
#> $components$assessment_b$calibration
#> [1] "oof_linear"
#> 
#> $components$assessment_b$audit_status
#> [1] "full"
#> 
#> $components$assessment_b$engine
#> [1] "glmnet"
#> 
#> $components$assessment_b$engine_version
#> [1] "5.1"
#> 
#> $components$assessment_b$validation
#> $components$assessment_b$validation$pearson_r
#> [1] 0.867912
#> 
#> $components$assessment_b$validation$squared_pearson_r
#> [1] 0.7532712
#> 
#> $components$assessment_b$validation$spearman_rho
#> [1] 0.8827806
#> 
#> $components$assessment_b$validation$rmse
#> [1] 0.4991858
#> 
#> $components$assessment_b$validation$mae
#> [1] 0.3867394
#> 
#> $components$assessment_b$validation$calibration_intercept
#> [1] -0.03326168
#> 
#> $components$assessment_b$validation$calibration_slope
#> [1] 1.060803
#> 
#> $components$assessment_b$validation$undefined_reasons
#> character(0)
#> 
#> 
#> 
#> 
#> $metadata
#> NULL
#> 
#> $sd_interpretation
#> [1] "Between-model sample SD is diagnostic, not Bayesian prior SD."
```

Components must share a schema and standardized outcome definition and
have learned calibration. Their preprocessing and CV plans may differ.
Repeated models and task labels remain accepted for backward
compatibility, but do not demonstrate independent evidence. Nested
ensembles are rejected. Assess whether the training assessments and
target population justify using their predictions together.

## Inspect calibrated standardized coefficients

Use
[`warm_start_coefficients()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_coefficients.md)
to inspect the fitted model weights without loading the original fitting
engine. This applies to elastic net and PLS. RBF-SVR has no equivalent
linear feature coefficients; requesting them produces an informative
error, including when it belongs to an ensemble. Individual models
return one row per frozen feature in schema order:

``` text
feature | retained | calibrated_std_coefficient
```

``` r

coef_tbl <- warm_start_coefficients(model)
coef_tbl
#> # A tibble: 46 × 3
#>    feature                  retained calibrated_std_coefficient
#>    <chr>                    <lgl>                         <dbl>
#>  1 n_tokens                 TRUE                         0.0650
#>  2 proportion_unique_tokens TRUE                         0     
#>  3 token_length_mean        TRUE                         0     
#>  4 token_length_std         TRUE                         0     
#>  5 sentence_length_mean     TRUE                         0     
#>  6 sentence_length_std      TRUE                         0     
#>  7 pos_prop_noun            TRUE                         0     
#>  8 pos_prop_verb            TRUE                         0     
#>  9 pos_prop_adj             TRUE                         0     
#> 10 pos_prop_adv             TRUE                         0     
#> # ℹ 36 more rows

coef_ensemble <- warm_start_coefficients(ensemble)
coef_ensemble
#> # A tibble: 46 × 3
#>    feature                  assessment_a_std_coefficient assessment_b_std_coef…¹
#>    <chr>                                           <dbl>                   <dbl>
#>  1 n_tokens                                       0.0650                   0.858
#>  2 proportion_unique_tokens                       0                        0    
#>  3 token_length_mean                              0                        0    
#>  4 token_length_std                               0                        0    
#>  5 sentence_length_mean                           0                        0    
#>  6 sentence_length_std                            0                        0    
#>  7 pos_prop_noun                                  0                        0    
#>  8 pos_prop_verb                                  0                        0    
#>  9 pos_prop_adj                                   0                        0    
#> 10 pos_prop_adv                                   0                        0    
#> # ℹ 36 more rows
#> # ℹ abbreviated name: ¹​assessment_b_std_coefficient
```

The predictors were centered and divided by their training-sample SDs,
and the fitted target was within-task standardized BT/BTL quality. For a
retained feature, the reported coefficient is the stored elastic-net or
PLS coefficient multiplied by the learned OOF calibration slope. Holding
the other included predictors fixed, it is therefore the change in
calibrated within-task standardized prediction for a one-training-SD
increase in that feature.

`retained = FALSE` with coefficient `NA` means preprocessing removed the
feature, so no fitted standardized coefficient exists. `retained = TRUE`
with coefficient `0` means the feature survived preprocessing but
elastic net assigned it zero calibrated weight at the selected alpha and
lambda. Positive and negative signs describe fitted conditional
direction. Correlated predictors can redistribute weight, so coefficient
magnitude is not unique predictive importance, causal influence, or a
share of explained variance.

Ensemble output has one ordered column per component:

``` text
feature | assessment_a_std_coefficient | assessment_b_std_coefficient | ...
```

Each component used its own training distribution to standardize
predictors; the columns do not share one raw-feature SD. Side-by-side
values show fitted direction, magnitude, and stability across
independently trained task models. `NA` and `0` retain their
component-specific meanings. The table neither estimates an aggregate
ensemble coefficient nor changes equal-weight prediction averaging. Once
a portable model exists, this inspection needs neither Python nor a
fitting backend.

## Understand saved models

Full models store their feature schema, preprocessing, calibration,
fitting settings, and validation evidence. Elastic net and PLS store
linear coefficients; RBF-SVR stores numeric support vectors and kernel
parameters. Prediction uses these saved numbers without loading the
original fitting engine. A nonlinear model does not provide linear
feature coefficients.

`prepare_warm_start_model(x, omit_audit = TRUE)` makes a smaller
deployment artifact while retaining the same predictions:

``` r

deployment <- prepare_warm_start_model(algorithm_ensemble, omit_audit = TRUE)
stopifnot(identical(predict(deployment, new_features)$ensemble_mean,
  algorithm_predictions$ensemble_mean))
```

Reduction removes detailed training and validation rows. The retained
summaries can no longer be independently recalculated from that reduced
object. Keep the full evidence separately when an audit is needed.
Reduction is not anonymization: metadata and, for SVR, support-vector
values still need review before sharing. Load RDS files only from
trusted sources.

Current single models use format 3 in both full and summary-only form.
Legacy model formats 1 and 2 retain their original meanings. Same-task
algorithm ensembles have their own format 1 and
`artifact_type = "algorithm_ensemble"`; cross-task ensembles use
`artifact_type = "ensemble"`. These format numbers are separate from the
package version.

For precise model fields, tuning grids, weighted error/selection rules,
and calibration conventions, see
[`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md)
and `pairwiseLLM_warm_model`. Feature definitions are in
[`warm_start_feature_schema()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_feature_schema.md);
the installed Python README records extraction setup and schema hashes.
In the source repository, `data-raw/warm-start/README.md` describes
model publication and private audit review. None of these examples fits
or publishes a production model.

Warm starts do not change Phase B linking or its prior, selection, and
stopping rules. See [Adaptive
Linking](https://shmercer.github.io/pairwiseLLM/articles/adaptive-linking.md)
for that separate workflow.

## Citation

> Mercer, S. H. (2026). *Guide: Adaptive Warm Start* \[R package
> vignette\]. Comprehensive R Archive Network.
> <https://doi.org/10.32614/CRAN.package.pairwiseLLM>
