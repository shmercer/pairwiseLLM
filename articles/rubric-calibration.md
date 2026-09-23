# Guide: Rubric Calibration

Rubric calibration converts completed comparative-judgment (CJ) results
into ordered performance levels. The ordinal methods learn the
relationship between CJ quality and human rubric scores. Percentile
scoring matches a chosen marginal distribution without learning rubric
criteria.

**You need:** a completed Bayesian CJ analysis and, for ordinal
calibration, human rubric labels for some of its items. **You get:** a
rubric label for each eligible item, full category probabilities for
ordinal methods, and diagnostics. The frequentist BT/Elo fits in Getting
Started are not accepted calibration inputs.

For example, you might have compared essays for organization and want to
report “developing”, “proficient”, or “advanced”. Human scores teach the
ordinal model how that trait’s CJ scale relates to those labels. If you
have no human scores, percentile levels describe relative standing under
your chosen distribution; they do not establish mastery of rubric
criteria.

## Choose the workflow

| Available evidence | Method and design | Interpretation |
|----|----|----|
| Completed CJ, no human rubric labels | `percentile`, `same_set` | Norm-referenced, distribution-matched levels |
| Human labels for a subset of one completed CJ analysis | `ordinal_linear`, `same_set` | Default two-stage calibration on that analysis’s scale |
| A human-scored historical reference and new target responses | `ordinal_linear`, `linked_anchors` | Reusable calibration after existing Phase B linking |
| Evidence of curvature in the CJ-to-rubric relationship | Explicit `ordinal_monotone`, either ordinal design | Nondecreasing nonlinear calibration, assessed with held-out labels |

`same_set` means that labeled and unlabeled items participated in **one
CJ fit**. It is a two-stage procedure, not a joint CJ/rubric likelihood.
Both ordinal methods retain the common cumulative-effect assumption; a
curved fit alone does not resolve a proportional-odds violation. There
is no automatic method selector.

## Start from completed CJ results

Use a completed
[`fit_bayes_btl_mcmc()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bayes_btl_mcmc.md)
result, a completed within-set
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md)
result (or its state), or an import-ready Phase A artifact. Raw score
tables and intermediate adaptive states are rejected. Fixed fits with
multiple refits use the last refit. All four Bayesian variants, `btl`,
`btl_e`, `btl_b`, and `btl_e_b`, use the same calibration interface. For
fixed-pair results, live versus batch collection is provenance only.

Inspect CJ reliability, connectivity, and estimation diagnostics before
calibration. Upstream failures remain visible; calibration does not
repair weak CJ measurement. Import readiness follows existing Phase A
quality rules and does not by itself certify that the originating run
stopped successfully.

For constructing these inputs, see [Standalone Bayesian
BTL](https://shmercer.github.io/pairwiseLLM/articles/bayesian-btl.md),
[Guide: Adaptive
Pairing](https://shmercer.github.io/pairwiseLLM/articles/adaptive-pairing.md),
and [Guide: Adaptive
Linking](https://shmercer.github.io/pairwiseLLM/articles/adaptive-linking.md).
Rubric functions do not generate pairs, collect judgments, or control
adaptive stopping.

The code below starts from your completed objects and label tables.
Those objects are not bundled example data, so dependent chunks are not
evaluated during normal documentation builds. No provider calls or
CmdStan sampling occur in the rubric examples. The downstream snippets
are tested with deterministic completed-result fixtures in the package’s
test suite.

pairwiseLLM requires **R \>= 4.4**. Percentile scoring needs no optional
modeling backend. Linear fitting requires `ordinal`; saved linear
prediction does not. Monotone fitting requires `mgcv >= 1.9-4` and
`withr`; saved monotone prediction requires `mgcv`. Install the optional
packages only when you use those methods:

``` r

install.packages("ordinal")
install.packages(c("mgcv", "withr"))
```

## Define the ordered rubric and labels

``` r

rubric_levels <- c("developing", "proficient", "advanced")
```

`completed_cj` below is the completed organization-specific CJ result.
`training_labels` and `evaluation_labels` are disjoint data frames with
columns `item_id` and `rubric_score`, optionally `trait`. Each ID must
occur in that CJ result. Use global IDs when the adaptive result or
artifact provides them; otherwise use the original fixed-fit IDs.
Alignment is by ID, never row position. Omitted items or `NA` rubric
scores are unlabeled.

A label table has this shape (illustrative IDs only; use IDs from your
completed analysis and enough labels to support fitting and validation):

| item_id  | rubric_score |
|----------|--------------|
| essay_01 | developing   |
| essay_07 | proficient   |
| essay_12 | advanced     |

``` r

training_labels <- read.csv("training_labels.csv", colClasses = "character")
evaluation_labels <- read.csv("evaluation_labels.csv", colClasses = "character")
```

Declare levels from lowest to highest quality. Numeric scores can infer
ascending levels, and ordered factors retain their declared levels.
Character scores need explicit `levels`. One fit has one category
structure (`K >= 3`, commonly 3–6). Every intended category must occur
among training labels; missing categories fail rather than being
collapsed. Higher CJ and rubric scores must both mean better
performance.

Choose calibration responses that cover the CJ range, including its
extremes, and check category counts. There is no universal sufficient
number of labels. Keep evaluation labels separate before fitting:
evaluation on training labels measures apparent performance, not
held-out accuracy.

## Same-set linear calibration

``` r

if (requireNamespace("ordinal", quietly = TRUE)) {
  calibration <- fit_rubric_calibration(
    completed_cj, training_labels,
    trait = "organization", levels = rubric_levels
  )
  scores <- predict(calibration)
  scores[, c("item_id", "rubric_score", "extrapolated")]
  calibration$category_counts
  calibration$calibration_range
  calibration$transformation
}
```

Only labeled training items determine the stored mean and sample SD used
in `z = (theta - center) / scale`. Linear calibration estimates
`logit P(Y <= k) = tau[k] - beta * z`. A positive slope means that
higher CJ quality favors higher rubric categories. Nonpositive slopes
warn; the software does not reverse coding. Sparse categories,
separation, and numerical or identification problems require review even
if a fit is returned.

`predict(calibration)` scores every original item, including unlabeled
items. Explicit same-set `newdata` may only reuse the original completed
result with unchanged items, accepted scores/uncertainty, and fit
evidence; reordering is allowed. Independent cohorts and refits require
a different deployment analysis.

## Read probabilities and hard scores

``` r

if (requireNamespace("ordinal", quietly = TRUE)) {
  probability_matrix <- do.call(rbind, scores$probabilities)
  head(probability_matrix)
  scores[, c("median_category", "modal_category", "expected_level")]
  modal_scores <- predict(calibration, hard_score = "mode")
}
```

Each `probabilities` entry is a full K-vector named by the original
ordered labels. The default hard score is the **median category**: the
first category whose cumulative probability reaches 0.5. Equality
selects the lower category. The mode selects the largest probability;
ties select the lowest category. Changing `hard_score` changes
`category` and `rubric_score`, while retaining all probabilities and
both decision summaries.

`expected_level` averages internal indices `1:K`, not arbitrary numeric
label values. It is a continuous summary, not a replacement for the
ordinal outcome. `extrapolated` marks scores outside the labeled
training range, even among original same-set items. Endpoints are inside
the range.

For an illustrative probability vector
`(developing = 0.20, proficient = 0.55, advanced = 0.25)`, cumulative
probability first reaches 0.5 at “proficient”, so that is the default
median-category score. The probabilities also show substantial
uncertainty; reporting the category alone discards it. These numbers
illustrate the decision rule and are not fitted results.

For a report, retain `item_id`, `rubric_score`, `probabilities`, and
`extrapolated` for ordinal predictions. Explain an extrapolation flag as
a score outside the labeled calibration range. Percentile predictions
instead require requested and achieved category proportions. Keep the
method, trait, and validation results with either kind of output.

## Evaluate held-out labels and inspect diagnostics

``` r

if (requireNamespace("ordinal", quietly = TRUE)) {
  assessment <- evaluate_rubric_predictions(
    calibration, evaluation_labels, diagnostics = TRUE
  )
  assessment$metrics
  assessment$calibration
  assessment$metadata$n_training_label_overlap
  assessment$diagnostics$common_effect
}
```

Normalized ranked probability score (`rps`) is the primary probabilistic
metric; lower is better. It averages squared cumulative-probability
errors over the `K - 1` boundaries. Log loss is secondary. Hard-score
summaries include exact and within-one accuracy, mean absolute category
error, and quadratic weighted kappa. Distances use internal category
indices. Undefined kappa and unavailable probability metrics have
explicit unavailable values/reasons.

Evaluation aligns IDs, excludes and counts missing labels, and reports
training overlap and extrapolation. Evaluation categories may be absent
even though all categories were required for training.
`assessment$calibration` compares observed and predicted cumulative
frequencies in probability and theta bins; ties stay together. These
summaries can reveal where predictions disagree with labels.

Diagnostics are off by default. When requested, linear fits use a
threshold-varying alternative for a proportional-odds likelihood-ratio
diagnostic. Monotone fits provide descriptive boundary checks, not an
omnibus test. Failed diagnostic refits return an unavailable status and
preserve the production fit. Formal and descriptive checks use training
data; held-out evaluation labels do not refit the calibration or its
training diagnostics.

For repeated label-only validation, keep completed CJ evidence fixed,
hide held-out rubric labels, and refit all calibration scaling and
parameters in each training fold. Ensure every fold’s training data
retain all intended categories. The package’s CV machinery is internal;
the exported evaluator supports explicit holdout workflows. Model/tuning
selection using validation results needs a separate outer assessment to
avoid optimistic reported performance.

## Explicit monotone alternative

``` r

if (requireNamespace("mgcv", quietly = TRUE) &&
    packageVersion("mgcv") >= "1.9.4" && requireNamespace("withr", quietly = TRUE)) {
  monotone_calibration <- fit_rubric_calibration(
    completed_cj, training_labels, method = "ordinal_monotone",
    trait = "organization", levels = rubric_levels, k = 6
  )
  monotone_scores <- predict(monotone_calibration)
  monotone_assessment <- evaluate_rubric_predictions(
    monotone_calibration, evaluation_labels
  )
  monotone_assessment$metrics
}
```

This fits a nondecreasing penalized cubic spline using
[`mgcv::scasm()`](https://rdrr.io/pkg/mgcv/man/scasm.html) and
[`mgcv::ocat()`](https://rdrr.io/pkg/mgcv/man/ocat.html). The default
basis dimension is `k = 6`; it must be at least five and is reduced with
a warning if there are fewer unique labeled scores. Fewer than five
unique scores fail. This is a backend feasibility constraint, not a
sample-size recommendation. Smoothing is estimated unless a positive
fixed `sp` is supplied. No unconstrained smoother replaces a failed
monotone fit.

Inspect convergence, probability behavior, and held-out performance
before preferring the more flexible model. Numerical cutpoints can be
unavailable for flat or unresolved crossings; scoring still uses
probabilities. Predictions outside the labeled range retain spline
extrapolation and are flagged, without establishing rubric calibration
there.

## Percentile levels without human labels

``` r

percentile_calibration <- fit_rubric_calibration(
  completed_cj, method = "percentile", trait = "organization",
  levels = rubric_levels, target_distribution = c(0.2, 0.5, 0.3)
)
percentile_scores <- predict(percentile_calibration)
percentile_calibration$backend$requested_proportions
percentile_calibration$backend$achieved_proportions
percentile_calibration$backend$cutpoints
percentile_calibration$backend$cutpoint_tie_counts
```

Omit `target_distribution` for equal proportions; `K` may replace
explicit level names. Quantiles use fixed R type 8. A score equal to an
internal cutpoint enters the higher category. All ties stay together, so
requested proportions can be unattainable and some categories can be
empty. If all scores are identical, all items enter the highest category
under this boundary convention.

These are **norm-referenced, distribution-matched CJ-derived performance
levels**. Using a historical rubric distribution does not show
individual agreement with human rubric raters. Percentile fits support
original-result reuse only, not independent cohorts or `linked_anchors`.
They return deterministic categories, not category probabilities;
evaluation supplies hard metrics only.

## Reuse a historical rubric reference through Phase B

The `rubric_reference_set` is the human-scored set whose CJ metric
supports the calibration. A Phase B **hub anchor** is a hub item
selected for routing recurring comparisons, not a known true rubric
score.

1.  Run trait-specific Phase A for the reference, retaining its ready
    artifact.
2.  Fit and save its ordinal calibration and retain the original
    reference artifact.
3.  Run compatible Phase A for each new target set.
4.  Explicitly select E1, E2, or E3 and fit the active cross-set
    evidence.
5.  Apply the saved calibration to the valid, identified common-scale
    result.

In the [linking
guide](https://shmercer.github.io/pairwiseLLM/articles/adaptive-linking.md),
ready artifacts are available in `reference_run$phase_a$manifest[["1"]]`
and `target_run$phase_a$manifest[["2"]]` for reference set 1 and target
set 2. Extract the artifact list for fitting; the rubric fitter does not
take a path or the whole Phase A manifest. The following assumes
`reference_phase_a` contains that original artifact and
`reference_labels` contains its human scores.

``` r

if (requireNamespace("ordinal", quietly = TRUE)) {
  linked_calibration <- fit_rubric_calibration(
    reference_phase_a, reference_labels,
    calibration_design = "linked_anchors",
    trait = "organization", levels = rubric_levels
  )
  reference_scores <- predict(linked_calibration)
}
```

The reference and target Phase A contracts must match the selected BTL
variant. The [explicit-evidence linking
guide](https://shmercer.github.io/pairwiseLLM/articles/linking-sessions.md)
describes evidence preparation and exact save/resume. This example
explicitly chooses E2, which requires posterior item draws in both
artifacts. `active_cross` contains only active cross-set observations;
`frozen_judge` records the shared Phase A judge parameters. No provider
calls or adaptive stopping are implicit in this fit.

``` r

link_input <- prepare_link_input(
  estimator = "gaussian_posterior_bridge",
  hub = list(set_id = "1",
    items = reference_phase_a$items[c("item_id", "global_item_id")]),
  spoke = list(set_id = "2",
    items = target_phase_a$items[c("item_id", "global_item_id")]),
  phase_a = list(hub = list(artifact = reference_phase_a),
    spoke = list(artifact = target_phase_a)),
  cross = active_cross, judge = frozen_judge
)
completed_phase_b <- start_link_session(link_input)
```

For several targets, pass a list of prepared inputs sharing the same hub
and judge to
[`start_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/start_link_session.md).
E2/E3 can update hub shapes; rubric calibration remains attached to the
original Phase A hub artifact. Rubric labels are not required during
linking. Held-out probe outcomes must stay outside `active_cross`.

``` r

if (requireNamespace("ordinal", quietly = TRUE)) {
  target_scores <- predict(linked_calibration, newdata = completed_phase_b)
  target_scores[, c("global_item_id", "set_id", "rubric_score", "theta_sd", "extrapolated")]
  attr(target_scores, "linking")$diagnostics
}
```

Explicit linked prediction returns targets only; `newdata = NULL`
predicts the original reference items. Every spoke needs a valid,
cross-set identified fit. The public `theta_link_eap` field aliases
`theta_link_mean`: posterior means for E1 and E3-MCMC, MAP locations for
E2/E3 Laplace. E1 uncertainty is conditional on fixed shapes; E2/E3
include shapes and offset. Predictions restore the original hub location
origin using its frozen Phase A mean. Estimator identity, uncertainty
scope and exact evidence provenance accompany the output. Legacy
anchored-joint Phase B sessions must restart from compatible Phase A
inputs.

Prediction validates the original reference IDs, locations/uncertainty,
evidence, trait, orientation, and canonical fit contract. A matching
configuration hash alone does not establish a shared metric. Target-only
Phase A scores, changed reference scales, and incompatible contracts
fail explicitly. **Never independently re-standardize the target
cohort**: prediction uses the stored labeled-reference mean and SD on
accepted Phase B common-scale locations.

Reference holdout performance assesses calibration on the reference
scale. To assess transport, use external human labels for linked
targets:

``` r

if (requireNamespace("ordinal", quietly = TRUE)) {
  target_assessment <- evaluate_rubric_predictions(
    linked_calibration, target_labels, newdata = completed_phase_b
  )
  target_assessment$metrics
  target_assessment$metadata$linking
}
```

Phase B diagnostics alone do not demonstrate rubric accuracy, and
reference validation alone does not measure the additional error from
linking.

## Separate analytic traits

Organization and evidence require separate trait-specific comparisons,
CJ fits, labels, and calibrations even when item IDs overlap. Each may
have different levels or a different Bayesian variant. Compatibility
must hold within each trait’s reference/target linking workflow. One
holistic CJ scale cannot supply multiple analytic trait calibrations.

``` r

if (requireNamespace("ordinal", quietly = TRUE)) {
  evidence_calibration <- fit_rubric_calibration(
    evidence_cj, evidence_training_labels, trait = "evidence",
    levels = c("limited", "adequate", "strong")
  )
  evidence_scores <- predict(evidence_calibration)
}
```

## Uncertainty and reporting

All three methods condition on accepted **CJ point locations**. Retained
CJ SDs, posterior draws, or linking uncertainty are not propagated into
rubric category probabilities. Ordinal parameter uncertainty is
conditional on those estimated predictors; monotone threshold
uncertainty is not supplied by the wrapper. There is no
posterior/bootstrap propagation engine or joint likelihood here.

Report the trait, BTL variant, method/design, ordered levels, labeled
category counts and range, held-out metrics, extrapolation, and
CJ/calibration/linking diagnostics. Distinguish numerical feasibility
from adequate measurement and external rubric validity. Small
deterministic examples and package recovery tests do not establish a
universal calibration sample size or validity for a new population.

## Citation

> Mercer, S. H. (2026). *Guide: Rubric Calibration* \[R package
> vignette\]. Comprehensive R Archive Network.
> <https://doi.org/10.32614/CRAN.package.pairwiseLLM>
