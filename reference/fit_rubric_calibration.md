# Fit a rubric calibration to completed comparative judgments

Establish a trait-specific rubric conversion from completed Bayesian BTL
results. Percentile scoring assigns deterministic, distribution-matched
performance levels. Linear ordinal calibration fits human rubric labels
with a proportional-odds cumulative-logit model. Monotone ordinal
calibration replaces its linear effect with a nondecreasing penalized
cubic spline.

## Usage

``` r
fit_rubric_calibration(
  cj,
  rubric = NULL,
  method = "ordinal_linear",
  calibration_design = "same_set",
  trait = NULL,
  levels = NULL,
  K = NULL,
  target_distribution = NULL,
  ...
)
```

## Arguments

- cj:

  A completed
  [`fit_bayes_btl_mcmc()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bayes_btl_mcmc.md)
  result, a completed within-set
  [`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md)
  result (or its `adaptive_state`), or an import-ready Phase A artifact
  list. Fixed results with several refits use the last refit. Raw score
  tables and intermediate adaptive states are not accepted.

- rubric:

  Data frame with `item_id` and `rubric_score`, optionally `trait`. Rows
  align by ID, never position. Missing scores denote unlabeled items.
  Use global item IDs for adaptive results and Phase A artifacts that
  provide them; otherwise use the original fixed-fit IDs. Omit for
  percentile scoring.

- method:

  One of `"ordinal_linear"` (default), `"ordinal_monotone"`, or
  `"percentile"`. Percentile scoring is
  norm-referenced/distribution-matched.

- calibration_design:

  `"same_set"` for one completed CJ scale, or `"linked_anchors"` for a
  reusable Phase A rubric reference artifact. Both ordinal methods
  support both designs. Percentile scoring supports only `"same_set"`.

- trait:

  Single trait identifier. Required when absent from CJ metadata. Each
  analytic trait requires its own CJ analysis and calibration.

- levels:

  Ordered original rubric labels, from lowest to highest quality. When
  omitted, use ordered-factor levels or ascending numeric rubric labels.
  Character and unordered-factor scores require explicit levels.

- K:

  Optional number of levels, an integer \>= 3. Must agree with `levels`
  and any target proportions. Typically 3–6.

- target_distribution:

  Optional positive proportions in level order, summing to one, for
  percentile scoring. Omission requests equal proportions. The absolute
  sum tolerance is `1e-8`; accepted proportions are normalized to sum to
  one for computation, while the requested values are retained. Named
  proportions must match the labels in their specified order.

- ...:

  For `ordinal_monotone` only, uniquely named `k` and `sp` controls. `k`
  is the cubic basis dimension (default 6, integer \>= 5). It is reduced
  with a warning to the number of unique labeled CJ scores if necessary;
  fewer than five unique scores fail. `sp = NULL` (default) estimates
  smoothing by extended Fellner-Schall (EFS); a finite positive scalar
  fixes the penalty parameter. Other methods require empty dots. Unknown
  controls fail.

## Value

For percentile scoring, a fitted `pairwiseLLM_rubric_calibration`
containing labels, `K`, normalized `cj`, source `calibration_range`,
trait, provenance, diagnostics, and an identity `transformation` (center
0, scale 1). Its `backend` stores `quantile_type`, `cutpoints`,
`cumulative_probs`, `requested_proportions`, normalized
`effective_proportions`, `achieved_proportions`, `achieved_counts`, and
`cutpoint_tie_counts`. Category summaries include every requested level,
including empty levels. `target_distribution` retains the requested
proportions (or equal defaults). `calibration_data` and
`category_counts` remain `NULL` because no human labels are fitted.
`diagnostics$category_probabilities_available` is `FALSE`. For linear
ordinal calibration, the same class retains ID-aligned
`calibration_data` (including unlabeled items), `category_counts`, and
the labeled `calibration_range`. `transformation$center` and `$scale`
store `mu_cal` and `sigma_cal`. The `backend` contains `name`,
`version`, `link`, `threshold`, the fitted `model`, `thresholds`,
`slope`, `vcov`, `standard_errors`, and `convergence`. With positive
slope, `cutpoints_z` stores `tau / beta` and `cutpoints_theta` maps
these to the original scale. Otherwise cutpoints are omitted.
Unavailable coefficient uncertainty is represented by `NA`, with a
warning. `diagnostics$ordinal` reports numerical convergence, gradient,
Hessian condition, covariance availability, nonpositive slope, singleton
categories, and conditioning on CJ locations. `warnings` retains ordinal
diagnostic messages and `diagnostics$category_probabilities_available`
is `TRUE`. Monotone ordinal calibration uses the same class, data,
transformation, and probability availability flag. Its `backend` stores
`name`, `version`, cumulative `link`, `latent_link`, fitted `model`,
`thresholds`, `intercept`, `basis` (requested/effective dimension,
constraint, order, knots), `smoothing` (method, requested/fitted penalty
parameters, penalty matrices), total `edf`, `smooth_edf`, `convergence`,
`threshold_standard_errors`, `cutpoints_z`, `cutpoints_theta`, and
`cutpoint_status` (`"unique"`, `"outside_range"`, or
`"flat_or_unresolved"`). `diagnostics$ordinal` stores convergence,
singleton categories, conditioning on CJ, unavailable threshold
uncertainty, and numerical `monotonicity` results. Linked calibrations
additionally retain `reference`: the reference `set_id`, sorted stable
item IDs with original Phase A locations/SDs, canonical `fit_contract`,
original `fit_contract_hash`, and within-set evidence/hash. Original
reference SDs are retained separately from Phase B's locked hub SDs.

## Details

Higher CJ locations and higher ordered rubric levels must mean better
performance; no orientation is silently reversed. All requested
categories must be observed for ordinal calibration.

Fitting `ordinal_linear` requires the optional package ordinal. Install
it with `install.packages("ordinal")`. Percentile scoring and prediction
from an already fitted linear calibration do not require it.

Monotone ordinal fitting and prediction require optional mgcv \>= 1.9-4
(`install.packages("mgcv")`). Fitting also requires optional withr
(`install.packages("withr")`) to isolate backend RNG use with a fixed
internal seed. Saved monotone models require mgcv for spline prediction.

For both ordinal methods, calibration items must belong to one completed
trait-specific CJ fit. Only labeled rows estimate the calibration:
`z = (theta - mu_cal) / sigma_cal`, where `mu_cal` is their mean and
`sigma_cal` their sample standard deviation. Both are stored and reused.
[`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) fits
`logit P(Y <= k | z) = tau_k - beta * z` with flexible ordered
thresholds. Zero or negative slopes produce a diagnostic warning; coding
is never reversed. Missing categories or degenerate calibration scores
fail. Singleton categories and numerical convergence/identification
problems produce warnings; no universal minimum calibration sample size
is enforced. Finite ordered estimates can be retained with warnings even
when convergence or uncertainty is unreliable. Such warnings require
review before use. Standard errors condition on estimated CJ point
locations; CJ measurement uncertainty is not propagated.
[`evaluate_rubric_predictions()`](https://shmercer.github.io/pairwiseLLM/reference/evaluate_rubric_predictions.md)
supplies metrics and optional assumption diagnostics; diagnostic refits
run only when requested during evaluation. Internal rubric-label
validation holds CJ fixed and refits calibration from training labels
alone.

`ordinal_monotone` uses
[`mgcv::scasm()`](https://rdrr.io/pkg/mgcv/man/scasm.html) with
`s(z, bs = "sc", xt = "m+", k = k)` and
[`mgcv::ocat()`](https://rdrr.io/pkg/mgcv/man/ocat.html) with integer
categories `1:K`. The model is `logit P(Y <= k | z) = tau_k - eta(z)`,
where `eta` includes an intercept and a nondecreasing smooth. The
backend fixes the first threshold at -1 for identification; its identity
link describes the latent location, while category cumulative
probabilities follow the logistic distribution. Bootstrap is disabled.
Smoothing is estimated by EFS unless `sp` is fixed. A 1,001-point
calibration grid verifies nondecreasing latent locations and
nonincreasing cumulative probabilities with tolerance `1e-8`. Invalid
probabilities or monotonicity failures abort; no unconstrained fallback
is used. Essentially flat effects, singleton categories, and convergence
problems warn. Small bases and some reversed/separated label patterns
can fail in the backend; these return contextual fit errors. The
five-unique-score requirement is a backend feasibility guard, not a
recommended calibration sample size. Threshold uncertainty is
unavailable from this wrapper (the fixed first threshold has standard
error zero; the remaining entries are `NA`). Numerical median cutpoints
are reported only for unique crossings within the calibration range.
Flat or unresolved crossings and thresholds outside the range have `NA`
cutpoints and an explanatory status; scoring always uses probabilities.
Predictions beyond the labeled range are flagged and retain the
backend's spline extrapolation; their calibration is not established.

Percentile scoring uses
[`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) with type 8
at cumulative target proportions. This quantile type is fixed. A score
equal to a cutpoint enters the higher category: category is one plus the
number of cutpoints less than or equal to the score. Repeated cutpoints
can leave categories empty; all exact-score ties receive the same
category, even when every score is equal (in which case all items enter
the highest category). Requested proportions may be unattainable because
of ties or finite sample size. Cutpoint tie counts count all source
observations exactly equal to each cutpoint, including a count of one
when only one observation equals that cutpoint.

These are norm-referenced CJ-derived performance levels, not
criterion-referenced scores. Matching a historical rubric distribution
does not establish agreement with rubric raters. Recomputing cutpoints
on another cohort changes the reference distribution. Stored percentile
cutpoints support reuse of the original completed result only;
independent cohorts and linked target prediction are not supported.
Scoring conditions on accepted point locations, even when CJ posterior
draws are available. Percentile category probabilities and uncertainty
propagation are unavailable.

Phase A artifacts are checked using existing import-readiness rules,
including the existing explicit quality-gate override. Import readiness
does not assert that the originating adaptive run terminated. Existing
CJ diagnostics are retained; failed diagnostics produce a warning.

With `linked_anchors`, first obtain an import-ready Phase A artifact for
the human-scored `rubric_reference_set` and fit its ordinal calibration.
Next run Phase A for the target set, then the existing
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md)
Phase B linking workflow with the rubric reference set as hub and
targets as one or more spokes. Pass the completed linking result (or its
state) to [`predict()`](https://rdrr.io/r/stats/predict.html). Rubric
labels are used for calibration; Phase B does not require them. A rubric
reference set contains externally scored material; a Phase B **hub
anchor** is an item selected for routing recurring comparisons.

Historical prediction validates the stored reference set, stable item
IDs, exact reference locations and uncertainty, within-set evidence,
trait, orientation, and canonical fit contract. A configuration hash
alone does not establish metric identity. Original hashes are retained
as provenance; compatible legacy hashes follow the existing Phase A
import rules. Target-only Phase A scores cannot be used with the stored
calibration. Prediction consumes accepted Phase B common-scale scores
and reuses the stored reference transformation, never the target
cohort's mean or SD. Phase B's `theta_link_eap` field represents its
accepted MAP location with Laplace/Hessian uncertainty. Rubric scoring
is downstream of CJ estimation and does not run comparisons or change
Phase B estimation. Ordinal calibration conditions on accepted CJ
locations; joint CJ/rubric likelihood estimation is outside this API.

## See also

[`predict.pairwiseLLM_rubric_calibration()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_rubric_calibration.md),
[`evaluate_rubric_predictions()`](https://shmercer.github.io/pairwiseLLM/reference/evaluate_rubric_predictions.md),
[`vignette("rubric-calibration", package = "pairwiseLLM")`](https://shmercer.github.io/pairwiseLLM/articles/rubric-calibration.md)
for practical workflows.

Other rubric calibration:
[`evaluate_rubric_predictions()`](https://shmercer.github.io/pairwiseLLM/reference/evaluate_rubric_predictions.md),
[`predict.pairwiseLLM_rubric_calibration()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_rubric_calibration.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Starting from an already completed CJ result; no sampling occurs here.
fit <- fit_rubric_calibration(completed_cj, method = "percentile",
  trait = "organization", levels = c("developing", "proficient", "advanced"),
  target_distribution = c(0.2, 0.5, 0.3))
predict(fit)
fit$backend$achieved_proportions
fit$backend$cutpoint_tie_counts
# Human labels cover a subset of this same completed CJ result.
if (requireNamespace("ordinal", quietly = TRUE)) {
  ordinal_fit <- fit_rubric_calibration(completed_cj, rubric = rubric_labels,
    trait = "organization", levels = c("developing", "proficient", "advanced"))
  predictions <- predict(ordinal_fit)
  predictions$probabilities
  predictions$expected_level
}
if (requireNamespace("mgcv", quietly = TRUE) &&
    packageVersion("mgcv") >= "1.9.4" && requireNamespace("withr", quietly = TRUE)) {
  monotone_fit <- fit_rubric_calibration(completed_cj, rubric = rubric_labels,
    method = "ordinal_monotone", trait = "organization",
    levels = c("developing", "proficient", "advanced"), k = 6)
  predict(monotone_fit)$probabilities
}
# Reference and target Phase A, followed by Phase B, are completed upstream.
if (requireNamespace("ordinal", quietly = TRUE)) {
  linked_fit <- fit_rubric_calibration(reference_phase_a, reference_labels,
    calibration_design = "linked_anchors", trait = "organization",
    levels = c("developing", "proficient", "advanced"))
  predict(linked_fit) # Original reference items.
  target_scores <- predict(linked_fit, completed_phase_b)
  target_scores$set_id
  attr(target_scores, "linking")$diagnostics
}
} # }
```
