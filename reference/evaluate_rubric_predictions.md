# Evaluate rubric predictions on observed ordered labels

Compute rubric prediction metrics conditional on completed CJ point
scores. Normalized ranked probability score (RPS) is the primary ordinal
probability metric. Additional assumption diagnostics run only when
requested here.

## Usage

``` r
evaluate_rubric_predictions(
  object,
  rubric,
  newdata = NULL,
  hard_score = c("median", "mode"),
  bins = 10L,
  diagnostics = FALSE
)
```

## Arguments

- object:

  A fitted `pairwiseLLM_rubric_calibration` object.

- rubric:

  Data frame with unique `item_id` and `rubric_score`, optionally
  `trait`. Labels align by ID and use the fitted level order. Missing
  labels are excluded and counted. Evaluation need not represent every
  category.

- newdata:

  As in
  [`predict.pairwiseLLM_rubric_calibration()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_rubric_calibration.md):
  `NULL` predicts original items; linked targets require accepted Phase
  B results.

- hard_score:

  Hard category rule, default `"median"`, optionally `"mode"`.

- bins:

  Positive integer number of equal-width bins (default 10) for
  cumulative calibration summaries. Empty bins are omitted; ties stay
  together.

- diagnostics:

  Logical; request additional training-model diagnostics. Default
  `FALSE` avoids diagnostic refits. Evaluation labels never refit the
  production model or enter its training-model diagnostics.

## Value

A list with `metrics` (one-row tibble), `per_item` (predictions,
observed labels, uncertainty, overlap flags and individual losses),
`calibration` (cumulative summaries, or `NULL` for percentile),
`diagnostics`, and `metadata`. Metrics are `n`, `rps`, `log_loss`,
`exact_accuracy`, `within_one_accuracy`, `mae`, and
`quadratic_weighted_kappa`. Unavailable probability metrics are NA.
Summary columns are `group_by`, `boundary`, `rubric_level`, `bin`, `n`,
`theta_min`, `theta_max`, `probability_min`, `probability_max`,
`predicted`, `observed`, and `residual` (observed minus predicted).
Metadata records exclusions, training overlap, extrapolation, metric
conventions, unavailable reasons, and original linking metadata when
present. Requested diagnostics contain `status`, `stored`,
`functional_form`, and `common_effect`; their statuses distinguish
formal, descriptive and unavailable results. No input object or upstream
CJ state is modified.

## Details

For K levels, RPS for item i is
`sum((F[i, k] - I(Y[i] <= k))^2) / (K - 1)` over boundaries
`k = 1, ..., K-1`. This is normalized RPS, not the unnormalized sum;
lower is better and zero is perfect. Log loss is `-log(p[i, Y[i]])`.
Observed-category probabilities are floored at `.Machine$double.xmin`
before taking logs, solely during evaluation to handle zero/underflow
probabilities. Stored predictions are unchanged. Both metrics are
averaged over observed labels.

Hard metrics are exact accuracy, within-one accuracy, mean absolute
category error, and quadratic weighted kappa. Distances use internal
indices `1:K`, not numeric gaps between user labels. Kappa is one minus
observed divided by expected weighted disagreement, with weights
`(i-j)^2/(K-1)^2` and independent empirical marginals. Zero expected
disagreement returns `NA` with a reason. Percentile scoring supplies
only hard metrics; no probabilities are invented.

Cumulative summaries compare `P(Y <= k)` with observed cumulative
frequencies within equal-width probability bins on `[0, 1]` and theta
bins over the evaluated range. Internal edges enter the higher bin; the
upper endpoint stays in the last bin. A constant theta range occupies
one bin. These descriptive tables require no plotting dependency and do
not constitute formal goodness-of-fit tests. Training-label overlap is
reported; apparent performance is not out-of-sample validation.

Requested diagnostics expose stored numerical/uncertainty information
and training-only cumulative summaries. For linear fits, an explicit
threshold-varying
[`ordinal::clm`](https://rdrr.io/pkg/ordinal/man/clm.html) alternative
supplies a proportional-odds likelihood-ratio diagnostic.
Convergence/identification failures produce an unavailable status. A
p-value below .05 triggers an exploratory review warning, not automatic
selection. This test is distinct from functional-form assessment.
Monotone common-effect checks are descriptive boundary-specific binomial
fits against the estimated latent effect (expected cumulative slope -1),
with no omnibus p-value or automatic adequacy threshold. Diagnostic
failures preserve the fitted calibration. Optional backends are required
only when used.

Internal rubric-label cross-validation holds out labels while keeping
the completed CJ evidence and theta fixed. Each fold re-estimates
calibration scaling and parameters from training labels alone.
Linked-reference CV assesses reference calibration; externally labeled
Phase B targets assess transport. Invalid training folds are reported
without collapsing categories, and prevent an overall CV estimate. If
resamples are used to choose methods or tuning, unbiased post-selection
assessment requires an outer validation layer. No automatic method
selection is supplied. Core fits condition on point theta; retained CJ
uncertainty is not propagated through a posterior/bootstrap engine.

## See also

[`fit_rubric_calibration()`](https://shmercer.github.io/pairwiseLLM/reference/fit_rubric_calibration.md),
[`predict.pairwiseLLM_rubric_calibration()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_rubric_calibration.md)

Other rubric calibration:
[`fit_rubric_calibration()`](https://shmercer.github.io/pairwiseLLM/reference/fit_rubric_calibration.md),
[`predict.pairwiseLLM_rubric_calibration()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_rubric_calibration.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Rubric labels align with items in an already completed CJ result.
if (requireNamespace("ordinal", quietly = TRUE)) {
  fit <- fit_rubric_calibration(completed_cj, training_labels,
    trait = "organization", levels = c("developing", "proficient", "advanced"))
  assessment <- evaluate_rubric_predictions(fit, evaluation_labels, diagnostics = TRUE)
  assessment$metrics
  assessment$calibration
  assessment$metadata$n_training_label_overlap
  assessment$diagnostics$common_effect
}
} # }
```
