# Predict rubric scores from a calibration

Predict rubric scores from a calibration

## Usage

``` r
# S3 method for class 'pairwiseLLM_rubric_calibration'
predict(object, newdata = NULL, hard_score = c("median", "mode"), ...)
```

## Arguments

- object:

  A `pairwiseLLM_rubric_calibration` object.

- newdata:

  Completed CJ result to predict, or `NULL` for the original items.
  Same-set scoring accepts the original completed result with unchanged
  item IDs, exact accepted scores and uncertainty, and matching
  fit/reference evidence. Item reordering is allowed; collection
  provenance does not affect scoring. Raw tables, independent cohorts,
  refits, and Phase B targets are not supported for same-set prediction.
  For `linked_anchors`, `NULL` scores the original reference items;
  explicit `newdata` must be a completed Phase B result (or its state)
  on the stored reference scale. It returns only target items across all
  spokes, in their input order. Every spoke must have an accepted Phase
  B refit and committed active hub-spoke evidence.

- hard_score:

  Ordinal hard-score rule: `"median"` (default) or `"mode"`. The median
  is the lowest category whose cumulative probability is at least 0.5;
  equality at a median cutpoint therefore selects the lower category.
  Modal ties select the lowest category. Ordinal output always retains
  all category probabilities, both decision rules, and expected level.
  Both choices give the same deterministic category for percentile
  scoring.

- ...:

  Reserved arguments; currently must be empty.

## Value

For percentile scoring, a tibble with `item_id`, accepted source
`theta`, integer `category` in `1:K`, original-label `rubric_score`, and
`extrapolated` (outside the fitted CJ range). Under the original-result
restriction, extrapolation flags are always false. Stored cutpoints are
reused unchanged. No category probabilities or probabilistic summaries
are returned. Both ordinal methods include the same five columns, plus
`probabilities`, a list-column of numeric K-vectors named by the
original ordered labels; integer `median_category` and `modal_category`;
and `expected_level = sum(k * P(Y = k))` for internal indices `k` in
`1:K`, regardless of the original labels' numerical spacing. This
expected rubric level is a continuous summary, not a replacement for the
ordinal result. `category` and `rubric_score` use the requested
`hard_score` rule. Ordinal `extrapolated` flags use the labeled
calibration range, so unlabeled source items can be extrapolated;
endpoints are included in the range. Stored standardization is reused.
Unfitted objects fail clearly. Linked target output adds `set_id`
(target spoke), `source_item_id`, `global_item_id`, and `theta_sd`
(accepted Phase B uncertainty, possibly `NA`). Its `linking` attribute
contains normalized `reference`, `fit_contract`, `provenance` (including
hub/spoke IDs and linking stage logs), `diagnostics`, and `reliability`.
Provenance estimation/uncertainty methods reflect validated spoke
contracts, including accepted-state reuse. Each is a single string when
all spokes agree, otherwise the distinct strings in spoke order;
per-spoke contracts retain the complete attribution. Failed upstream
diagnostics warn and remain available; accepted scores do not assert
adequate precision or successful stopping. CJ uncertainty is retained as
metadata and is not propagated into category probabilities. Reference
predictions and same-set output keep their existing columns and have no
`linking` attribute.

## See also

[`fit_rubric_calibration()`](https://shmercer.github.io/pairwiseLLM/reference/fit_rubric_calibration.md),
[`evaluate_rubric_predictions()`](https://shmercer.github.io/pairwiseLLM/reference/evaluate_rubric_predictions.md)

Other rubric calibration:
[`evaluate_rubric_predictions()`](https://shmercer.github.io/pairwiseLLM/reference/evaluate_rubric_predictions.md),
[`fit_rubric_calibration()`](https://shmercer.github.io/pairwiseLLM/reference/fit_rubric_calibration.md),
[`prepare_linked_rubric_reference()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_linked_rubric_reference.md)
