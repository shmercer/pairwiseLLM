# Decide whether to stop adaptive sampling based on stop metrics

This helper applies combined stopping criteria to the output of
[`bt_stop_metrics`](https://shmercer.github.io/pairwiseLLM/reference/bt_stop_metrics.md).
It is intended for round-based adaptive sampling: compute metrics each
round, then call this function to decide whether to continue.

## Usage

``` r
bt_should_stop(
  metrics,
  prev_metrics = NULL,
  reliability_target = 0.9,
  sepG_target = 3,
  rel_se_p90_target = 0.3,
  rel_se_p90_min_improve = 0.01,
  max_item_misfit_prop = 0.05,
  max_judge_misfit_prop = 0.05
)
```

## Arguments

- metrics:

  A one-row tibble returned by
  [`bt_stop_metrics`](https://shmercer.github.io/pairwiseLLM/reference/bt_stop_metrics.md).

- prev_metrics:

  Optional one-row tibble of prior-round metrics (same shape as
  `metrics`). Used to compute percent improvement criteria.

- reliability_target:

  Optional numeric. If not `NA`, require
  `metrics$reliability >= reliability_target`.

- sepG_target:

  Optional numeric. If not `NA`, require `metrics$sepG >= sepG_target`.

- rel_se_p90_target:

  Optional numeric. If not `NA`, precision target is met when
  `metrics$rel_se_p90 <= rel_se_p90_target`.

- rel_se_p90_min_improve:

  Optional numeric. If not `NA` and `prev_metrics` is provided, compute
  percent improvement `(prev - current) / prev`. Stalling is defined as
  `improve_pct <= rel_se_p90_min_improve`.

- max_item_misfit_prop:

  Optional numeric. If not `NA`, require
  `metrics$item_misfit_prop <= max_item_misfit_prop` (when metric is
  available).

- max_judge_misfit_prop:

  Optional numeric. If not `NA`, require
  `metrics$judge_misfit_prop <= max_judge_misfit_prop` (when metric is
  available).

## Value

A list with:

- stop:

  Logical; `TRUE` if stopping criteria are met.

- details:

  A tibble giving each criterion, its value, threshold, and pass/fail.

- improve:

  A tibble with computed percent improvement (if `prev_metrics`
  supplied).

## Details

Stopping requires:

- Reliability and separation thresholds (if provided), AND

- Fit thresholds (item/judge misfit proportions, if provided), AND

- Either precision target is met (`rel_se_p90 <= rel_se_p90_target`), OR
  improvement has stalled relative to the previous round
  (`rel_se_p90_improve_pct <= rel_se_p90_min_improve`).

## Examples

``` r
m1 <- tibble::tibble(
  reliability = 0.92, sepG = 3.2, rel_se_p90 = 0.25,
  item_misfit_prop = 0.00, judge_misfit_prop = 0.00
)
res <- bt_should_stop(m1, reliability_target = 0.90, sepG_target = 3.0, rel_se_p90_target = 0.30)
res$stop
#> [1] TRUE
res$details
#> # A tibble: 6 × 4
#>   criterion            value threshold pass 
#>   <chr>                <dbl>     <dbl> <lgl>
#> 1 reliability           0.92      0.9  TRUE 
#> 2 sepG                  3.2       3    TRUE 
#> 3 item_misfit_prop      0         0.05 TRUE 
#> 4 judge_misfit_prop     0         0.05 TRUE 
#> 5 rel_se_p90_precision  0.25      0.3  TRUE 
#> 6 rel_se_p90_stability NA         0.01 FALSE
```
