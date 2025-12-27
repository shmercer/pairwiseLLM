# Decide whether to stop adaptive sampling based on stop metrics

Applies combined stopping criteria to the output of
[`bt_stop_metrics`](https://shmercer.github.io/pairwiseLLM/reference/bt_stop_metrics.md).
Intended use is round-based adaptive sampling:

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
  max_judge_misfit_prop = 0.05,
  core_theta_cor_target = NA_real_,
  core_theta_spearman_target = NA_real_,
  core_max_abs_shift_target = NA_real_,
  core_p90_abs_shift_target = NA_real_
)
```

## Arguments

- metrics:

  A one-row tibble returned by
  [`bt_stop_metrics`](https://shmercer.github.io/pairwiseLLM/reference/bt_stop_metrics.md).

- prev_metrics:

  Optional one-row tibble of prior-round metrics (same shape as
  `metrics`). Used to compute percent improvement for the stability
  criterion.

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

- core_theta_cor_target:

  Optional numeric. If not `NA`, require
  `metrics$core_theta_cor >= core_theta_cor_target`.

- core_theta_spearman_target:

  Optional numeric. If not `NA`, require
  `metrics$core_theta_spearman >= core_theta_spearman_target`.

- core_max_abs_shift_target:

  Optional numeric. If not `NA`, require
  `metrics$core_max_abs_shift <= core_max_abs_shift_target`.

- core_p90_abs_shift_target:

  Optional numeric. If not `NA`, require
  `metrics$core_p90_abs_shift <= core_p90_abs_shift_target`.

## Value

A list with:

- stop:

  Logical; `TRUE` if stopping criteria are met.

- details:

  A tibble listing each criterion, its value, threshold, and pass/fail.

- improve:

  A tibble with computed percent improvement (if `prev_metrics`
  supplied).

## Details

1.  Fit or update the model,

2.  Compute metrics with
    [`bt_stop_metrics()`](https://shmercer.github.io/pairwiseLLM/reference/bt_stop_metrics.md),

3.  Decide stop/continue with `bt_should_stop()`.

The decision can incorporate:

- Reliability and separation thresholds (when available),

- Fit thresholds (item/judge misfit proportions; when available),

- Precision target (`rel_se_p90 <= rel_se_p90_target`),

- Optional stability criterion vs `prev_metrics`,

- Optional drift guardrails for core linking workflows (disabled by
  default).

Core drift guardrails are enabled by setting one or more `core_*_target`
arguments (otherwise they default to `NA` and are ignored).

## Examples

``` r
# Example metrics (as if returned by bt_stop_metrics())
m <- tibble::tibble(
  reliability = 0.92,
  sepG = 3.2,
  rel_se_p90 = 0.25,
  item_misfit_prop = 0.00,
  judge_misfit_prop = 0.00
)

# Stop if precision target is met and other thresholds pass
bt_should_stop(m, rel_se_p90_target = 0.30)$stop
#> [1] TRUE

# Include a previous round to evaluate stability (diminishing returns)
prev_m <- tibble::tibble(
  reliability = 0.91,
  sepG = 3.1,
  rel_se_p90 = 0.26,
  item_misfit_prop = 0.00,
  judge_misfit_prop = 0.00
)
bt_should_stop(m, prev_metrics = prev_m, rel_se_p90_min_improve = 0.01)$stop
#> [1] TRUE

# Drift gating example: only stop if core drift guardrails pass
m2 <- dplyr::bind_cols(
  m,
  tibble::tibble(
    core_theta_cor = 0.80,
    core_theta_spearman = 1.00,
    core_max_abs_shift = 0.60,
    core_p90_abs_shift = 0.50
  )
)

# This will NOT stop because correlation guardrail fails (0.80 < 0.90)
bt_should_stop(m2, core_theta_cor_target = 0.90)$stop
#> [1] FALSE

# This WILL stop because drift thresholds are relaxed
bt_should_stop(
  m2,
  core_theta_cor_target = 0.70,
  core_max_abs_shift_target = 0.70,
  core_p90_abs_shift_target = 0.60
)$stop
#> [1] TRUE
```
