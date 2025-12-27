# Compute drift metrics between two theta estimates

This helper summarizes how a set of item scores (theta) has changed
between two model fits (e.g., across waves/batches when using a core
linking set).

## Usage

``` r
bt_drift_metrics(
  current,
  previous,
  ids = NULL,
  prefix = "",
  abs_shift_probs = c(0.9, 0.95),
  methods = c("pearson", "spearman")
)
```

## Arguments

- current:

  Current theta estimates.

- previous:

  Previous theta estimates.

- ids:

  Optional character vector of IDs to compute drift on (e.g., a core
  set). If `NULL`, uses the intersection of IDs present in both inputs.

- prefix:

  Optional string prefix to apply to output column names.

- abs_shift_probs:

  Numeric vector of probabilities for absolute-shift quantiles. Default
  `c(0.9, 0.95)`.

- methods:

  Character vector indicating which correlation(s) to compute.
  Supported: `"pearson"`, `"spearman"`. Default both.

## Value

A one-row tibble with drift summary columns, including:

- n:

  Number of items used for drift computation.

- theta_cor:

  Pearson correlation between current and previous theta (if requested).

- theta_spearman:

  Spearman correlation between current and previous theta (if
  requested).

- mean_abs_shift:

  Mean absolute shift in theta.

- p90_abs_shift:

  90th percentile absolute shift (if requested).

- p95_abs_shift:

  95th percentile absolute shift (if requested).

- max_abs_shift:

  Maximum absolute shift.

- mean_signed_shift:

  Mean signed shift (current - previous).

## Details

Inputs can be:

- A list returned by
  [`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
  (uses `$theta`),

- A tibble/data frame containing columns `ID` and `theta`, or

- A named numeric vector of theta values.

## Examples

``` r
cur <- tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 1, 2))
prev <- tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 0.5, 2.5))
bt_drift_metrics(cur, prev, prefix = "core_")
#> # A tibble: 1 × 8
#>   core_n core_mean_abs_shift core_max_abs_shift core_mean_signed_shift
#>    <int>               <dbl>              <dbl>                  <dbl>
#> 1      3               0.333                0.5                      0
#> # ℹ 4 more variables: core_p90_abs_shift <dbl>, core_p95_abs_shift <dbl>,
#> #   core_theta_cor <dbl>, core_theta_spearman <dbl>
```
