# Compute stopping metrics from a Bradley–Terry model fit

This helper computes round-level summary metrics used for adaptive
sampling and stopping decisions. It is designed to work with the object
returned by
[`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
(which includes `fit$theta` with columns `ID`, `theta`, and `se`).

## Usage

``` r
bt_stop_metrics(
  fit,
  ids = NULL,
  prev_fit = NULL,
  core_ids = NULL,
  se_probs = c(0.5, 0.9, 0.95),
  fit_bounds = c(0.7, 1.3)
)
```

## Arguments

- fit:

  A list returned by
  [`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
  containing a `$theta` tibble/data frame with columns `ID`, `theta`,
  `se`.

- ids:

  Optional character vector of item IDs to compute precision summaries
  on. If `NULL`, uses all items in `fit$theta`.

- prev_fit:

  Optional prior fit (same structure as `fit`) used for drift metrics.
  Must be provided together with `core_ids`.

- core_ids:

  Optional character vector of core IDs used for drift metrics. Must be
  provided together with `prev_fit`.

- se_probs:

  Numeric vector of probabilities for SE quantiles. Default:
  `c(0.5, 0.9, 0.95)`.

- fit_bounds:

  Numeric length-2 vector giving lower/upper bounds for acceptable
  infit/outfit when diagnostics are available. Default: `c(0.7, 1.3)`.

## Value

A one-row tibble of stopping metrics.

## Details

The output is a one-row tibble with:

- Precision summaries (e.g., SE mean, max, and quantiles),

- Scale summaries (SD of `theta`),

- Scale-free precision metrics (SE divided by SD of `theta`),

- Optional fit/diagnostic summaries (separation index and misfit
  proportions),

- Optional drift metrics for a core set relative to a prior fit.

You can compute precision summaries on a subset of IDs (e.g., only
newly-added items) via `ids`. Drift metrics are added when both
`prev_fit` and `core_ids` are provided.

## Examples

``` r
# A minimal, CRAN-safe "mock fit" with the required structure:
fit <- list(
  engine = "mock",
  theta = tibble::tibble(
    ID = c("A", "B", "C", "D"),
    theta = c(0, 1, 2, 3),
    se = c(0.20, 0.30, 0.40, 0.50)
  )
)

# Compute metrics on all items
bt_stop_metrics(fit)
#> # A tibble: 1 × 15
#>   engine n_items n_total_items theta_sd se_mean se_max rel_se_mean rel_se_p90
#>   <chr>    <int>         <int>    <dbl>   <dbl>  <dbl>       <dbl>      <dbl>
#> 1 mock         4             4     1.29    0.35    0.5       0.271      0.364
#> # ℹ 7 more variables: reliability <dbl>, sepG <dbl>, item_misfit_prop <dbl>,
#> #   judge_misfit_prop <dbl>, se_p50 <dbl>, se_p90 <dbl>, se_p95 <dbl>

# Compute metrics only on a subset (e.g., newly-added items)
bt_stop_metrics(fit, ids = c("A", "C"))
#> # A tibble: 1 × 15
#>   engine n_items n_total_items theta_sd se_mean se_max rel_se_mean rel_se_p90
#>   <chr>    <int>         <int>    <dbl>   <dbl>  <dbl>       <dbl>      <dbl>
#> 1 mock         2             4     1.41     0.3    0.4       0.212      0.269
#> # ℹ 7 more variables: reliability <dbl>, sepG <dbl>, item_misfit_prop <dbl>,
#> #   judge_misfit_prop <dbl>, se_p50 <dbl>, se_p90 <dbl>, se_p95 <dbl>

# Add core drift metrics relative to a previous fit
prev_fit <- list(
  engine = "mock",
  theta = tibble::tibble(
    ID = c("A", "B", "C", "D"),
    theta = c(0, 0.5, 2.5, 3),
    se = c(0.20, 0.20, 0.20, 0.20)
  )
)
bt_stop_metrics(
  fit,
  prev_fit = prev_fit,
  core_ids = c("A", "B", "C", "D")
)
#> # A tibble: 1 × 23
#>   engine n_items n_total_items theta_sd se_mean se_max rel_se_mean rel_se_p90
#>   <chr>    <int>         <int>    <dbl>   <dbl>  <dbl>       <dbl>      <dbl>
#> 1 mock         4             4     1.29    0.35    0.5       0.271      0.364
#> # ℹ 15 more variables: reliability <dbl>, sepG <dbl>, item_misfit_prop <dbl>,
#> #   judge_misfit_prop <dbl>, se_p50 <dbl>, se_p90 <dbl>, se_p95 <dbl>,
#> #   core_n <int>, core_mean_abs_shift <dbl>, core_max_abs_shift <dbl>,
#> #   core_mean_signed_shift <dbl>, core_p90_abs_shift <dbl>,
#> #   core_p95_abs_shift <dbl>, core_theta_cor <dbl>, core_theta_spearman <dbl>
```
