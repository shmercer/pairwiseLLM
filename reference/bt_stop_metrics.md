# Compute stopping metrics from a Bradley–Terry model fit

This helper computes round-level summary metrics that are useful for
adaptive sampling and stopping decisions. It is designed to work with
the object returned by
[`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md).

## Usage

``` r
bt_stop_metrics(fit, se_probs = c(0.5, 0.9, 0.95), fit_bounds = c(0.7, 1.3))
```

## Arguments

- fit:

  A list returned by
  [`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md).

- se_probs:

  Numeric vector of probabilities for SE quantiles. Default:
  `c(0.5, 0.9, 0.95)`.

- fit_bounds:

  Numeric length-2 vector giving lower/upper bounds for acceptable
  infit/outfit. Default: `c(0.7, 1.3)`.

## Value

A one-row tibble with summary metrics. Key columns include:

- engine:

  Modeling engine used ("sirt", "BradleyTerry2", or `NA`).

- n_items:

  Number of objects in `fit$theta`.

- theta_sd:

  SD of `theta`.

- se_mean:

  Mean SE.

- se_p90:

  90th percentile SE (if requested in `se_probs`).

- se_p95:

  95th percentile SE (if requested in `se_probs`).

- se_max:

  Maximum SE.

- rel_se_mean:

  `se_mean / theta_sd` (scale-free; `NA` if `theta_sd <= 0`).

- rel_se_p90:

  `se_p90 / theta_sd` (scale-free; `NA` if `theta_sd <= 0`).

- reliability:

  MLE reliability (typically for sirt) or `NA`.

- sepG:

  Separation index if available, otherwise `NA`.

- item_misfit_prop:

  Proportion of items with infit/outfit outside `fit_bounds`.

- judge_misfit_prop:

  Proportion of judges with infit/outfit outside `fit_bounds`.

## Details

The output includes precision summaries (SE distribution), scale
summaries (SD of `theta`), and scale-free precision metrics (SE /
SD(theta)). If diagnostic outputs are available (from
`fit_bt_model(..., return_diagnostics = TRUE)` using the sirt engine),
the output also includes separation index (`sepG`) and item/judge misfit
proportions based on infit/outfit bounds.

## Examples

``` r
# Minimal example using a mock fit object (runs without sirt installed)
fit_mock <- list(
  engine = "mock",
  reliability = 0.90,
  theta = tibble::tibble(
    ID = c("A", "B", "C"),
    theta = c(0.0, 1.0, -1.0),
    se = c(0.3, 0.2, 0.4)
  )
)
bt_stop_metrics(fit_mock)
#> # A tibble: 1 × 14
#>   engine n_items theta_sd se_mean se_max rel_se_mean rel_se_p90 reliability
#>   <chr>    <int>    <dbl>   <dbl>  <dbl>       <dbl>      <dbl>       <dbl>
#> 1 mock         3        1     0.3    0.4         0.3       0.38         0.9
#> # ℹ 6 more variables: sepG <dbl>, item_misfit_prop <dbl>,
#> #   judge_misfit_prop <dbl>, se_p50 <dbl>, se_p90 <dbl>, se_p95 <dbl>

# Real example (only runs if sirt is installed)
if (requireNamespace("sirt", quietly = TRUE)) {
  data("example_writing_pairs", package = "pairwiseLLM")
  bt <- build_bt_data(example_writing_pairs)
  fit <- fit_bt_model(bt, engine = "sirt", verbose = FALSE, return_diagnostics = TRUE)
  bt_stop_metrics(fit)
}
#> # A tibble: 1 × 14
#>   engine n_items theta_sd se_mean se_max rel_se_mean rel_se_p90 reliability
#>   <chr>    <int>    <dbl>   <dbl>  <dbl>       <dbl>      <dbl>       <dbl>
#> 1 sirt        20     1.51   0.918   1.16       0.609      0.748       0.622
#> # ℹ 6 more variables: sepG <dbl>, item_misfit_prop <dbl>,
#> #   judge_misfit_prop <dbl>, se_p50 <dbl>, se_p90 <dbl>, se_p95 <dbl>
```
