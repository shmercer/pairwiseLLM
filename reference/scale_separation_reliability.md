# Calculate scale-separation reliability

Calculate conventional SSR as `1 - mean(se^2) / stats::var(theta)` and
expose its variance components. The observed variance uses the sample
denominator `n - 1`. Estimates and SEs must use the same scale and item
order.

## Usage

``` r
scale_separation_reliability(theta, se)
```

## Arguments

- theta:

  Real numeric vector of finite item estimates, with at least two
  entries and positive sample variance.

- se:

  Real numeric vector of finite, nonnegative standard errors, in the
  same order and of the same length as `theta`. Names are not used to
  align the vectors.

## Value

A list with `observed_variance`, `mean_squared_se`,
`true_score_variance` (observed variance minus mean squared SE), `ssr`,
`n_items`, `n_finite`, `valid`, and `status`. Successful calculations
have `valid = TRUE` and status `"ok"` or
`"negative_true_score_variance"`. Invalid inputs or nonfinite calculated
components raise an error; no items are dropped and no coefficient is
returned for an undefined calculation.

## Details

Negative SSR and negative estimated true-score variance are retained,
not clipped. They indicate that mean squared uncertainty exceeds
observed score variance. SSR depends on the estimator, its SE
convention, and the estimated score variance; it is not an
estimator-free measure of recovery or accuracy.

## See also

[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)

Other frequentist models:
[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md),
[`build_elo_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_elo_data.md),
[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md),
[`fit_elo_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_elo_model.md),
[`predict.pairwiseLLM_bt_alpha()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_bt_alpha.md),
[`predict.pairwiseLLM_bt_firth()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_bt_firth.md),
[`summarize_bt_fit()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_bt_fit.md)

## Examples

``` r
scale_separation_reliability(c(-1, 0, 1), c(0.2, 0.3, 0.4))
#> $observed_variance
#> [1] 1
#> 
#> $mean_squared_se
#> [1] 0.09666667
#> 
#> $true_score_variance
#> [1] 0.9033333
#> 
#> $ssr
#> [1] 0.9033333
#> 
#> $n_items
#> [1] 3
#> 
#> $n_finite
#> [1] 3
#> 
#> $valid
#> [1] TRUE
#> 
#> $status
#> [1] "ok"
#> 
```
