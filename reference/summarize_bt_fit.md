# Summarize a Bradley–Terry model fit

This helper takes the object returned by
[`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
and returns a tibble with one row per object (e.g., writing sample),
including:

- `ID`: object identifier

- `theta`: estimated ability parameter

- `se`: standard error of `theta`

- `rank`: rank order of `theta` (1 = highest by default)

- `engine`: modeling engine used ("sirt", "BradleyTerry2", "brglm2", or
  "alpha")

- `reliability`: raw sirt reliability, calculated Firth/alpha SSR, or
  `NA`

## Usage

``` r
summarize_bt_fit(fit, decreasing = TRUE, verbose = TRUE)
```

## Arguments

- fit:

  A list returned by
  [`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md).

- decreasing:

  Logical; should higher `theta` values receive lower rank numbers? If
  `TRUE` (default), the highest `theta` gets `rank = 1`.

- verbose:

  Logical. If `TRUE` (default), emit warnings when coercing. If `FALSE`,
  suppress coercion warnings during ranking.

## Value

A tibble with columns:

- ID:

  Object identifier.

- theta:

  Estimated ability parameter.

- se:

  Standard error of `theta`.

- rank:

  Rank of `theta`; 1 = highest (if `decreasing = TRUE`).

- engine:

  Modeling engine used ("sirt", "BradleyTerry2", "brglm2", or "alpha").

- reliability:

  Reliability (numeric scalar, or `NA`) repeated on each row.

## Details

Standard errors describe model uncertainty; small differences in
estimates should not be interpreted without considering that
uncertainty. Reliability is a study-level summary repeated on each row,
not an item-specific score. The returned rows retain input order; sort
explicitly by `rank` or `theta` when preparing a ranked report.

## See also

[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md),
[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)

Other frequentist models:
[`bootstrap_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/bootstrap_bt_model.md),
[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md),
[`build_elo_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_elo_data.md),
[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md),
[`fit_elo_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_elo_model.md),
[`predict.pairwiseLLM_bt_alpha()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_bt_alpha.md),
[`predict.pairwiseLLM_bt_firth()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_bt_firth.md),
[`predict.pairwiseLLM_bt_lapse()`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_bt_lapse.md),
[`scale_separation_reliability()`](https://shmercer.github.io/pairwiseLLM/reference/scale_separation_reliability.md)

## Examples

``` r
# Example using built-in comparison data
data("example_writing_pairs")
bt <- build_bt_data(example_writing_pairs)

if (requireNamespace("sirt", quietly = TRUE)) {
  fit1 <- fit_bt_model(bt, engine = "sirt", verbose = FALSE)
  summarize_bt_fit(fit1)
}
#> # A tibble: 20 × 6
#>    ID      theta    se  rank engine reliability
#>    <chr>   <dbl> <dbl> <int> <chr>        <dbl>
#>  1 S18    2.88   1.16      1 sirt         0.622
#>  2 S20    1.73   0.985     3 sirt         0.622
#>  3 S19    0.865  0.772     7 sirt         0.622
#>  4 S17    0.900  0.833     6 sirt         0.622
#>  5 S13    1.91   0.794     2 sirt         0.622
#>  6 S15    1.12   0.842     4 sirt         0.622
#>  7 S16    0.711  0.805     8 sirt         0.622
#>  8 S14    0.921  0.836     5 sirt         0.622
#>  9 S11    0.185  0.851    11 sirt         0.622
#> 10 S12   -0.239  0.826    13 sirt         0.622
#> 11 S09    0.402  0.819     9 sirt         0.622
#> 12 S10   -0.0193 0.853    12 sirt         0.622
#> 13 S08    0.206  0.849    10 sirt         0.622
#> 14 S07   -0.776  0.984    14 sirt         0.622
#> 15 S06   -1.05   1.01     16 sirt         0.622
#> 16 S05   -1.33   1.04     17 sirt         0.622
#> 17 S02   -0.919  0.810    15 sirt         0.622
#> 18 S04   -2.66   1.12     19 sirt         0.622
#> 19 S01   -1.85   1.01     18 sirt         0.622
#> 20 S03   -3.00   1.16     20 sirt         0.622
if (requireNamespace("BradleyTerry2", quietly = TRUE)) {
  fit2 <- fit_bt_model(bt, engine = "BradleyTerry2", verbose = FALSE)
  summarize_bt_fit(fit2)
}
#> # A tibble: 20 × 6
#>    ID       theta    se  rank engine        reliability
#>    <chr>    <dbl> <dbl> <int> <chr>               <dbl>
#>  1 S01   0         0       20 BradleyTerry2          NA
#>  2 S02   1.90e+ 0  1.58    17 BradleyTerry2          NA
#>  3 S03   6.93e-17  1.49    19 BradleyTerry2          NA
#>  4 S04   9.94e- 1  1.48    18 BradleyTerry2          NA
#>  5 S05   2.79e+ 0  1.71    16 BradleyTerry2          NA
#>  6 S06   3.68e+ 0  1.83    15 BradleyTerry2          NA
#>  7 S07   4.54e+ 0  1.93    14 BradleyTerry2          NA
#>  8 S08   6.04e+ 0  2.05    13 BradleyTerry2          NA
#>  9 S09   6.66e+ 0  2.08    11 BradleyTerry2          NA
#> 10 S10   6.66e+ 0  2.08    12 BradleyTerry2          NA
#> 11 S11   7.25e+ 0  2.10     9 BradleyTerry2          NA
#> 12 S12   7.25e+ 0  2.10    10 BradleyTerry2          NA
#> 13 S13   9.48e+ 0  2.19     7 BradleyTerry2          NA
#> 14 S14   8.93e+ 0  2.17     8 BradleyTerry2          NA
#> 15 S15   9.48e+ 0  2.19     5 BradleyTerry2          NA
#> 16 S16   9.48e+ 0  2.19     6 BradleyTerry2          NA
#> 17 S17   1.00e+ 1  2.22     4 BradleyTerry2          NA
#> 18 S18   1.23e+ 1  2.45     1 BradleyTerry2          NA
#> 19 S19   1.06e+ 1  2.26     3 BradleyTerry2          NA
#> 20 S20   1.23e+ 1  2.45     2 BradleyTerry2          NA
```
