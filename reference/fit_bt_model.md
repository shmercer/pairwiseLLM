# Fit a Bradley–Terry model with sirt and fallback to BradleyTerry2

This function fits a Bradley–Terry paired-comparison model to data
prepared by
[`build_bt_data`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md).
It supports two modeling engines:

- sirt: [`btm`](https://rdrr.io/pkg/sirt/man/btm.html) — the preferred
  engine, which produces ability estimates, standard errors, and MLE
  reliability.

- BradleyTerry2: [`BTm`](https://rdrr.io/pkg/BradleyTerry2/man/BTm.html)
  — used as a fallback if sirt is unavailable or fails; computes ability
  estimates and standard errors, but not reliability.

## Usage

``` r
fit_bt_model(
  bt_data,
  engine = c("auto", "sirt", "BradleyTerry2"),
  verbose = TRUE,
  ...
)
```

## Arguments

- bt_data:

  A data frame or tibble with exactly three columns: two character ID
  columns and one numeric `result` column equal to 0 or 1. Usually
  produced by
  [`build_bt_data`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md).

- engine:

  Character string specifying the modeling engine. One of: `"auto"`
  (default), `"sirt"`, or `"BradleyTerry2"`.

- verbose:

  Logical. If `TRUE` (default), show engine output (iterations,
  warnings). If `FALSE`, suppress noisy output to keep examples and
  reports clean.

- ...:

  Additional arguments passed through to
  [`sirt::btm()`](https://rdrr.io/pkg/sirt/man/btm.html) or
  [`BradleyTerry2::BTm()`](https://rdrr.io/pkg/BradleyTerry2/man/BTm.html).

## Value

A list with the following elements:

- engine:

  The engine actually used ("sirt" or "BradleyTerry2").

- fit:

  The fitted model object.

- theta:

  A tibble with columns:

  - `ID`: object identifier

  - `theta`: estimated ability parameter

  - `se`: standard error of `theta`

- reliability:

  MLE reliability (sirt engine only). `NA` for BradleyTerry2 models.

## Details

When `engine = "auto"` (the default), the function attempts sirt first
and automatically falls back to BradleyTerry2 only if necessary. In all
cases, the output format is standardized, so downstream code can rely on
consistent fields.

The input `bt_data` must contain exactly three columns:

1.  object1: character ID for the first item in the pair

2.  object2: character ID for the second item

3.  result: numeric indicator (1 = object1 wins, 0 = object2 wins)

Ability estimates (`theta`) represent latent "writing quality"
parameters on a log-odds scale. Standard errors are included for both
modeling engines. MLE reliability is only available from sirt.

## Examples

``` r
# Example using built-in comparison data
data("example_writing_pairs")
bt <- build_bt_data(example_writing_pairs)

fit1 <- fit_bt_model(bt, engine = "sirt")
#> Warning: NAs introduced by coercion
#> **** Iteration 1 | Maximum parameter change=0.9874205
#> **** Iteration 2 | Maximum parameter change=0.9604
#> **** Iteration 3 | Maximum parameter change=0.941192
#> **** Iteration 4 | Maximum parameter change=0.9223682
#> **** Iteration 5 | Maximum parameter change=0.9039208
#> **** Iteration 6 | Maximum parameter change=0.8858424
#> **** Iteration 7 | Maximum parameter change=0.8681255
#> **** Iteration 8 | Maximum parameter change=0.850763
#> **** Iteration 9 | Maximum parameter change=0.8337478
#> **** Iteration 10 | Maximum parameter change=0.8170728
#> **** Iteration 11 | Maximum parameter change=0.8007314
#> **** Iteration 12 | Maximum parameter change=0.7847167
#> **** Iteration 13 | Maximum parameter change=0.7690224
#> **** Iteration 14 | Maximum parameter change=0.7536419
#> **** Iteration 15 | Maximum parameter change=0.7385691
#> **** Iteration 16 | Maximum parameter change=0.7237977
#> **** Iteration 17 | Maximum parameter change=0.7093218
#> **** Iteration 18 | Maximum parameter change=0.6951353
#> **** Iteration 19 | Maximum parameter change=0.6812326
#> **** Iteration 20 | Maximum parameter change=0.667608
#> **** Iteration 21 | Maximum parameter change=0.6542558
#> **** Iteration 22 | Maximum parameter change=0.6411707
#> **** Iteration 23 | Maximum parameter change=0.6283473
#> **** Iteration 24 | Maximum parameter change=0.6157803
#> **** Iteration 25 | Maximum parameter change=0.6034647
#> **** Iteration 26 | Maximum parameter change=0.5913954
#> **** Iteration 27 | Maximum parameter change=0.5795675
#> **** Iteration 28 | Maximum parameter change=0.5679762
#> **** Iteration 29 | Maximum parameter change=0.5566167
#> **** Iteration 30 | Maximum parameter change=0.5454843
#> **** Iteration 31 | Maximum parameter change=0.5345746
#> **** Iteration 32 | Maximum parameter change=0.5238831
#> **** Iteration 33 | Maximum parameter change=0.5134055
#> **** Iteration 34 | Maximum parameter change=0.5031374
#> **** Iteration 35 | Maximum parameter change=0.4930746
#> **** Iteration 36 | Maximum parameter change=0.4832131
#> **** Iteration 37 | Maximum parameter change=0.4735489
#> **** Iteration 38 | Maximum parameter change=0.4640779
#> **** Iteration 39 | Maximum parameter change=0.4547963
#> **** Iteration 40 | Maximum parameter change=0.4457004
#> **** Iteration 41 | Maximum parameter change=0.4367864
#> **** Iteration 42 | Maximum parameter change=0.4280507
#> **** Iteration 43 | Maximum parameter change=0.4194897
#> **** Iteration 44 | Maximum parameter change=0.4110999
#> **** Iteration 45 | Maximum parameter change=0.4028779
#> **** Iteration 46 | Maximum parameter change=0.3948203
#> **** Iteration 47 | Maximum parameter change=0.3869239
#> **** Iteration 48 | Maximum parameter change=0.3791854
#> **** Iteration 49 | Maximum parameter change=0.3716017
#> **** Iteration 50 | Maximum parameter change=0.3641697
#> **** Iteration 51 | Maximum parameter change=0.3568863
#> **** Iteration 52 | Maximum parameter change=0.3497486
#> **** Iteration 53 | Maximum parameter change=0.3427536
#> **** Iteration 54 | Maximum parameter change=0.3358985
#> **** Iteration 55 | Maximum parameter change=0.3291805
#> **** Iteration 56 | Maximum parameter change=0.3225969
#> **** Iteration 57 | Maximum parameter change=0.316145
#> **** Iteration 58 | Maximum parameter change=0.3098221
#> **** Iteration 59 | Maximum parameter change=0.3036257
#> **** Iteration 60 | Maximum parameter change=0.2975531
#> **** Iteration 61 | Maximum parameter change=0.2916021
#> **** Iteration 62 | Maximum parameter change=0.28577
#> **** Iteration 63 | Maximum parameter change=0.2800546
#> **** Iteration 64 | Maximum parameter change=0.2744535
#> **** Iteration 65 | Maximum parameter change=0.2689645
#> **** Iteration 66 | Maximum parameter change=0.2635852
#> **** Iteration 67 | Maximum parameter change=0.2583135
#> **** Iteration 68 | Maximum parameter change=0.2531472
#> **** Iteration 69 | Maximum parameter change=0.2480843
#> **** Iteration 70 | Maximum parameter change=0.2431226
#> **** Iteration 71 | Maximum parameter change=0.2382601
#> **** Iteration 72 | Maximum parameter change=0.2334949
#> **** Iteration 73 | Maximum parameter change=0.228825
#> **** Iteration 74 | Maximum parameter change=0.2242485
#> **** Iteration 75 | Maximum parameter change=0.2197636
#> **** Iteration 76 | Maximum parameter change=0.2153683
#> **** Iteration 77 | Maximum parameter change=0.2110609
#> **** Iteration 78 | Maximum parameter change=0.2068397
#> **** Iteration 79 | Maximum parameter change=0.2027029
#> **** Iteration 80 | Maximum parameter change=0.1986489
#> **** Iteration 81 | Maximum parameter change=0.1946759
#> **** Iteration 82 | Maximum parameter change=0.1907824
#> **** Iteration 83 | Maximum parameter change=0.1869667
#> **** Iteration 84 | Maximum parameter change=0.1832274
#> **** Iteration 85 | Maximum parameter change=0.1795628
#> **** Iteration 86 | Maximum parameter change=0.1759716
#> **** Iteration 87 | Maximum parameter change=0.1724521
#> **** Iteration 88 | Maximum parameter change=0.1690031
#> **** Iteration 89 | Maximum parameter change=0.165623
#> **** Iteration 90 | Maximum parameter change=0.1623106
#> **** Iteration 91 | Maximum parameter change=0.1590644
#> **** Iteration 92 | Maximum parameter change=0.1558831
#> **** Iteration 93 | Maximum parameter change=0.1527654
#> **** Iteration 94 | Maximum parameter change=0.1497101
#> **** Iteration 95 | Maximum parameter change=0.1467159
#> **** Iteration 96 | Maximum parameter change=0.1437816
#> **** Iteration 97 | Maximum parameter change=0.140906
#> **** Iteration 98 | Maximum parameter change=0.1380878
#> **** Iteration 99 | Maximum parameter change=0.1353261
#> **** Iteration 100 | Maximum parameter change=0.1326196
fit2 <- fit_bt_model(bt, engine = "BradleyTerry2")
#> Warning: the ‘nobars’ function has moved to the reformulas package. Please update your imports, or ask an upstream package maintainter to do so.
#> This warning is displayed once per session.
#> Warning: the ‘findbars’ function has moved to the reformulas package. Please update your imports, or ask an upstream package maintainter to do so.
#> This warning is displayed once per session.
```
