# Summarize a Bradley–Terry model fit

This helper takes the object returned by
[`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
and returns a tibble with one row per object (e.g., writing sample),
including:

- `ID`: object identifier

- `theta`: estimated ability parameter

- `se`: standard error of `theta`

- `rank`: rank order of `theta` (1 = highest by default)

- `engine`: modeling engine used ("sirt" or "BradleyTerry2")

- `reliability`: MLE reliability (for sirt) or `NA`

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

  Modeling engine used ("sirt" or "BradleyTerry2").

- reliability:

  MLE reliability (numeric scalar) repeated on each row.

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

summarize_bt_fit(fit1)
#> Warning: NAs introduced by coercion
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
summarize_bt_fit(fit2)
#> Warning: NAs introduced by coercion
#> # A tibble: 20 × 6
#>    ID        theta    se  rank engine        reliability
#>    <chr>     <dbl> <dbl> <int> <chr>               <dbl>
#>  1 S01    0         0       19 BradleyTerry2          NA
#>  2 S02    1.90e+ 0  1.58    17 BradleyTerry2          NA
#>  3 S03   -6.04e-16  1.49    20 BradleyTerry2          NA
#>  4 S04    9.94e- 1  1.48    18 BradleyTerry2          NA
#>  5 S05    2.79e+ 0  1.71    16 BradleyTerry2          NA
#>  6 S06    3.68e+ 0  1.83    15 BradleyTerry2          NA
#>  7 S07    4.54e+ 0  1.93    14 BradleyTerry2          NA
#>  8 S08    6.04e+ 0  2.05    13 BradleyTerry2          NA
#>  9 S09    6.66e+ 0  2.08    11 BradleyTerry2          NA
#> 10 S10    6.66e+ 0  2.08    12 BradleyTerry2          NA
#> 11 S11    7.25e+ 0  2.10    10 BradleyTerry2          NA
#> 12 S12    7.25e+ 0  2.10     9 BradleyTerry2          NA
#> 13 S13    9.48e+ 0  2.19     5 BradleyTerry2          NA
#> 14 S14    8.93e+ 0  2.17     8 BradleyTerry2          NA
#> 15 S15    9.48e+ 0  2.19     6 BradleyTerry2          NA
#> 16 S16    9.48e+ 0  2.19     7 BradleyTerry2          NA
#> 17 S17    1.00e+ 1  2.22     4 BradleyTerry2          NA
#> 18 S18    1.23e+ 1  2.45     1 BradleyTerry2          NA
#> 19 S19    1.06e+ 1  2.26     3 BradleyTerry2          NA
#> 20 S20    1.23e+ 1  2.45     2 BradleyTerry2          NA
```
