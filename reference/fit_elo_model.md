# Fit an EloChoice model to pairwise comparison data

This function fits an Elo-based paired-comparison model using the
EloChoice package. It is intended to complement
[`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
by providing an alternative scoring framework based on Elo ratings
rather than Bradley–Terry models.

## Usage

``` r
fit_elo_model(elo_data, runs = 5, verbose = FALSE, ...)
```

## Arguments

- elo_data:

  A data frame or tibble containing `winner` and `loser` columns.
  Typically produced using
  [`build_elo_data`](https://shmercer.github.io/pairwiseLLM/reference/build_elo_data.md).

- runs:

  Integer number of randomizations to use in
  [`EloChoice::elochoice`](https://rdrr.io/pkg/EloChoice/man/elochoice.html).
  Default is 5.

- verbose:

  Logical. If `TRUE` (default), show any messages/warnings emitted by
  the underlying fitting functions. If `FALSE`, suppress noisy output to
  keep examples and reports clean.

- ...:

  Additional arguments passed to
  [`EloChoice::elochoice()`](https://rdrr.io/pkg/EloChoice/man/elochoice.html).

## Value

A named list with components:

- engine:

  Character scalar identifying the scoring engine (`"EloChoice"`).

- fit:

  The `"elochoice"` model object.

- elo:

  A tibble with columns `ID` and `elo`.

- reliability:

  Numeric scalar: mean unweighted reliability index.

- reliability_weighted:

  Numeric scalar: mean weighted reliability index.

## Details

The input `elo_data` must contain two columns:

1.  `winner`: ID of the winning sample in each pairwise trial

2.  `loser`: ID of the losing sample in each trial

These can be created from standard pairwise comparison output using
[`build_elo_data`](https://shmercer.github.io/pairwiseLLM/reference/build_elo_data.md).

Internally, this function calls:

- [`elochoice`](https://rdrr.io/pkg/EloChoice/man/elochoice.html) — to
  estimate Elo ratings using repeated randomization of trial order;

- [`reliability`](https://rdrr.io/pkg/EloChoice/man/reliability.html) —
  to compute unweighted and weighted reliability indices as described in
  Clark et al. (2018).

If the EloChoice package is not installed, a helpful error message is
shown telling the user how to install it.

The returned object mirrors the structure of
[`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
for consistency across scoring engines:

- `engine` — always `"EloChoice"`.

- `fit` — the raw `"elochoice"` object returned by
  [`EloChoice::elochoice()`](https://rdrr.io/pkg/EloChoice/man/elochoice.html).

- `elo` — a tibble with columns:

  - `ID`: sample identifier

  - `elo`: estimated Elo rating

  (Unlike Bradley–Terry models, EloChoice does not provide standard
  errors for these ratings, so none are returned.)

- `reliability` — the mean unweighted reliability index (mean proportion
  of “upsets” across randomizations).

- `reliability_weighted` — the mean weighted reliability index (weighted
  version of the upset measure).

## References

Clark AP, Howard KL, Woods AT, Penton-Voak IS, Neumann C (2018). "Why
rate when you could compare? Using the 'EloChoice' package to assess
pairwise comparisons of perceived physical strength." *PLOS ONE*, 13(1),
e0190393.
[doi:10.1371/journal.pone.0190393](https://doi.org/10.1371/journal.pone.0190393)
.

## Examples

``` r
data("example_writing_pairs", package = "pairwiseLLM")

elo_data <- build_elo_data(example_writing_pairs)

fit <- fit_elo_model(elo_data, runs = 5, verbose = FALSE)
fit$elo
#> # A tibble: 20 × 2
#>    ID       elo
#>    <chr>  <dbl>
#>  1 S01   -386. 
#>  2 S02   -297. 
#>  3 S03   -399. 
#>  4 S04   -317. 
#>  5 S05   -262. 
#>  6 S06   -190  
#>  7 S07   -168. 
#>  8 S08    -37.8
#>  9 S09    -24.2
#> 10 S10    -55.6
#> 11 S11     -0.6
#> 12 S12     38.6
#> 13 S13    202. 
#> 14 S14    175. 
#> 15 S15    197. 
#> 16 S16    187  
#> 17 S17    251. 
#> 18 S18    417. 
#> 19 S19    264  
#> 20 S20    406. 
fit$reliability
#> [1] 0.8237247
fit$reliability_weighted
#> [1] 0.9223417
```
