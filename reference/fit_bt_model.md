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
fit_bt_model(bt_data, engine = c("auto", "sirt", "BradleyTerry2"), ...)
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

if (FALSE) { # \dontrun{
fit1 <- fit_bt_model(bt, engine = "sirt")
fit2 <- fit_bt_model(bt, engine = "BradleyTerry2")
} # }
```
