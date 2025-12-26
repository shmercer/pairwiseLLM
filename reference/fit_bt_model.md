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
  return_diagnostics = FALSE,
  include_residuals = FALSE,
  ...
)
```

## Arguments

- bt_data:

  A data frame or tibble with either three columns (`object1`,
  `object2`, `result`) or four columns including `judge`. Usually
  produced by
  [`build_bt_data`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md).

- engine:

  Character string specifying the modeling engine. One of: `"auto"`
  (default), `"sirt"`, or `"BradleyTerry2"`.

- verbose:

  Logical. If `TRUE` (default), show engine output (iterations,
  warnings). If `FALSE`, suppress noisy output to keep examples and
  reports clean.

- return_diagnostics:

  Logical. If `TRUE`, return a `diagnostics` list when using the sirt
  engine. Defaults to `FALSE`.

- include_residuals:

  Logical. If `TRUE` and `return_diagnostics = TRUE`, include residuals
  (if available) in the diagnostics. Defaults to `FALSE`.

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

- diagnostics:

  `NULL` unless `return_diagnostics = TRUE` and the sirt engine is used.
  When returned, includes selected fields from
  [`sirt::btm()`](https://rdrr.io/pkg/sirt/man/btm.html) output (e.g.,
  separation index, optional item/judge fit, probabilities, and optional
  residuals).

## Details

When `engine = "auto"` (the default), the function attempts sirt first
and automatically falls back to BradleyTerry2 only if necessary. In all
cases, the output format is standardized, so downstream code can rely on
consistent fields.

The input `bt_data` must contain either:

1.  three columns: `object1`, `object2`, `result`, or

2.  four columns: `object1`, `object2`, `result`, `judge`

Where `object1` and `object2` are item IDs and `result` is a numeric
indicator (1 = `object1` wins, 0 = `object2` wins). If a `judge` column
is included, it is used by the sirt engine (and ignored by
BradleyTerry2).

Ability estimates (`theta`) represent latent "writing quality"
parameters on a log-odds scale. Standard errors are included for both
modeling engines. MLE reliability is only available from sirt.

## Examples

``` r
data("example_writing_pairs")
bt <- build_bt_data(example_writing_pairs)

if (requireNamespace("sirt", quietly = TRUE)) {
  fit1 <- fit_bt_model(bt, engine = "sirt", verbose = FALSE)
}

if (requireNamespace("BradleyTerry2", quietly = TRUE)) {
  fit2 <- fit_bt_model(bt, engine = "BradleyTerry2", verbose = FALSE)
}

# Judge column example (sirt only)
bt_j <- bt
bt_j$judge <- "judge_1"
if (requireNamespace("sirt", quietly = TRUE)) {
  fit3 <- fit_bt_model(bt_j, engine = "sirt", verbose = FALSE, return_diagnostics = TRUE)
}
```
