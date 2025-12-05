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
summarize_bt_fit(fit, decreasing = TRUE)
```

## Arguments

- fit:

  A list returned by
  [`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md).

- decreasing:

  Logical; should higher `theta` values receive lower rank numbers? If
  `TRUE` (default), the highest `theta` gets `rank = 1`.

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
