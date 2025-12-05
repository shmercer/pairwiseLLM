# Sample reversed versions of a subset of pairs

Given a table of pairs with columns `ID1`, `text1`, `ID2`, and `text2`,
this function selects a subset of rows and returns a new tibble where
the order of each selected pair is reversed.

## Usage

``` r
sample_reverse_pairs(pairs, reverse_pct = NULL, n_reverse = NULL, seed = NULL)
```

## Arguments

- pairs:

  A data frame or tibble with columns `ID1`, `text1`, `ID2`, and
  `text2`.

- reverse_pct:

  Optional proportion of rows to reverse (between 0 and 1). If
  `n_reverse` is also supplied, `n_reverse` takes precedence and
  `reverse_pct` is ignored.

- n_reverse:

  Optional absolute number of rows to reverse. If supplied, this takes
  precedence over `reverse_pct`.

- seed:

  Optional integer seed for reproducible sampling.

## Value

A tibble containing the reversed pairs only (i.e., with `ID1` swapped
with `ID2` and `text1` swapped with `text2`).

## Examples

``` r
data("example_writing_samples")
pairs <- make_pairs(example_writing_samples)

# Reverse 20% of the pairs
rev20 <- sample_reverse_pairs(pairs, reverse_pct = 0.2, seed = 123)
```
