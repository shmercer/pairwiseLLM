# Deterministically alternate sample order in pairs

This helper takes a table of paired writing samples (with columns `ID1`,
`text1`, `ID2`, and `text2`) and reverses the sample order for every
second row (rows 2, 4, 6, ...). This provides a perfectly balanced
reversal pattern without the randomness of
[`randomize_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/randomize_pair_order.md).

## Usage

``` r
alternate_pair_order(pairs)
```

## Arguments

- pairs:

  A tibble or data frame with columns `ID1`, `text1`, `ID2`, and
  `text2`.

## Value

A tibble identical to `pairs` except that rows 2, 4, 6, ... have
`ID1`/`text1` and `ID2`/`text2` swapped.

## Details

This is useful when you want a fixed 50/50 mix of original and reversed
pairs for bias control, benchmarking, or debugging, without relying on
the random number generator or seeds.

## Examples

``` r
data("example_writing_samples")
pairs <- make_pairs(example_writing_samples)

pairs_alt <- alternate_pair_order(pairs)

head(pairs[, c("ID1", "ID2")])
#> # A tibble: 6 × 2
#>   ID1   ID2  
#>   <chr> <chr>
#> 1 S01   S02  
#> 2 S01   S03  
#> 3 S01   S04  
#> 4 S01   S05  
#> 5 S01   S06  
#> 6 S01   S07  
head(pairs_alt[, c("ID1", "ID2")])
#> # A tibble: 6 × 2
#>   ID1   ID2  
#>   <chr> <chr>
#> 1 S01   S02  
#> 2 S03   S01  
#> 3 S01   S04  
#> 4 S05   S01  
#> 5 S01   S06  
#> 6 S07   S01  
```
