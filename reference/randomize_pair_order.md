# Randomly assign samples to positions SAMPLE_1 and SAMPLE_2

This helper takes a table of paired writing samples (with columns `ID1`,
`text1`, `ID2`, and `text2`) and, for each row, randomly decides whether
to keep the current order or swap the two samples. The result is that
approximately half of the pairs will have the original order and half
will be reversed, on average.

## Usage

``` r
randomize_pair_order(pairs, seed = NULL)
```

## Arguments

- pairs:

  A data frame or tibble with columns `ID1`, `text1`, `ID2`, and
  `text2`. Typically created by
  [`make_pairs`](https://shmercer.github.io/pairwiseLLM/reference/make_pairs.md)
  (optionally followed by
  [`sample_pairs`](https://shmercer.github.io/pairwiseLLM/reference/sample_pairs.md)).

- seed:

  Optional integer seed for reproducible randomization. If `NULL`
  (default), the current RNG state is used and not modified.

## Value

A tibble with the same columns as `pairs`, but with some rows'
`ID1`/`text1` and `ID2`/`text2` swapped at random.

## Details

This is useful for reducing position biases in LLM-based paired
comparisons, while still allowing reverse-order consistency checks via
[`sample_reverse_pairs`](https://shmercer.github.io/pairwiseLLM/reference/sample_reverse_pairs.md)
and
[`compute_reverse_consistency`](https://shmercer.github.io/pairwiseLLM/reference/compute_reverse_consistency.md).

If you want a *deterministic* alternation of positions (for example,
first pair as-is, second pair swapped, third pair as-is, and so on), use
[`alternate_pair_order`](https://shmercer.github.io/pairwiseLLM/reference/alternate_pair_order.md)
instead of this function.

## See also

[`alternate_pair_order`](https://shmercer.github.io/pairwiseLLM/reference/alternate_pair_order.md)
for deterministic alternating order,
[`sample_reverse_pairs`](https://shmercer.github.io/pairwiseLLM/reference/sample_reverse_pairs.md)
and
[`compute_reverse_consistency`](https://shmercer.github.io/pairwiseLLM/reference/compute_reverse_consistency.md)
for reverse-order checks.

## Examples

``` r
data("example_writing_samples", package = "pairwiseLLM")

# Build all pairs
pairs_all <- make_pairs(example_writing_samples)

# Randomly flip the order within pairs
set.seed(123)
pairs_rand <- randomize_pair_order(pairs_all, seed = 123)

head(pairs_all[, c("ID1", "ID2")])
#> # A tibble: 6 × 2
#>   ID1   ID2  
#>   <chr> <chr>
#> 1 S01   S02  
#> 2 S01   S03  
#> 3 S01   S04  
#> 4 S01   S05  
#> 5 S01   S06  
#> 6 S01   S07  
head(pairs_rand[, c("ID1", "ID2")])
#> # A tibble: 6 × 2
#>   ID1   ID2  
#>   <chr> <chr>
#> 1 S02   S01  
#> 2 S01   S03  
#> 3 S04   S01  
#> 4 S01   S05  
#> 5 S01   S06  
#> 6 S07   S01  
```
