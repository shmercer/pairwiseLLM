# Randomly sample pairs of writing samples

This function samples a subset of rows from a pairs data frame returned
by
[`make_pairs`](https://shmercer.github.io/pairwiseLLM/reference/make_pairs.md).
You can specify either the proportion of pairs to retain (`pair_pct`),
the absolute number of pairs (`n_pairs`), or both (in which case the
minimum of the two is used).

## Usage

``` r
sample_pairs(pairs, pair_pct = 1, n_pairs = NULL, seed = NULL)
```

## Arguments

- pairs:

  A tibble with columns `ID1`, `text1`, `ID2`, and `text2`.

- pair_pct:

  Proportion of pairs to sample (between 0 and 1). Defaults to 1 (all
  pairs).

- n_pairs:

  Optional integer specifying the maximum number of pairs to sample.

- seed:

  Optional integer seed for reproducible sampling.

## Value

A tibble containing the sampled rows of `pairs`.

## Examples

``` r
samples <- tibble::tibble(
  ID   = c("S1", "S2", "S3", "S4"),
  text = paste("Sample", 1:4)
)
pairs_all <- make_pairs(samples)

# Sample 50% of all pairs
sample_pairs(pairs_all, pair_pct = 0.5, seed = 123)
#> # A tibble: 3 × 4
#>   ID1   text1    ID2   text2   
#>   <chr> <chr>    <chr> <chr>   
#> 1 S1    Sample 1 S4    Sample 4
#> 2 S3    Sample 3 S4    Sample 4
#> 3 S1    Sample 1 S3    Sample 3

# Sample exactly 3 pairs
sample_pairs(pairs_all, n_pairs = 3, seed = 123)
#> # A tibble: 3 × 4
#>   ID1   text1    ID2   text2   
#>   <chr> <chr>    <chr> <chr>   
#> 1 S1    Sample 1 S4    Sample 4
#> 2 S3    Sample 3 S4    Sample 4
#> 3 S1    Sample 1 S3    Sample 3

# Using built-in examples and sample 10% of all pairs
data("example_writing_samples")
pairs_ex <- make_pairs(example_writing_samples)
pairs_ex_sample <- sample_pairs(pairs_ex, pair_pct = 0.10, seed = 1)
nrow(pairs_ex_sample)
#> [1] 19
```
