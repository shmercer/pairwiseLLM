# Create all unordered pairs of writing samples

Given a data frame of samples with columns `ID` and `text`, this
function generates all unordered pairs (combinations) of samples. Each
pair appears exactly once, with `ID1` \< `ID2` in lexicographic order.

## Usage

``` r
make_pairs(samples)
```

## Arguments

- samples:

  A tibble or data frame with columns `ID` and `text`.

## Value

A tibble with columns:

- `ID1`, `text1`

- `ID2`, `text2`

## See also

[`read_samples_df()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_df.md),
[`read_samples_dir()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_dir.md)

Other pairing and data:
[`alternate_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/alternate_pair_order.md),
[`randomize_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/randomize_pair_order.md),
[`read_samples_df()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_df.md),
[`read_samples_dir()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_dir.md),
[`sample_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_pairs.md),
[`sample_reverse_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_reverse_pairs.md)

## Examples

``` r
samples <- tibble::tibble(
  ID   = c("S1", "S2", "S3"),
  text = c("Sample 1", "Sample 2", "Sample 3")
)

pairs_all <- make_pairs(samples)
pairs_all
#> # A tibble: 3 × 4
#>   ID1   text1    ID2   text2   
#>   <chr> <chr>    <chr> <chr>   
#> 1 S1    Sample 1 S2    Sample 2
#> 2 S1    Sample 1 S3    Sample 3
#> 3 S2    Sample 2 S3    Sample 3

# Using the built-in example data
data("example_writing_samples")
pairs_example <- make_pairs(example_writing_samples)
nrow(pairs_example) # should be choose(10, 2) = 45
#> [1] 190
```
