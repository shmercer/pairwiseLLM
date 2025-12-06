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

# Using the built-in example data (10 writing samples)
data("example_writing_samples")
pairs_example <- make_pairs(example_writing_samples)
nrow(pairs_example) # should be choose(10, 2) = 45
#> [1] 190
```
