# Example dataset of paired comparisons for writing samples

A complete set of unordered paired comparison outcomes for the ten
samples in
[`example_writing_samples`](https://shmercer.github.io/pairwiseLLM/reference/example_writing_samples.md).
For each pair of IDs, the `better_id` field indicates which sample is
assumed to be better, based on the `quality_score` in
`example_writing_samples`.

## Usage

``` r
data("example_writing_pairs")
```

## Format

A tibble with 45 rows and 3 variables:

- ID1:

  Character ID of the first sample in the pair.

- ID2:

  Character ID of the second sample in the pair.

- better_id:

  Character ID of the sample judged better in this pair (either `ID1` or
  `ID2`).

## Details

This dataset is useful for demonstrating functions that process paired
comparisons (e.g., building Bradley-Terry data and fitting
[`btm`](https://rdrr.io/pkg/sirt/man/btm.html) models) without requiring
any calls to an LLM.

## Examples

``` r
data("example_writing_pairs")
head(example_writing_pairs)
#> # A tibble: 6 × 3
#>   ID1   ID2   better_id
#>   <chr> <chr> <chr>    
#> 1 S01   S02   S02      
#> 2 S01   S03   S03      
#> 3 S01   S04   S04      
#> 4 S01   S05   S05      
#> 5 S01   S06   S06      
#> 6 S01   S07   S07      
```
