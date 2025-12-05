# Compute consistency between forward and reverse pair comparisons

Given two data frames of pairwise comparison results (one for the
"forward" ordering of pairs, one for the "reverse" ordering), this
function identifies pairs that were evaluated in both orders and
computes the proportion of consistent judgments.

## Usage

``` r
compute_reverse_consistency(main_results, reverse_results)
```

## Arguments

- main_results:

  A data frame or tibble containing pairwise comparison results for the
  "forward" ordering of pairs, with columns `ID1`, `ID2`, and
  `better_id`.

- reverse_results:

  A data frame or tibble containing results for the corresponding
  "reverse" ordering, with the same column requirements.

## Value

A list with two elements:

- `summary`: a tibble with one row and columns `n_pairs`,
  `n_consistent`, and `prop_consistent`.

- `details`: a tibble with one row per overlapping pair, including
  columns `key`, `ID1_main`, `ID2_main`, `ID1_rev`, `ID2_rev`,
  `better_id_main`, `better_id_rev`, and `is_consistent`.

Pairs for which `better_id` is `NA` in either data frame are excluded
from the consistency calculation.

## Details

Consistency is defined at the level of IDs: a pair is consistent if the
same ID is selected as better in both data frames. This assumes that
each result data frame contains at least the columns `ID1`, `ID2`, and
`better_id`, where `better_id` is the ID of the better sample (not
"SAMPLE_1"/"SAMPLE_2").

## Examples

``` r
# Simple synthetic example
main <- tibble::tibble(
  ID1       = c("S1", "S1", "S2"),
  ID2       = c("S2", "S3", "S3"),
  better_id = c("S1", "S3", "S2")
)

rev <- tibble::tibble(
  ID1       = c("S2", "S3", "S3"),
  ID2       = c("S1", "S1", "S2"),
  better_id = c("S1", "S3", "S2")
)

rc <- compute_reverse_consistency(main, rev)
rc$summary
#> # A tibble: 1 × 3
#>   n_pairs n_consistent prop_consistent
#>     <int>        <int>           <dbl>
#> 1       3            3               1

# Using the example writing pairs: reverse the first 10 pairs
data("example_writing_pairs")
main2 <- example_writing_pairs[1:10, ]
rev2  <- main2
rev2$ID1 <- main2$ID2
rev2$ID2 <- main2$ID1
rc2 <- compute_reverse_consistency(main2, rev2)
rc2$summary
#> # A tibble: 1 × 3
#>   n_pairs n_consistent prop_consistent
#>     <int>        <int>           <dbl>
#> 1      10           10               1
```
