# Compute consistency between forward and reverse pair comparisons

Given two data frames of pairwise comparison results (one for the
"forward" ordering of pairs, one for the "reverse" ordering), this
function identifies unordered pairs that were evaluated in both
directions and computes the proportion of consistent judgments.

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
  `n_consistent`, and `prop_consistent`. Here, `n_pairs` counts
  unordered pair keys with a non-missing majority winner in both
  directions.

- `details`: a tibble with one row per unordered pair key, including
  columns `key`, `ID1_main`, `ID2_main`, `ID1_rev`, `ID2_rev`,
  `better_id_main`, `better_id_rev`, and `is_consistent`. Additional
  columns provide vote counts and tie flags.

## Details

Consistency is defined at the level of IDs: a pair is consistent if the
same ID is selected as better in both directions. This function assumes
each input contains columns `ID1`, `ID2`, and `better_id`, where
`better_id` is the ID of the better sample (not "SAMPLE_1"/"SAMPLE_2").

**Per-key majority agreement (duplicates supported).** If a pair appears
multiple times in `main_results` and/or `reverse_results` (e.g.,
submitted twice), this function aggregates each unordered pair key
separately in each direction and takes the *majority* `better_id`. If
there is a tie for the majority winner within a direction, that
direction's majority winner is set to `NA` and the key is excluded from
the consistency calculation.

The output `details` contains exactly one row per unordered pair key,
which keeps it compatible with
[`check_positional_bias`](https://shmercer.github.io/pairwiseLLM/reference/check_positional_bias.md).

## Examples

``` r
main <- tibble::tibble(
  ID1       = c("A", "A", "X"),
  ID2       = c("B", "B", "Y"),
  better_id = c("A", "B", "X")  # duplicate A-B with disagreement
)
rev <- tibble::tibble(
  ID1       = c("B"),
  ID2       = c("A"),
  better_id = c("A")
)
compute_reverse_consistency(main, rev)$summary
#> # A tibble: 1 × 3
#>   n_pairs n_consistent prop_consistent
#>     <int>        <int>           <dbl>
#> 1       0            0              NA
```
