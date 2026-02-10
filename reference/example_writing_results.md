# Example canonical results table for writing comparisons

Canonical `results_tbl` representation of
[`example_writing_pairs`](https://shmercer.github.io/pairwiseLLM/reference/example_writing_pairs.md),
intended for direct use with
[`fit_bayes_btl_mcmc`](https://shmercer.github.io/pairwiseLLM/reference/fit_bayes_btl_mcmc.md)
and other functions that require adaptive schema-compatible results
input.

## Usage

``` r
data("example_writing_results")
```

## Format

A tibble with 190 rows and 12 variables:

- pair_uid:

  Deterministic pair attempt ID.

- unordered_key:

  Unordered pair key (`"min:max"`).

- ordered_key:

  Ordered pair key (`"A_id:B_id"`).

- A_id:

  Character ID in first position.

- B_id:

  Character ID in second position.

- better_id:

  Character ID judged better in this comparison.

- winner_pos:

  Integer winner position (`1L` or `2L`).

- phase:

  Phase label.

- iter:

  Integer step index.

- received_at:

  `POSIXct` timestamp in UTC.

- backend:

  Backend label for provenance.

- model:

  Model label for provenance.

## Examples

``` r
data("example_writing_results")
head(example_writing_results)
#> # A tibble: 6 × 12
#>   pair_uid  unordered_key ordered_key A_id  B_id  better_id winner_pos phase 
#>   <chr>     <chr>         <chr>       <chr> <chr> <chr>          <int> <chr> 
#> 1 S01:S02#1 S01:S02       S01:S02     S01   S02   S02                2 phase2
#> 2 S01:S03#1 S01:S03       S01:S03     S01   S03   S03                2 phase2
#> 3 S01:S04#1 S01:S04       S01:S04     S01   S04   S04                2 phase2
#> 4 S01:S05#1 S01:S05       S01:S05     S01   S05   S01                1 phase2
#> 5 S01:S06#1 S01:S06       S01:S06     S01   S06   S06                2 phase2
#> 6 S01:S07#1 S01:S07       S01:S07     S01   S07   S07                2 phase2
#> # ℹ 4 more variables: iter <int>, received_at <dttm>, backend <chr>,
#> #   model <chr>
```
