# Build canonical `results_tbl` data for Bayesian BTL MCMC

Converts non-adaptive pairwise outcomes (for example, rows like
`example_writing_pairs` with `ID1`, `ID2`, `better_id`) into the
canonical `results_tbl` schema required by
[`fit_bayes_btl_mcmc()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bayes_btl_mcmc.md).

## Usage

``` r
build_btl_results_data(
  results,
  phase = "phase2",
  backend = "non_adaptive_import",
  model = "unknown",
  iter_start = 1L,
  received_at_start = as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
)
```

## Arguments

- results:

  A data frame or tibble containing columns `ID1`, `ID2`, and
  `better_id`.

- phase:

  Length-1 phase label for all rows. Must be one of `"phase1"`,
  `"phase2"`, or `"phase3"`. Defaults to `"phase2"`.

- backend:

  Length-1 backend label to record in output metadata.

- model:

  Length-1 model label to record in output metadata.

- iter_start:

  Integer starting value for `iter`. Defaults to `1L`.

- received_at_start:

  Length-1 `POSIXct` timestamp for the first row. Subsequent rows
  increment by one second.

## Value

A tibble in canonical `results_tbl` format with columns: `pair_uid`,
`unordered_key`, `ordered_key`, `A_id`, `B_id`, `better_id`,
`winner_pos`, `phase`, `iter`, `received_at`, `backend`, `model`.

## Details

The output is deterministic and schema-valid:

- stable `unordered_key` / `ordered_key` values,

- deterministic `pair_uid` as `"<unordered_key>#<occurrence>"`,

- deterministic `iter` and `received_at` sequences.

## Examples

``` r
data("example_writing_pairs", package = "pairwiseLLM")

results_tbl <- build_btl_results_data(example_writing_pairs)
head(results_tbl)
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

ids <- sort(unique(c(results_tbl$A_id, results_tbl$B_id)))
ids
#>  [1] "S01" "S02" "S03" "S04" "S05" "S06" "S07" "S08" "S09" "S10" "S11" "S12"
#> [13] "S13" "S14" "S15" "S16" "S17" "S18" "S19" "S20"
```
