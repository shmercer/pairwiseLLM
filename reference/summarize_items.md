# Summarize adaptive items

Build an item-level diagnostics summary from the canonical item logs.
This is a pure view and does not recompute posterior quantities or
exposure metrics.

## Usage

``` r
summarize_items(
  state,
  posterior = NULL,
  refit = NULL,
  bind = FALSE,
  top_n = NULL,
  sort_by = c("rank_mean", "theta_mean", "theta_sd", "degree", "pos_A_rate"),
  include_optional = TRUE
)
```

## Arguments

- state:

  An `adaptive_state` or list containing adaptive logs.

- posterior:

  Optional `item_log_list` (list of item log tables) or an item log
  table. When `NULL`, uses `state$logs$item_log_list` when available.

- refit:

  Optional refit index. When `NULL`, the most recent refit is returned;
  when set, the `k`-th refit is returned.

- bind:

  Logical; when `TRUE`, stack all refits into a single table.

- top_n:

  Optional positive integer; return only the top `n` rows after sorting.

- sort_by:

  Column used for sorting. Defaults to `"rank_mean"`.

- include_optional:

  Logical; include optional diagnostic columns.

## Value

A tibble with one row per item per refit. Columns reflect the canonical
item log schema (for example `refit_id`, `ID`, `theta_mean`,
`rank_mean`, `deg`, and `posA_prop`). Rank percentiles summarize
per-draw induced ranks (lower is better). When
`include_optional = FALSE`, optional columns such as repeated-pair or
adjacency diagnostics are dropped if present.

## Details

Rank percentiles are computed from the per-draw induced ranks (lower is
better). Rank uncertainty grows when draws disagree on the ordering.
Degree and position exposure metrics summarize how frequently each item
was shown and whether it appeared as the first option (A position). When
`refit = NULL`, the most recent refit is returned; when `refit = k`, the
`k`-th refit is returned. When `bind = TRUE`, all refits are stacked
into a single table and `refit` must be `NULL`.

## Examples

``` r
# summarize_items() expects an item_log_list (list of per-refit item tables).
# This example constructs a minimal logs object that matches what adaptive runs emit.

item_log_1 <- tibble::tibble(
  refit_id = 1L,
  ID = c("A", "B", "C"),
  theta_mean = c(0.4, 0.1, -0.2),
  theta_sd = c(0.2, 0.3, 0.25),
  rank_mean = c(1.2, 2.1, 2.7),
  degree = c(10L, 9L, 8L),
  pos_A_rate = c(0.55, 0.50, 0.48)
)

item_log_2 <- dplyr::mutate(
  item_log_1,
  refit_id = 2L,
  theta_mean = theta_mean + c(0.1, 0.05, 0.02),
  rank_mean = rank_mean + c(-0.1, 0.0, 0.1)
)

logs <- list(item_log_list = list(item_log_1, item_log_2))

# Default returns the most recent refit:
summarize_items(logs)
#> # A tibble: 3 × 7
#>   refit_id ID    theta_mean theta_sd rank_mean degree pos_A_rate
#>      <int> <chr>      <dbl>    <dbl>     <dbl>  <int>      <dbl>
#> 1        2 A           0.5      0.2        1.1     10       0.55
#> 2        2 B           0.15     0.3        2.1      9       0.5 
#> 3        2 C          -0.18     0.25       2.8      8       0.48

# Select a specific refit:
summarize_items(logs, refit = 1)
#> # A tibble: 3 × 7
#>   refit_id ID    theta_mean theta_sd rank_mean degree pos_A_rate
#>      <int> <chr>      <dbl>    <dbl>     <dbl>  <int>      <dbl>
#> 1        1 A            0.4     0.2        1.2     10       0.55
#> 2        1 B            0.1     0.3        2.1      9       0.5 
#> 3        1 C           -0.2     0.25       2.7      8       0.48

# Stack all refits into one table:
summarize_items(logs, bind = TRUE)
#> # A tibble: 6 × 7
#>   refit_id ID    theta_mean theta_sd rank_mean degree pos_A_rate
#>      <int> <chr>      <dbl>    <dbl>     <dbl>  <int>      <dbl>
#> 1        2 A           0.5      0.2        1.1     10       0.55
#> 2        1 A           0.4      0.2        1.2     10       0.55
#> 3        1 B           0.1      0.3        2.1      9       0.5 
#> 4        2 B           0.15     0.3        2.1      9       0.5 
#> 5        1 C          -0.2      0.25       2.7      8       0.48
#> 6        2 C          -0.18     0.25       2.8      8       0.48

# Sort and take the top rows:
summarize_items(logs, sort_by = "rank_mean", top_n = 2)
#> # A tibble: 2 × 7
#>   refit_id ID    theta_mean theta_sd rank_mean degree pos_A_rate
#>      <int> <chr>      <dbl>    <dbl>     <dbl>  <int>      <dbl>
#> 1        2 A           0.5       0.2       1.1     10       0.55
#> 2        2 B           0.15      0.3       2.1      9       0.5 
```
