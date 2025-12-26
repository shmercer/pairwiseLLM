# Simulate a judge for BT pairwise comparisons

This helper produces synthetic pairwise comparison results for testing,
vignettes, and benchmarking. It takes a table of pairs (with texts) and
returns `better_id` outcomes based on a latent "true ability" vector.

## Usage

``` r
simulate_bt_judge(
  pairs,
  true_theta,
  judges = "judge_1",
  judge_col = NULL,
  deterministic = FALSE,
  seed = NULL,
  round_robin = TRUE
)
```

## Arguments

- pairs:

  A tibble/data.frame with columns `ID1`, `text1`, `ID2`, `text2`.

- true_theta:

  Named numeric vector of latent abilities. Names must include the IDs
  used in `pairs`. Missing IDs are treated as 0.

- judges:

  Optional character vector of judge identifiers. If length \> 1, each
  row is assigned a judge (round-robin by default).

- judge_col:

  Optional character scalar column name to use for judge labels. If
  `NULL` (default), no judge column is returned.

- deterministic:

  Logical; if `TRUE`, always choose the higher `true_theta`. If equal,
  break ties at random (seed-controlled). Default `FALSE`.

- seed:

  Optional integer seed; RNG state is restored afterwards.

- round_robin:

  Logical; if `TRUE` (default) and `length(judges) > 1`, assign judges
  in repeating order. If `FALSE`, assign judges uniformly at random.

## Value

A tibble with columns `ID1`, `ID2`, `better_id`, and optionally a judge
column.

## Details

The simulator can be deterministic (always pick higher true ability) or
stochastic using a Bradley–Terry / logistic probability model.

## Examples

``` r
pairs <- tibble::tibble(
  ID1 = c("A", "B"),
  text1 = c("a", "b"),
  ID2 = c("C", "D"),
  text2 = c("c", "d")
)
true_theta <- c(A = 2, B = 1, C = 0, D = -1)
simulate_bt_judge(pairs, true_theta, deterministic = TRUE, seed = 1)
#> # A tibble: 2 × 3
#>   ID1   ID2   better_id
#>   <chr> <chr> <chr>    
#> 1 A     C     A        
#> 2 B     D     B        
```
