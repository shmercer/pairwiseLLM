# Propose a core-linking round given an existing BT fit

Convenience wrapper around
[`select_core_link_pairs`](https://shmercer.github.io/pairwiseLLM/reference/select_core_link_pairs.md)
that uses `fit$theta` from a model fitted by
[`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md).

## Usage

``` r
bt_core_link_round(samples, fit, core_ids, include_text = FALSE, ...)
```

## Arguments

- samples:

  A tibble/data.frame with columns `ID` and `text`.

- fit:

  A list returned by
  [`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
  that contains a `$theta` tibble with columns `ID`, `theta`, `se`.

- core_ids:

  Character vector of core IDs.

- include_text:

  If TRUE, attach `text1`/`text2` columns by joining against `samples`.

- ...:

  Passed through to
  [`select_core_link_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/select_core_link_pairs.md)
  (e.g., `existing_pairs`, `round_size`, `seed`, etc.).

## Value

A list with:

- pairs:

  Tibble of proposed pairs (ID1, ID2, pair_type; plus text columns if
  requested).

- plan:

  One-row tibble summarizing how many of each pair_type were returned.

## Examples

``` r
samples <- tibble::tibble(ID = paste0("S", 1:8), text = paste("t", 1:8))
theta <- tibble::tibble(ID = samples$ID, theta = rnorm(8), se = runif(8, 0.2, 0.6))
fit <- list(theta = theta)
out <- bt_core_link_round(samples, fit, core_ids = paste0("S", 1:3), round_size = 6, seed = 1)
out$plan
#> # A tibble: 1 × 4
#>   n_total n_core_new n_new_new n_core_core
#>     <int>      <int>     <int>       <int>
#> 1       6          5         1           0
head(out$pairs)
#> # A tibble: 6 × 3
#>   ID1   ID2   pair_type
#>   <chr> <chr> <chr>    
#> 1 S4    S6    new_new  
#> 2 S2    S7    core_new 
#> 3 S8    S2    core_new 
#> 4 S5    S2    core_new 
#> 5 S3    S4    core_new 
#> 6 S7    S1    core_new 
```
