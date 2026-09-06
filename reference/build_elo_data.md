# Build EloChoice comparison data from pairwise results

This function converts pairwise comparison results into the two-column
format used by the EloChoice package: one column for the winner and one
for the loser of each trial.

## Usage

``` r
build_elo_data(results)
```

## Arguments

- results:

  A data frame or tibble with either `ID1`/`ID2`/`better_id` or
  `A_id`/`B_id`/`better_id`.

## Value

A tibble with two columns:

- `winner`: ID of the winning sample

- `loser`: ID of the losing sample

Rows with invalid or missing `better_id` are dropped.

## Details

It accepts either:

- legacy columns `ID1`, `ID2`, `better_id`, or

- canonical columns `A_id`, `B_id`, `better_id`.

Rows where `better_id` does not match either side of the pair (including
`NA`) are excluded.

## See also

[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md),
[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)

Other frequentist models:
[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md),
[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md),
[`fit_elo_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_elo_model.md),
[`summarize_bt_fit()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_bt_fit.md)

## Examples

``` r
results <- tibble::tibble(
  ID1       = c("S1", "S1", "S2", "S3"),
  ID2       = c("S2", "S3", "S3", "S4"),
  better_id = c("S1", "S3", "S2", "S4")
)

elo_data <- build_elo_data(results)
elo_data
#> # A tibble: 4 × 2
#>   winner loser
#>   <chr>  <chr>
#> 1 S1     S2   
#> 2 S3     S1   
#> 3 S2     S3   
#> 4 S4     S3   
```
