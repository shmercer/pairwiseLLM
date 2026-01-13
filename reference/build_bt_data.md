# Build Bradley-Terry comparison data from pairwise results

This function converts pairwise comparison results into the three-column
format commonly used for Bradley-Terry models: the first two columns
contain object labels and the third column contains the comparison
result (1 for a win of the first object, 0 for a win of the second).

## Usage

``` r
build_bt_data(results)
```

## Arguments

- results:

  A data frame or tibble with columns `ID1`, `ID2`, and `better_id`.

## Value

A tibble with three columns:

- `object1`: ID from `ID1`

- `object2`: ID from `ID2`

- `result`: numeric value, 1 if `better_id == ID1`, 0 if
  `better_id == ID2`

Rows with invalid or missing `better_id` are dropped.

## Details

It assumes that the input contains columns `ID1`, `ID2`, and
`better_id`, where `better_id` is the ID of the better sample. Rows
where `better_id` does not match either `ID1` or `ID2` (including `NA`)
are excluded.

## Examples

``` r
results <- tibble::tibble(
  ID1       = c("S1", "S1", "S2"),
  ID2       = c("S2", "S3", "S3"),
  better_id = c("S1", "S3", "S2")
)

bt_data <- build_bt_data(results)
bt_data
#> # A tibble: 3 × 3
#>   object1 object2 result
#>   <chr>   <chr>    <dbl>
#> 1 S1      S2           1
#> 2 S1      S3           0
#> 3 S2      S3           1

# Using the example writing pairs
data("example_writing_pairs")
bt_ex <- build_bt_data(example_writing_pairs)
head(bt_ex)
#> # A tibble: 6 × 3
#>   object1 object2 result
#>   <chr>   <chr>    <dbl>
#> 1 S01     S02          0
#> 2 S01     S03          0
#> 3 S01     S04          0
#> 4 S01     S05          1
#> 5 S01     S06          0
#> 6 S01     S07          0
```
