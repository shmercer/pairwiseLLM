# Add text columns (text1/text2) to a pairs table

Joins `samples$text` onto a pairs table using pair IDs, creating (or
filling) `text1` and `text2`.

## Usage

``` r
add_pair_texts(
  pairs,
  samples,
  id1_col = NULL,
  id2_col = NULL,
  overwrite = TRUE
)
```

## Arguments

- pairs:

  A data frame of pair IDs. Must contain either `ID1`/`ID2`,
  `object1`/`object2`, or the columns given by `id1_col` and `id2_col`.

- samples:

  A data frame with columns `ID` and `text`.

- id1_col, id2_col:

  Optional. Column names in `pairs` holding the first and second IDs. If
  `NULL`, inferred from `pairs` (`ID1`/`ID2` or `object1`/`object2`).

- overwrite:

  Logical. If `TRUE` (default), any existing `text1`/`text2` columns in
  `pairs` are overwritten from `samples`. If `FALSE`, existing
  non-missing `text1`/`text2` values are preserved and only missing
  values are filled.

## Value

A tibble with `ID1`, `text1`, `ID2`, `text2`, plus any other columns
originally present in `pairs`.

## Details

Supported input schemas for `pairs`:

- `ID1`/`ID2`

- `object1`/`object2`

- or specify `id1_col`/`id2_col`

## Examples

``` r
samples <- tibble::tibble(ID = c("A", "B"), text = c("aaa", "bbb"))
pairs <- tibble::tibble(ID1 = "A", ID2 = "B")
add_pair_texts(pairs, samples)
#> # A tibble: 1 × 4
#>   ID1   text1 ID2   text2
#>   <chr> <chr> <chr> <chr>
#> 1 A     aaa   B     bbb  

# Preserve existing non-missing text
pairs2 <- tibble::tibble(ID1 = "A", ID2 = "B", text1 = "keep", text2 = NA_character_)
add_pair_texts(pairs2, samples, overwrite = FALSE)
#> # A tibble: 1 × 4
#>   ID1   text1 ID2   text2
#>   <chr> <chr> <chr> <chr>
#> 1 A     keep  B     bbb  
```
