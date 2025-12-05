# Read writing samples from a data frame

This function extracts ID and text columns from a data frame and
enforces that IDs are unique. By default, it assumes the first column is
the ID and the second column is the text.

## Usage

``` r
read_samples_df(df, id_col = 1, text_col = 2)
```

## Arguments

- df:

  A data frame or tibble containing at least two columns.

- id_col:

  Column specifying the IDs. Can be a column name (string) or a column
  index (integer). Defaults to 1.

- text_col:

  Column specifying the writing samples (character). Can be a column
  name or index. Defaults to 2.

## Value

A tibble with columns:

- `ID`: character ID for each sample

- `text`: character string of the writing sample

Any remaining columns in `df` are retained unchanged.

## Examples

``` r
df <- data.frame(
  StudentID = c("S1", "S2"),
  Response  = c("This is sample 1.", "This is sample 2."),
  Grade     = c(8, 9),
  stringsAsFactors = FALSE
)

samples <- read_samples_df(df, id_col = "StudentID", text_col = "Response")
samples
#> # A tibble: 2 × 3
#>   ID    text              Grade
#>   <chr> <chr>             <dbl>
#> 1 S1    This is sample 1.     8
#> 2 S2    This is sample 2.     9

# Using the built-in example dataset (keep only ID and text)
data("example_writing_samples")
samples2 <- read_samples_df(
  example_writing_samples[, c("ID", "text")],
  id_col   = "ID",
  text_col = "text"
)
head(samples2)
#> # A tibble: 6 × 2
#>   ID    text                                                                    
#>   <chr> <chr>                                                                   
#> 1 S01   Writing assessment is hard. People write different things. It is confus…
#> 2 S02   It is hard to grade writing. Some are long and some are short. I do not…
#> 3 S03   Assessing writing is difficult because everyone writes differently and …
#> 4 S04   Grading essays is tough work. You have to read a lot. Sometimes the han…
#> 5 S05   Writing assessment is challenging because teachers must judge ideas, or…
#> 6 S06   It is difficult to assess writing because it is subjective. One teacher…
```
