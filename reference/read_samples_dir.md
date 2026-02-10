# Read writing samples from a directory of .txt files

This function reads all text files in a directory and uses the filename
(without extension) as the sample ID and the file contents as the text.

## Usage

``` r
read_samples_dir(path = ".", pattern = "\\.txt$")
```

## Arguments

- path:

  Directory containing .txt files.

- pattern:

  A regular expression used to match file names. Defaults to
  `"\\.txt$"`, meaning all files ending in `.txt`.

## Value

A tibble with columns:

- `ID`: filename without extension

- `text`: file contents as a single character string

## Examples

``` r
# Create a temporary directory with sample text files
samples_dir <- tempfile()
dir.create(samples_dir)

writeLines("This is sample A.", file.path(samples_dir, "A.txt"))
writeLines("This is sample B.", file.path(samples_dir, "B.txt"))

# Read samples into a tibble
samples <- read_samples_dir(samples_dir)

samples
#> # A tibble: 2 × 2
#>   ID    text             
#>   <chr> <chr>            
#> 1 A     This is sample A.
#> 2 B     This is sample B.
```
