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
if (FALSE) { # \dontrun{
# Suppose the working directory contains S1.txt and S2.txt
samples <- read_samples_dir(path = ".", pattern = "\\\\.txt$")
samples
} # }
```
