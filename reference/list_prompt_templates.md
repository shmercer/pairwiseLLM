# List available prompt templates

This function lists template names that are available either as built-in
text files under `inst/templates` or as user-registered templates in the
current R session.

## Usage

``` r
list_prompt_templates(include_builtin = TRUE, include_registered = TRUE)
```

## Arguments

- include_builtin:

  Logical; include built-in template names (the default is `TRUE`).

- include_registered:

  Logical; include user-registered names (the default is `TRUE`).

## Value

A sorted character vector of unique template names.

## Details

Built-in templates are identified by files named `<name>.txt` within
`inst/templates`. For example, a file `inst/templates/minimal.txt` will
be listed as `"minimal"`.

## Examples

``` r
list_prompt_templates()
#> [1] "default" "test1"   "test2"   "test3"   "test4"   "test5"  
```
