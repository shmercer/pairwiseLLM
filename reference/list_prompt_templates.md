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

## See also

[`trait_description()`](https://shmercer.github.io/pairwiseLLM/reference/trait_description.md),
[`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md)

Other prompts and traits:
[`build_prompt()`](https://shmercer.github.io/pairwiseLLM/reference/build_prompt.md),
[`get_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/get_prompt_template.md),
[`register_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/register_prompt_template.md),
[`remove_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/remove_prompt_template.md),
[`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md),
[`trait_description()`](https://shmercer.github.io/pairwiseLLM/reference/trait_description.md)

## Examples

``` r
list_prompt_templates()
#> [1] "default" "test1"   "test2"   "test3"   "test4"   "test5"  
```
