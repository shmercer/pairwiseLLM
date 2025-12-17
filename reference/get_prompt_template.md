# Retrieve a named prompt template

This function retrieves a prompt template from either:

- the user registry (see
  [`register_prompt_template`](https://shmercer.github.io/pairwiseLLM/reference/register_prompt_template.md)),
  or

- a built-in template stored under `inst/templates`.

## Usage

``` r
get_prompt_template(name = "default")
```

## Arguments

- name:

  Character scalar giving the template name.

## Value

A single character string containing the prompt template.

## Details

The function first checks user-registered templates, then looks for a
built-in text file `inst/templates/<name>.txt`. The special name
`"default"` falls back to
[`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md)
when no user-registered or built-in template is found.

## See also

[`register_prompt_template`](https://shmercer.github.io/pairwiseLLM/reference/register_prompt_template.md),
[`list_prompt_templates`](https://shmercer.github.io/pairwiseLLM/reference/list_prompt_templates.md),
[`remove_prompt_template`](https://shmercer.github.io/pairwiseLLM/reference/remove_prompt_template.md)

## Examples

``` r
# Get the built-in default template
tmpl_default <- get_prompt_template("default")

# List available template names
list_prompt_templates()
#> [1] "default" "test1"   "test2"   "test3"   "test4"   "test5"  
```
