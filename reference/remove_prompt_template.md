# Remove a registered prompt template

This function removes a template from the user registry created by
[`register_prompt_template`](https://shmercer.github.io/pairwiseLLM/reference/register_prompt_template.md).
It does not affect built-in templates stored under `inst/templates`.

## Usage

``` r
remove_prompt_template(name, quiet = FALSE)
```

## Arguments

- name:

  Character scalar; name of the template to remove.

- quiet:

  Logical; if `FALSE` (default), an error is thrown when `name` is not
  found in the user registry. When `TRUE`, the function simply returns
  `FALSE` in that case.

## Value

Invisibly, `TRUE` if a template was removed, `FALSE` otherwise.

## See also

[`register_prompt_template`](https://shmercer.github.io/pairwiseLLM/reference/register_prompt_template.md),
[`get_prompt_template`](https://shmercer.github.io/pairwiseLLM/reference/get_prompt_template.md),
[`list_prompt_templates`](https://shmercer.github.io/pairwiseLLM/reference/list_prompt_templates.md)

## Examples

``` r
# Register and then remove a template
register_prompt_template("to_delete", template = set_prompt_template())
remove_prompt_template("to_delete")
```
