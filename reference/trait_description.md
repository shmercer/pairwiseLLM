# Get a trait name and description for prompts

This helper returns both a short display name and a longer description
for a scoring trait. These can be inserted into the prompt template via
the `{TRAIT_NAME}` and `{TRAIT_DESCRIPTION}` placeholders.

## Usage

``` r
trait_description(
  name = c("overall_quality", "organization", "IRRC"),
  custom_name = NULL,
  custom_description = NULL
)
```

## Arguments

- name:

  Character identifier for a built-in trait. One of `"overall_quality"`,
  `"organization"`, or `"IRRC"`. `"IRRC"` is case-sensitive and returns
  an overall-writing rubric spanning prompt task, development of
  explanation, organization, and language use. Ignored if
  `custom_description` is supplied.

- custom_name:

  Optional short label to use when supplying a `custom_description`.
  Defaults to "Custom trait" if `custom_description` is provided but
  `custom_name` is `NULL`.

- custom_description:

  Optional full-text definition of a custom trait. When supplied,
  built-in `name` values are ignored and this text is returned instead.

## Value

A list with two elements:

- name:

  Short display label for the trait (e.g., "Overall Quality").

- description:

  Full-text definition of the trait, suitable for inclusion in the
  prompt.

## See also

[`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md),
[`build_prompt()`](https://shmercer.github.io/pairwiseLLM/reference/build_prompt.md)

Other prompts and traits:
[`build_prompt()`](https://shmercer.github.io/pairwiseLLM/reference/build_prompt.md),
[`get_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/get_prompt_template.md),
[`list_prompt_templates()`](https://shmercer.github.io/pairwiseLLM/reference/list_prompt_templates.md),
[`register_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/register_prompt_template.md),
[`remove_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/remove_prompt_template.md),
[`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md)

## Examples

``` r
td <- trait_description("overall_quality")
td$name
#> [1] "Overall Quality"
td$description
#> [1] "Overall quality of the writing, considering how well ideas are expressed,\nhow clearly the writing is organized, and how effective the language and\nconventions are."

custom_td <- trait_description(
  custom_name = "Ideas",
  custom_description = "Quality and development of ideas in the writing."
)
custom_td$name
#> [1] "Ideas"
custom_td$description
#> [1] "Quality and development of ideas in the writing."
```
