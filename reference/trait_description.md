# Get a trait name and description for prompts

This helper returns both a short display name and a longer description
for a scoring trait. These can be inserted into the prompt template via
the `{TRAIT_NAME}` and `{TRAIT_DESCRIPTION}` placeholders.

## Usage

``` r
trait_description(
  name = c("overall_quality", "organization"),
  custom_name = NULL,
  custom_description = NULL
)
```

## Arguments

- name:

  Character identifier for a built-in trait. One of `"overall_quality"`
  or `"organization"`. Ignored if `custom_description` is supplied.

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

## Examples

``` r
td <- trait_description("overall_quality")
td$name
#> [1] "Overall Quality"
td$description
#> [1] "Overall quality of the writing, considering how well ideas are expressed,\n      how clearly the writing is organized, and how effective the language and\n      conventions are."

custom_td <- trait_description(
  custom_name = "Ideas",
  custom_description = "Quality and development of ideas in the writing."
)
custom_td$name
#> [1] "Ideas"
custom_td$description
#> [1] "Quality and development of ideas in the writing."
```
