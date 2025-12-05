# Build a concrete LLM prompt from a template

This function takes a prompt template (typically from
[`set_prompt_template`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md)),
a trait name and description, and two writing samples, and fills in the
required placeholders.

## Usage

``` r
build_prompt(template, trait_name, trait_desc, text1, text2)
```

## Arguments

- template:

  Character string containing the prompt template.

- trait_name:

  Character scalar giving a short label for the trait (e.g., "Overall
  Quality").

- trait_desc:

  Character scalar giving the full definition of the trait.

- text1:

  Character scalar containing the text for SAMPLE_1.

- text2:

  Character scalar containing the text for SAMPLE_2.

## Value

A single character string containing the completed prompt.

## Details

The template must contain the placeholders: `{TRAIT_NAME}`,
`{TRAIT_DESCRIPTION}`, `{SAMPLE_1}`, and `{SAMPLE_2}`.

## Examples

``` r
tmpl <- set_prompt_template()
td   <- trait_description("overall_quality")
prompt <- build_prompt(
  template   = tmpl,
  trait_name = td$name,
  trait_desc = td$description,
  text1      = "This is sample 1.",
  text2      = "This is sample 2."
)
cat(substr(prompt, 1, 200), "...\n")
#> You are a debate adjudicator. Your task is to weigh the comparative strengths of two writing samples regarding a specific trait.
#> 
#> TRAIT: Overall Quality
#> DEFINITION: Overall quality of the writing, con ...
```
