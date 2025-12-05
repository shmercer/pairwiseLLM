# Get or set a prompt template for pairwise comparisons

This function returns a default prompt template that includes
placeholders for the trait name, trait description, and two writing
samples. Any custom template must contain the placeholders
`{TRAIT_NAME}`, `{TRAIT_DESCRIPTION}`, `{SAMPLE_1}`, and `{SAMPLE_2}`.

## Usage

``` r
set_prompt_template(template = NULL, file = NULL)
```

## Arguments

- template:

  Optional character string containing a custom template. If `NULL`, a
  default template is returned.

- file:

  Optional path to a text file containing a template. Ignored if
  `template` is not `NULL`.

## Value

A character string containing the prompt template.

## Details

The default template is stored as a plain-text file in
`inst/templates/default.txt` and loaded at run time. This makes it easy
to inspect and modify the prompt text without changing the R code.

## Examples

``` r
# Get the default template shipped with the package
tmpl <- set_prompt_template()
cat(substr(tmpl, 1, 200), "...\n")
#> You are a debate adjudicator. Your task is to weigh the comparative strengths of two writing samples regarding a specific trait.
#> 
#> TRAIT: {TRAIT_NAME}
#> DEFINITION: {TRAIT_DESCRIPTION}
#> 
#> SAMPLES:
#> 
#> === SAM ...

# Use a custom template defined in-line
custom <- "
You are an expert writing assessor for {TRAIT_NAME}.

{TRAIT_NAME} is defined as {TRAIT_DESCRIPTION}.

Which of the samples below is better on {TRAIT_NAME}?

SAMPLE 1:
{SAMPLE_1}

SAMPLE 2:
{SAMPLE_2}

<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> or
<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>
"

tmpl2 <- set_prompt_template(template = custom)
cat(substr(tmpl2, 1, 120), "...\n")
#> 
#> You are an expert writing assessor for {TRAIT_NAME}.
#> 
#> {TRAIT_NAME} is defined as {TRAIT_DESCRIPTION}.
#> 
#> Which of the sam ...
```
