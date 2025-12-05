# Register a named prompt template

This function validates a template (or reads it from a file) and stores
it under a user-provided name for reuse in the current R session.
Registered templates live in a package-internal registry.

## Usage

``` r
register_prompt_template(name, template = NULL, file = NULL, overwrite = FALSE)
```

## Arguments

- name:

  Character scalar; name under which to store the template.

- template:

  Optional character string containing a custom template. If `NULL`, the
  template is read from `file`, or the package default is used when both
  `template` and `file` are `NULL`.

- file:

  Optional path to a text file containing a template. Ignored if
  `template` is not `NULL`.

- overwrite:

  Logical; if `FALSE` (default), an error is thrown when `name` already
  exists in the registry.

## Value

Invisibly, the validated template string.

## Details

To make templates persistent across sessions, call this function in your
`.Rprofile` or in a project startup script.

Any template must contain the placeholders `{TRAIT_NAME}`,
`{TRAIT_DESCRIPTION}`, `{SAMPLE_1}`, and `{SAMPLE_2}`.

## Examples

``` r
# Register a custom template for this session
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

register_prompt_template("my_custom", template = custom)

# Retrieve and inspect it
tmpl <- get_prompt_template("my_custom")
cat(substr(tmpl, 1, 160), "...\n")
#> 
#> You are an expert writing assessor for {TRAIT_NAME}.
#> 
#> {TRAIT_NAME} is defined as {TRAIT_DESCRIPTION}.
#> 
#> Which of the samples below is better on {TRAIT_NAME}?
#> 
#> S ...
```
