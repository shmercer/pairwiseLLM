# Create a Gemini Batch job from request objects

This is a thin wrapper around the REST endpoint
`/v1beta/models/<MODEL>:batchGenerateContent`. It accepts a list of
GenerateContent request objects and returns the created Batch job.

## Usage

``` r
gemini_create_batch(
  requests,
  model,
  api_key = Sys.getenv("GEMINI_API_KEY"),
  api_version = "v1beta",
  display_name = NULL
)
```

## Arguments

- requests:

  List of GenerateContent request objects, each of the form
  `list(contents = ..., generationConfig = ...)`. You can obtain this
  list from the output of
  [`build_gemini_batch_requests`](https://shmercer.github.io/pairwiseLLM/reference/build_gemini_batch_requests.md)
  via `batch$request`.

- model:

  Gemini model name, for example `"gemini-3-pro-preview"`.

- api_key:

  Optional Gemini API key. Defaults to `Sys.getenv("GEMINI_API_KEY")`.

- api_version:

  API version string for the path; defaults to `"v1beta"`.

- display_name:

  Optional display name for the batch.

## Value

A list representing the Batch job object returned by Gemini. Important
fields include `name`, `metadata$state`, and (after completion)
`response$inlinedResponses` or `response$responsesFile`.

## Details

Typically you will not call this directly; instead, use
[`run_gemini_batch_pipeline`](https://shmercer.github.io/pairwiseLLM/reference/run_gemini_batch_pipeline.md)
which builds requests from a tibble of pairs, creates the batch, polls
for completion, and parses the results.

## Examples

``` r
# --- Offline preparation: build GenerateContent requests ---

data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 2, seed = 123)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

batch_tbl <- build_gemini_batch_requests(
  pairs             = pairs,
  model             = "gemini-3-pro-preview",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  thinking_level    = "low"
)

# Extract the list of request objects
requests <- batch_tbl$request

# Inspect a single GenerateContent request (purely local)
requests[[1]]
#> $contents
#> $contents[[1]]
#> $contents[[1]]$role
#> [1] "user"
#> 
#> $contents[[1]]$parts
#> $contents[[1]]$parts[[1]]
#> $contents[[1]]$parts[[1]]$text
#> [1] "You are a debate adjudicator. Your task is to weigh the comparative strengths of two writing samples regarding a specific trait.\n\nTRAIT: Overall Quality\nDEFINITION: Overall quality of the writing, considering how well ideas are expressed,\n      how clearly the writing is organized, and how effective the language and\n      conventions are.\n\nSAMPLES:\n\n=== SAMPLE_1 ===\nEvaluating writing is challenging because no rubric can fully capture what\n    makes a text effective for a particular audience. Two essays might receive\n    the same score for completely different reasons, obscuring the feedback\n    loop.\n\n=== SAMPLE_2 ===\nWriting assessment is challenging because of the trade-off between\n    validity and reliability. Highly standardized scoring protocols often strip\n    away the subjective appreciation of voice and creativity, while holistic\n    scoring captures the 'whole' but risks being unreliable.\n\nEVALUATION PROCESS (Mental Simulation):\n\n1.  **Advocate for SAMPLE_1**: Mentally list the single strongest point of evidence that makes SAMPLE_1 the winner.\n2.  **Advocate for SAMPLE_2**: Mentally list the single strongest point of evidence that makes SAMPLE_2 the winner.\n3.  **Adjudicate**: Compare the *strength of the evidence* identified in steps 1 and 2. Which sample provided the more compelling demonstration of the definition above?\n\nCRITICAL:\n- You must construct a mental argument for BOTH samples before deciding.\n- Do not default to the first sample read.\n- If the samples are close, strictly follow the trait definition to break the tie.\n\nFINAL DECISION:\nOutput your decision based on the stronger evidence.\n\n<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>\nOR\n<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>\n\n(Provide only the XML tag)."
#> 
#> 
#> 
#> 
#> 
#> $generationConfig
#> $generationConfig$thinkingConfig
#> $generationConfig$thinkingConfig$includeThoughts
#> [1] FALSE
#> 
#> $generationConfig$thinkingConfig$thinkingLevel
#> [1] "Low"
#> 
#> 
#> 

# --- Online step: create the Gemini Batch job ---
# Requires network access and a valid Gemini API key.
if (FALSE) { # \dontrun{
batch <- gemini_create_batch(
  requests = requests,
  model    = "gemini-3-pro-preview"
)

batch$name
batch$metadata$state
} # }
```
