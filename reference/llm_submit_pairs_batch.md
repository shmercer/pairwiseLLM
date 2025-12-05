# Submit pairs to an LLM backend via batch API

`llm_submit_pairs_batch()` is a backend-agnostic front-end for running
provider batch pipelines (OpenAI, Anthropic, Gemini).

It mirrors
[`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
but uses the provider batch APIs under the hood via
[`run_openai_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md),
[`run_anthropic_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_anthropic_batch_pipeline.md),
and
[`run_gemini_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_gemini_batch_pipeline.md).

For OpenAI, this helper will by default:

- Use the `chat.completions` batch style for most models, and

- Automatically switch to the `responses` style endpoint when:

  - `model` starts with `"gpt-5.1"` and

  - either `include_thoughts = TRUE` **or** a non-`"none"` `reasoning`
    effort is supplied in `...`.

You can override this by explicitly passing
`endpoint = "chat.completions"` or `endpoint = "responses"` in `...`.

For Anthropic, this helper delegates temperature and extended-thinking
behaviour to
[`run_anthropic_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_anthropic_batch_pipeline.md)
and
[`build_anthropic_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_anthropic_batch_requests.md),
which apply the following rules:

- When `reasoning = "none"` (no extended thinking), the default
  temperature is `0` (deterministic) unless you explicitly supply a
  different `temperature` in `...`.

- When `reasoning = "enabled"` (extended thinking), Anthropic requires
  `temperature = 1`. If you supply a different value in `...`, an error
  is raised. Default values in this mode are `max_tokens = 2048` and
  `thinking_budget_tokens = 1024`, subject to
  `1024 <= thinking_budget_tokens < max_tokens`.

- Setting `include_thoughts = TRUE` while leaving `reasoning = "none"`
  causes
  [`run_anthropic_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_anthropic_batch_pipeline.md)
  to upgrade to `reasoning = "enabled"`, which implies `temperature = 1`
  for the batch.

For Gemini, this helper simply forwards `include_thoughts` and other
arguments to
[`run_gemini_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_gemini_batch_pipeline.md),
which is responsible for interpreting any thinking-related options.

Currently, this function *synchronously* runs the full batch pipeline
for each backend (build requests, create batch, poll until complete,
download results, parse). The returned object contains both metadata and
a normalized `results` tibble. See
[`llm_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/llm_download_batch_results.md)
to extract the results.

## Usage

``` r
llm_submit_pairs_batch(
  pairs,
  backend = c("openai", "anthropic", "gemini"),
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  include_thoughts = FALSE,
  include_raw = FALSE,
  ...
)
```

## Arguments

- pairs:

  A data frame or tibble of pairs with columns `ID1`, `text1`, `ID2`,
  and `text2`. Additional columns are allowed and will be carried
  through where supported.

- backend:

  Character scalar; one of `"openai"`, `"anthropic"`, or `"gemini"`.
  Matching is case-insensitive.

- model:

  Character scalar model name to use for the batch job.

- trait_name:

  A short name for the trait being evaluated (e.g. `"overall_quality"`).

- trait_description:

  A human-readable description of the trait.

- prompt_template:

  A prompt template created by
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md)
  or a compatible character scalar.

- include_thoughts:

  Logical; whether to request and parse model "thoughts" (where
  supported). For OpenAI GPT-5.1, setting this to `TRUE` will by default
  cause the batch to use the `responses` endpoint (unless you explicitly
  pass an `endpoint` in `...`). For Anthropic, setting this to `TRUE`
  while `reasoning = "none"` (in `...`) upgrades to
  `reasoning = "enabled"`, which implies `temperature = 1` for the
  batch.

- include_raw:

  Logical; whether to include raw provider responses in the result
  (where supported by backends).

- ...:

  Additional arguments passed through to the backend-specific
  `run_*_batch_pipeline()` functions. This can include provider-specific
  options such as temperature or batch configuration fields. For OpenAI,
  this may include `endpoint`, `temperature`, `top_p`, `logprobs`,
  `reasoning`, etc. For Anthropic, this may include `reasoning`,
  `max_tokens`, `temperature`, or `thinking_budget_tokens`.

## Value

A list of class `"pairwiseLLM_batch"` containing at least:

- `backend`: the backend identifier (`"openai"`, `"anthropic"`,
  `"gemini"`),

- `batch_input_path`: path to the JSONL request file (if applicable),

- `batch_output_path`: path to the JSONL output file (if applicable),

- `batch`: provider-specific batch object (e.g., job metadata),

- `results`: a tibble of parsed comparison results in the standard
  pairwiseLLM schema.

Additional fields returned by the backend-specific pipeline functions
are preserved.

## Examples

``` r
if (FALSE) { # \dontrun{
pairs <- make_pairs(c("A", "B", "C"))

batch <- llm_submit_pairs_batch(
  pairs             = pairs,
  backend           = "openai",
  model             = "gpt-4o-mini",
  trait_name        = "overall_quality",
  trait_description = "Overall quality of the response.",
  prompt_template   = set_prompt_template(),
  include_thoughts  = FALSE
)

res <- llm_download_batch_results(batch)
} # }
```
