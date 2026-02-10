# Build an LLM judge function for adaptive ranking

Creates a judge function compatible with
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md)
by wrapping
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)
and converting provider responses into adaptive binary outcomes (`Y` in
`{0,1}`).

## Usage

``` r
make_adaptive_judge_llm(
  backend = c("openai", "anthropic", "gemini", "together", "ollama"),
  model,
  trait = "overall_quality",
  trait_name = NULL,
  trait_description = NULL,
  prompt_template = set_prompt_template(),
  endpoint = "chat.completions",
  api_key = NULL,
  include_raw = FALSE,
  text_col = "text",
  judge_args = list()
)
```

## Arguments

- backend:

  Backend passed to
  [`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md).

- model:

  Model identifier passed to
  [`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md).

- trait:

  Built-in trait key used when no custom trait is supplied. Ignored when
  both `trait_name` and `trait_description` are supplied.

- trait_name:

  Optional custom trait display name.

- trait_description:

  Optional custom trait definition.

- prompt_template:

  Prompt template string. Defaults to
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- endpoint:

  Endpoint family passed to
  [`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md).
  Only used when `backend = "openai"`; ignored otherwise.

- api_key:

  Optional API key passed to
  [`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md).

- include_raw:

  Logical; forwarded to
  [`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md).

- text_col:

  Name of the text column expected in adaptive item rows.

- judge_args:

  Named list of additional fixed arguments forwarded to
  [`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md).
  Use this for provider-specific controls such as `reasoning`,
  `service_tier`, `temperature`, `top_p`, `logprobs`, `host`, or
  `include_thoughts`.

## Value

A function `judge(A, B, state, ...)` returning a list with fields
`is_valid`, `Y`, and `invalid_reason`.

## Details

The returned function has signature `judge(A, B, state, ...)` and
enforces the adaptive transactional contract: it returns
`is_valid = TRUE` with `Y` in `{0,1}` when the model response identifies
one of the two presented items, and returns `is_valid = FALSE`
otherwise.

Model configuration is split into:

- fixed build-time options via `judge_args`,

- per-run overrides via `judge_call_args` in
  [`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md),

- optional per-step overrides via `...` passed through
  [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md).

Collectively this supports all
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)
options, including backend-specific parameters such as OpenAI
`reasoning` and `service_tier`.

## See also

[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)

Other adaptive ranking:
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
[`summarize_adaptive()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_adaptive.md)

## Examples

``` r
judge <- make_adaptive_judge_llm(
  backend = "openai",
  model = "gpt-5.1",
  endpoint = "responses",
  judge_args = list(
    reasoning = "low",
    service_tier = "flex",
    include_thoughts = FALSE
  )
)
```
