# Backend-agnostic live comparisons for a tibble of pairs

`submit_llm_pairs()` is a backend-neutral wrapper around row-wise
comparison for multiple pairs. It takes a tibble of pairs (`ID1`,
`text1`, `ID2`, `text2`), submits each pair to the selected backend, and
aggregates the results.

## Usage

``` r
submit_llm_pairs(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  backend = c("openai", "anthropic", "gemini", "vertex", "together", "ollama",
    "typesafe"),
  endpoint = c("chat.completions", "responses", "decisions"),
  api_key = NULL,
  verbose = TRUE,
  status_every = 1,
  progress = TRUE,
  include_raw = FALSE,
  save_path = NULL,
  parallel = FALSE,
  workers = 1,
  execution_control = list(),
  ...
)
```

## Arguments

- pairs:

  Tibble or data frame with at least columns `ID1`, `text1`, `ID2`,
  `text2`. Typically created by
  [`make_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/make_pairs.md),
  [`sample_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_pairs.md),
  and
  [`randomize_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/randomize_pair_order.md).

- model:

  Model identifier for the chosen backend. For `"openai"` this should be
  an OpenAI model name (for example `"gpt-4.1"`, `"gpt-5.1"`). For
  `"anthropic"`, use Anthropic model names such as
  `"claude-sonnet-4-5"`. For `"gemini"` and `"vertex"`, use the
  corresponding Gemini model names (for example
  `"gemini-3.5-flash-lite"` or `"gemini-2.5-flash"`). For "together",
  use Together.ai model identifiers such as
  `"deepseek-ai/DeepSeek-V4-Flash-0731"`. For `"ollama"`, use a local
  model name known to the Ollama server (for example
  `"mistral-small3.2:24b"`, `"qwen3:32b"`, `"gemma3:27b"`).

- trait_name:

  Trait name to pass through to the backend-specific comparison function
  (for example `"Overall Quality"`).

- trait_description:

  Full-text trait description passed to the backend.

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- backend:

  Character scalar indicating which LLM provider to use. One of
  `"openai"`, `"anthropic"`, `"gemini"`, `"vertex"`, `"together"`,
  `"ollama"`, or `"typesafe"` (Jev decisions).

- endpoint:

  Character scalar specifying which endpoint family to use for backends
  that support multiple live APIs. For the `"openai"` backend this must
  be `"chat.completions"`, `"responses"`, or `"decisions"`, matching
  [`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md).
  For `"anthropic"`, `"gemini"`, `"vertex"`, `"together"`, and
  `"ollama"`, this is currently ignored. For TypeSafe, omit it or use
  `"systemone"`.

- api_key:

  Optional API key for the selected backend. If `NULL`, the
  backend-specific helper will use its own default environment variable.
  For `"ollama"`, this argument is ignored (no API key is required for
  local inference).

- verbose:

  Logical; if `TRUE`, prints status, timing, and result summaries (for
  backends that support it).

- status_every:

  Integer; print status and timing for every `status_every`-th pair.
  Defaults to 1 (every pair). Errors are always printed.

- progress:

  Logical; if `TRUE`, shows a textual progress bar for backends that
  support it.

- include_raw:

  Logical; if `TRUE`, each row of the returned tibble will include a
  `raw_response` list-column with the parsed JSON body from the backend
  (for backends that support this).

- save_path:

  Character string; optional file path (e.g., "output.csv") to save
  results incrementally. If the file exists, the function reads it to
  identify and skip pairs that have already been processed (resume
  mode). Supported by all backends.

- parallel:

  Logical; if `TRUE`, enables parallel processing using `future.apply`
  for generative backends, requiring future and future.apply. Decision
  concurrency uses optional promises/later.

- workers:

  Integer; the number of parallel workers (threads) to use if
  `parallel = TRUE`. Defaults to 1.

- execution_control:

  Named list of decision-only scheduling, retry, timeout and lock
  controls; defaults to [`list()`](https://rdrr.io/r/base/list.html).
  See
  [decision_bulk](https://shmercer.github.io/pairwiseLLM/reference/decision_bulk.md)
  for all defaults, persistent budgets, probability storage and recovery
  guarantees.

- ...:

  Additional backend-specific parameters. For `"openai"` these are
  forwarded to
  [`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md)
  and typically include `temperature`, `top_p`, `logprobs`, `reasoning`,
  `service_tier`, and `include_thoughts`. For both OpenAI live
  endpoints, `service_tier = "standard"` or `"default"` sends
  `"default"`; `"auto"`, `"flex"`, and `"priority"` are sent unchanged.
  Omission or `NULL` allows OpenAI's project-configured `"auto"`
  behavior. Model/tier availability is validated by OpenAI. For
  `"anthropic"`, `"gemini"`, and `"vertex"`, they are forwarded to
  [`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md),
  [`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md),
  or
  [`submit_vertex_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_vertex_pairs_live.md)
  and may include options such as `max_output_tokens`,
  `include_thoughts`, `service_tier`, `thinking_level`, and
  provider-specific controls. For `"ollama"`, arguments are forwarded to
  [`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md)
  and may include `host`, `think`, `num_ctx`, and other Ollama-specific
  options.

  For OpenAI and Gemini Developer API, `store` accepts `NULL`, `TRUE`,
  or `FALSE`. It controls OpenAI response storage or Gemini request
  logging; omission or `NULL` preserves provider/project defaults. See
  the selected provider helper for details; this is not a cross-provider
  retention policy.

## Value

A list containing:

- results:

  A tibble with one row per successfully processed pair.

- failed_pairs:

  A tibble containing rows that failed to process (for supported
  backends).

- failed_attempts:

  A tibble containing normalized failure records (invalid winners, parse
  failures, HTTP/timeouts) suitable for debugging.

## Details

This function supports parallel processing, incremental saving, and
resume capability for the `"openai"`, `"anthropic"`, `"gemini"`,
`"vertex"`, `"together"`, and `"ollama"` backends.

Start with `parallel = FALSE` and a small collection example. Cloud
calls transmit sample text to the chosen provider and may incur charges.
Inspect all three returned components before modeling; a successful
retry can leave an entry in `failed_attempts` even when the pair has a
valid result. Use a separate `save_path` for each study/model/trait
configuration. See [Getting
Started](https://shmercer.github.io/pairwiseLLM/articles/getting-started.html)
and the [recovery
guide](https://shmercer.github.io/pairwiseLLM/articles/provider-controls-and-recovery.html).

At present, the following backends are implemented:

- `"openai"` →
  [`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md)

- `"anthropic"` →
  [`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md)

- `"gemini"` →
  [`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md)

- `"vertex"` →
  [`submit_vertex_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_vertex_pairs_live.md)

- `"together"` →
  [`submit_together_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_together_pairs_live.md)

- `"ollama"` →
  [`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md)

- `"typesafe"` →
  [`submit_typesafe_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_typesafe_pairs_live.md)

For OpenAI Decisions and TypeSafe Jev, omitted `prompt_template` selects
the frozen decision instructions. Explicit generative templates fail.
See
[decision_bulk](https://shmercer.github.io/pairwiseLLM/reference/decision_bulk.md)
for concurrent live execution, occurrence IDs, strict resume identity,
atomic RDS checkpoints, CSV exports, raw-response privacy and the
one-time automatic ambiguous reissue. Concurrent decision execution uses
optional promises/later, with `workers` bounding in-flight requests;
sequential decision execution requires neither. Other backends retain
their existing future-based execution and CSV resumption behavior.

## See also

- [`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md),
  [`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md),
  [`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md),
  [`submit_vertex_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_vertex_pairs_live.md),
  [`submit_together_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_together_pairs_live.md),
  and
  [`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md)
  for backend-specific implementations.

Other live backends:
[`anthropic_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md),
[`check_llm_api_keys()`](https://shmercer.github.io/pairwiseLLM/reference/check_llm_api_keys.md),
[`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md),
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md),
[`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md),
[`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md),
[`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md),
[`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md),
[`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md),
[`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md),
[`submit_together_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_together_pairs_live.md),
[`submit_typesafe_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_typesafe_pairs_live.md),
[`submit_vertex_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_vertex_pairs_live.md),
[`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md),
[`typesafe_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/typesafe_compare_pair_live.md),
[`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires an API key for the chosen cloud backend.

data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 5, seed = 123) |>
  randomize_pair_order(seed = 456)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Small sequential collection example (paid requests, not a full ranking design)
res_live <- submit_llm_pairs(
  pairs             = pairs,
  model             = "gpt-4.1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  backend           = "openai",
  endpoint          = "chat.completions",
  parallel          = FALSE,
  save_path         = "results_openai.csv"
)

# Inspect successes, unresolved pairs, and failed attempts separately.
res_live$results
res_live$failed_pairs
res_live$failed_attempts
# After inspecting failures and collecting an adequate comparison design:
bt_data <- build_bt_data(res_live$results)

# Optional: parallel = TRUE, workers = 2 needs future and future.apply.

# Live comparisons using a local Ollama backend with incremental saving
res_ollama <- submit_llm_pairs(
  pairs             = pairs,
  model             = "mistral-small3.2:24b",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  backend           = "ollama",
  save_path         = "results_ollama.csv",
  verbose           = TRUE
)

# GPT-5 live comparisons with service tier
res_gpt5 <- submit_llm_pairs(
  pairs             = pairs,
  model             = "gpt-5",
  trait_name        = td$name,
  trait_description = td$description,
  backend           = "openai",
  endpoint          = "responses",
  reasoning         = "none",
  service_tier      = "flex"
)

res_ollama$results
} # }
```
