# Collect and resume live TypeSafe Jev decisions

Collect multiple binary decisions with the same scientific contract as
[`typesafe_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/typesafe_compare_pair_live.md).
Use
[`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
for provider-neutral dispatch. These are live requests, not managed
Batch jobs.

## Usage

``` r
submit_typesafe_pairs_live(
  pairs,
  model = "jev-1.13.0",
  trait_name,
  trait_description,
  prompt_template = NULL,
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

  Requested Jev identifier; defaults to versioned `"jev-1.13.0"`.

- trait_name:

  Trait name to pass through to the backend-specific comparison function
  (for example `"Overall Quality"`).

- trait_description:

  Full-text trait description passed to the backend.

- prompt_template:

  Must be omitted or `NULL`; custom decision instructions are supplied
  with `decision_template` in `...`.

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

  Only `decision_template` is supported. See
  [`typesafe_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/typesafe_compare_pair_live.md)
  for the complete scientific/provider contract.

## Value

The `results`, `failed_pairs`, and `failed_attempts` list documented in
[`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md).

## Details

See
[decision_bulk](https://shmercer.github.io/pairwiseLLM/reference/decision_bulk.md)
for execution controls, persistent retry budgets, locking, probability
storage, and interruption/recovery guarantees.

## See also

[`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md),
[`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md),
[decision_bulk](https://shmercer.github.io/pairwiseLLM/reference/decision_bulk.md)

Other live backends:
[`anthropic_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md),
[`check_llm_api_keys()`](https://shmercer.github.io/pairwiseLLM/reference/check_llm_api_keys.md),
[`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md),
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md),
[`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md),
[`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md),
[`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md),
[`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md),
[`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md),
[`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md),
[`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md),
[`submit_together_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_together_pairs_live.md),
[`submit_vertex_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_vertex_pairs_live.md),
[`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md),
[`typesafe_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/typesafe_compare_pair_live.md),
[`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md)
