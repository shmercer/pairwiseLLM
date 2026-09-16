# Provider Controls and Recovery

This article explains the package’s reasoning/thinking controls and how
to recover useful work after provider failures. Model availability
changes independently of package behavior. Consult [Backends and Tested
Model
Configurations](https://shmercer.github.io/pairwiseLLM/articles/model-compatibility.html)
for the dated registry rather than treating examples here as a current
model catalog. The release registry used here was tested on 2026-09-05
with pairwiseLLM 1.3.1.

## Start with the symptom

You need the original inputs and any saved result files or job IDs. The
goal is to identify what succeeded and continue the remaining work. For
a first request, use [Getting
Started](https://shmercer.github.io/pairwiseLLM/articles/getting-started.md);
add provider controls only when your chosen model or study needs them.

| What you see | What to inspect or do next |
|----|----|
| Some live pairs have no winner | Inspect `failed_pairs` and `failed_attempts`; preserve `results`. |
| A batch is pending | Poll the saved job ID or resume its registry; avoid a second submission. |
| Polling or download hits a temporary network/server error | Retrieval retries transient errors; if exhausted, resume the existing job later. |
| An invalid key or job ID | Correct the credential/identifier; permanent errors are surfaced immediately. |
| Adaptive attempts produce no committed judgments | Inspect `judge_invalid_reason` and provider details in the step log. |
| A local model is unavailable or runs out of memory | Check the Ollama server/tag, then model size, context, and concurrency. |

## Storage, sampling, and service tiers

Omitted `temperature` and `top_p` leave model/provider sampling defaults
in effect; explicit values are forwarded subject to endpoint
constraints. A `service_tier` is a provider-specific processing option,
not a portable speed or price promise. OpenAI live requests accept
`"standard"`/`"default"`, `"auto"`, `"flex"`, and `"priority"`; explicit
standard/default sends `"default"`. Gemini Developer API and Vertex use
`"standard"`, `"flex"`, or `"priority"`, through different request
surfaces. Availability is provider- and model-dependent. See
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)
and the selected provider’s help for supported arguments.

For OpenAI, `store` controls response storage; for Gemini Developer API
it controls request logging. `NULL` omits the setting, preserving
provider/project defaults. Neither `store = FALSE` nor omission
establishes a general zero-retention policy. For OpenAI Batch storage,
output limits, and separate error files, see the [batch
guide](https://shmercer.github.io/pairwiseLLM/articles/advanced-batch-workflows.html#openai-storage-output-limits-and-error-files).

## Controls by backend

| Backend | Public controls | Important constraint |
|----|----|----|
| OpenAI | `reasoning`, `include_thoughts`, endpoint sampling controls | Reasoning support depends on model and endpoint; unsupported combinations abort during request normalization. |
| Anthropic | `reasoning = "none"` or `"enabled"`, `include_thoughts`, `thinking_budget_tokens` | Extended thinking requires `temperature = 1` and `1024 <= thinking_budget_tokens < max_tokens`. Claude 5 adaptive thinking is not implemented or advertised. |
| Gemini Developer API | `thinking_level`, `include_thoughts`, `service_tier` | Accepted thinking levels depend on the detected model family; `service_tier` is `NULL`/`"standard"`, `"flex"`, or `"priority"`. |
| Vertex | `thinking_level` or `thinking_budget`, `include_thoughts`, `service_tier` | Use the control supported by the selected Gemini family; Vertex batch is not implemented. |
| Together | OpenAI-compatible sampling arguments | The package does not expose a separate tested reasoning mode for this backend. |
| Ollama | `think`, `include_thoughts`, `num_ctx`, sampling options | Capabilities and memory use depend on the locally installed tag and server. |

Omitting a sampling or reasoning option generally leaves the
provider/model default in effect; it does not mean that every provider
uses the same value. `include_thoughts` controls whether available
reasoning text is requested or retained, but providers may return
summaries, redacted blocks, or no reasoning text. Never make a workflow
depend on hidden reasoning being present.

The exact tested request profiles are in the installed registry:

``` r

library(pairwiseLLM)

registry_path <- system.file("extdata", "model_compatibility.csv", package = "pairwiseLLM")
if (!nzchar(registry_path)) {
  source_paths <- c(
    file.path("inst", "extdata", "model_compatibility.csv"),
    file.path("..", "inst", "extdata", "model_compatibility.csv")
  )
  registry_path <- source_paths[file.exists(source_paths)][1]
}
registry <- utils::read.csv(registry_path, check.names = FALSE)

unique(registry[c(
  "backend", "endpoint", "reasoning_mode", "package_version", "test_date", "status"
)])
#>      backend             endpoint     reasoning_mode package_version  test_date
#> 1     openai            responses   provider-default           1.3.1 2026-09-05
#> 16    openai     chat.completions   provider-default           1.3.1 2026-09-05
#> 20 anthropic             messages               none           1.3.1 2026-09-05
#> 24 anthropic             messages            enabled           1.3.1 2026-09-05
#> 25    gemini      generateContent thinking_level=low           1.3.1 2026-09-05
#> 26    gemini batchGenerateContent thinking_level=low           1.3.1 2026-09-05
#> 41    gemini      generateContent thinking_level=low           1.3.1 2026-09-05
#> 42    gemini batchGenerateContent thinking_level=low           1.3.1 2026-09-05
#> 47    vertex      generateContent thinking_level=low           1.3.1 2026-09-05
#> 55    vertex      generateContent  thinking_budget=0           1.3.1 2026-09-05
#> 57    vertex      generateContent  thinking_budget=0           1.3.1 2026-09-05
#> 58  together     chat.completions               none           1.3.1 2026-09-05
#> 60  together     chat.completions               none           1.3.1 2026-09-05
#> 76    gemini      generateContent thinking_level=low           1.3.1 2026-09-02
#>            status
#> 1  tested-current
#> 16 tested-current
#> 20 tested-current
#> 24 tested-current
#> 25 tested-current
#> 26 tested-current
#> 41     unverified
#> 42     unverified
#> 47 tested-current
#> 55 tested-current
#> 57     unverified
#> 58 tested-current
#> 60     unverified
#> 76        retired
```

## Live calls: retain successes and failures

[`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
and backend-specific row-wise submitters return three views:

- `results`: normalized rows with a valid winner belonging to the
  submitted pair;
- `failed_pairs`: source pairs that did not produce a valid judgment;
- `failed_attempts`: retry- or attempt-level HTTP, timeout, parse,
  refusal, and validation details.

``` r

# Live API example; requires the selected provider credential and may incur cost.
library(pairwiseLLM)
data("example_writing_samples", package = "pairwiseLLM")
pairs <- make_pairs(example_writing_samples) |> sample_pairs(n_pairs = 10, seed = 123)
trait <- trait_description("overall_quality")
template <- get_prompt_template("default")
result <- submit_llm_pairs(
  pairs = pairs,
  backend = "openai",
  model = "gpt-5.6-luna",
  endpoint = "responses",
  trait_name = trait$name,
  trait_description = trait$description,
  prompt_template = template,
  reasoning = "none",
  save_path = "live-results.csv",
  parallel = FALSE
)

result$results
result$failed_pairs
result$failed_attempts
```

With `save_path`, completed rows are written incrementally and matching
pairs are skipped on a later call. Preserve the saved file, retry only
`failed_pairs`, and re-bind results after confirming that the retry did
not duplicate successful `custom_id` values. A returned HTTP success is
not a valid judgment unless parsing also produced a winner belonging to
the submitted pair.

## Batch recovery

Batch workflows separate local request creation, remote submission,
polling, download, and parsing. Keep the provider job ID, input JSONL,
downloaded output/error files, and parser results together. The
multi-batch helpers write `jobs_registry.csv`; after interruption, use
`llm_resume_multi_batches(jobs = NULL, output_dir = ...)` to reload that
registry.

Do not resubmit merely because a remote job is still pending. Poll the
existing job first. If a job reaches a terminal failure, preserve its
status/error payload, isolate the affected request IDs, and submit only
those requests after correcting the cause. See [Advanced: Submitting and
Polling Multiple
Batches](https://shmercer.github.io/pairwiseLLM/articles/advanced-batch-workflows.html)
for the complete unevaluated API flow.

Transient batch status/download failures are retried without creating a
new job. Multi-batch resume defers exhausted transient retrievals to
later polling rounds; permanent authentication, invalid-ID, parsing, and
local-write errors surface immediately. Polling timeouts are checked
between requests, so a request and its retries can extend the elapsed
time beyond the configured polling limit.

## Adaptive recovery

An invalid adaptive judgment consumes an attempted step but is not
committed to the comparison history and does not advance the refit
cadence. Inspect `judge_invalid_reason`, `llm_status_code`, and
`llm_error_message` in
[`adaptive_step_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_step_log.md).
Validate and load an existing session before adding budget; never
overwrite a failed directory merely to force resume.

## Ollama setup and resources

Install Ollama separately, start its server, and pull a model before
calling the package. Tags are local and environment-dependent. Large
models and context windows can exhaust accelerator or system memory;
reduce `num_ctx`, concurrency, or model size when requests are evicted
or time out.

``` r

# Local-system example; requires the Ollama CLI/server and a previously pulled tag.
options(pairwiseLLM.ollama_host = "http://127.0.0.1:11434")
ensure_only_ollama_model_loaded("qwen3:32b")
```

[`ensure_only_ollama_model_loaded()`](https://shmercer.github.io/pairwiseLLM/reference/ensure_only_ollama_model_loaded.md)
runs `ollama ps`, parses the first whitespace-delimited field, and calls
`ollama stop` for every active model except the requested one. It does
not pull or load the requested model. If listing fails or cannot be
parsed, it returns without unloading anything; a failed stop is reported
and processing continues. Because unloading affects the machine-wide
server, do not use it when another process may need those models.

## Privacy and retention

Cloud requests transmit sample and prompt text to the selected provider.
Raw responses and thoughts may repeat submitted text and increase
storage. Set `include_raw = TRUE` only under an appropriate privacy and
retention policy, and avoid placing credentials or raw responses in
registries, vignettes, bug reports, or version control.

## Estimate costs before a larger run

For large jobs, it is often useful to estimate token usage and cost
before launching a live run or submitting a batch. `pairwiseLLM`
includes
[`estimate_llm_pairs_cost()`](https://shmercer.github.io/pairwiseLLM/reference/estimate_llm_pairs_cost.md),
which runs a small **pilot** (paid live calls) and then estimates the
rest of the job by calibrating input tokens from prompt byte length.

The output includes both:

- **Expected cost** (using mean output tokens from usable pilot calls)
- **Budget cost** (using a high quantile of pilot output tokens,
  controlled by `budget_quantile`)

If you are running a discounted batch workflow, set `mode = "batch"` and
supply a `batch_discount` multiplier.

``` r

library(pairwiseLLM)
data("example_writing_samples", package = "pairwiseLLM")

# Create a moderate set of pairs
pairs_big <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 100, seed = 123) |>
  randomize_pair_order(seed = 456)

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

est <- estimate_llm_pairs_cost(
  pairs = pairs_big,
  backend = "anthropic",                # "openai", "anthropic", "gemini", "together"
  model = "claude-sonnet-4-5",
  trait_name = td$name,
  trait_description = td$description,
  prompt_template = tmpl,
  mode = "batch",
  batch_discount = 0.5,                 # set to 1 for no discount
  n_test = 10,                          # paid pilot calls (live)
  budget_quantile = 0.9,                # p90 output tokens
  cost_per_million_input = 3.0,         # fill in your provider pricing
  cost_per_million_output = 15.0
)

est$summary
```

### Deterministic calculation example

Suppose a two-pair live pilot records input-token counts of 20 and 40
and completion-token counts of 10 and 30. The prompt-byte calibration
predicts 60 and 80 input tokens for the two remaining pairs. With
`budget_quantile = 0.9`, R’s type-7 sample quantile of `c(10, 30)` is
28.

At input and output prices of 1 currency unit per million tokens and
`batch_discount = 0.5`, the estimator calculates:

- observed live pilot tokens: 60 input and 40 output;
- remaining expected tokens: 140 input and `mean(c(10, 30)) * 2 = 40`
  output;
- remaining budget output tokens: `28 * 2 = 56`;
- expected cost: `(60 + 40 + 0.5 * (140 + 40)) / 1e6 = 0.000190`; and
- budget cost: `(60 + 40 + 0.5 * (140 + 56)) / 1e6 = 0.000198`.

The pilot observations are included once at live prices. They are not
averaged into the estimated remaining count and are not discounted.

### Reuse pilot results

The estimator returns the original pilot output object and the pairs not
included in the pilot (`remaining_pairs`). Use `remaining_pairs` to
submit only the remaining work after you are satisfied with the
estimate. Pilot judgments are not automatically merged into the later
submission result:

``` r

remaining_pairs <- est$remaining_pairs

# Example: submit only the remaining pairs as a batch

batch <- llm_submit_pairs_batch(
          backend = "anthropic",
          model = "claude-sonnet-4-5",
          pairs = remaining_pairs,
          trait_name = td$name,
          trait_description = td$description,
          prompt_template = tmpl)

results <- llm_download_batch_results(batch)
```

Notes:

- The estimator does not require a provider tokenizer; it uses prompt
  byte length calibrated on the pilot.
- Ollama is not supported in the estimator (local models do not incur
  token costs).
- Reasoning/thinking tokens are treated as output tokens for pricing.
- Supply current prices for the selected provider, model, endpoint, and
  mode; the package does not maintain or validate a pricing catalog.

## Citation

> Mercer, S. H. (2026). *Provider controls and recovery* \[R package
> vignette\]. Comprehensive R Archive Network.
> <https://doi.org/10.32614/CRAN.package.pairwiseLLM>
