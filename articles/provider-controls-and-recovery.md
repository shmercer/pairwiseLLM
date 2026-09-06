# Provider Controls and Recovery

This article explains the package’s reasoning/thinking controls and how
to recover useful work after provider failures. Model availability
changes independently of package behavior. Consult [Backends and Tested
Model
Configurations](https://shmercer.github.io/pairwiseLLM/articles/model-compatibility.md)
for the dated registry rather than treating examples here as a current
model catalog. The release registry used here was tested on 2026-09-05
with pairwiseLLM 1.3.1.

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
those requests after correcting the cause. See [Advanced Batch
Workflows](https://shmercer.github.io/pairwiseLLM/articles/advanced-batch-workflows.md)
for the complete unevaluated API flow.

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

## Citation

> Mercer, S. H. (2026). *Provider controls and recovery* \[R package
> vignette\]. Comprehensive R Archive Network.
> <https://doi.org/10.32614/CRAN.package.pairwiseLLM>
