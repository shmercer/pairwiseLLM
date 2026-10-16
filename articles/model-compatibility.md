# Backends and Tested Model Configurations

## Choose a backend and model

Start with a provider you can access, check whether you need live or
batch processing, and configure only its credential. A model identifier
is the exact name sent to that service. Use its official catalog to
check availability, then try a small request with minimal controls
before planning a larger run.

This page provides the backend/key table and dated test evidence. A past
success is useful evidence about one configuration, not a recommendation
or a promise that it remains available. See [Getting
Started](https://shmercer.github.io/pairwiseLLM/articles/getting-started.md)
for a first request and [Provider Controls and
Recovery](https://shmercer.github.io/pairwiseLLM/articles/provider-controls-and-recovery.md)
for troubleshooting.

## What the package supports

`pairwiseLLM` implements request and response handling for seven
backends. The package generally forwards `model` identifiers to the
selected provider; it does not maintain an exhaustive model allowlist.

| Backend | Provider surface | Live | Batch | Credential |
|----|----|:--:|:--:|----|
| `openai` | OpenAI generative endpoints; opt-in Decisions | yes | generative only | `OPENAI_API_KEY` |
| `anthropic` | Anthropic | yes | yes | `ANTHROPIC_API_KEY` |
| `gemini` | Gemini Developer API | yes | yes | `GEMINI_API_KEY` |
| `vertex` | Vertex AI Gemini API | yes | no | `VERTEX_API_KEY` |
| `together` | Together AI | yes | no | `TOGETHER_API_KEY` |
| `typesafe` | TypeSafe Jev (`systemone`) | yes | no | `TYPESAFE_API_KEY` |
| `ollama` | Local Ollama server | yes | no | none |

Backend implementation is not the same as compatibility with every
model. Endpoint compatibility depends on the request shape a model
accepts; maintainer testing covers only the exact configurations below;
and current availability is controlled by the provider. Preview
identifiers and reasoning controls can change independently of this
package.

Check current availability in the official catalogs for
[OpenAI](https://developers.openai.com/api/docs/models/all),
[Anthropic](https://platform.claude.com/docs/en/models/overview),
[Gemini Developer API](https://ai.google.dev/gemini-api/docs/models),
[Vertex
AI](https://docs.cloud.google.com/gemini-enterprise-agent-platform/models/model-versions),
and [Together AI](https://docs.together.ai/docs/serverless/models).
[TypeSafe](https://docs.typesafe.ai/models) documents Jev versions and
aliases. Ollama tags depend on what is installed on the local server.

## Dated compatibility record

The machine-readable source is installed at
`inst/extdata/model_compatibility.csv`. Absence from this registry does
not imply incompatibility. A `true` value records a successful
maintained smoke test for that exact mode; a live success is not
evidence of batch success. The dated detailed result artifacts are
`inst/extdata/model_smoke_results_2026-09-05.csv` and
`inst/extdata/model_batch_smoke_results_2026-09-05.csv`. The October 16
decision qualification is summarized below and in the registry; its
private raw journals are not distributed. Standard generative
configurations omit `temperature` and `top_p`, so their model/provider
defaults apply.

``` r

registry_path <- system.file(
  "extdata",
  "model_compatibility.csv",
  package = "pairwiseLLM"
)
if (!nzchar(registry_path)) {
  source_paths <- c(
    file.path("inst", "extdata", "model_compatibility.csv"),
    file.path("..", "inst", "extdata", "model_compatibility.csv")
  )
  registry_path <- source_paths[file.exists(source_paths)][1]
}
registry <- utils::read.csv(registry_path, check.names = FALSE)
registry[c(
  "backend", "model_id", "endpoint", "live_tested", "batch_tested",
  "reasoning_mode", "package_version", "test_date", "status"
)]
#>      backend                                model_id             endpoint
#> 1     openai                             gpt-5.6-sol            responses
#> 2     openai                           gpt-5.6-terra            responses
#> 3     openai                            gpt-5.6-luna            responses
#> 4     openai                                 gpt-5.5            responses
#> 5     openai                                 gpt-5.4            responses
#> 6     openai                            gpt-5.4-mini            responses
#> 7     openai                            gpt-5.4-nano            responses
#> 8     openai                           gpt-5.3-codex            responses
#> 9     openai                                 gpt-5.2            responses
#> 10    openai                                 gpt-5.1            responses
#> 11    openai                                   gpt-5            responses
#> 12    openai                              gpt-5-mini            responses
#> 13    openai                              gpt-5-nano            responses
#> 14    openai                                      o3            responses
#> 15    openai                                 gpt-4.1            responses
#> 16    openai                                 gpt-4.1     chat.completions
#> 17    openai                            gpt-4.1-mini            responses
#> 18    openai                             gpt-4o-mini            responses
#> 19    openai                                  gpt-4o            responses
#> 20 anthropic                        claude-fable-5-1             messages
#> 21 anthropic                           claude-opus-5             messages
#> 22 anthropic                         claude-sonnet-5             messages
#> 23 anthropic               claude-haiku-4-5-20251001             messages
#> 24 anthropic               claude-haiku-4-5-20251001             messages
#> 25    gemini                        gemini-3.8-flash      generateContent
#> 26    gemini                        gemini-3.8-flash batchGenerateContent
#> 27    gemini                        gemini-3.7-flash      generateContent
#> 28    gemini                        gemini-3.7-flash batchGenerateContent
#> 29    gemini                        gemini-3.6-flash      generateContent
#> 30    gemini                        gemini-3.6-flash batchGenerateContent
#> 31    gemini                        gemini-3.5-flash      generateContent
#> 32    gemini                        gemini-3.5-flash batchGenerateContent
#> 33    gemini                   gemini-3.5-flash-lite      generateContent
#> 34    gemini                   gemini-3.5-flash-lite batchGenerateContent
#> 35    gemini                   gemini-3.1-flash-lite      generateContent
#> 36    gemini                   gemini-3.1-flash-lite batchGenerateContent
#> 37    gemini                  gemini-3.1-pro-preview      generateContent
#> 38    gemini                  gemini-3.1-pro-preview batchGenerateContent
#> 39    gemini                  gemini-3-flash-preview      generateContent
#> 40    gemini                  gemini-3-flash-preview batchGenerateContent
#> 41    gemini                        gemini-2.5-flash      generateContent
#> 42    gemini                        gemini-2.5-flash batchGenerateContent
#> 43    gemini                   gemini-2.5-flash-lite      generateContent
#> 44    gemini                   gemini-2.5-flash-lite batchGenerateContent
#> 45    gemini                          gemini-2.5-pro      generateContent
#> 46    gemini                          gemini-2.5-pro batchGenerateContent
#> 47    vertex                        gemini-3.8-flash      generateContent
#> 48    vertex                        gemini-3.7-flash      generateContent
#> 49    vertex                        gemini-3.6-flash      generateContent
#> 50    vertex                        gemini-3.5-flash      generateContent
#> 51    vertex                   gemini-3.5-flash-lite      generateContent
#> 52    vertex                   gemini-3.1-flash-lite      generateContent
#> 53    vertex                  gemini-3.1-pro-preview      generateContent
#> 54    vertex                  gemini-3-flash-preview      generateContent
#> 55    vertex                        gemini-2.5-flash      generateContent
#> 56    vertex                   gemini-2.5-flash-lite      generateContent
#> 57    vertex                          gemini-2.5-pro      generateContent
#> 58  together                thinkingmachines/Inkling     chat.completions
#> 59  together                    MiniMaxAI/MiniMax-M3     chat.completions
#> 60  together                  Qwen/Qwen3.8-2.4T-A95B     chat.completions
#> 61  together                        Qwen/Qwen3.7-Max     chat.completions
#> 62  together                       Qwen/Qwen3.6-Plus     chat.completions
#> 63  together                         Qwen/Qwen3.5-9B     chat.completions
#> 64  together                      moonshotai/Kimi-K3     chat.completions
#> 65  together                         zai-org/GLM-5.3     chat.completions
#> 66  together                   zai-org/GLM-5.3-Flash     chat.completions
#> 67  together                         zai-org/GLM-5.2     chat.completions
#> 68  together                     openai/gpt-oss-120b     chat.completions
#> 69  together      deepseek-ai/DeepSeek-V4-Flash-0731     chat.completions
#> 70  together        deepseek-ai/DeepSeek-V4-Pro-0813     chat.completions
#> 71  together meta-llama/Llama-3.3-70B-Instruct-Turbo     chat.completions
#> 72  together                       Qwen/Qwen3.7-Plus     chat.completions
#> 73  together             Prism-ML/Ternary-Bonsai-27B     chat.completions
#> 74  together            meta-models/Muse-Glimmer-30B     chat.completions
#> 75  together                      Qwen/Qwen3.8-Flash     chat.completions
#> 76    gemini                    gemini-3-pro-preview      generateContent
#> 77    openai                              gpt-6-luna            decisions
#> 78  typesafe                              jev-1.13.0            systemone
#>    live_tested batch_tested     reasoning_mode package_version  test_date
#> 1         TRUE         TRUE   provider-default           1.3.1 2026-09-05
#> 2         TRUE         TRUE   provider-default           1.3.1 2026-09-05
#> 3         TRUE         TRUE   provider-default           1.3.1 2026-09-05
#> 4         TRUE         TRUE   provider-default           1.3.1 2026-09-05
#> 5         TRUE         TRUE   provider-default           1.3.1 2026-09-05
#> 6         TRUE         TRUE   provider-default           1.3.1 2026-09-05
#> 7         TRUE         TRUE   provider-default           1.3.1 2026-09-05
#> 8         TRUE        FALSE   provider-default           1.3.1 2026-09-05
#> 9         TRUE         TRUE   provider-default           1.3.1 2026-09-05
#> 10        TRUE         TRUE   provider-default           1.3.1 2026-09-05
#> 11        TRUE         TRUE   provider-default           1.3.1 2026-09-05
#> 12        TRUE         TRUE   provider-default           1.3.1 2026-09-05
#> 13        TRUE         TRUE   provider-default           1.3.1 2026-09-05
#> 14        TRUE         TRUE   provider-default           1.3.1 2026-09-05
#> 15        TRUE         TRUE   provider-default           1.3.1 2026-09-05
#> 16        TRUE        FALSE   provider-default           1.3.1 2026-09-05
#> 17        TRUE         TRUE   provider-default           1.3.1 2026-09-05
#> 18        TRUE         TRUE   provider-default           1.3.1 2026-09-05
#> 19        TRUE         TRUE   provider-default           1.3.1 2026-09-05
#> 20        TRUE         TRUE               none           1.3.1 2026-09-05
#> 21        TRUE         TRUE               none           1.3.1 2026-09-05
#> 22        TRUE         TRUE               none           1.3.1 2026-09-05
#> 23        TRUE         TRUE               none           1.3.1 2026-09-05
#> 24        TRUE        FALSE            enabled           1.3.1 2026-09-05
#> 25        TRUE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 26       FALSE         TRUE thinking_level=low           1.3.1 2026-09-05
#> 27        TRUE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 28       FALSE         TRUE thinking_level=low           1.3.1 2026-09-05
#> 29        TRUE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 30       FALSE         TRUE thinking_level=low           1.3.1 2026-09-05
#> 31        TRUE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 32       FALSE         TRUE thinking_level=low           1.3.1 2026-09-05
#> 33        TRUE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 34       FALSE         TRUE thinking_level=low           1.3.1 2026-09-05
#> 35        TRUE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 36       FALSE         TRUE thinking_level=low           1.3.1 2026-09-05
#> 37        TRUE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 38       FALSE         TRUE thinking_level=low           1.3.1 2026-09-05
#> 39        TRUE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 40       FALSE         TRUE thinking_level=low           1.3.1 2026-09-05
#> 41       FALSE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 42       FALSE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 43       FALSE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 44       FALSE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 45       FALSE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 46       FALSE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 47        TRUE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 48        TRUE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 49        TRUE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 50        TRUE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 51        TRUE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 52        TRUE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 53        TRUE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 54        TRUE        FALSE thinking_level=low           1.3.1 2026-09-05
#> 55        TRUE        FALSE  thinking_budget=0           1.3.1 2026-09-05
#> 56        TRUE        FALSE  thinking_budget=0           1.3.1 2026-09-05
#> 57       FALSE        FALSE  thinking_budget=0           1.3.1 2026-09-05
#> 58        TRUE        FALSE               none           1.3.1 2026-09-05
#> 59        TRUE        FALSE               none           1.3.1 2026-09-05
#> 60       FALSE        FALSE               none           1.3.1 2026-09-05
#> 61       FALSE        FALSE               none           1.3.1 2026-09-05
#> 62       FALSE        FALSE               none           1.3.1 2026-09-05
#> 63        TRUE        FALSE               none           1.3.1 2026-09-05
#> 64        TRUE        FALSE               none           1.3.1 2026-09-05
#> 65        TRUE        FALSE               none           1.3.1 2026-09-05
#> 66        TRUE        FALSE               none           1.3.1 2026-09-05
#> 67        TRUE        FALSE               none           1.3.1 2026-09-05
#> 68        TRUE        FALSE               none           1.3.1 2026-09-05
#> 69        TRUE        FALSE               none           1.3.1 2026-09-05
#> 70        TRUE        FALSE               none           1.3.1 2026-09-05
#> 71        TRUE        FALSE               none           1.3.1 2026-09-05
#> 72       FALSE        FALSE               none           1.3.1 2026-09-05
#> 73        TRUE        FALSE               none           1.3.1 2026-09-05
#> 74        TRUE        FALSE               none           1.3.1 2026-09-05
#> 75       FALSE        FALSE               none           1.3.1 2026-09-05
#> 76       false        false thinking_level=low           1.3.1 2026-09-02
#> 77        TRUE        FALSE   provider-default           1.6.0 2026-10-16
#> 78        TRUE        FALSE   provider-default           1.6.0 2026-10-16
#>            status
#> 1  tested-current
#> 2  tested-current
#> 3  tested-current
#> 4  tested-current
#> 5  tested-current
#> 6  tested-current
#> 7  tested-current
#> 8  tested-current
#> 9  tested-current
#> 10 tested-current
#> 11 tested-current
#> 12 tested-current
#> 13 tested-current
#> 14 tested-current
#> 15 tested-current
#> 16 tested-current
#> 17 tested-current
#> 18 tested-current
#> 19 tested-current
#> 20 tested-current
#> 21 tested-current
#> 22 tested-current
#> 23 tested-current
#> 24 tested-current
#> 25 tested-current
#> 26 tested-current
#> 27 tested-current
#> 28 tested-current
#> 29 tested-current
#> 30 tested-current
#> 31 tested-current
#> 32 tested-current
#> 33 tested-current
#> 34 tested-current
#> 35 tested-current
#> 36 tested-current
#> 37 tested-current
#> 38 tested-current
#> 39 tested-current
#> 40 tested-current
#> 41     unverified
#> 42     unverified
#> 43     unverified
#> 44     unverified
#> 45     unverified
#> 46     unverified
#> 47 tested-current
#> 48 tested-current
#> 49 tested-current
#> 50 tested-current
#> 51 tested-current
#> 52 tested-current
#> 53 tested-current
#> 54 tested-current
#> 55 tested-current
#> 56 tested-current
#> 57     unverified
#> 58 tested-current
#> 59 tested-current
#> 60     unverified
#> 61     unverified
#> 62     unverified
#> 63 tested-current
#> 64 tested-current
#> 65 tested-current
#> 66 tested-current
#> 67 tested-current
#> 68 tested-current
#> 69 tested-current
#> 70 tested-current
#> 71 tested-current
#> 72     unverified
#> 73 tested-current
#> 74 tested-current
#> 75     unverified
#> 76        retired
#> 77 tested-current
#> 78 tested-current
```

Registry statuses mean:

- `tested-current`: the recorded configuration passed on the stated date
  and was current in the provider catalog when checked.
- `tested-legacy`: a successful dated test whose identifier is now
  legacy.
- `retired`: the provider has retired the identifier; it is retained
  only as historical evidence.
- `unverified`: availability or a partial response was observed, but no
  successful parsed package result was established.

### Optional future checks

The opt-in `inst/scripts/smoke_model_compatibility.R` harness can
re-test selected live or batch configurations and write a dated result
artifact. It makes billable network calls and is intentionally excluded
from the package test suite. For example:

``` text
PAIRWISELLM_RUN_PROVIDER_SMOKE=true \
  Rscript inst/scripts/smoke_model_compatibility.R \
  --mode=live --providers=openai,anthropic
```

Selected providers must have their recorded API-key environment
variables set; otherwise the harness records `skipped-no-key` and exits
unsuccessfully. Use `--allow-missing-keys=true` only when those skipped
rows are intentional.

Claude 5 reasoning is not advertised here: that family uses
adaptive-thinking semantics that this package does not currently
implement and test. Likewise, Ollama names such as `qwen3:32b`,
`gemma3:27b`, and `mistral-small3.2:24b` are only dated,
environment-dependent local examples, not claims about what another
Ollama installation provides.

## Decision-model live qualification

On **2026-10-16**, an owner-authorized live smoke run used pairwiseLLM
1.6.0, PR \#343 HEAD `8a220cb2a6ebdf55faed4d1712ce4e23c2b2dd79`:

| Provider | Exact requested model | Live endpoint                      |
|----------|-----------------------|------------------------------------|
| OpenAI   | `gpt-6-luna`          | `decisions` (`POST /v1/decisions`) |
| TypeSafe | `jev-1.13.0`          | `systemone` (`POST /v1/systemone`) |

Both used the bundled **20 synthetic writing samples**, Overall Quality,
the same **60 primary pairs** (each item appeared in six pairs), and
**10 reversed presentation audits per provider**, plus a separate
single-pair preflight for each. No provider-managed Batch jobs were
used. Both preflights succeeded; both providers returned 60/60 valid
primary decisions and all 10 reverse audits. All 120 primary SAMPLE_1
probabilities were finite and within \[0,1\], selected winners agreed
with their probabilities and belonged to their submitted pairs, and all
20 items had finite Firth-adjusted BT estimates for each provider.

Primary winners agreed on 53/60 pairs (88.33%; kappa 0.7619). The two
sets of BT scores had Pearson correlation 0.9211 and Spearman
correlation 0.9053. Winner agreement under reversed presentation was
7/10 for OpenAI and 5/10 for TypeSafe. These are descriptive results
from a small synthetic pilot. The reversal results raise a
position-consistency concern for further study; they do not establish an
implementation defect or that the providers are interchangeable.
Provider probabilities have not been shown to be calibrated, and this
run does not validate educational scoring or replace BT inference.

The live evidence establishes compatibility and end-to-end completion
for these configurations on that date. Its aggregate summaries do not
establish exact transmission counts, every outbound JSON field,
interruption/retry behavior, lock exclusion, or zero new sends on
restart. Those implementation paths are qualified separately with
provider-free regression tests. OpenAI Decisions currently accepts only
`gpt-6-luna` in this package. Other Jev versions and the moving
`jev-latest`/`jev-preview` aliases are accepted but were not tested
here; record requested and reported model versions, and do not assume
aliases freeze model weights or future availability.

### Reproduce the design without sharing private data

Prepare the bundled samples locally, select a connected degree-six graph
with 60 unique primary pairs, freeze the same oriented pair table for
both providers, and select 10 of those pairs for reversed audits with
distinct occurrence IDs. Use Overall Quality and the default decision
instructions. This offline example builds a connected degree-six
schedule and ten reverse audits, without calls:

``` r

data("example_writing_samples", package = "pairwiseLLM")
s <- example_writing_samples
edges <- expand.grid(i = seq_len(nrow(s)), offset = 1:3)
j <- (edges$i - 1L + edges$offset) %% nrow(s) + 1L
primary <- data.frame(
  ID1 = s$ID[edges$i], text1 = s$text[edges$i], ID2 = s$ID[j], text2 = s$text[j],
  pair_uid = paste0("primary-", seq_len(nrow(edges)))
)
audit <- primary[1:10, c("ID2", "text2", "ID1", "text1")]
names(audit) <- c("ID1", "text1", "ID2", "text2")
audit$pair_uid <- paste0("reverse-", seq_len(nrow(audit)))
c(samples = nrow(s), primary_pairs = nrow(primary), reverse_audits = nrow(audit))
#>        samples  primary_pairs reverse_audits 
#>             20             60             10
```

This reproduces the pilot design, not necessarily its exact original
schedule. After separate owner authorization, preflight one pair per
provider, collect the primary and reverse work into separate saved runs,
and retain their checkpoints. Inspect completeness, ID mappings,
probability validity, binary BT output and reversal consistency
separately. Record date, package commit, requested/reported models,
response IDs, usage and actual cost when available. Keep sample text,
credentials, raw responses and complete returned objects private;
publish only appropriate aggregate summaries. No additional live run is
required to use the dated evidence above.

A future tiny compatibility check can select the two decision profiles
in the existing smoke harness. First inspect the selection without
calls:

``` text
Rscript inst/scripts/smoke_model_compatibility.R --list=true --mode=live \
  --providers=openai,typesafe \
  --profiles=openai_decisions_default,typesafe_systemone_default
```

Running without `--list=true` additionally requires the explicit
billable opt-in shown above and separate owner approval. This one-pair
harness does not reproduce the 20-sample pilot or its reversal analysis.

## Citation

> Mercer, S. H. (2026). *Backends and tested model configurations* \[R
> package vignette\]. Comprehensive R Archive Network.
> <https://doi.org/10.32614/CRAN.package.pairwiseLLM>
