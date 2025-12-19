# Resume polling and download results for multiple batch jobs

This function takes the output of
[`llm_submit_pairs_multi_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_multi_batch.md)
(or a previously written registry CSV) and polls each batch until
completion, downloading and parsing results as they finish. It
implements a conservative polling loop with a configurable interval
between rounds and a small delay between individual jobs to reduce the
risk of API rate‑limit errors. The httr2 retry wrapper is still invoked
for each API call, so transient HTTP errors will be retried with
exponential back‑off.

## Usage

``` r
llm_resume_multi_batches(
  jobs = NULL,
  output_dir = NULL,
  interval_seconds = 60,
  per_job_delay = 2,
  write_results_csv = FALSE,
  keep_jsonl = TRUE,
  write_registry = FALSE,
  tag_prefix = "<BETTER_SAMPLE>",
  tag_suffix = "</BETTER_SAMPLE>",
  verbose = FALSE,
  write_combined_csv = FALSE,
  combined_csv_path = NULL,
  openai_max_retries = 3
)
```

## Arguments

- jobs:

  A list of job objects returned by
  [`llm_submit_pairs_multi_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_multi_batch.md).
  If `NULL`, a registry CSV is loaded from `output_dir` and converted
  into an internal jobs structure.

- output_dir:

  Directory containing the batch files and (optionally) the registry
  CSV. If `jobs` is `NULL`, this directory must be supplied so that the
  registry can be loaded. When `jobs` is provided and `output_dir` is
  `NULL`, the directory is inferred from the first job’s
  `batch_output_path`. When writing results CSVs or updating the
  registry, this directory is used.

- interval_seconds:

  Number of seconds to wait between rounds of polling unfinished
  batches. The default (`60`) mirrors the example in the advanced
  vignette.

- per_job_delay:

  Number of seconds to wait between polling individual jobs within a
  single round. A small delay (e.g. 2) can help prevent 429 (Too Many
  Requests) responses.

- write_results_csv:

  Logical; if `TRUE`, each batch’s parsed results are written to a CSV
  file (`csv_path`) in `output_dir` as soon as they are available. If
  `FALSE` (the default), results are kept in memory.

- keep_jsonl:

  Logical; if `FALSE`, the `.jsonl` input and output files will be
  deleted after the job results have been parsed. Defaults to `TRUE`.

- write_registry:

  Logical; if `TRUE`, a CSV registry of batch jobs will be written (or
  updated) at the end of polling. When reading jobs from a saved
  registry via `output_dir`, this argument can be used to control
  whether the registry is refreshed on disk as job statuses change.
  Defaults to `FALSE`. See
  [`llm_submit_pairs_multi_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_multi_batch.md)
  for additional details on the registry format.

- tag_prefix, tag_suffix:

  Character strings passed to
  [`parse_anthropic_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_anthropic_batch_output.md)
  and
  [`parse_gemini_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_gemini_batch_output.md).
  These tags mark the start and end of the “better” sample in each
  provider’s output. The defaults match those used in the vignette.

- verbose:

  Logical; if `TRUE`, prints progress messages during polling and result
  processing. Messages include the batch ID, provider, and current state
  on each polling round, as well as summary messages when combined
  results are written to disk. Defaults to `FALSE`.

- write_combined_csv:

  Logical; if `TRUE`, the combined results tibble returned by the
  function will also be written to a CSV file. The path to write this
  file is determined by `combined_csv_path`. Defaults to `FALSE`.

- combined_csv_path:

  Optional file path for the combined results CSV. If
  `write_combined_csv = TRUE` and `combined_csv_path` is `NULL`, the
  combined results will be written to
  `file.path(output_dir, "combined_results.csv")`. When a non‑NULL value
  is supplied, it is treated as an absolute path if it begins with “/”,
  “~/”, or a Windows drive letter (e.g. “C:”), or if it contains a
  directory component (i.e. `dirname(combined_csv_path) != "."`). In
  that case it will be used exactly as given. Otherwise the file name is
  assumed to be relative to `output_dir`. This argument is ignored when
  `write_combined_csv = FALSE`.

- openai_max_retries:

  Integer giving the maximum number of times to retry certain OpenAI API
  calls when a transient HTTP 5xx error occurs. In particular, when
  downloading batch output with
  [`openai_download_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/openai_download_batch_output.md),
  the function will attempt to fetch the output file up to
  `openai_max_retries` times if an `httr2_http_500` error is raised.
  Between retries the function sleeps for `per_job_delay` seconds. Set
  to a small positive value (e.g. 3) to automatically recover from
  occasional server errors. Defaults to 3.

## Value

A list with two elements: `jobs`, the updated jobs list with each
element containing parsed results and a `done` flag, and `combined`, a
tibble obtained by binding all completed results (`NULL` if no batches
completed). If `write_results_csv` is `TRUE`, the combined tibble is
still returned in memory. If `write_combined_csv` is `TRUE`, the
combined tibble is also written to a CSV file on disk (see
`combined_csv_path` for details) but is still returned in memory.

## Examples

``` r
# Continuing the example from llm_submit_pairs_multi_batch():
# After submitting multiple batches, resume polling and combine the results.
if (FALSE) { # \dontrun{
# Suppose `outdir` is the directory where batch files were written and
# `jobs` is the list of job metadata returned by llm_submit_pairs_multi_batch().

results <- llm_resume_multi_batches(
  jobs               = jobs,
  output_dir         = outdir,
  interval_seconds   = 60,
  per_job_delay      = 2,
  write_results_csv  = TRUE,
  keep_jsonl         = FALSE,
  write_registry     = TRUE,
  verbose            = TRUE,
  write_combined_csv = TRUE
)

# The combined results are available in the `combined` element
print(results$combined)
} # }
```
