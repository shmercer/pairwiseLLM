# Multi‑batch submission and polling wrappers

These functions provide higher‑level wrappers around the existing
provider‑specific batch APIs in **pairwiseLLM**. They allow a large
tibble of pairwise comparisons to be automatically split into multiple
batch jobs, submitted concurrently (without polling), recorded in a
registry for safe resumption, and later polled until completion and
merged into a single results data frame. They do not modify any of the
underlying API functions such as
[`run_openai_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md)
or
[`run_anthropic_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_anthropic_batch_pipeline.md),
but orchestrate these calls to support resilient multi‑batch workflows.

## Usage

``` r
llm_submit_pairs_multi_batch(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  backend = c("openai", "anthropic", "gemini"),
  batch_size = NULL,
  n_segments = NULL,
  output_dir = tempfile("llm_multi_batch_"),
  write_registry = FALSE,
  keep_jsonl = TRUE,
  verbose = FALSE,
  ...,
  openai_max_retries = 3
)
```

## Arguments

- pairs:

  A tibble of pairs with columns `ID1`, `text1`, `ID2`, `text2`.
  Typically produced by
  [`make_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/make_pairs.md),
  [`sample_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_pairs.md),
  and
  [`randomize_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/randomize_pair_order.md).

- model:

  Model identifier for the chosen backend. Passed through to the
  corresponding `run_*_batch_pipeline()` function.

- trait_name, trait_description, prompt_template:

  Parameters forwarded to
  [`run_openai_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md),
  [`run_anthropic_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_anthropic_batch_pipeline.md),
  or
  [`run_gemini_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_gemini_batch_pipeline.md).
  See those functions for details.

- backend:

  One of `"openai"`, `"anthropic"`, or `"gemini"`. Determines which
  provider pipeline is used for each batch.

- batch_size:

  Integer giving the maximum number of pairs per batch. Exactly one of
  `batch_size` or `n_segments` must be supplied; if `batch_size` is
  supplied, the number of segments is computed as
  `ceiling(nrow(pairs) / batch_size)`. The final segment may contain
  fewer pairs than `batch_size`.

- n_segments:

  Integer giving the number of segments to create. Exactly one of
  `batch_size` or `n_segments` must be supplied; if `n_segments` is
  supplied, each segment contains approximately
  `nrow(pairs) / n_segments` pairs. The last segment may be smaller.

- output_dir:

  Directory in which to write all batch files, including the `.jsonl`
  input/output files, the optional registry CSV, and (if requested)
  parsed results CSVs. A temporary directory is created by default.

- write_registry:

  Logical; if `TRUE`, a CSV registry of batch jobs is written to
  `file.path(output_dir, "jobs_registry.csv")`. The registry can be
  reloaded with
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
  and passed to
  [`llm_resume_multi_batches()`](https://shmercer.github.io/pairwiseLLM/reference/llm_resume_multi_batches.md)
  for polling and resumption. If `FALSE`, the registry is returned in
  memory only.

- keep_jsonl:

  Logical; if `FALSE`, the `.jsonl` input and output files for each
  batch will be deleted after the job results have been parsed in
  [`llm_resume_multi_batches()`](https://shmercer.github.io/pairwiseLLM/reference/llm_resume_multi_batches.md).
  Since the provider APIs require file paths, the files are always
  created during submission; this option controls whether to retain them
  on disk after completion.

- verbose:

  Logical; if `TRUE`, prints progress messages during batch submission.
  Messages include the segment index, the number of pairs in each
  segment, the chosen provider, and confirmation that the batch has been
  created along with the input file path. Defaults to `FALSE`.

- ...:

  Additional arguments passed through to the provider‑specific
  `run_*_batch_pipeline()` function. These may include arguments such as
  `include_thoughts`, `reasoning`, `include_raw`, `temperature`, etc.

- openai_max_retries:

  Integer giving the maximum number of times to retry the initial OpenAI
  batch submission when a transient HTTP 5xx error occurs. When creating
  a segment on the OpenAI backend,
  [`run_openai_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md)
  internally uploads the JSONL file and creates the batch. On rare
  occasions this call can return a 500 error; specifying a positive
  value here (e.g. 3) will automatically retry the submission up to that
  many times. Between retries, the function sleeps for a brief period
  proportional to the current attempt. Defaults to 3.

## Value

A list with two elements: `jobs`, a list of per‑batch metadata (similar
to the example in the advanced vignette), and `registry`, a tibble
summarising all jobs. The `registry` contains columns `segment_index`,
`provider`, `model`, `batch_id`, `batch_input_path`,
`batch_output_path`, `csv_path`, `pairs_path`, `done`, and `results`
(initialized to `NULL`). If `write_registry` is `TRUE`, the tibble is
also written to disk as `jobs_registry.csv`.

## `llm_submit_pairs_multi_batch()`

Splits a tibble of comparison pairs into chunks and submits one batch
per chunk using the appropriate provider pipeline. Each batch is created
with `poll = FALSE`, so the function returns immediately after the batch
jobs have been created. Metadata for each batch—including the
`batch_id`, provider type, and input/output file paths—is collected and
(optionally) written to a CSV registry for later resumption.

## Examples

``` r
# Example: split a small set of pairs into five segments, submit
# them to the Gemini backend, and then poll and combine the results.
# Requires a funded API key and internet access.
if (FALSE) { # \dontrun{
# Construct ten random pairs from the example writing samples
set.seed(123)
pairs <- sample_pairs(example_writing_samples, n_pairs = 10)

# Directory to store batch files and results
outdir <- tempfile("multi_batch_example_")

# Submit the pairs in five batches.  We write the registry to disk
# and print progress messages as each batch is created.
job_info <- llm_submit_pairs_multi_batch(
  pairs             = pairs,
  model             = "gemini-3-pro-preview",
  trait_name        = "writing_quality",
  trait_description = "Which text shows better writing quality?",
  n_segments        = 5,
  output_dir        = outdir,
  write_registry    = TRUE,
  verbose           = TRUE
)

# Resume polling until all batches complete.  The per-batch and
# combined results are written to CSV files, the registry is
# refreshed on disk, and progress messages are printed.
results <- llm_resume_multi_batches(
  jobs               = job_info$jobs,
  output_dir         = outdir,
  interval_seconds   = 60,
  per_job_delay      = 2,
  write_results_csv  = TRUE,
  keep_jsonl         = FALSE,
  write_registry     = TRUE,
  verbose            = TRUE,
  write_combined_csv = TRUE
)

# Access the combined results tibble
head(results$combined)
} # }
```
