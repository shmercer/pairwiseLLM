# Full Bayesian BTL inference via CmdStanR (adaptive-compatible)

Runs full Bayesian posterior inference for a Bradley–Terry–Luce (BTL)
style model using the package’s CmdStan machinery, but in a standalone
(non-adaptive) context. The function is designed so downstream
diagnostics and reporting can reuse the existing adaptive summary tools
(notably
[`summarize_items()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_items.md)
and
[`summarize_refits()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_refits.md))
without requiring new summary functions.

## Usage

``` r
fit_bayes_btl_mcmc(
  results,
  ids,
  model_variant = "btl_e_b",
  cmdstan = list(iter_warmup = 1000, iter_sampling = 1000, seed = NULL, core_fraction =
    0.8),
  pair_counts = NULL,
  subset_method = c("first", "sample"),
  seed = NULL
)
```

## Arguments

- results:

  Canonical `results_tbl` with `A_id`, `B_id`, and `better_id` (plus the
  standard adaptive results columns). See `validate_results_tbl()` for
  required structure.

- ids:

  Character vector of all sample ids (length `N`).

- model_variant:

  Model variant label: `"btl"`, `"btl_e"`, `"btl_b"`, or `"btl_e_b"`.
  Defaults to `"btl_e_b"`.

- cmdstan:

  List of CmdStan settings. Common fields:

  chains

  :   Number of chains (defaults to `min(8, physical_cores)` via
      internal resolution).

  iter_warmup

  :   Warmup iterations (default `1000`).

  iter_sampling

  :   Sampling iterations (default `1000`).

  seed

  :   Optional integer seed forwarded to CmdStan (default `NULL`).

  core_fraction

  :   Fraction of physical cores for parallelization (default `0.8`).

  output_dir

  :   Optional directory for CmdStan output.

- pair_counts:

  Optional integer vector of subset sizes (e.g., `c(200, 500, 1000)`).
  When provided, the model is fit once per subset size and the round log
  contains one row per fit. If `NULL`, a single fit is run using all
  rows in `results`.

- subset_method:

  Subset strategy when `pair_counts` is provided: `"first"` (default)
  uses the first `n` rows of `results` for each refit; `"sample"` draws
  a random permutation once and then takes the first `n` rows of that
  permutation for each refit.

- seed:

  Optional integer seed for deterministic subset selection when
  `subset_method = "sample"`. When `NULL`, falls back to `cmdstan$seed`
  if provided.

## Value

A list with:

- item_log_list:

  List of item-log tables, one per refit, matching the canonical
  adaptive item log schema. This is the preferred structure for reuse
  with
  [`summarize_items()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_items.md).

- item_summary:

  A single tibble formed by row-binding `item_log_list` (kept for
  backward compatibility). Each row corresponds to an item within a
  refit; `refit_id` identifies the refit.

- round_log:

  Tibble matching the canonical adaptive round log schema (one row per
  refit).

- fits:

  List of BTL fit contracts (one per refit).

- fit:

  Single fit contract (only when one refit is run).

## Details

Internally, the function can optionally refit the model on increasing
subsets of the observed comparisons (via `pair_counts`). Each refit is
treated as a "refit" in the adaptive logging sense, producing:

- one round-log row per refit (compatible with `round_log_schema()`),

- one item-log table per refit (compatible with
  `.adaptive_item_log_schema()`).

## Examples

``` r
if (FALSE) { # \dontrun{
results <- tibble::tibble(
  pair_uid = "A:B#1",
  unordered_key = "A:B",
  ordered_key = "A:B",
  A_id = "A",
  B_id = "B",
  better_id = "A",
  winner_pos = 1L,
  phase = "phase2",
  iter = 1L,
  received_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
  backend = "openai",
  model = "gpt-test"
)

fit <- fit_bayes_btl_mcmc(
  results,
  ids = c("A", "B"),
  model_variant = "btl_e_b"
)

# Generate summaries
summarize_refits(fit)
summarize_items(fit)
} # }
```
