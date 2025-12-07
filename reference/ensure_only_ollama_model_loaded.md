# Ensure only one Ollama model is loaded in memory

`ensure_only_ollama_model_loaded()` is a small convenience helper for
managing memory when working with large local models via Ollama. It
inspects the current set of active models using the `ollama ps` command
and attempts to unload any models that are not the one you specify.

## Usage

``` r
ensure_only_ollama_model_loaded(model, verbose = TRUE)
```

## Arguments

- model:

  Character scalar giving the Ollama model name that should remain
  loaded (for example `"mistral-small3.2:24b"`, `"qwen3:32b"`,
  `"gemma3:27b"`). All other models currently reported by `ollama ps`
  will be candidates for unloading.

- verbose:

  Logical; if `TRUE` (the default), the function prints informational
  messages about the models detected and any unload operations
  performed. If `FALSE`, the function runs quietly.

## Value

Invisibly returns a character vector containing the names of models that
were requested to be unloaded (i.e., those passed to `ollama stop`). If
no models were unloaded, an empty character vector is returned.

## Details

This can be useful when running multiple large models (for example
`"mistral-small3.2:24b"`, `"qwen3:32b"`, `"gemma3:27b"`) on a single
machine, where keeping all of them loaded simultaneously may exhaust GPU
or system memory.

The function is intentionally conservative:

- If the `ollama` command is not available on the system *or*
  `ollama ps` returns an error or empty output, no action is taken and a
  message is printed when `verbose = TRUE`.

- If no active models are reported, no action is taken.

- Only models with names different from `model` are passed to
  `ollama stop <name>`.

This helper is not called automatically by the package; it is intended
to be used programmatically in development scripts and ad hoc workflows
before running comparisons with
[`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md)
or
[`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md).

This function relies on the `ollama` command-line interface being
available on the system `PATH`. If the command cannot be executed or
returns a non-zero status code, the function will issue a message (when
`verbose = TRUE`) and return without making any changes.

The exact output format of `ollama ps` is treated as an implementation
detail: this helper assumes that the first non-empty line is a header
and that subsequent non-empty lines begin with the model name as the
first whitespace-separated field. If the format changes in a future
version of Ollama, parsing may fail and the function will simply fall
back to doing nothing.

Because `ollama stop` affects the global Ollama server state for the
current machine, you should only use this helper in environments where
you are comfortable unloading models that might be in use by other
processes.

## See also

- [`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md)
  for single-pair Ollama comparisons.

- [`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md)
  for row-wise Ollama comparisons across many pairs.

## Examples

``` r
if (FALSE) { # \dontrun{
# Keep only mistral-small3.2:24b loaded in Ollama, unloading any
# other active models that `ollama ps` reports.
ensure_only_ollama_model_loaded("mistral-small3.2:24b")

# Use before running a set of comparisons with the Ollama backend:
#
#   data("example_writing_samples", package = "pairwiseLLM")
#   pairs <- example_writing_samples |>
#     make_pairs() |>
#     sample_pairs(n_pairs = 10, seed = 123) |>
#     randomize_pair_order(seed = 456)
#
#   td   <- trait_description("overall_quality")
#   tmpl <- set_prompt_template()
#
#   ensure_only_ollama_model_loaded("qwen3:32b")
#
#   res <- submit_llm_pairs(
#     pairs             = pairs,
#     model             = "qwen3:32b",
#     trait_name        = td$name,
#     trait_description = td$description,
#     prompt_template   = tmpl,
#     backend           = "ollama",
#     think             = TRUE
#   )
} # }
```
