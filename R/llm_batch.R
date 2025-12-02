#' Submit pairs to an LLM backend via batch API
#'
#' @description
#' `llm_submit_pairs_batch()` is a backend-agnostic front-end for running
#' provider batch pipelines (OpenAI, Anthropic, Gemini).
#'
#' It mirrors [submit_llm_pairs()] but uses the provider batch APIs under the
#' hood via `run_openai_batch_pipeline()`, `run_anthropic_batch_pipeline()`,
#' and `run_gemini_batch_pipeline()`.
#'
#' Currently, this function *synchronously* runs the full batch pipeline for
#' each backend (build requests, create batch, poll until complete, download
#' results, parse). The returned object contains both metadata and a normalized
#' `results` tibble. See [llm_download_batch_results()] to extract the results.
#'
#' @param pairs A data frame or tibble of pairs with columns `ID1`, `text1`,
#'   `ID2`, and `text2`. Additional columns are allowed and will be carried
#'   through where supported.
#' @param backend Character scalar; one of `"openai"`, `"anthropic"`, or
#'   `"gemini"`. Matching is case-insensitive.
#' @param model Character scalar model name to use for the batch job.
#' @param trait_name A short name for the trait being evaluated (e.g.
#'   `"overall_quality"`).
#' @param trait_description A human-readable description of the trait.
#' @param prompt_template A prompt template created by [set_prompt_template()]
#'   or a compatible character scalar.
#' @param include_thoughts Logical; whether to request and parse model
#'   "thoughts" (where supported).
#' @param include_raw Logical; whether to include raw provider responses in the
#'   result (where supported by backends).
#' @param ... Additional arguments passed through to the backend-specific
#'   `run_*_batch_pipeline()` functions. This can include provider-specific
#'   options such as temperature or batch configuration fields.
#'
#' @return
#' A list of class `"pairwiseLLM_batch"` containing at least:
#'
#' - `backend`: the backend identifier (`"openai"`, `"anthropic"`, `"gemini"`),
#' - `batch_input_path`: path to the JSONL request file (if applicable),
#' - `batch_output_path`: path to the JSONL output file (if applicable),
#' - `batch`: provider-specific batch object (e.g., job metadata),
#' - `results`: a tibble of parsed comparison results in the standard
#'   pairwiseLLM schema.
#'
#' Additional fields returned by the backend-specific pipeline functions are
#' preserved.
#'
#' @examples
#' \dontrun{
#' pairs <- make_pairs(c("A", "B", "C"))
#'
#' batch <- llm_submit_pairs_batch(
#'   pairs             = pairs,
#'   backend           = "openai",
#'   model             = "gpt-4o-mini",
#'   trait_name        = "overall_quality",
#'   trait_description = "Overall quality of the response.",
#'   prompt_template   = set_prompt_template(),
#'   include_thoughts  = FALSE
#' )
#'
#' res <- llm_download_batch_results(batch)
#' }
#'
#' @export
llm_submit_pairs_batch <- function(
    pairs,
    backend           = c("openai", "anthropic", "gemini"),
    model,
    trait_name,
    trait_description,
    prompt_template   = set_prompt_template(),
    include_thoughts  = FALSE,
    include_raw       = FALSE,
    ...
) {
  backend <- match.arg(tolower(backend), c("openai", "anthropic", "gemini"))

  # Basic validation, mirroring submit_llm_pairs() style
  if (!rlang::is_scalar_character(model) || !nzchar(model)) {
    rlang::abort("`model` must be a non-empty character scalar.")
  }

  required_cols <- c("ID1", "text1", "ID2", "text2")
  missing_cols <- setdiff(required_cols, names(pairs))
  if (length(missing_cols) > 0L) {
    rlang::abort(paste0(
      "`pairs` must contain columns: ",
      paste(required_cols, collapse = ", "),
      ". Missing: ",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Dispatch to backend-specific batch pipeline
  out <- switch(
    backend,
    openai = run_openai_batch_pipeline(
      pairs             = pairs,
      model             = model,
      trait_name        = trait_name,
      trait_description = trait_description,
      prompt_template   = prompt_template,
      include_thoughts  = include_thoughts,
      include_raw       = include_raw,
      ...
    ),
    anthropic = run_anthropic_batch_pipeline(
      pairs             = pairs,
      model             = model,
      trait_name        = trait_name,
      trait_description = trait_description,
      prompt_template   = prompt_template,
      include_thoughts  = include_thoughts,
      include_raw       = include_raw,
      ...
    ),
    gemini = run_gemini_batch_pipeline(
      pairs             = pairs,
      model             = model,
      trait_name        = trait_name,
      trait_description = trait_description,
      prompt_template   = prompt_template,
      include_thoughts  = include_thoughts,
      include_raw       = include_raw,
      ...
    )
  )

  # Normalize and tag result
  if (!is.list(out)) {
    rlang::abort("Backend batch pipeline did not return a list.")
  }

  out$backend <- backend

  class(out) <- unique(c("pairwiseLLM_batch", class(out)))

  out
}

#' Extract results from a pairwiseLLM batch object
#'
#' @description
#' Helper to extract the parsed results tibble from a batch object returned by
#' [llm_submit_pairs_batch()]. This is a thin wrapper around the `results`
#' element returned by backend-specific batch pipelines and is designed to be
#' forward-compatible with future, more asynchronous batch workflows.
#'
#' @param x An object returned by [llm_submit_pairs_batch()] (class
#'   `"pairwiseLLM_batch"`), or a compatible list that contains a `results`
#'   element.
#' @param ... Reserved for future use; currently ignored.
#'
#' @return A tibble containing batch comparison results in the standard
#' pairwiseLLM schema.
#'
#' @examples
#' \dontrun{
#' batch <- llm_submit_pairs_batch(...)
#' res   <- llm_download_batch_results(batch)
#' }
#'
#' @export
llm_download_batch_results <- function(x, ...) {
  if (inherits(x, "pairwiseLLM_batch")) {
    if (!"results" %in% names(x)) {
      rlang::abort("Batch object does not contain a `results` element.")
    }
    return(x$results)
  }

  # Allow for more flexible inputs later if needed
  if (is.list(x) && "results" %in% names(x)) {
    return(x$results)
  }

  rlang::abort(
    "Unsupported input to `llm_download_batch_results()`: ",
    "expected an object returned by `llm_submit_pairs_batch()` ",
    "or a list with a `results` element."
  )
}
