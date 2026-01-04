.normalize_backend_arg <- function(backend) {
  match.arg(
    backend,
    choices = c("openai", "anthropic", "gemini", "together", "ollama")
  )
}

#' Backend-agnostic live comparison for a single pair of samples
#'
#' `llm_compare_pair()` is a thin wrapper around backend-specific comparison
#' functions. It currently supports the `"openai"`, `"anthropic"`, `"gemini"`,
#' `"together"`, and `"ollama"` backends and forwards the call to the
#' appropriate live comparison helper:
#' \itemize{
#'   \item `"openai"`    → [openai_compare_pair_live()]
#'   \item `"anthropic"` → [anthropic_compare_pair_live()]
#'   \item `"gemini"`    → [gemini_compare_pair_live()]
#'   \item `"together"`  → [together_compare_pair_live()]
#'   \item `"ollama"`    → [ollama_compare_pair_live()]
#' }
#'
#' All backends are expected to return a tibble with a compatible structure,
#' including:
#' \itemize{
#'   \item \code{custom_id}, \code{ID1}, \code{ID2}
#'   \item \code{model}, \code{object_type}, \code{status_code},
#'         \code{error_message}
#'   \item \code{thoughts} (reasoning / thinking text when available)
#'   \item \code{content} (visible assistant output)
#'   \item \code{better_sample}, \code{better_id}
#'   \item \code{prompt_tokens}, \code{completion_tokens}, \code{total_tokens}
#' }
#'
#' For the `"openai"` backend, the \code{endpoint} argument controls whether
#' the Chat Completions API (\code{"chat.completions"}) or the Responses API
#' (\code{"responses"}) is used. For the `"anthropic"`, `"gemini"`, `"together"`,
#' and `"ollama"` backends, \code{endpoint} is currently ignored and the default
#' live API for that provider is used.
#'
#' Optional validation can be enabled via \code{validate}. When enabled, the
#' backend helper may compute a compact diagnostic report (for example, invalid
#' winner tokens) without interrupting pipelines. If \code{validate_strict = TRUE},
#' validation is enforced and invalid winners may error.
#'
#' @param ID1 Character ID for the first sample.
#' @param text1 Character string containing the first sample's text.
#' @param ID2 Character ID for the second sample.
#' @param text2 Character string containing the second sample's text.
#' @param model Model identifier for the chosen backend.
#' @param trait_name Short label for the trait (for example `"Overall Quality"`).
#' @param trait_description Full-text definition of the trait.
#' @param prompt_template Prompt template string, typically from
#'   [set_prompt_template()].
#' @param backend Character scalar indicating which LLM provider to use.
#'   One of `"openai"`, `"anthropic"`, `"gemini"`, `"together"`, or `"ollama"`.
#' @param endpoint Character scalar specifying which endpoint family to use
#'   for backends that support multiple live APIs. For the `"openai"` backend
#'   this must be one of `"chat.completions"` or `"responses"`. For `"anthropic"`,
#'   `"gemini"`, `"together"`, and `"ollama"`, this argument is currently ignored.
#' @param api_key Optional API key for the selected backend. If `NULL`, the
#'   backend-specific helper will use its own default environment variable
#'   (for example `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `GEMINI_API_KEY`,
#'   `TOGETHER_API_KEY`). For `"ollama"`, this argument is ignored.
#' @param include_raw Logical; if `TRUE`, the returned tibble includes a
#'   `raw_response` list-column when supported by the backend.
#' @param validate Logical; if `TRUE`, request validation/diagnostics from the
#'   backend helper (when supported).
#' @param validate_strict Logical; only used when `validate = TRUE`. If `TRUE`,
#'   enforce validity and error on invalid winners; if `FALSE`, validation is
#'   report-only.
#' @param ... Additional backend-specific parameters forwarded to the selected
#'   backend helper.
#'
#' @return A tibble with one row and the same columns as the underlying
#'   backend-specific live helper.
#'
#' @examples
#' \dontrun{
#' llm_compare_pair(
#'   ID1 = "A",
#'   text1 = "Response A...",
#'   ID2 = "B",
#'   text2 = "Response B...",
#'   model = "gpt-4o-mini",
#'   trait_name = "Overall quality",
#'   trait_description = "Which response is better overall?",
#'   backend = "openai"
#' )
#' }
#'
#' @export
llm_compare_pair <- function(
  ID1,
  text1,
  ID2,
  text2,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  backend = c("openai", "anthropic", "gemini", "together", "ollama"),
  endpoint = c("chat.completions", "responses"),
  api_key = NULL,
  include_raw = FALSE,
  validate = FALSE,
  validate_strict = FALSE,
  ...
) {
  backend <- .normalize_backend_arg(backend)

  # This branch is effectively unreachable under normal usage because
  # `.normalize_backend_arg()` uses `match.arg()`; we keep it as a defensive
  # guard and for targeted unit tests.
  allowed_backends <- c("openai", "anthropic", "gemini", "together", "ollama")
  if (!backend %in% allowed_backends) {
    stop("Backend `", backend, "` is not implemented yet.")
  }

  # Normalize empty-string keys to NULL
  if (!is.null(api_key) && identical(api_key, "")) {
    api_key <- NULL
  }

  if (backend == "openai") {
    endpoint <- match.arg(endpoint)

    return(
      openai_compare_pair_live(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        endpoint          = endpoint,
        api_key           = api_key,
        include_raw       = include_raw,
        validate          = validate,
        validate_strict   = validate_strict,
        ...
      )
    )
  }

  if (backend == "anthropic") {
    return(
      anthropic_compare_pair_live(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        include_raw       = include_raw,
        validate          = validate,
        validate_strict   = validate_strict,
        ...
      )
    )
  }

  if (backend == "gemini") {
    return(
      gemini_compare_pair_live(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        include_raw       = include_raw,
        validate          = validate,
        validate_strict   = validate_strict,
        ...
      )
    )
  }

  if (backend == "together") {
    return(
      together_compare_pair_live(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        include_raw       = include_raw,
        validate          = validate,
        validate_strict   = validate_strict,
        ...
      )
    )
  }

  if (backend == "ollama") {
    return(
      ollama_compare_pair_live(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        include_raw       = include_raw,
        validate          = validate,
        validate_strict   = validate_strict,
        ...
      )
    )
  }

  stop(
    "Backend '", backend, "' is not implemented yet. ",
    "Currently supported backends are: ",
    "\"openai\", \"anthropic\", \"gemini\", \"together\", and \"ollama\".",
    call. = FALSE
  )
}

#' Backend-agnostic live comparisons for a tibble of pairs
#'
#' `submit_llm_pairs()` is a backend-neutral wrapper around row-wise comparison
#' for multiple pairs. It takes a tibble of pairs (`ID1`, `text1`, `ID2`,
#' `text2`), submits each pair to the selected backend, and aggregates results.
#'
#' This function supports parallel processing and incremental saving/resume via
#' \code{save_path} for all currently supported backends.
#'
#' At present, the following backends are implemented:
#' \itemize{
#'   \item `"openai"`    → [submit_openai_pairs_live()]
#'   \item `"anthropic"` → [submit_anthropic_pairs_live()]
#'   \item `"gemini"`    → [submit_gemini_pairs_live()]
#'   \item `"together"`  → [submit_together_pairs_live()]
#'   \item `"ollama"`    → [submit_ollama_pairs_live()]
#' }
#'
#' @param pairs Tibble or data frame with at least columns `ID1`, `text1`,
#'   `ID2`, `text2`.
#' @param model Model identifier for the chosen backend.
#' @param trait_name Trait name to pass through to the backend-specific
#'   comparison function (for example `"Overall Quality"`).
#' @param trait_description Full-text trait description passed to the backend.
#' @param prompt_template Prompt template string, typically from
#'   [set_prompt_template()].
#' @param backend Character scalar indicating which LLM provider to use.
#' @param endpoint Character scalar specifying which endpoint family to use for
#'   backends that support multiple live APIs. For the `"openai"` backend this
#'   must be one of `"chat.completions"` or `"responses"`. For other backends,
#'   this argument is currently ignored.
#' @param api_key Optional API key for the selected backend. If `NULL`, the
#'   backend-specific helper will use its own default environment variable.
#'   For `"ollama"`, this argument is ignored.
#' @param verbose Logical; if `TRUE`, prints status updates.
#' @param status_every Integer; print status every `status_every` pairs.
#' @param progress Logical; if `TRUE`, shows a textual progress indicator when supported.
#' @param include_raw Logical; if `TRUE`, include a `raw_response` list-column when supported.
#' @param validate Logical; if `TRUE`, attach a compact `validation_report` to the
#'   returned list using [validate_backend_results()].
#' @param validate_strict Logical; only used when `validate = TRUE`. If `TRUE`,
#'   enforce validity (error on invalid winners); if `FALSE`, validation is report-only.
#' @param save_path Optional path to save results incrementally. If the file exists,
#'   the function will resume by skipping already processed pairs when supported.
#' @param parallel Logical; if `TRUE`, enable parallel processing (requires \pkg{future}).
#' @param workers Integer; number of parallel workers when `parallel = TRUE`.
#' @param ... Additional backend-specific parameters forwarded to the selected backend helper.
#'
#' @return A list containing:
#' \describe{
#'   \item{results}{A tibble with one row per successfully processed pair.}
#'   \item{failed_pairs}{A tibble of pairs that failed to process when supported.}
#'   \item{validation_report}{Present when `validate = TRUE`.}
#' }
#'
#' @examples
#' \dontrun{
#' pairs <- tibble::tibble(
#'   ID1 = c("A", "B"),
#'   text1 = c("Response A...", "Response B..."),
#'   ID2 = c("B", "A"),
#'   text2 = c("Response B...", "Response A...")
#' )
#'
#' out <- submit_llm_pairs(
#'   pairs = pairs,
#'   model = "gpt-4o-mini",
#'   trait_name = "Overall quality",
#'   trait_description = "Which response is better overall?",
#'   backend = "openai"
#' )
#' }
#'
#' @export
submit_llm_pairs <- function(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  backend = c("openai", "anthropic", "gemini", "together", "ollama"),
  endpoint = c("chat.completions", "responses"),
  api_key = NULL,
  verbose = TRUE,
  status_every = 1,
  progress = TRUE,
  include_raw = FALSE,
  validate = FALSE,
  validate_strict = FALSE,
  save_path = NULL,
  parallel = FALSE,
  workers = 1,
  ...
) {
  backend <- .normalize_backend_arg(backend)

  # Defensive guard (normally unreachable; see comment in `llm_compare_pair()`)
  allowed_backends <- c("openai", "anthropic", "gemini", "together", "ollama")
  if (!backend %in% allowed_backends) {
    stop("Backend `", backend, "` is not implemented yet.")
  }

  # Normalize empty-string keys to NULL
  if (!is.null(api_key) && identical(api_key, "")) {
    api_key <- NULL
  }

  if (backend == "openai") {
    endpoint <- match.arg(endpoint)

    return(
      submit_openai_pairs_live(
        pairs             = pairs,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        endpoint          = endpoint,
        api_key           = api_key,
        verbose           = verbose,
        status_every      = status_every,
        progress          = progress,
        include_raw       = include_raw,
        validate          = validate,
        validate_strict   = validate_strict,
        save_path         = save_path,
        parallel          = parallel,
        workers           = workers,
        ...
      )
    )
  }

  if (backend == "anthropic") {
    return(
      submit_anthropic_pairs_live(
        pairs             = pairs,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        verbose           = verbose,
        status_every      = status_every,
        progress          = progress,
        include_raw       = include_raw,
        validate          = validate,
        validate_strict   = validate_strict,
        save_path         = save_path,
        parallel          = parallel,
        workers           = workers,
        ...
      )
    )
  }

  if (backend == "gemini") {
    return(
      submit_gemini_pairs_live(
        pairs             = pairs,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        verbose           = verbose,
        status_every      = status_every,
        progress          = progress,
        include_raw       = include_raw,
        validate          = validate,
        validate_strict   = validate_strict,
        save_path         = save_path,
        parallel          = parallel,
        workers           = workers,
        ...
      )
    )
  }

  if (backend == "together") {
    return(
      submit_together_pairs_live(
        pairs             = pairs,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        verbose           = verbose,
        status_every      = status_every,
        progress          = progress,
        include_raw       = include_raw,
        validate          = validate,
        validate_strict   = validate_strict,
        save_path         = save_path,
        parallel          = parallel,
        workers           = workers,
        ...
      )
    )
  }

  if (backend == "ollama") {
    return(
      submit_ollama_pairs_live(
        pairs             = pairs,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        verbose           = verbose,
        status_every      = status_every,
        progress          = progress,
        include_raw       = include_raw,
        validate          = validate,
        validate_strict   = validate_strict,
        save_path         = save_path,
        parallel          = parallel,
        workers           = workers,
        ...
      )
    )
  }

  stop(
    "Backend '", backend, "' is not implemented yet. ",
    "Currently supported backends are: ",
    "\"openai\", \"anthropic\", \"gemini\", \"together\", and \"ollama\".",
    call. = FALSE
  )
}
