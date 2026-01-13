#' Backend-agnostic live comparison for a single pair of samples
#'
#' `llm_compare_pair()` is a thin wrapper around backend-specific comparison
#' functions. It currently supports the `"openai"`, `"anthropic"`, `"gemini"`,
#' `"together"`, and `"ollama"` backends and forwards the call to the
#' appropriate live comparison helper:
#' \itemize{
#'   \item `"openai"`   → [openai_compare_pair_live()]
#'   \item `"anthropic"` → [anthropic_compare_pair_live()]
#'   \item `"gemini"`   → [gemini_compare_pair_live()]
#'   \item `"together"`  → [together_compare_pair_live()]
#'   \item `"ollama"`   → [ollama_compare_pair_live()]
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
#' (\code{"responses"}) is used. For the `"anthropic"`, `"gemini"`, and
#' `"ollama"` backends, \code{endpoint} is currently ignored and the default
#' live API for that provider is used.
#'
#' @param ID1 Character ID for the first sample.
#' @param text1 Character string containing the first sample's text.
#' @param ID2 Character ID for the second sample.
#' @param text2 Character string containing the second sample's text.
#' @param model Model identifier for the chosen backend. For `"openai"` this
#'   should be an OpenAI model name (for example `"gpt-4.1"`, `"gpt-5.1"`).
#'   For `"anthropic"` and `"gemini"`, use the corresponding provider model
#'   names (for example `"claude-4-5-sonnet"` or
#'   `"gemini-3-pro-preview"`). For "together", use Together.ai model identifiers
#'   such as `"deepseek-ai/DeepSeek-R1"` or `"deepseek-ai/DeepSeek-V3"`. For
#'   `"ollama"`, use a local model name known to the Ollama server (for example
#'    `"mistral-small3.2:24b"`, `"qwen3:32b"`, `"gemma3:27b"`).
#' @param trait_name Short label for the trait (for example
#'   `"Overall Quality"`).
#' @param trait_description Full-text definition of the trait.
#' @param prompt_template Prompt template string, typically from
#'   [set_prompt_template()].
#' @param backend Character scalar indicating which LLM provider to use.
#'   One of `"openai"`, `"anthropic"`, `"gemini"`, `"together"`, or `"ollama"`.
#' @param endpoint Character scalar specifying which endpoint family to use
#'   for backends that support multiple live APIs. For the `"openai"` backend
#'   this must be one of `"chat.completions"` or `"responses"`, matching
#'   [openai_compare_pair_live()]. For `"anthropic"`, `"gemini"`, and
#'   `"ollama"`, this argument is currently ignored.
#' @param api_key Optional API key for the selected backend. If `NULL`, the
#'   backend-specific helper will use its own default environment variable
#'   (for example `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `GEMINI_API_KEY`,
#'   `TOGETHER_API_KEY`). For `"ollama"`, this argument is ignored (no API key
#'   is required for local inference).
#' @param include_raw Logical; if `TRUE`, the returned tibble includes a
#'   `raw_response` list-column with the parsed JSON body (or `NULL` on parse
#'   failure). Support for this may vary across backends.
#' @param ... Additional backend-specific parameters. For `"openai"` these
#'   are passed on to [openai_compare_pair_live()] and typically include
#'   arguments such as `temperature`, `top_p`, `logprobs`, `reasoning`, and
#'   `include_thoughts`. For `"anthropic"` and `"gemini"` they are forwarded to
#'   the corresponding live helper and may include parameters such as
#'   `reasoning`, `include_thoughts`, `max_output_tokens`, or
#'   provider-specific options. For `"ollama"`, arguments are forwarded to
#'   [ollama_compare_pair_live()] and may include `host`, `think`,
#'   `num_ctx`, and other Ollama-specific controls.
#'
#' @return A tibble with one row and the same columns as the underlying
#'   backend-specific live helper (for example [openai_compare_pair_live()]
#'   for `"openai"`). All backends are intended to return a compatible
#'   structure including `thoughts`, `content`, and token counts.
#'
#' @examples
#' \dontrun{
#' # Requires an API key for the chosen cloud backend. For OpenAI, set
#' # OPENAI_API_KEY in your environment. Running these examples will incur
#' # API usage costs.
#' #
#' # For local Ollama use, an Ollama server must be running and the models
#' # must be pulled in advance. No API key is required for the `"ollama"`
#' # backend.
#'
#' data("example_writing_samples", package = "pairwiseLLM")
#' samples <- example_writing_samples[1:2, ]
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' # Single live comparison using the OpenAI backend and chat.completions
#' res_live <- llm_compare_pair(
#'   ID1               = samples$ID[1],
#'   text1             = samples$text[1],
#'   ID2               = samples$ID[2],
#'   text2             = samples$text[2],
#'   model             = "gpt-4.1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   backend           = "openai",
#'   endpoint          = "chat.completions",
#'   temperature       = 0
#' )
#'
#' res_live$better_id
#'
#' # Using the OpenAI responses endpoint with gpt-5.1 and reasoning = "low"
#' res_live_gpt5 <- llm_compare_pair(
#'   ID1               = samples$ID[1],
#'   text1             = samples$text[1],
#'   ID2               = samples$ID[2],
#'   text2             = samples$text[2],
#'   model             = "gpt-5.1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   backend           = "openai",
#'   endpoint          = "responses",
#'   reasoning         = "low",
#'   include_thoughts  = TRUE,
#'   temperature       = NULL,
#'   top_p             = NULL,
#'   logprobs          = NULL,
#'   include_raw       = TRUE
#' )
#'
#' str(res_live_gpt5$raw_response[[1]], max.level = 2)
#'
#' # Example: single live comparison using a local Ollama backend
#' res_ollama <- llm_compare_pair(
#'   ID1 = samples$ID[1],
#'   text1 = samples$text[1],
#'   ID2 = samples$ID[2],
#'   text2 = samples$text[2],
#'   model = "mistral-small3.2:24b",
#'   trait_name = td$name,
#'   trait_description = td$description,
#'   prompt_template = tmpl,
#'   backend = "ollama",
#'   host = getOption(
#'     "pairwiseLLM.ollama_host",
#'     "http://127.0.0.1:11434"
#'   ),
#'   think = FALSE
#' )
#'
#' res_ollama$better_id
#' }
#'
#' @seealso
#' * [openai_compare_pair_live()], [anthropic_compare_pair_live()],
#'   [gemini_compare_pair_live()], [together_compare_pair_live()], and
#'   [ollama_compare_pair_live()] for backend-specific implementations.
#' * [submit_llm_pairs()] for row-wise comparisons over a tibble of pairs.
#' * [build_bt_data()] and [fit_bt_model()] for Bradley–Terry modelling of
#'   comparison results.
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
  ...
) {
  backend <- match.arg(backend)

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
#' `text2`), submits each pair to the selected backend, and aggregates the results.
#'
#' This function supports parallel processing, incremental saving, and resume
#' capability for the `"openai"`, `"anthropic"`, `"gemini"`, and `"together"`
#' backends.
#'
#' At present, the following backends are implemented:
#' \itemize{
#'   \item `"openai"`   → [submit_openai_pairs_live()]
#'   \item `"anthropic"` → [submit_anthropic_pairs_live()]
#'   \item `"gemini"`   → [submit_gemini_pairs_live()]
#'   \item `"together"`  → [submit_together_pairs_live()]
#'   \item `"ollama"`   → [submit_ollama_pairs_live()]
#' }
#'
#' @param pairs Tibble or data frame with at least columns `ID1`, `text1`,
#'   `ID2`, `text2`. Typically created by [make_pairs()], [sample_pairs()], and
#'   [randomize_pair_order()].
#' @param model Model identifier for the chosen backend. For `"openai"` this
#'   should be an OpenAI model name (for example `"gpt-4.1"`, `"gpt-5.1"`).
#'   For `"anthropic"` and `"gemini"`, use the corresponding provider model
#'   names (for example `"claude-4-5-sonnet"` or
#'   `"gemini-3-pro-preview"`). For "together", use Together.ai model identifiers
#'   such as `"deepseek-ai/DeepSeek-R1"` or `"deepseek-ai/DeepSeek-V3"`. For
#'   `"ollama"`, use a local model name known to the Ollama server (for example
#'    `"mistral-small3.2:24b"`, `"qwen3:32b"`, `"gemma3:27b"`).
#' @param trait_name Trait name to pass through to the backend-specific
#'   comparison function (for example `"Overall Quality"`).
#' @param trait_description Full-text trait description passed to the backend.
#' @param prompt_template Prompt template string, typically from
#'   [set_prompt_template()].
#' @param backend Character scalar indicating which LLM provider to use.
#'   One of `"openai"`, `"anthropic"`, `"gemini"`, `"together"`, or `"ollama"`.
#' @param endpoint Character scalar specifying which endpoint family to use for
#'   backends that support multiple live APIs. For the `"openai"` backend this
#'   must be one of `"chat.completions"` or `"responses"`, matching
#'   [submit_openai_pairs_live()]. For `"anthropic"`, `"gemini"`, `"together"`,
#'   and `"ollama"`, this is currently ignored.
#' @param api_key Optional API key for the selected backend. If `NULL`, the
#'   backend-specific helper will use its own default environment variable.
#'   For `"ollama"`, this argument is ignored (no API key is required for
#'   local inference).
#' @param verbose Logical; if `TRUE`, prints status, timing, and result
#'   summaries (for backends that support it).
#' @param status_every Integer; print status and timing for every
#'   `status_every`-th pair. Defaults to 1 (every pair). Errors are always
#'   printed.
#' @param progress Logical; if `TRUE`, shows a textual progress bar for
#'   backends that support it.
#' @param include_raw Logical; if `TRUE`, each row of the returned tibble will
#'   include a `raw_response` list-column with the parsed JSON body from the
#'   backend (for backends that support this).
#' @param save_path Character string; optional file path (e.g., "output.csv")
#'   to save results incrementally. If the file exists, the function reads it
#'   to identify and skip pairs that have already been processed (resume mode).
#'   Supported by `"openai"`, `"anthropic"`, `"gemini"`, and `"together"`.
#' @param parallel Logical; if `TRUE`, enables parallel processing using
#'   \code{future.apply}. Requires the \code{future} package. Supported by
#'   `"openai"`, `"anthropic"`, `"gemini"`, and `"together"`.
#' @param workers Integer; the number of parallel workers (threads) to use if
#'   \code{parallel = TRUE}. Defaults to 1.
#' @param ... Additional backend-specific parameters. For `"openai"` these
#'   are forwarded to [submit_openai_pairs_live()] and typically include
#'   `temperature`, `top_p`, `logprobs`, `reasoning`, and `include_thoughts`.
#'   For `"anthropic"` and `"gemini"`, they are forwarded to
#'   [submit_anthropic_pairs_live()] or [submit_gemini_pairs_live()] and
#'   may include options such as `max_output_tokens`, `include_thoughts`, and
#'   provider-specific controls. For `"ollama"`, arguments are forwarded to
#'   [submit_ollama_pairs_live()] and may include `host`, `think`,
#'   `num_ctx`, and other Ollama-specific options.
#'
#' @return A list containing:
#' \describe{
#'   \item{results}{A tibble with one row per successfully processed pair.}
#'   \item{failed_pairs}{A tibble containing rows that failed to process (for
#'     supported backends).}
#' }
#' Note: The `"ollama"` backend currently returns a single tibble of results
#' (failures may throw errors or appear as NA rows depending on implementation).
#'
#' @examples
#' \dontrun{
#' # Requires an API key for the chosen cloud backend.
#'
#' data("example_writing_samples", package = "pairwiseLLM")
#'
#' pairs <- example_writing_samples |>
#'   make_pairs() |>
#'   sample_pairs(n_pairs = 5, seed = 123) |>
#'   randomize_pair_order(seed = 456)
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' # Parallel execution with OpenAI (requires future package)
#' res_live <- submit_llm_pairs(
#'   pairs             = pairs,
#'   model             = "gpt-4.1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   backend           = "openai",
#'   endpoint          = "chat.completions",
#'   parallel          = TRUE,
#'   workers           = 4,
#'   save_path         = "results_openai.csv"
#' )
#'
#' # Check results
#' head(res_live$results)
#'
#' # Live comparisons using a local Ollama backend
#' res_ollama <- submit_llm_pairs(
#'   pairs             = pairs,
#'   model             = "mistral-small3.2:24b",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   backend           = "ollama",
#'   verbose           = TRUE
#' )
#'
#' res_ollama$better_id
#' }
#'
#' @seealso
#' * [submit_openai_pairs_live()], [submit_anthropic_pairs_live()],
#'   [submit_gemini_pairs_live()], [submit_together_pairs_live()], and
#'   [submit_ollama_pairs_live()] for backend-specific implementations.
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
  save_path = NULL,
  parallel = FALSE,
  workers = 1,
  ...
) {
  backend <- match.arg(backend)

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
        save_path         = save_path,
        parallel          = parallel,
        workers           = workers,
        ...
      )
    )
  }

  if (backend == "ollama") {
    # Note: 'ollama' backend has not been updated with save_path/parallel args yet.
    # They are excluded from this call to avoid errors or warnings in '...'
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
#' `text2`), submits each pair to the selected backend, and aggregates the results.
#'
#' This function supports parallel processing, incremental saving, and resume
#' capability for the `"openai"`, `"anthropic"`, `"gemini"`, `"together"`,
#' and `"ollama"` backends.
#'
#' At present, the following backends are implemented:
#' \itemize{
#'   \item `"openai"`   → [submit_openai_pairs_live()]
#'   \item `"anthropic"` → [submit_anthropic_pairs_live()]
#'   \item `"gemini"`   → [submit_gemini_pairs_live()]
#'   \item `"together"`  → [submit_together_pairs_live()]
#'   \item `"ollama"`   → [submit_ollama_pairs_live()]
#' }
#'
#' @param pairs Tibble or data frame with at least columns `ID1`, `text1`,
#'   `ID2`, `text2`. Typically created by [make_pairs()], [sample_pairs()], and
#'   [randomize_pair_order()].
#' @param model Model identifier for the chosen backend. For `"openai"` this
#'   should be an OpenAI model name (for example `"gpt-4.1"`, `"gpt-5.1"`).
#'   For `"anthropic"` and `"gemini"`, use the corresponding provider model
#'   names (for example `"claude-4-5-sonnet"` or
#'   `"gemini-3-pro-preview"`). For "together", use Together.ai model identifiers
#'   such as `"deepseek-ai/DeepSeek-R1"` or `"deepseek-ai/DeepSeek-V3"`. For
#'   `"ollama"`, use a local model name known to the Ollama server (for example
#'    `"mistral-small3.2:24b"`, `"qwen3:32b"`, `"gemma3:27b"`).
#' @param trait_name Trait name to pass through to the backend-specific
#'   comparison function (for example `"Overall Quality"`).
#' @param trait_description Full-text trait description passed to the backend.
#' @param prompt_template Prompt template string, typically from
#'   [set_prompt_template()].
#' @param backend Character scalar indicating which LLM provider to use.
#'   One of `"openai"`, `"anthropic"`, `"gemini"`, `"together"`, or `"ollama"`.
#' @param endpoint Character scalar specifying which endpoint family to use for
#'   backends that support multiple live APIs. For the `"openai"` backend this
#'   must be one of `"chat.completions"` or `"responses"`, matching
#'   [submit_openai_pairs_live()]. For `"anthropic"`, `"gemini"`, `"together"`,
#'   and `"ollama"`, this is currently ignored.
#' @param api_key Optional API key for the selected backend. If `NULL`, the
#'   backend-specific helper will use its own default environment variable.
#'   For `"ollama"`, this argument is ignored (no API key is required for
#'   local inference).
#' @param verbose Logical; if `TRUE`, prints status, timing, and result
#'   summaries (for backends that support it).
#' @param status_every Integer; print status and timing for every
#'   `status_every`-th pair. Defaults to 1 (every pair). Errors are always
#'   printed.
#' @param progress Logical; if `TRUE`, shows a textual progress bar for
#'   backends that support it.
#' @param include_raw Logical; if `TRUE`, each row of the returned tibble will
#'   include a `raw_response` list-column with the parsed JSON body from the
#'   backend (for backends that support this).
#' @param save_path Character string; optional file path (e.g., "output.csv")
#'   to save results incrementally. If the file exists, the function reads it
#'   to identify and skip pairs that have already been processed (resume mode).
#'   Supported by all backends.
#' @param parallel Logical; if `TRUE`, enables parallel processing using
#'   \code{future.apply}. Requires the \code{future} package. Supported by
#'   all backends (though defaults may vary).
#' @param workers Integer; the number of parallel workers (threads) to use if
#'   \code{parallel = TRUE}. Defaults to 1.
#' @param ... Additional backend-specific parameters. For `"openai"` these
#'   are forwarded to [submit_openai_pairs_live()] and typically include
#'   `temperature`, `top_p`, `logprobs`, `reasoning`, and `include_thoughts`.
#'   For `"anthropic"` and `"gemini"`, they are forwarded to
#'   [submit_anthropic_pairs_live()] or [submit_gemini_pairs_live()] and
#'   may include options such as `max_output_tokens`, `include_thoughts`, and
#'   provider-specific controls. For `"ollama"`, arguments are forwarded to
#'   [submit_ollama_pairs_live()] and may include `host`, `think`,
#'   `num_ctx`, and other Ollama-specific options.
#'
#' @return A list containing:
#' \describe{
#'   \item{results}{A tibble with one row per successfully processed pair.}
#'   \item{failed_pairs}{A tibble containing rows that failed to process (for
#'     supported backends).}
#' }
#'
#' @examples
#' \dontrun{
#' # Requires an API key for the chosen cloud backend.
#'
#' data("example_writing_samples", package = "pairwiseLLM")
#'
#' pairs <- example_writing_samples |>
#'   make_pairs() |>
#'   sample_pairs(n_pairs = 5, seed = 123) |>
#'   randomize_pair_order(seed = 456)
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' # Parallel execution with OpenAI (requires future package)
#' res_live <- submit_llm_pairs(
#'   pairs             = pairs,
#'   model             = "gpt-4.1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   backend           = "openai",
#'   endpoint          = "chat.completions",
#'   parallel          = TRUE,
#'   workers           = 4,
#'   save_path         = "results_openai.csv"
#' )
#'
#' # Live comparisons using a local Ollama backend with incremental saving
#' res_ollama <- submit_llm_pairs(
#'   pairs             = pairs,
#'   model             = "mistral-small3.2:24b",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   backend           = "ollama",
#'   save_path         = "results_ollama.csv",
#'   verbose           = TRUE
#' )
#'
#' res_ollama$results
#' }
#'
#' @seealso
#' * [submit_openai_pairs_live()], [submit_anthropic_pairs_live()],
#'   [submit_gemini_pairs_live()], [submit_together_pairs_live()], and
#'   [submit_ollama_pairs_live()] for backend-specific implementations.
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
    save_path = NULL,
    parallel = FALSE,
    workers = 1,
    ...
) {
  backend <- match.arg(backend)

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
