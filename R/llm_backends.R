#' Backend-agnostic live comparison for a single pair of samples
#'
#' `llm_compare_pair()` is a thin wrapper around backend-specific comparison
#' functions. At present it supports the `"openai"` backend and forwards the
#' call to [openai_compare_pair_live()]. Future backends (for example Anthropic
#' or Gemini) will be added behind the same interface.
#'
#' The return value has the same structure as [openai_compare_pair_live()],
#' making it easy to plug into downstream helpers such as [build_bt_data()] and
#' [fit_bt_model()].
#'
#' @param ID1 Character ID for the first sample.
#' @param text1 Character string containing the first sample's text.
#' @param ID2 Character ID for the second sample.
#' @param text2 Character string containing the second sample's text.
#' @param model Model identifier for the chosen backend. For the `"openai"`
#'   backend this should be an OpenAI model name (for example `"gpt-4.1"`,
#'   `"gpt-5.1"`).
#' @param trait_name Short label for the trait (for example `"Overall Quality"`).
#' @param trait_description Full-text definition of the trait.
#' @param prompt_template Prompt template string, typically from
#'   [set_prompt_template()].
#' @param backend Character scalar indicating which LLM provider to use.
#'   Currently only `"openai"` is implemented. Additional backends will be
#'   added in future versions.
#' @param endpoint Character scalar specifying which endpoint family to use for
#'   backends that support multiple live APIs. For the `"openai"` backend this
#'   must be one of `"chat.completions"` or `"responses"`, matching
#'   [openai_compare_pair_live()].
#' @param api_key Optional API key for the selected backend. For the `"openai"`
#'   backend this defaults to `Sys.getenv("OPENAI_API_KEY")`. If `NULL` or an
#'   empty string is supplied for the OpenAI backend, an error is raised.
#' @param include_raw Logical; if `TRUE`, the returned tibble includes a
#'   `raw_response` list-column with the parsed JSON body (or `NULL` on parse
#'   failure). Support for this may vary across backends.
#' @param ... Additional backend-specific parameters. For the `"openai"`
#'   backend these are passed on to [openai_compare_pair_live()] and typically
#'   include arguments such as `temperature`, `top_p`, `logprobs`, and
#'   `reasoning`. The same validation rules for gpt-5 models apply as in the
#'   OpenAI helpers.
#'
#' @return A tibble with one row and the same columns as
#'   [openai_compare_pair_live()] for the `"openai"` backend. Future backends
#'   will return tibbles with a compatible structure.
#'
#' @examples
#' \dontrun{
#' # Requires an API key for the chosen backend. For OpenAI, set
#' # OPENAI_API_KEY in your environment. Running this example will incur
#' # API usage costs.
#'
#' library(pairwiseLLM)
#'
#' data("example_writing_samples", package = "pairwiseLLM")
#' samples <- example_writing_samples[1:2, ]
#'
#' td   <- trait_description("overall_quality")
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
#'   temperature       = NULL,
#'   top_p             = NULL,
#'   logprobs          = NULL,
#'   include_raw       = TRUE
#' )
#'
#' str(res_live_gpt5$raw_response[[1]], max.level = 2)
#' }
#'
#' @seealso
#' * [openai_compare_pair_live()] for the underlying OpenAI implementation.
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
    backend         = c("openai"),
    endpoint        = c("chat.completions", "responses"),
    api_key         = Sys.getenv("OPENAI_API_KEY"),
    include_raw     = FALSE,
    ...
) {
  backend  <- match.arg(backend)
  endpoint <- match.arg(endpoint)

  if (backend == "openai") {
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

  stop(
    "Backend '", backend, "' is not implemented yet. ",
    "Currently only backend = \"openai\" is supported.",
    call. = FALSE
  )
}


#' Backend-agnostic live comparisons for a tibble of pairs
#'
#' `submit_llm_pairs()` is a backend-neutral wrapper around row-wise comparison
#' for multiple pairs. It takes a tibble of pairs (`ID1`, `text1`, `ID2`,
#' `text2`), submits each pair to the selected backend, and binds the results
#' into a single tibble.
#'
#' At present, only the `"openai"` backend is implemented and the function is a
#' thin wrapper around [submit_openai_pairs_live()], which itself calls
#' [openai_compare_pair_live()] row-wise.
#'
#' The output has the same columns as [openai_compare_pair_live()], with one row
#' per pair, making it easy to pass into [build_bt_data()] and [fit_bt_model()].
#'
#' @param pairs Tibble or data frame with at least columns `ID1`, `text1`,
#'   `ID2`, `text2`. Typically created by [make_pairs()], [sample_pairs()], and
#'   [randomize_pair_order()].
#' @param model Model identifier for the chosen backend. For the `"openai"`
#'   backend this should be an OpenAI model name (for example `"gpt-4.1"`,
#'   `"gpt-5.1"`).
#' @param trait_name Trait name to pass through to the backend-specific
#'   comparison function (for example `"Overall Quality"`).
#' @param trait_description Full-text trait description passed to the backend.
#' @param prompt_template Prompt template string, typically from
#'   [set_prompt_template()].
#' @param backend Character scalar indicating which LLM provider to use.
#'   Currently only `"openai"` is implemented. Additional backends will be
#'   added in future versions.
#' @param endpoint Character scalar specifying which endpoint family to use for
#'   backends that support multiple live APIs. For the `"openai"` backend this
#'   must be one of `"chat.completions"` or `"responses"`, matching
#'   [submit_openai_pairs_live()].
#' @param api_key Optional API key for the selected backend. For the `"openai"`
#'   backend this defaults to `Sys.getenv("OPENAI_API_KEY")`.
#' @param verbose Logical; if `TRUE`, prints status, timing, and result
#'   summaries. Support for this may vary across backends.
#' @param status_every Integer; print status and timing for every
#'   `status_every`-th pair. Defaults to 1 (every pair). Errors are always
#'   printed.
#' @param progress Logical; if `TRUE`, shows a textual progress bar for
#'   backends that support it.
#' @param include_raw Logical; if `TRUE`, each row of the returned tibble will
#'   include a `raw_response` list-column with the parsed JSON body from the
#'   backend (for backends that support this).
#' @param ... Additional backend-specific parameters. For the `"openai"`
#'   backend these are forwarded to [openai_compare_pair_live()] and typically
#'   include arguments such as `temperature`, `top_p`, `logprobs`, and
#'   `reasoning`.
#'
#' @return A tibble with one row per pair and the same columns as
#'   [openai_compare_pair_live()] for the `"openai"` backend. Future backends
#'   will return tibbles with a compatible structure.
#'
#' @examples
#' \dontrun{
#' # Requires an API key for the chosen backend. For OpenAI, set
#' # OPENAI_API_KEY in your environment. Running this example will incur
#' # API usage costs.
#'
#' library(pairwiseLLM)
#'
#' data("example_writing_samples", package = "pairwiseLLM")
#'
#' pairs <- example_writing_samples |>
#'   make_pairs() |>
#'   sample_pairs(n_pairs = 5, seed = 123) |>
#'   randomize_pair_order(seed = 456)
#'
#' td   <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' # Live comparisons for multiple pairs using the OpenAI backend
#' res_live <- submit_llm_pairs(
#'   pairs             = pairs,
#'   model             = "gpt-4.1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   backend           = "openai",
#'   endpoint          = "chat.completions",
#'   temperature       = 0,
#'   verbose           = TRUE,
#'   status_every      = 2,
#'   progress          = TRUE,
#'   include_raw       = FALSE
#' )
#'
#' res_live$better_id
#'
#' # Using gpt-5.1 with reasoning = "low" on the responses endpoint
#' res_live_gpt5 <- submit_llm_pairs(
#'   pairs             = pairs,
#'   model             = "gpt-5.1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   backend           = "openai",
#'   endpoint          = "responses",
#'   reasoning         = "low",
#'   temperature       = NULL,
#'   top_p             = NULL,
#'   logprobs          = NULL,
#'   verbose           = TRUE,
#'   status_every      = 3,
#'   progress          = TRUE,
#'   include_raw       = TRUE
#' )
#'
#' str(res_live_gpt5$raw_response[[1]], max.level = 2)
#' }
#'
#' @seealso
#' * [submit_openai_pairs_live()] for the underlying OpenAI implementation.
#' * [llm_compare_pair()] for single-pair comparisons.
#' * [build_bt_data()] and [fit_bt_model()] for Bradley–Terry modelling of
#'   comparison results.
#'
#' @export
submit_llm_pairs <- function(
    pairs,
    model,
    trait_name,
    trait_description,
    prompt_template = set_prompt_template(),
    backend         = c("openai"),
    endpoint        = c("chat.completions", "responses"),
    api_key         = Sys.getenv("OPENAI_API_KEY"),
    verbose         = TRUE,
    status_every    = 1,
    progress        = TRUE,
    include_raw     = FALSE,
    ...
) {
  backend  <- match.arg(backend)
  endpoint <- match.arg(endpoint)

  if (backend == "openai") {
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
        ...
      )
    )
  }

  stop(
    "Backend '", backend, "' is not implemented yet. ",
    "Currently only backend = \"openai\" is supported.",
    call. = FALSE
  )
}
