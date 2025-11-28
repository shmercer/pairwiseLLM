#' @importFrom httr2 resp_status
NULL

#' Live OpenAI comparison for a single pair of samples
#'
#' This function sends a single pairwise comparison prompt to the OpenAI API
#' and parses the result into a small tibble. It is the live/on-demand analogue
#' of \code{\link{build_openai_batch_requests}} + \code{\link{parse_openai_batch_output}}.
#'
#' It supports both the Chat Completions endpoint ("/v1/chat/completions") and
#' the Responses endpoint ("/v1/responses", e.g., for gpt-5.1 with reasoning),
#' using the same prompt template and model/parameter rules as the batch pipeline.
#'
#' @param ID1 Character ID for the first sample.
#' @param text1 Character string containing the first sample's text.
#' @param ID2 Character ID for the second sample.
#' @param text2 Character string containing the second sample's text.
#' @param model OpenAI model name (e.g. "gpt-4.1", "gpt-5.1").
#' @param trait_name Short label for the trait (e.g., "Overall Quality").
#' @param trait_description Full-text definition of the trait.
#' @param prompt_template Prompt template string, typically from
#'   \code{\link{set_prompt_template}}.
#' @param endpoint Which OpenAI endpoint to use. One of
#'   \code{"chat.completions"} or \code{"responses"}.
#' @param tag_prefix Prefix for the better-sample tag. Defaults to
#'   \code{"<BETTER_SAMPLE>"}.
#' @param tag_suffix Suffix for the better-sample tag. Defaults to
#'   \code{"</BETTER_SAMPLE>"}.
#' @param api_key Optional OpenAI API key. Defaults to
#'   \code{Sys.getenv("OPENAI_API_KEY")}.
#' @param ... Additional OpenAI parameters, e.g. \code{temperature}, \code{top_p},
#'   \code{logprobs}, \code{reasoning}. The same validation rules for gpt-5.x
#'   models are applied as in \code{\link{build_openai_batch_requests}}.
#'
#' @return A tibble with one row and columns:
#' \describe{
#'   \item{custom_id}{ID string of the form \code{"LIVE_<ID1>_vs_<ID2>"}.}
#'   \item{ID1, ID2}{The sample IDs you supplied.}
#'   \item{model}{Model name reported by the API.}
#'   \item{object_type}{OpenAI object type (e.g. "chat.completion" or "response").}
#'   \item{status_code}{HTTP-style status code (200 if successful).}
#'   \item{error_message}{Error message if something goes wrong; otherwise NA.}
#'   \item{content}{Raw assistant content string (the LLM output).}
#'   \item{better_sample}{Either "SAMPLE_1", "SAMPLE_2", or NA.}
#'   \item{better_id}{ID1 if SAMPLE_1 chosen, ID2 if SAMPLE_2 chosen, otherwise NA.}
#'   \item{prompt_tokens}{Prompt/input token count (if reported).}
#'   \item{completion_tokens}{Completion/output token count (if reported).}
#'   \item{total_tokens}{Total token count (if reported).}
#' }
#'
#' @examples
#' \dontrun{
#' data("example_writing_samples")
#' td   <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' pair <- example_writing_samples[1:2, ]
#'
#' res <- openai_compare_pair_live(
#'   ID1               = pair$ID[1],
#'   text1             = pair$text[1],
#'   ID2               = pair$ID[2],
#'   text2             = pair$text[2],
#'   model             = "gpt-4.1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   endpoint          = "chat.completions",
#'   temperature       = 0
#' )
#'
#' res$better_id
#' }
#'
#' @export
openai_compare_pair_live <- function(
    ID1,
    text1,
    ID2,
    text2,
    model,
    trait_name,
    trait_description,
    prompt_template = set_prompt_template(),
    endpoint        = c("chat.completions", "responses"),
    tag_prefix      = "<BETTER_SAMPLE>",
    tag_suffix      = "</BETTER_SAMPLE>",
    api_key         = Sys.getenv("OPENAI_API_KEY"),
    ...
) {
  endpoint <- match.arg(endpoint)

  # Basic input checks
  if (!is.character(ID1)  || length(ID1)  != 1L) stop("`ID1` must be a single character.",  call. = FALSE)
  if (!is.character(ID2)  || length(ID2)  != 1L) stop("`ID2` must be a single character.",  call. = FALSE)
  if (!is.character(text1) || length(text1) != 1L) stop("`text1` must be a single character.", call. = FALSE)
  if (!is.character(text2) || length(text2) != 1L) stop("`text2` must be a single character.", call. = FALSE)
  if (!is.character(model) || length(model) != 1L) stop("`model` must be a single character.", call. = FALSE)

  # ------------------------------------------------------------------------
  # Validate model vs temperature / top_p / logprobs / reasoning
  # (mirrors build_openai_batch_requests) :contentReference[oaicite:6]{index=6}
  # ------------------------------------------------------------------------
  dots        <- list(...)
  temperature <- dots$temperature %||% NULL
  top_p       <- dots$top_p       %||% NULL
  logprobs    <- dots$logprobs    %||% NULL
  reasoning   <- dots$reasoning   %||% NULL

  is_gpt5  <- grepl("^gpt-5", model)
  is_gpt51 <- grepl("^gpt-5\\.1", model)

  if (is_gpt51) {
    if (!is.null(reasoning) && reasoning != "none") {
      if (!is.null(temperature) || !is.null(top_p) || !is.null(logprobs)) {
        stop(
          "For gpt-5.1 with reasoning effort not equal to 'none', ",
          "temperature, top_p, and logprobs must be NULL.",
          call. = FALSE
        )
      }
    }
  } else if (is_gpt5) {
    if (!is.null(temperature) || !is.null(top_p) || !is.null(logprobs)) {
      stop(
        "For gpt-5* models other than gpt-5.1, temperature, top_p, ",
        "and logprobs must be NULL.",
        call. = FALSE
      )
    }
  }

  # ------------------------------------------------------------------------
  # Build the prompt (same as in batch_openai) :contentReference[oaicite:7]{index=7}
  # ------------------------------------------------------------------------
  prompt <- build_prompt(
    template   = prompt_template,
    trait_name = trait_name,
    trait_desc = trait_description,
    text1      = text1,
    text2      = text2
  )

  # ------------------------------------------------------------------------
  # Construct request body + endpoint path
  # ------------------------------------------------------------------------
  if (endpoint == "chat.completions") {
    body <- list(
      model    = model,
      messages = list(
        list(
          role    = "user",
          content = prompt
        )
      )
    )
    if (!is.null(temperature)) body$temperature <- temperature
    if (!is.null(top_p))       body$top_p       <- top_p
    if (!is.null(logprobs))    body$logprobs    <- logprobs

    path <- "/chat/completions"
  } else {
    # endpoint == "responses"
    body <- list(
      model = model,
      input = prompt
    )
    if (!is.null(reasoning)) {
      body$reasoning <- list(effort = reasoning)
    }
    if (!is.null(temperature)) body$temperature <- temperature
    if (!is.null(top_p))       body$top_p       <- top_p
    if (!is.null(logprobs))    body$logprobs    <- logprobs

    path <- "/responses"
  }

  # ------------------------------------------------------------------------
  # Perform request via shared helper (.openai_request) :contentReference[oaicite:8]{index=8}
  # ------------------------------------------------------------------------
  req <- .openai_request(path, api_key) |>
    req_body_json(body)

  resp <- req_perform(req)

  status_code <- resp_status(resp)
  error_message <- NA_character_

  # Try to parse body; if it fails, return an error row
  body_parsed <- tryCatch(
    resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(body_parsed)) {
    error_message <- "Failed to parse response body as JSON."

    return(tibble::tibble(
      custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
      ID1               = ID1,
      ID2               = ID2,
      model             = NA_character_,
      object_type       = NA_character_,
      status_code       = status_code,
      error_message     = error_message,
      content           = NA_character_,
      better_sample     = NA_character_,
      better_id         = NA_character_,
      prompt_tokens     = NA_real_,
      completion_tokens = NA_real_,
      total_tokens      = NA_real_
    ))
  }

  body <- body_parsed

  # ------------------------------------------------------------------------
  # Extract object_type, model, content, usage
  #   (mirrors parse_openai_batch_output) :contentReference[oaicite:9]{index=9} :contentReference[oaicite:10]{index=10}
  # ------------------------------------------------------------------------
  object_type <- body$object %||% NA_character_
  model_name  <- body$model  %||% NA_character_

  content <- NA_character_

  if (identical(object_type, "chat.completion")) {
    choices <- body$choices %||% list()
    if (length(choices) >= 1L) {
      message <- choices[[1]]$message
      if (!is.null(message) && !is.null(message$content)) {
        content <- as.character(message$content)
      }
    }
  } else if (identical(object_type, "response")) {
    output <- body$output %||% list()
    if (length(output) >= 1L) {
      blocks <- output[[1]]$content %||% list()
      texts <- vapply(
        blocks,
        function(b) {
          if (!is.null(b$type) && identical(b$type, "output_text")) {
            as.character(b$text %||% "")
          } else {
            ""
          }
        },
        character(1)
      )
      content <- paste(texts, collapse = "")
    }
  }

  # Better-sample tag detection
  better_sample <- NA_character_
  if (!is.na(content)) {
    if (grepl(paste0(tag_prefix, "SAMPLE_1", tag_suffix), content, fixed = TRUE)) {
      better_sample <- "SAMPLE_1"
    } else if (grepl(paste0(tag_prefix, "SAMPLE_2", tag_suffix), content, fixed = TRUE)) {
      better_sample <- "SAMPLE_2"
    }
  }

  better_id <- NA_character_
  if (!is.na(better_sample)) {
    if (better_sample == "SAMPLE_1") {
      better_id <- ID1
    } else if (better_sample == "SAMPLE_2") {
      better_id <- ID2
    }
  }

  usage <- body$usage %||% list()
  prompt_tokens     <- usage$prompt_tokens   %||% usage$input_tokens   %||% NA_real_
  completion_tokens <- usage$completion_tokens %||% usage$output_tokens %||% NA_real_
  total_tokens      <- usage$total_tokens    %||% NA_real_

  tibble::tibble(
    custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
    ID1               = ID1,
    ID2               = ID2,
    model             = model_name,
    object_type       = object_type,
    status_code       = status_code,
    error_message     = error_message,
    content           = content,
    better_sample     = better_sample,
    better_id         = better_id,
    prompt_tokens     = as.numeric(prompt_tokens),
    completion_tokens = as.numeric(completion_tokens),
    total_tokens      = as.numeric(total_tokens)
  )
}

#' Live OpenAI comparisons for a tibble of pairs
#'
#' This is a thin row-wise wrapper around \code{\link{openai_compare_pair_live}}.
#' It takes a tibble of pairs (ID1/text1/ID2/text2), submits each pair to the
#' OpenAI API, and binds the results into a single tibble.
#'
#' The output has the same columns as \code{\link{openai_compare_pair_live}},
#' with one row per pair, making it easy to pass into
#' \code{\link{build_bt_data}} and \code{\link{fit_bt_model}}.
#'
#' @param pairs Tibble/data frame with at least columns \code{ID1}, \code{text1},
#'   \code{ID2}, \code{text2}. Typically created by \code{\link{make_pairs}},
#'   \code{\link{sample_pairs}}, and \code{\link{randomize_pair_order}}.
#' @param model OpenAI model name (e.g. "gpt-4.1", "gpt-5.1").
#' @param trait_name Trait name to pass to \code{openai_compare_pair_live}.
#' @param trait_description Trait description to pass to
#'   \code{openai_compare_pair_live}.
#' @param prompt_template Prompt template string, typically from
#'   \code{\link{set_prompt_template}}.
#' @param endpoint Which OpenAI endpoint to target. One of
#'   \code{"chat.completions"} or \code{"responses"}.
#' @param api_key Optional OpenAI API key.
#' @param ... Additional OpenAI parameters (temperature, top_p, logprobs,
#'   reasoning, etc.) passed on to \code{openai_compare_pair_live}.
#'
#' @return A tibble with one row per pair and the same columns as
#'   \code{\link{openai_compare_pair_live}}.
#'
#' @examples
#' \dontrun{
#' data("example_writing_samples")
#'
#' pairs <- example_writing_samples |>
#'   make_pairs() |>
#'   sample_pairs(n_pairs = 3, seed = 123) |>
#'   randomize_pair_order(seed = 456)
#'
#' td   <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' res_live <- submit_openai_pairs_live(
#'   pairs             = pairs,
#'   model             = "gpt-4.1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   endpoint          = "chat.completions",
#'   temperature       = 0
#' )
#'
#' res_live$better_id
#' }
#'
#' @export
submit_openai_pairs_live <- function(
    pairs,
    model,
    trait_name,
    trait_description,
    prompt_template = set_prompt_template(),
    endpoint        = c("chat.completions", "responses"),
    api_key         = Sys.getenv("OPENAI_API_KEY"),
    ...
) {
  endpoint <- match.arg(endpoint)

  pairs <- tibble::as_tibble(pairs)
  required_cols <- c("ID1", "text1", "ID2", "text2")
  missing_cols  <- setdiff(required_cols, names(pairs))

  if (length(missing_cols) > 0L) {
    stop(
      "`pairs` must contain columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  n <- nrow(pairs)
  if (n == 0L) {
    return(tibble::tibble(
      custom_id         = character(0),
      ID1               = character(0),
      ID2               = character(0),
      model             = character(0),
      object_type       = character(0),
      status_code       = integer(0),
      error_message     = character(0),
      content           = character(0),
      better_sample     = character(0),
      better_id         = character(0),
      prompt_tokens     = numeric(0),
      completion_tokens = numeric(0),
      total_tokens      = numeric(0)
    ))
  }

  out <- vector("list", n)

  for (i in seq_len(n)) {
    out[[i]] <- openai_compare_pair_live(
      ID1               = as.character(pairs$ID1[i]),
      text1             = as.character(pairs$text1[i]),
      ID2               = as.character(pairs$ID2[i]),
      text2             = as.character(pairs$text2[i]),
      model             = model,
      trait_name        = trait_name,
      trait_description = trait_description,
      prompt_template   = prompt_template,
      endpoint          = endpoint,
      api_key           = api_key,
      ...
    )
  }

  dplyr::bind_rows(out)
}
