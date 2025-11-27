#' Build OpenAI batch JSONL lines for paired comparisons
#'
#' This function takes a data frame of paired writing samples and produces
#' one JSON object (as a character string) per row, suitable for use with
#' the OpenAI Batch API.
#'
#' By default it targets the Chat Completions endpoint
#' (\code{/v1/chat/completions}), which is appropriate for models such as
#' \code{"gpt-4.1"} and \code{"gpt-4.1-mini"}. It can also target the
#' Responses endpoint (\code{/v1/responses}) for models such as
#' \code{"gpt-5.1"}, including an optional \code{reasoning} configuration.
#'
#' ⚠️ GPT-5.x constraints (Responses endpoint):
#' \itemize{
#'   \item For \code{model = "gpt-5.1"}, the fields \code{temperature},
#'         \code{top_p}, and \code{logprobs} are only allowed when
#'         \code{reasoning_effort = "none"}.
#'   \item For other GPT-5 family models (e.g., \code{"gpt-5"},
#'         \code{"gpt-5-mini"}, \code{"gpt-5-nano"}) on the Responses
#'         endpoint, \code{temperature}, \code{top_p}, and \code{logprobs}
#'         must be omitted entirely. If any of these arguments are non-NULL,
#'         this function will error with an explanatory message.
#' }
#'
#' @param pairs A data frame or tibble with at least the columns
#'   \code{ID1}, \code{ID2}, \code{text1}, and \code{text2}.
#' @param model Character string with the OpenAI model name
#'   (e.g., \code{"gpt-4.1"}, \code{"gpt-5.1"}).
#' @param trait_description Character string with the description of the
#'   trait being evaluated (e.g., "overall writing quality").
#' @param prompt_template Character string template containing placeholders
#'   for \code{\\{TRAIT_DESCRIPTION\\}}, \code{\\{SAMPLE_1\\}},
#'   and \code{\\{SAMPLE_2\\}}. The function performs straightforward
#'   string substitution on these markers.
#' @param system_prompt Optional system prompt string. If \code{NULL}
#'   (default), no system message is included (chat endpoint only).
#' @param temperature Numeric temperature to include in the request body.
#'   Defaults to \code{0}, which is usually preferred for scoring and
#'   comparison tasks. For GPT-5.x on the Responses endpoint, see the
#'   constraints above.
#' @param top_p Optional numeric \eqn{(0,1]} controlling nucleus sampling.
#'   Defaults to \code{1}. Only included in the request body if not \code{NULL}.
#'   For GPT-5.x on the Responses endpoint, see the constraints above.
#' @param logprobs Optional control for log-probabilities. The value is passed
#'   through without validation and omitted if \code{NULL}, but GPT-5.x models
#'   on the Responses endpoint impose additional restrictions (see details).
#' @param endpoint Character string specifying which OpenAI endpoint to
#'   target. One of \code{"chat.completions"} (default) or
#'   \code{"responses"}. This controls the \code{url} field and the shape
#'   of the \code{body}.
#' @param reasoning_effort Optional character string for the Responses
#'   endpoint, specifying the reasoning effort. Typical values include
#'   \code{"none"}, \code{"low"}, \code{"medium"}, or \code{"high"}.
#'   If \code{NULL} (default), no \code{reasoning} field is included.
#' @param custom_id_prefix Character string used as a prefix for the
#'   \code{custom_id} field. The suffix is constructed as
#'   \code{"ID1_vs_ID2"}.
#'
#' @return A tibble with one row per pair and columns:
#'   \itemize{
#'     \item \code{ID1}, \code{ID2}, \code{text1}, \code{text2}
#'     \item \code{custom_id}: the string used for the batch request
#'     \item \code{jsonl}: the JSON object as a single-character string
#'   }
#'
#' @examples
#' data("example_writing_samples")
#' pairs <- make_pairs(example_writing_samples)
#'
#' tmpl <- paste(
#'   "You are comparing two student writing samples.",
#'   "Decide which sample shows better {TRAIT_DESCRIPTION}.",
#'   "",
#'   "SAMPLE 1:",
#'   "{SAMPLE_1}",
#'   "",
#'   "SAMPLE 2:",
#'   "{SAMPLE_2}",
#'   "",
#'   "Respond ONLY with:",
#'   "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>  or",
#'   "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>",
#'   sep = "\n"
#' )
#'
#' trait <- "overall writing quality"
#'
#' # Chat Completions endpoint (e.g., gpt-4.1)
#' batch_chat <- build_openai_batch_requests(
#'   pairs             = pairs[1:2, ],
#'   model             = "gpt-4.1",
#'   trait_description = trait,
#'   prompt_template   = tmpl,
#'   endpoint          = "chat.completions"
#' )
#'
#' @import tibble
#' @importFrom jsonlite toJSON
#' @export
build_openai_batch_requests <- function(pairs,
                                        model,
                                        trait_description,
                                        prompt_template,
                                        system_prompt = NULL,
                                        temperature = 0,
                                        top_p = 1,
                                        logprobs = NULL,
                                        endpoint = c("chat.completions", "responses"),
                                        reasoning_effort = NULL,
                                        custom_id_prefix = "EXP_") {
  pairs <- tibble::as_tibble(pairs)

  required_cols <- c("ID1", "ID2", "text1", "text2")
  if (!all(required_cols %in% names(pairs))) {
    stop(
      "`pairs` must contain columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  endpoint <- match.arg(endpoint)
  url <- switch(
    endpoint,
    "chat.completions" = "/v1/chat/completions",
    "responses"        = "/v1/responses"
  )

  # --- GPT-5.x + Responses endpoint constraints ---
  is_responses   <- endpoint == "responses"
  is_gpt5_family <- startsWith(model, "gpt-5")
  is_gpt51       <- identical(model, "gpt-5.1")

  if (is_responses && is_gpt5_family) {
    if (is_gpt51) {
      # gpt-5.1: temperature/top_p/logprobs allowed ONLY when reasoning_effort == "none"
      if (!is.null(reasoning_effort) && reasoning_effort != "none") {
        if (!is.null(temperature) || !is.null(top_p) || !is.null(logprobs)) {
          stop(
            "For model 'gpt-5.1' with reasoning_effort != 'none', the fields ",
            "`temperature`, `top_p`, and `logprobs` are not supported on the ",
            "Responses endpoint. Please set these arguments to NULL, or set ",
            "reasoning_effort = 'none'.",
            call. = FALSE
          )
        }
      }
    } else {
      # Other gpt-5* models: cannot include these fields at all
      if (!is.null(temperature) || !is.null(top_p) || !is.null(logprobs)) {
        stop(
          "For GPT-5 models other than 'gpt-5.1' on the Responses endpoint, ",
          "the fields `temperature`, `top_p`, and `logprobs` are not supported. ",
          "Please set these arguments to NULL.",
          call. = FALSE
        )
      }
    }
  }

  jsonl_vec  <- character(nrow(pairs))
  custom_ids <- character(nrow(pairs))

  for (i in seq_len(nrow(pairs))) {
    id1   <- pairs$ID1[i]
    id2   <- pairs$ID2[i]
    txt1  <- pairs$text1[i]
    txt2  <- pairs$text2[i]

    # Build the prompt by substituting into the template
    prompt <- prompt_template
    prompt <- gsub("{TRAIT_DESCRIPTION}", trait_description, prompt, fixed = TRUE)
    prompt <- gsub("{SAMPLE_1}",        txt1,              prompt, fixed = TRUE)
    prompt <- gsub("{SAMPLE_2}",        txt2,              prompt, fixed = TRUE)

    cid <- paste0(custom_id_prefix, id1, "_vs_", id2)

    if (endpoint == "chat.completions") {
      messages <- if (is.null(system_prompt)) {
        list(list(role = "user", content = prompt))
      } else {
        list(
          list(role = "system", content = system_prompt),
          list(role = "user",   content = prompt)
        )
      }

      body <- list(
        model       = model,
        messages    = messages,
        temperature = temperature
      )

      if (!is.null(top_p)) {
        body$top_p <- top_p
      }
      if (!is.null(logprobs)) {
        body$logprobs <- logprobs
      }

    } else { # endpoint == "responses"
      body <- list(
        model       = model,
        input       = prompt
      )

      # For GPT-5.1 w/ reasoning_effort = "none", these are allowed.
      # For other allowed cases (non-GPT-5, or GPT-5.1+none), the checks above
      # ensure we don't violate API rules.
      if (!is.null(temperature)) {
        body$temperature <- temperature
      }
      if (!is.null(top_p)) {
        body$top_p <- top_p
      }
      if (!is.null(logprobs)) {
        body$logprobs <- logprobs
      }
      if (!is.null(reasoning_effort)) {
        body$reasoning <- list(effort = reasoning_effort)
      }
    }

    obj <- list(
      custom_id = cid,
      method    = "POST",
      url       = url,
      body      = body
    )

    jsonl_vec[i]  <- jsonlite::toJSON(obj, auto_unbox = TRUE)
    custom_ids[i] <- cid
  }

  tibble::tibble(
    ID1       = pairs$ID1,
    ID2       = pairs$ID2,
    text1     = pairs$text1,
    text2     = pairs$text2,
    custom_id = custom_ids,
    jsonl     = jsonl_vec
  )
}

#' Write OpenAI batch JSONL lines to a file
#'
#' This is a small convenience wrapper around the output of
#' \code{\link{build_openai_batch_requests}}. It takes a tibble with a
#' \code{jsonl} column (one JSON object per row) and writes those lines
#' to a file suitable for uploading to the OpenAI Batch API.
#'
#' @param batch_tbl A data frame or tibble produced by
#'   \code{\link{build_openai_batch_requests}} with a \code{jsonl} column
#'   containing one JSON object (as a character string) per row.
#' @param path File path to write to (e.g., \code{"openai_batch.jsonl"}).
#'
#' @return Invisibly returns the input \code{batch_tbl}.
#'
#' @examples
#' \dontrun{
#' data("example_writing_samples")
#' pairs <- make_pairs(example_writing_samples)
#'
#' tmpl  <- "Evaluate {TRAIT_DESCRIPTION}.\n\nSample 1:\n{SAMPLE_1}\n\nSample 2:\n{SAMPLE_2}"
#' trait <- "overall writing quality"
#'
#' batch_tbl <- build_openai_batch_requests(
#'   pairs             = pairs[1:2, ],
#'   model             = "gpt-4.1",
#'   trait_description = trait,
#'   prompt_template   = tmpl,
#'   endpoint          = "chat.completions"
#' )
#'
#' write_openai_batch_file(batch_tbl, "openai_batch_gpt4.1.jsonl")
#' }
#'
#' @export
write_openai_batch_file <- function(batch_tbl, path) {
  if (!"jsonl" %in% names(batch_tbl)) {
    stop("`batch_tbl` must have a `jsonl` column.", call. = FALSE)
  }

  lines <- as.character(batch_tbl$jsonl)
  writeLines(lines, con = path)
  invisible(batch_tbl)
}

