#' Build OpenAI batch JSONL lines for paired comparisons
#'
#' This helper constructs one JSON object per pair of writing samples,
#' suitable for use with the OpenAI batch API. It supports both
#' \code{/v1/chat/completions} and \code{/v1/responses} endpoints.
#'
#' @param pairs A data frame or tibble with columns \code{ID1}, \code{text1},
#'   \code{ID2}, and \code{text2}.
#' @param model Character scalar giving the OpenAI model name.
#'   Supports standard names (e.g. \code{"gpt-4.1"}) and date-stamped versions
#'   (e.g. \code{"gpt-5.2-2025-12-11"}).
#' @param trait_name Short label for the trait (e.g., "Overall Quality").
#' @param trait_description Full-text definition of the trait.
#' @param prompt_template Character template containing the placeholders
#'   \code{{TRAIT_NAME}}, \code{{TRAIT_DESCRIPTION}}, \code{{SAMPLE_1}},
#'   and \code{{SAMPLE_2}}. Defaults to \code{set_prompt_template()}.
#' @param endpoint Which OpenAI endpoint to target. One of
#'   \code{"chat.completions"} (default) or \code{"responses"}.
#' @param temperature Optional temperature parameter. Defaults to `0` for
#'   standard models (deterministic). Must be `NULL` for reasoning models
#'   (enabled).
#' @param top_p Optional top_p parameter.
#' @param logprobs Optional logprobs parameter.
#' @param reasoning Optional reasoning effort for \code{gpt-5.1/5.2} when using
#'   the \code{/v1/responses} endpoint. Typically \code{"none"}, \code{"low"},
#'   \code{"medium"}, or \code{"high"}.
#' @param include_thoughts Logical; if TRUE and using \code{responses} endpoint
#'   with reasoning, requests a summary. Defaults \code{reasoning} to \code{"low"}
#'   for gpt-5.1/5.2 if not specified.
#' @param request_id_prefix String prefix for \code{custom_id}; the full
#'   ID takes the form \code{"<prefix>_<ID1>_vs_<ID2>"}.
#'
#' @return A tibble with one row per pair and columns:
#'   \itemize{
#'     \item \code{custom_id}: ID string used by the batch API.
#'     \item \code{method}: HTTP method (\code{"POST"}).
#'     \item \code{url}: Endpoint path (\code{"/v1/chat/completions"} or
#'           \code{"/v1/responses"}).
#'     \item \code{body}: List column containing the request body.
#'   }
#'
#' @examples
#' \dontrun{
#' # Requires OPENAI_API_KEY and network access.
#' library(pairwiseLLM)
#'
#' data("example_writing_samples", package = "pairwiseLLM")
#'
#' pairs <- example_writing_samples |>
#'   make_pairs() |>
#'   sample_pairs(n_pairs = 3, seed = 123) |>
#'   randomize_pair_order(seed = 456)
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' # 1. Basic chat.completions batch (no thoughts)
#' batch_tbl_chat <- build_openai_batch_requests(
#'   pairs             = pairs,
#'   model             = "gpt-4.1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl,
#'   endpoint          = "chat.completions",
#'   temperature       = 0
#' )
#'
#' # 2. GPT-5.2-2025-12-11 Responses Batch with Reasoning
#' batch_resp <- build_openai_batch_requests(
#'   pairs = pairs,
#'   model = "gpt-5.2-2025-12-11",
#'   trait_name = td$name,
#'   trait_description = td$description,
#'   prompt_template = tmpl,
#'   endpoint = "responses",
#'   include_thoughts = TRUE, # implies reasoning="low" if not set
#'   reasoning = "medium"
#' )
#' batch_tbl_chat
#' batch_tbl_resp
#' }
#'
#' @import tibble
#' @export
build_openai_batch_requests <- function(pairs,
                                        model,
                                        trait_name,
                                        trait_description,
                                        prompt_template = set_prompt_template(),
                                        endpoint = c(
                                          "chat.completions",
                                          "responses"
                                        ),
                                        temperature = NULL,
                                        top_p = NULL,
                                        logprobs = NULL,
                                        reasoning = NULL,
                                        include_thoughts = FALSE,
                                        request_id_prefix = "EXP") {
  endpoint <- match.arg(endpoint)
  pairs <- tibble::as_tibble(pairs)

  if (nrow(pairs) == 0L) {
    return(tibble::tibble(
      custom_id = character(0), method = character(0), url = character(0), body = list()
    ))
  }

  # Validate model vs temperature / top_p / logprobs / reasoning
  is_gpt5 <- grepl("^gpt-5", model)
  is_reasoning_model <- grepl("^gpt-5\\.[12]", model)

  # Default reasoning if thoughts requested on responses endpoint
  if (endpoint == "responses" && isTRUE(include_thoughts) && is.null(reasoning)) {
    if (is_reasoning_model) {
      reasoning <- "low"
    } else {
      warning("include_thoughts requested for non-reasoning model; ignores thoughts.", call. = FALSE)
    }
  }

  reasoning_active <- is_reasoning_model && (!is.null(reasoning) && reasoning != "none")

  # Apply default temperature = 0 if strictly not reasoning
  if (is.null(temperature) && !reasoning_active) {
    temperature <- 0
  }

  if (is_reasoning_model && reasoning_active) {
    if (!is.null(temperature) || !is.null(top_p) || !is.null(logprobs)) {
      stop("For gpt-5.1/5.2 with reasoning, temperature/top_p/logprobs must be NULL.", call. = FALSE)
    }
  }

  out_list <- vector("list", nrow(pairs))

  for (i in seq_len(nrow(pairs))) {
    id1 <- as.character(pairs$ID1[i])
    id2 <- as.character(pairs$ID2[i])
    prompt <- build_prompt(
      template = prompt_template,
      trait_name = trait_name,
      trait_desc = trait_description,
      text1 = as.character(pairs$text1[i]),
      text2 = as.character(pairs$text2[i])
    )
    custom_id <- sprintf("%s_%s_vs_%s", request_id_prefix, id1, id2)

    if (endpoint == "chat.completions") {
      body <- list(model = model, messages = list(list(role = "user", content = prompt)))
      if (!is.null(temperature)) body$temperature <- temperature
      if (!is.null(top_p)) body$top_p <- top_p
      if (!is.null(logprobs)) body$logprobs <- logprobs
      obj <- list(custom_id = custom_id, method = "POST", url = "/v1/chat/completions", body = body)
    } else {
      body <- list(model = model, input = prompt)
      if (!is.null(reasoning)) {
        block <- list(effort = reasoning)
        if (!identical(reasoning, "none") && isTRUE(include_thoughts)) block$summary <- "auto"
        body$reasoning <- block
      }
      if (!is.null(temperature)) body$temperature <- temperature
      if (!is.null(top_p)) body$top_p <- top_p
      if (!is.null(logprobs)) body$logprobs <- logprobs
      obj <- list(custom_id = custom_id, method = "POST", url = "/v1/responses", body = body)
    }
    out_list[[i]] <- obj
  }

  tibble::tibble(
    custom_id = vapply(out_list, `[[`, character(1), "custom_id"),
    method = vapply(out_list, `[[`, character(1), "method"),
    url = vapply(out_list, `[[`, character(1), "url"),
    body = lapply(out_list, `[[`, "body")
  )
}

#' Write an OpenAI batch table to a JSONL file
#'
#' This helper takes the output of \code{\link{build_openai_batch_requests}}
#' (or a compatible table) and writes one JSON object per line, in the
#' format expected by the OpenAI batch API.
#'
#' The input can either:
#' \itemize{
#'   \item Already contain a character column \code{jsonl} (one JSON string
#'         per row), in which case that column is used directly, or
#'   \item Contain the columns \code{custom_id}, \code{method},
#'         \code{url}, and \code{body}, in which case the JSON strings are
#'         constructed automatically.
#' }
#'
#' @param batch_tbl A data frame or tibble, typically the result of
#'   \code{\link{build_openai_batch_requests}}.
#' @param path File path where the JSONL file should be written.
#'
#' @return Invisibly returns \code{path}.
#'
#' @examples
#' \dontrun{
#' data("example_writing_samples")
#' pairs_all <- make_pairs(example_writing_samples)
#' pairs_small <- sample_pairs(pairs_all, n_pairs = 5, seed = 1)
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' batch_tbl <- build_openai_batch_requests(
#'   pairs             = pairs_small,
#'   model             = "gpt-4.1",
#'   trait_name        = td$name,
#'   trait_description = td$description,
#'   prompt_template   = tmpl
#' )
#'
#' write_openai_batch_file(batch_tbl, "batch_forward.jsonl")
#' }
#'
#' @importFrom jsonlite toJSON
#' @export
write_openai_batch_file <- function(batch_tbl, path) {
  batch_tbl <- tibble::as_tibble(batch_tbl)

  # If a jsonl column already exists, use it directly (backward compatible)
  if ("jsonl" %in% names(batch_tbl)) {
    json_lines <- batch_tbl$jsonl
    if (!is.character(json_lines)) {
      stop("`jsonl` column must be a character vector.", call. = FALSE)
    }
  } else {
    # Otherwise, construct JSONL from custom_id / method / url / body
    required_cols <- c("custom_id", "method", "url", "body")
    missing_cols <- setdiff(required_cols, names(batch_tbl))
    if (length(missing_cols) > 0L) {
      stop(
        "`batch_tbl` must have either a `jsonl` column or columns: ",
        paste(required_cols, collapse = ", "),
        call. = FALSE
      )
    }

    n <- nrow(batch_tbl)
    if (n == 0L) {
      json_lines <- character(0)
    } else {
      json_lines <- vapply(
        seq_len(n),
        function(i) {
          jsonlite::toJSON(
            list(
              custom_id = batch_tbl$custom_id[i],
              method    = batch_tbl$method[i],
              url       = batch_tbl$url[i],
              body      = batch_tbl$body[[i]]
            ),
            auto_unbox = TRUE
          )
        },
        FUN.VALUE = character(1)
      )
    }
  }

  # Write one JSON object per line
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(json_lines, con = con, sep = "\n")

  invisible(path)
}
