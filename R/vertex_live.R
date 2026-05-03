# Internal: low-level helpers for the Vertex AI Gemini API (express mode)

#' Vertex parameter normalization helpers
#'
#' @keywords internal
#' @noRd
normalize_vertex_service_tier <- function(service_tier) {
  if (is.null(service_tier)) {
    return(NULL)
  }

  if (!is.character(service_tier) || length(service_tier) != 1L || is.na(service_tier)) {
    rlang::abort("`service_tier` must be NULL or a non-missing character scalar.")
  }

  if (identical(service_tier, "standard")) {
    return(NULL)
  }

  if (identical(service_tier, "flex")) {
    return("shared")
  }

  if (identical(service_tier, "priority")) {
    return("dedicated")
  }

  rlang::abort(
    paste0(
      "`service_tier` must be one of NULL, \"standard\", \"flex\", or ",
      "\"priority\" for the Vertex AI Gemini API."
    )
  )
}

#' @keywords internal
.vertex_service_tier_headers <- function(service_tier) {
  request_type <- normalize_vertex_service_tier(service_tier)
  if (is.null(request_type)) {
    return(list())
  }

  headers <- list("X-Vertex-AI-LLM-Request-Type" = request_type)
  if (identical(request_type, "shared") && identical(service_tier, "flex")) {
    headers[["X-Vertex-AI-LLM-Shared-Request-Type"]] <- "flex"
  }
  headers
}

#' @keywords internal
.vertex_base_url <- function() {
  "https://aiplatform.googleapis.com"
}

#' @keywords internal
.vertex_model_resource <- function(model) {
  if (!is.character(model) || length(model) != 1L || is.na(model) || !nzchar(model)) {
    rlang::abort("`model` must be a non-empty character scalar.")
  }

  if (grepl("^publishers/google/models/.+", model)) {
    return(model)
  }

  if (grepl("^publishers/", model)) {
    rlang::abort(
      paste0(
        "`model` must be a Google publisher model for the Vertex AI Gemini API ",
        '(for example "gemini-2.5-flash" or ',
        '"publishers/google/models/gemini-2.5-flash").'
      )
    )
  }

  paste0("publishers/google/models/", model)
}

#' @keywords internal
.vertex_is_gemini_3_model <- function(model_resource) {
  grepl("(^|/)gemini-3", model_resource)
}

#' @keywords internal
.vertex_request <- function(path, api_key = NULL, service_tier = "standard") {
  api_key <- .vertex_api_key(api_key)
  tier_headers <- .vertex_service_tier_headers(service_tier)

  req <- httr2::request(.vertex_base_url())
  req <- httr2::req_url_path_append(req, sub("^/", "", path))
  req <- httr2::req_url_query(req, key = api_key)
  req <- httr2::req_headers(req, "Content-Type" = "application/json")

  if (length(tier_headers) > 0L) {
    req <- do.call(httr2::req_headers, c(list(req), tier_headers))
  }

  req
}

#' @keywords internal
.vertex_req_body_json <- function(req, body) {
  httr2::req_body_json(req, data = body)
}

#' @keywords internal
#' @noRd
.vertex_req_perform <- function(req) {
  .retry_httr2_request(req)
}

#' @keywords internal
.vertex_resp_body_json <- function(resp, ...) {
  httr2::resp_body_json(resp, ...)
}

#' @keywords internal
.vertex_resp_status <- function(resp) {
  httr2::resp_status(resp)
}

#' Live Vertex AI Gemini comparison for a single pair of samples
#'
#' This function sends a single pairwise comparison prompt to the Vertex AI
#' Gemini API using the express-mode REST `generateContent` endpoint and parses
#' the result into a one-row tibble that mirrors the structure used by the other
#' live backends.
#'
#' The prompt template should instruct the model to choose exactly one of
#' SAMPLE_1 or SAMPLE_2 and wrap the decision in `<BETTER_SAMPLE>` tags.
#'
#' @param ID1 Character ID for the first sample.
#' @param text1 Character containing the first sample text.
#' @param ID2 Character ID for the second sample.
#' @param text2 Character containing the second sample text.
#' @param model Vertex Gemini model identifier. You may supply either a short
#'   model name such as `"gemini-2.5-flash"` or the fully qualified publisher
#'   model resource
#'   `"publishers/google/models/gemini-2.5-flash"`.
#' @param trait_name Short label for the trait (e.g. `"Overall Quality"`).
#' @param trait_description Full-text trait / rubric description.
#' @param prompt_template Prompt template string, typically from
#'   [set_prompt_template()].
#' @param api_key Optional Vertex API key (defaults to
#'   `Sys.getenv("VERTEX_API_KEY")`).
#' @param temperature Optional numeric temperature. If `NULL` (default), the
#'   parameter is omitted and Vertex uses the provider default.
#' @param top_p Optional nucleus sampling parameter. If `NULL`, omitted.
#' @param top_k Optional top-k sampling parameter. If `NULL`, omitted.
#' @param max_output_tokens Optional maximum output token count. If `NULL`,
#'   omitted.
#' @param thinking_level Optional Gemini 3 thinking level. Supported public
#'   values are `"minimal"`, `"low"`, `"medium"`, and `"high"`, but exact
#'   support varies by Gemini 3 model family. This parameter is only valid for
#'   Gemini 3 and later models. Do not supply it together with
#'   `thinking_budget`.
#' @param thinking_budget Optional thinking budget in tokens. If supplied, the
#'   request includes `generationConfig$thinkingConfig$thinkingBudget`. For
#'   models earlier than Gemini 3, this is the supported control surface. Do
#'   not supply it together with `thinking_level` on Gemini 3 models.
#' @param service_tier Vertex AI service tier. Use `"standard"` (default) or
#'   `NULL` for provider default behavior. Use `"flex"` to request the
#'   documented shared flex headers or `"priority"` to request the documented
#'   `dedicated` request-type header.
#' @param api_version API version to use, default `"v1"`.
#' @param include_raw Logical; if `TRUE`, the returned tibble includes a
#'   `raw_response` list-column with the parsed JSON body.
#' @param include_thoughts Logical; if `TRUE`, requests explicit reasoning
#'   output via `generationConfig$thinkingConfig$includeThoughts` and stores the
#'   first returned text part as `thoughts` when available.
#' @param pair_uid Optional stable per-pair identifier; when supplied, this
#'   value is used verbatim as `custom_id` (otherwise `custom_id` defaults to
#'   `"LIVE_<ID1>_vs_<ID2>"`).
#' @param ... Reserved for future extensions.
#'
#' @return A tibble with one row and columns:
#'   * `custom_id` - stable ID for the pair (`pair_uid` if supplied).
#'   * `ID1`, `ID2` - provided sample IDs.
#'   * `model` - model name returned by the API (or the requested model).
#'   * `object_type` - `"generateContent"` on success, otherwise `NA`.
#'   * `status_code` - HTTP status code (200 on success).
#'   * `error_message` - error message for failures, otherwise `NA`.
#'   * `thoughts` - explicit reasoning text if `include_thoughts = TRUE` and the
#'     model returns it; otherwise `NA`.
#'   * `content` - concatenated text of the assistant's final answer (used to
#'     locate the `<BETTER_SAMPLE>` tag).
#'   * `better_sample` - `"SAMPLE_1"`, `"SAMPLE_2"`, or `NA`.
#'   * `better_id` - `ID1` if `SAMPLE_1` is chosen,
#'     `ID2` if `SAMPLE_2`, or `NA`.
#'   * `prompt_tokens`, `completion_tokens`, `total_tokens` - usage counts if
#'     reported by the API, otherwise `NA_real_`.
#'
#' @export
vertex_compare_pair_live <- function(
  ID1,
  text1,
  ID2,
  text2,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  api_key = NULL,
  temperature = NULL,
  top_p = NULL,
  top_k = NULL,
  max_output_tokens = NULL,
  thinking_level = NULL,
  thinking_budget = NULL,
  service_tier = "standard",
  api_version = "v1",
  include_raw = FALSE,
  include_thoughts = FALSE,
  pair_uid = NULL,
  ...
) {
  model_resource <- .vertex_model_resource(model)
  service_tier_header <- normalize_vertex_service_tier(service_tier)
  is_gemini_3 <- .vertex_is_gemini_3_model(model_resource)

  if (!is.null(thinking_level)) {
    thinking_level <- match.arg(
      thinking_level,
      c("minimal", "low", "medium", "high")
    )
  }

  if (is_gemini_3 && !is.null(thinking_level) && !is.null(thinking_budget)) {
    rlang::abort(
      paste0(
        "Do not supply both `thinking_level` and `thinking_budget` for Gemini 3 ",
        "Vertex models."
      )
    )
  }

  if (!is_gemini_3 && !is.null(thinking_level)) {
    rlang::abort(
      paste0(
        "`thinking_level` is only supported for Gemini 3 and later Vertex models. ",
        "Use `thinking_budget` for Gemini 2.5 and earlier models."
      )
    )
  }

  ID1 <- as.character(ID1)
  ID2 <- as.character(ID2)
  text1 <- as.character(text1)
  text2 <- as.character(text2)

  prompt <- build_prompt(
    template = prompt_template,
    trait_name = trait_name,
    trait_desc = trait_description,
    text1 = text1,
    text2 = text2
  )

  generation_config <- list()

  if (!is.null(temperature)) {
    generation_config$temperature <- temperature
  }
  if (!is.null(top_p)) {
    generation_config$topP <- top_p
  }
  if (!is.null(top_k)) {
    generation_config$topK <- top_k
  }
  if (!is.null(max_output_tokens)) {
    generation_config$maxOutputTokens <- max_output_tokens
  }
  if (!is.null(thinking_level) || !is.null(thinking_budget) || isTRUE(include_thoughts)) {
    thinking_config <- list(includeThoughts = isTRUE(include_thoughts))
    if (!is.null(thinking_level)) {
      thinking_config$thinkingLevel <- toupper(thinking_level)
    }
    if (!is.null(thinking_budget)) {
      thinking_config$thinkingBudget <- thinking_budget
    }
    generation_config$thinkingConfig <- thinking_config
  }

  body <- list(
    contents = list(
      list(
        role = "user",
        parts = list(
          list(text = prompt)
        )
      )
    )
  )

  if (length(generation_config) > 0L) {
    body$generationConfig <- generation_config
  }

  path <- sprintf("/%s/%s:generateContent", api_version, model_resource)

  req <- .vertex_request(
    path = path,
    api_key = api_key,
    service_tier = if (is.null(service_tier_header)) NULL else service_tier
  )
  req <- .vertex_req_body_json(req, body = body)

  resp <- NULL
  body_parsed <- NULL
  status_code <- NA_integer_
  error_message <- NA_character_
  retry_failures <- tibble::tibble()

  result <- tryCatch(
    {
      resp <- .vertex_req_perform(req)
      status_code <- .vertex_resp_status(resp)
      body_parsed <- .vertex_resp_body_json(resp, simplifyVector = FALSE)
      retry_failures <- attr(resp, "retry_failures")
      if (is.null(retry_failures)) {
        retry_failures <- tibble::tibble()
      }
      list(
        resp = resp,
        body_parsed = body_parsed,
        status_code = status_code,
        error_message = NA_character_,
        retry_failures = retry_failures
      )
    },
    error = function(err) {
      status_code <- NA_integer_
      error_message <- conditionMessage(err)
      retry_failures <- attr(err, "retry_failures")
      if (is.null(retry_failures)) {
        retry_failures <- tibble::tibble()
      }

      if (inherits(err, "httr2_http") && !is.null(err$resp)) {
        status_code <- httr2::resp_status(err$resp)
        body_raw <- tryCatch(
          httr2::resp_body_string(err$resp),
          error = function(e) NA_character_
        )
        if (!is.na(body_raw) && nzchar(body_raw)) {
          error_message <- paste0(error_message, " | body: ", body_raw)
        }
      }

      list(
        resp = NULL,
        body_parsed = NULL,
        status_code = status_code,
        error_message = error_message,
        retry_failures = retry_failures
      )
    }
  )

  body_parsed <- result$body_parsed
  status_code <- result$status_code
  error_message <- result$error_message
  retry_failures <- result$retry_failures
  custom_id <- .pairwiseLLM_make_custom_id(ID1, ID2, pair_uid)

  if (is.null(body_parsed)) {
    res <- tibble::tibble(
      custom_id = custom_id,
      ID1 = ID1,
      ID2 = ID2,
      model = model,
      object_type = NA_character_,
      status_code = status_code,
      error_message = error_message,
      thoughts = NA_character_,
      content = NA_character_,
      better_sample = NA_character_,
      better_id = NA_character_,
      prompt_tokens = NA_real_,
      completion_tokens = NA_real_,
      total_tokens = NA_real_
    )
    if (include_raw) {
      res$raw_response <- list(NULL)
    }
    res$retry_failures <- list(retry_failures)
    return(res)
  }

  object_type <- "generateContent"
  model_name <- body_parsed$modelVersion %||% body_parsed$model %||% model

  thoughts <- NA_character_
  content <- NA_character_

  candidates <- body_parsed$candidates %||% list()
  if (length(candidates) > 0L) {
    first <- candidates[[1]]
    cont <- first$content %||% list()
    if (length(cont) > 0L) {
      parts <- cont$parts %||% cont
      if (is.list(parts) && length(parts) > 0L) {
        if (isTRUE(include_thoughts) && length(parts) >= 2L) {
          if (!is.null(parts[[1]]$text)) {
            thoughts <- as.character(parts[[1]]$text %||% "")
          }
          collected <- vapply(
            parts[-1],
            function(p) if (!is.null(p$text)) as.character(p$text) else "",
            FUN.VALUE = character(1L)
          )
          if (length(collected) > 0L) {
            content <- paste(collected, collapse = "")
          }
        } else {
          collected <- vapply(
            parts,
            function(p) if (!is.null(p$text)) as.character(p$text) else "",
            FUN.VALUE = character(1L)
          )
          if (length(collected) > 0L) {
            content <- paste(collected, collapse = "")
          }
        }
      }
    }
  }

  better_sample <- NA_character_
  tag_prefix <- "<BETTER_SAMPLE>"
  tag_suffix <- "</BETTER_SAMPLE>"

  if (!is.na(content)) {
    if (grepl(paste0(tag_prefix, "SAMPLE_1", tag_suffix), content, fixed = TRUE)) {
      better_sample <- "SAMPLE_1"
    } else if (grepl(paste0(tag_prefix, "SAMPLE_2", tag_suffix), content, fixed = TRUE)) {
      better_sample <- "SAMPLE_2"
    }
  }

  better_id <- NA_character_
  if (!is.na(better_sample)) {
    better_id <- if (better_sample == "SAMPLE_1") ID1 else ID2
  }

  usage <- body_parsed$usageMetadata %||% list()
  prompt_tokens <- usage$promptTokenCount %||% NA_real_
  completion_tokens <- usage$candidatesTokenCount %||% NA_real_
  total_tokens <- usage$totalTokenCount %||% NA_real_

  res <- tibble::tibble(
    custom_id = custom_id,
    ID1 = ID1,
    ID2 = ID2,
    model = model_name,
    object_type = object_type,
    status_code = status_code,
    error_message = error_message,
    thoughts = thoughts,
    content = content,
    better_sample = better_sample,
    better_id = better_id,
    prompt_tokens = as.numeric(prompt_tokens),
    completion_tokens = as.numeric(completion_tokens),
    total_tokens = as.numeric(total_tokens)
  )

  if (include_raw) {
    res$raw_response <- list(body_parsed)
  }
  res$retry_failures <- list(retry_failures)

  res
}

#' Live Vertex AI Gemini comparisons for a tibble of pairs
#'
#' This is a row-wise wrapper around [vertex_compare_pair_live()]. It takes a
#' tibble of pairs (`ID1` / `text1` / `ID2` / `text2`), submits each pair to the
#' Vertex AI Gemini API, and collects the results with optional incremental
#' saving and resume support.
#'
#' @param pairs Tibble/data frame with columns `ID1`, `text1`, `ID2`, `text2`.
#' @param model Vertex Gemini model name (for example `"gemini-2.5-flash"`).
#' @param trait_name Trait name.
#' @param trait_description Trait description.
#' @param prompt_template Prompt template string, typically from
#'   [set_prompt_template()].
#' @param api_key Optional Vertex API key.
#' @param temperature Optional numeric temperature; forwarded to
#'   [vertex_compare_pair_live()].
#' @param top_p Optional numeric; forwarded to [vertex_compare_pair_live()].
#' @param top_k Optional numeric; forwarded to [vertex_compare_pair_live()].
#' @param max_output_tokens Optional integer; forwarded to
#'   [vertex_compare_pair_live()].
#' @param thinking_level Optional Gemini 3 thinking level; forwarded to
#'   [vertex_compare_pair_live()].
#' @param thinking_budget Optional integer; forwarded to
#'   [vertex_compare_pair_live()].
#' @param service_tier Vertex AI service tier forwarded to
#'   [vertex_compare_pair_live()].
#' @param api_version API version; default `"v1"`.
#' @param verbose Logical; print status/timing every `status_every` pairs.
#' @param status_every Integer; how often to print status (default 1 = every
#'   pair).
#' @param progress Logical; show a text progress bar.
#' @param include_raw Logical; if `TRUE`, each row of the returned tibble will
#'   include a `raw_response` list-column with the parsed JSON body.
#' @param include_thoughts Logical; if `TRUE`, requests explicit reasoning
#'   output and stores it in the `thoughts` column of the result.
#' @param save_path Character string; optional file path to save results
#'   incrementally. If the file exists, the function reads it to identify and
#'   skip pairs that have already been processed (resume mode). Requires the
#'   \code{readr} package.
#' @param parallel Logical; if `TRUE`, enables parallel processing using
#'   \code{future.apply}. Requires the \code{future} and \code{future.apply}
#'   packages.
#' @param workers Integer; the number of parallel workers to use if
#'   \code{parallel = TRUE}. Defaults to 1.
#' @param ... Reserved for future extensions; passed through to
#'   [vertex_compare_pair_live()].
#'
#' @return A list containing three elements:
#' \describe{
#'   \item{results}{A tibble with one row per successfully processed pair.}
#'   \item{failed_pairs}{A tibble containing the rows from \code{pairs} that
#'     failed to process, along with an \code{error_message} column.}
#'   \item{failed_attempts}{A tibble of attempt-level failures separate from
#'     observed outcomes.}
#' }
#'
#' @export
submit_vertex_pairs_live <- function(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  api_key = NULL,
  temperature = NULL,
  top_p = NULL,
  top_k = NULL,
  max_output_tokens = NULL,
  thinking_level = NULL,
  thinking_budget = NULL,
  service_tier = "standard",
  api_version = "v1",
  verbose = TRUE,
  status_every = 1L,
  progress = TRUE,
  include_raw = FALSE,
  include_thoughts = FALSE,
  save_path = NULL,
  parallel = FALSE,
  workers = 1,
  ...
) {
  pairs <- tibble::as_tibble(pairs)
  pairs_input <- pairs
  required_cols <- c("ID1", "text1", "ID2", "text2")

  ensure_pair_ids <- function(res, id1, id2, pair_uid = NULL) {
    res_tbl <- tibble::as_tibble(res)
    expected_id <- .pairwiseLLM_make_custom_id(id1, id2, pair_uid)
    res_tbl$custom_id <- expected_id
    res_tbl$ID1 <- if (!"ID1" %in% names(res_tbl)) id1 else as.character(res_tbl$ID1)
    res_tbl$ID2 <- if (!"ID2" %in% names(res_tbl)) id2 else as.character(res_tbl$ID2)
    res_tbl$ID1 <- id1
    res_tbl$ID2 <- id2

    if (!".from_catch" %in% names(res_tbl)) {
      res_tbl$.from_catch <- FALSE
    }
    res_tbl
  }

  if (!all(required_cols %in% names(pairs))) {
    rlang::abort(paste0(
      "`pairs` must contain columns: ",
      paste(required_cols, collapse = ", ")
    ))
  }

  if (!is.null(save_path)) {
    if (!requireNamespace("readr", quietly = TRUE)) {
      rlang::abort("The 'readr' package is required for incremental saving. Please install it.")
    }
    save_dir <- dirname(save_path)
    if (!dir.exists(save_dir) && save_dir != ".") {
      if (verbose) message(sprintf("Creating output directory: '%s'", save_dir))
      dir.create(save_dir, recursive = TRUE)
    }
  }

  if (parallel && workers > 1) {
    if (!requireNamespace("future", quietly = TRUE) || !requireNamespace("future.apply", quietly = TRUE)) {
      rlang::abort("Packages 'future' and 'future.apply' are required for parallel processing.")
    }
    if (verbose) {
      message(sprintf("Setting up parallel plan with %d workers (multisession)...", workers))
    }

    old_plan <- `future::plan`("multisession", workers = workers)
    on.exit(`future::plan`(old_plan), add = TRUE)
  }

  existing_results <- NULL
  if (!is.null(save_path) && file.exists(save_path)) {
    if (verbose) {
      message(sprintf("Found existing file at '%s'. Checking for resumable pairs...", save_path))
    }
    tryCatch(
      {
        existing_results <- readr::read_csv(save_path, show_col_types = FALSE)
        existing_ids <- if ("custom_id" %in% names(existing_results)) {
          existing_results$custom_id
        } else if ("pair_uid" %in% names(existing_results)) {
          existing_results$pair_uid
        } else {
          character(0)
        }
        current_ids <- .pairwiseLLM_make_custom_id(
          pairs$ID1,
          pairs$ID2,
          if ("pair_uid" %in% names(pairs)) pairs$pair_uid else NULL
        )
        to_process_idx <- !current_ids %in% existing_ids
        if (sum(!to_process_idx) > 0) {
          if (verbose) {
            message(sprintf("Skipping %d pairs already present in '%s'.", sum(!to_process_idx), save_path))
          }
          pairs <- pairs[to_process_idx, ]
        }
      },
      error = function(e) {
        warning(
          "Could not read existing save file to resume. Processing all pairs. Error: ",
          e$message,
          call. = FALSE
        )
      }
    )
  }

  n <- nrow(pairs)

  empty_res <- function() {
    res <- tibble::tibble(
      custom_id = character(0),
      ID1 = character(0),
      ID2 = character(0),
      model = character(0),
      object_type = character(0),
      status_code = integer(0),
      error_message = character(0),
      thoughts = character(0),
      content = character(0),
      better_sample = character(0),
      better_id = character(0),
      prompt_tokens = numeric(0),
      completion_tokens = numeric(0),
      total_tokens = numeric(0)
    )
    if (include_raw) {
      res$raw_response <- list()
    }
    res
  }

  if (n == 0L) {
    if (verbose) message("No new pairs to process.")
    final_res <- if (!is.null(existing_results)) existing_results else empty_res()
    empty_failed_attempts <- tibble::tibble(
      A_id = character(0),
      B_id = character(0),
      unordered_key = character(0),
      ordered_key = character(0),
      backend = character(0),
      model = character(0),
      error_code = character(0),
      error_detail = character(0),
      attempted_at = as.POSIXct(character(0))
    )
    return(list(
      results = final_res,
      failed_pairs = pairs[0, ],
      failed_attempts = empty_failed_attempts
    ))
  }

  if (!is.numeric(status_every) || length(status_every) != 1L || status_every < 1) {
    rlang::abort("`status_every` must be a single positive integer.")
  }
  status_every <- as.integer(status_every)

  fmt_secs <- function(x) sprintf("%.1fs", x)
  all_new_results <- vector("list", n)

  use_parallel <- parallel && workers > 1 && requireNamespace("future.apply", quietly = TRUE)

  if (use_parallel) {
    if (verbose) message(sprintf("Processing %d pairs in PARALLEL (Vertex)...", n))

    chunk_size <- 20
    chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))

    start_time <- Sys.time()
    pb <- if (progress) utils::txtProgressBar(min = 0, max = n, style = 3) else NULL
    total_processed <- 0

    for (chunk_indices in chunks) {
      work_fn <- function(i) {
        id1 <- as.character(pairs$ID1[i])
        id2 <- as.character(pairs$ID2[i])
        pair_uid <- if ("pair_uid" %in% names(pairs)) pairs$pair_uid[i] else NULL
        res <- tryCatch(
          {
            vertex_compare_pair_live(
              ID1 = id1,
              text1 = as.character(pairs$text1[i]),
              ID2 = id2,
              text2 = as.character(pairs$text2[i]),
              model = model,
              trait_name = trait_name,
              trait_description = trait_description,
              prompt_template = prompt_template,
              api_key = api_key,
              temperature = temperature,
              top_p = top_p,
              top_k = top_k,
              max_output_tokens = max_output_tokens,
              thinking_level = thinking_level,
              thinking_budget = thinking_budget,
              service_tier = service_tier,
              api_version = api_version,
              include_raw = include_raw,
              include_thoughts = include_thoughts,
              pair_uid = pair_uid,
              ...
            )
          },
          error = function(e) {
            retry_failures <- attr(e, "retry_failures")
            if (is.null(retry_failures)) {
              retry_failures <- tibble::tibble()
            }
            tibble::tibble(
              custom_id = .pairwiseLLM_make_custom_id(id1, id2, pair_uid),
              ID1 = id1,
              ID2 = id2,
              model = model,
              object_type = NA_character_,
              status_code = NA_integer_,
              error_message = paste0("Error: ", conditionMessage(e)),
              .from_catch = TRUE,
              thoughts = NA_character_,
              content = NA_character_,
              better_sample = NA_character_,
              better_id = NA_character_,
              prompt_tokens = NA_real_,
              completion_tokens = NA_real_,
              total_tokens = NA_real_,
              raw_response = if (include_raw) list(NULL) else NULL,
              retry_failures = list(retry_failures)
            )
          }
        )
        ensure_pair_ids(res, id1, id2, pair_uid = pair_uid)
      }

      chunk_results_list <- `future.apply::future_lapply`(chunk_indices, work_fn)
      all_new_results[chunk_indices] <- chunk_results_list

      if (!is.null(save_path)) {
        chunk_df <- dplyr::bind_rows(chunk_results_list)
        if ("raw_response" %in% names(chunk_df)) chunk_df$raw_response <- NULL
        write_mode <- if (file.exists(save_path)) "append" else "write"
        tryCatch(
          {
            readr::write_csv(chunk_df, save_path, append = (write_mode == "append"))
          },
          error = function(e) {
            warning("Failed to save incremental results: ", e$message, call. = FALSE)
          }
        )
      }

      total_processed <- total_processed + length(chunk_indices)
      if (!is.null(pb)) utils::setTxtProgressBar(pb, total_processed)
    }
    if (!is.null(pb)) close(pb)
  } else {
    if (verbose) {
      message(sprintf(
        paste0(
          "Submitting %d live pair(s) for comparison (model=%s, backend=vertex, ",
          "include_thoughts=%s)..."
        ),
        n,
        model,
        include_thoughts
      ))
    }

    start_time <- Sys.time()
    pb <- if (progress) utils::txtProgressBar(min = 0, max = n, style = 3) else NULL

    for (i in seq_len(n)) {
      id1_i <- as.character(pairs$ID1[i])
      id2_i <- as.character(pairs$ID2[i])
      pair_uid <- if ("pair_uid" %in% names(pairs)) pairs$pair_uid[i] else NULL
      show_status <- verbose && ((i - 1) %% status_every == 0L)

      if (show_status) {
        message(sprintf(
          "[Vertex live pair %d of %d] Comparing %s vs %s ...",
          i,
          n,
          id1_i,
          id2_i
        ))
      }

      res <- tryCatch(
        {
          vertex_compare_pair_live(
            ID1 = id1_i,
            text1 = as.character(pairs$text1[i]),
            ID2 = id2_i,
            text2 = as.character(pairs$text2[i]),
            model = model,
            trait_name = trait_name,
            trait_description = trait_description,
            prompt_template = prompt_template,
            api_key = api_key,
            temperature = temperature,
            top_p = top_p,
            top_k = top_k,
            max_output_tokens = max_output_tokens,
            thinking_level = thinking_level,
            thinking_budget = thinking_budget,
            service_tier = service_tier,
            api_version = api_version,
            include_raw = include_raw,
            include_thoughts = include_thoughts,
            pair_uid = pair_uid,
            ...
          )
        },
        error = function(e) {
          retry_failures <- attr(e, "retry_failures")
          if (is.null(retry_failures)) {
            retry_failures <- tibble::tibble()
          }
          tibble::tibble(
            custom_id = .pairwiseLLM_make_custom_id(id1_i, id2_i, pair_uid),
            ID1 = id1_i,
            ID2 = id2_i,
            model = model,
            object_type = NA_character_,
            status_code = NA_integer_,
            error_message = paste0("Error: ", conditionMessage(e)),
            .from_catch = TRUE,
            thoughts = NA_character_,
            content = NA_character_,
            better_sample = NA_character_,
            better_id = NA_character_,
            prompt_tokens = NA_real_,
            completion_tokens = NA_real_,
            total_tokens = NA_real_,
            raw_response = if (include_raw) list(NULL) else NULL,
            retry_failures = list(retry_failures)
          )
        }
      )
      res <- ensure_pair_ids(res, id1_i, id2_i, pair_uid = pair_uid)
      all_new_results[[i]] <- res

      if (!is.null(save_path)) {
        write_df <- res
        if ("raw_response" %in% names(write_df)) write_df$raw_response <- NULL
        col_names <- !file.exists(save_path)
        tryCatch(
          {
            readr::write_csv(write_df, save_path, append = !col_names, col_names = col_names)
          },
          error = function(e) warning("Failed to save incremental result: ", e$message, call. = FALSE)
        )
      }

      if (!is.null(pb)) utils::setTxtProgressBar(pb, i)

      if (show_status) {
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        avg <- elapsed / i
        remaining <- avg * (n - i)
        message(sprintf(
          "  Elapsed: %s | Avg per pair: %s | Est. remaining: %s",
          fmt_secs(elapsed),
          fmt_secs(avg),
          fmt_secs(remaining)
        ))
      }
    }
    if (!is.null(pb)) close(pb)
  }

  new_results_df <- dplyr::bind_rows(all_new_results)

  if (verbose) {
    total_elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    message(sprintf(
      "Completed %d pairs in %s (avg %.2fs/pair).",
      n,
      fmt_secs(total_elapsed),
      total_elapsed / n
    ))
  }

  final_results <- if (!is.null(existing_results)) {
    dplyr::bind_rows(existing_results, new_results_df)
  } else {
    new_results_df
  }

  final_results <- final_results |>
    dplyr::select(-dplyr::any_of(c(".from_catch")))

  normalized <- .normalize_llm_results(
    raw = final_results,
    pairs = pairs_input,
    backend = "vertex",
    model = model,
    include_raw = include_raw
  )

  list(
    results = normalized$results,
    failed_pairs = normalized$failed_pairs,
    failed_attempts = normalized$failed_attempts
  )
}
