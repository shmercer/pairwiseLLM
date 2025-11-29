# dev/check-anthropic-reasoning-parsing.R

# Small helper: x %||% y
`%||%` <- function(x, y) if (!is.null(x)) x else y

# ------------------------------------------------------------------
# Parse an Anthropic response body like anthropic_compare_pair_live()
# ------------------------------------------------------------------
parse_anthropic_pair_message <- function(
    body,
    ID1,
    ID2,
    tag_prefix = "<BETTER_SAMPLE>",
    tag_suffix = "</BETTER_SAMPLE>"
) {
  object_type <- body$type  %||% NA_character_
  model_name  <- body$model %||% NA_character_

  # Collect text content from assistant message blocks
  content <- NA_character_
  if (!is.null(body$content) && length(body$content) > 0L) {
    collected <- character(0)
    for (blk in body$content) {
      if (!is.null(blk$text)) {
        collected <- c(collected, as.character(blk$text %||% ""))
      }
    }
    if (length(collected) > 0L) {
      content <- paste(collected, collapse = "")
    }
  }

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
    better_id <- if (better_sample == "SAMPLE_1") ID1 else ID2
  }

  usage <- body$usage %||% list()
  prompt_tokens     <- usage$input_tokens  %||% NA_real_
  completion_tokens <- usage$output_tokens %||% NA_real_
  total_tokens      <- usage$total_tokens  %||% NA_real_

  tibble::tibble(
    ID1               = ID1,
    ID2               = ID2,
    model             = model_name,
    object_type       = object_type,
    content           = content,
    better_sample     = better_sample,
    better_id         = better_id,
    prompt_tokens     = as.numeric(prompt_tokens),
    completion_tokens = as.numeric(completion_tokens),
    total_tokens      = as.numeric(total_tokens)
  )
}

# ------------------------------------------------------------------
# Live checker: hit Anthropic and parse like the package does
# ------------------------------------------------------------------
check_anthropic_reasoning_parsing_live <- function(
    models = c("claude-sonnet-4-5",
               "claude-haiku-4-5",
               "claude-opus-4-5"),
    use_thinking       = FALSE,
    anthropic_version  = "2023-06-01",
    verbose            = TRUE
) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required.", call. = FALSE)
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required.", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.", call. = FALSE)
  }

  key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (!nzchar(key)) {
    stop("ANTHROPIC_API_KEY is not set.", call. = FALSE)
  }

  # Use your package's examples and prompt helpers
  data("example_writing_samples", package = "pairwiseLLM", envir = environment())
  samples <- get("example_writing_samples", envir = environment())

  if (nrow(samples) < 2) {
    stop("Need at least two example writing samples.", call. = FALSE)
  }

  td   <- pairwiseLLM::trait_description("overall_quality")
  tmpl <- pairwiseLLM::set_prompt_template()

  s1 <- samples[1, ]
  s2 <- samples[2, ]

  # Build the same prompt your package uses
  prompt <- pairwiseLLM::build_prompt(
    template   = tmpl,
    trait_name = td$name,
    trait_desc = td$description,
    text1      = s1$text,
    text2      = s2$text
  )

  out_list <- list()
  idx <- 1L

  for (m in models) {
    if (verbose) {
      cat("\n==================================================\n")
      cat("[Live parsing check ", idx, "] model = ", m,
          " | thinking = ", use_thinking, "\n", sep = "")
    }

    # Token + temperature settings:
    # - Non-thinking: temp = 0 (deterministic), moderate max_tokens.
    # - Thinking: Anthropic requires temp = 1, and
    #   budget_tokens >= 1024 and < max_tokens.
    if (use_thinking) {
      max_tokens    <- 2048L
      budget_tokens <- 1024L
      temperature   <- 1
    } else {
      max_tokens    <- 768L
      budget_tokens <- NULL
      temperature   <- 0
    }

    body <- list(
      model      = m,
      max_tokens = max_tokens,
      messages   = list(
        list(
          role    = "user",
          content = list(
            list(
              type = "text",
              text = prompt
            )
          )
        )
      ),
      temperature = temperature
    )

    if (!is.null(budget_tokens)) {
      body$thinking <- list(
        type          = "enabled",
        budget_tokens = budget_tokens
      )
    }

    req <- httr2::request("https://api.anthropic.com/v1/messages") |>
      httr2::req_headers(
        "content-type"      = "application/json",
        "x-api-key"         = key,
        "anthropic-version" = anthropic_version
      ) |>
      httr2::req_body_json(body) |>
      # Don't throw on non-200; we want to see the body
      httr2::req_error(is_error = function(resp) FALSE)

    resp   <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)
    text   <- httr2::resp_body_string(resp)

    parsed <- tryCatch(
      jsonlite::fromJSON(text, simplifyVector = FALSE),
      error = function(e) NULL
    )

    if (is.null(parsed)) {
      if (verbose) {
        cat("  ❌ Could not parse JSON body.\n")
        cat("  Raw body snippet: ", substr(text, 1, 160), "\n", sep = "")
      }
      out_list[[idx]] <- tibble::tibble(
        requested_model   = m,
        thinking_enabled  = use_thinking,
        status_code       = status,
        ok                = FALSE,
        error_message     = "JSON parse error",
        returned_model    = NA_character_,
        better_id         = NA_character_,
        content_snippet   = substr(text, 1, 160),
        prompt_tokens     = NA_real_,
        completion_tokens = NA_real_,
        total_tokens      = NA_real_
      )
      idx <- idx + 1L
      next
    }

    # Handle error responses from Anthropic
    if (!is.null(parsed$error)) {
      msg <- parsed$error$message %||% sprintf("HTTP %s from Anthropic", status)
      if (verbose) {
        cat("  ❌ API error: ", msg, "\n", sep = "")
        cat("  Raw body snippet: ", substr(text, 1, 160), "\n", sep = "")
      }
      out_list[[idx]] <- tibble::tibble(
        requested_model   = m,
        thinking_enabled  = use_thinking,
        status_code       = status,
        ok                = FALSE,
        error_message     = msg,
        returned_model    = NA_character_,
        better_id         = NA_character_,
        content_snippet   = substr(text, 1, 160),
        prompt_tokens     = NA_real_,
        completion_tokens = NA_real_,
        total_tokens      = NA_real_
      )
      idx <- idx + 1L
      next
    }

    # Success path: parse like anthropic_compare_pair_live()
    parsed_row <- parse_anthropic_pair_message(
      body = parsed,
      ID1  = s1$ID,
      ID2  = s2$ID
    )

    ok <- (!is.na(parsed_row$better_id) && status == 200L)

    if (verbose) {
      cat("  status_code:     ", status, "\n", sep = "")
      cat("  returned_model:  ", parsed_row$model, "\n", sep = "")
      cat("  better_id:       ", parsed_row$better_id, "\n", sep = "")
      cat("  tokens (p/c/t):  ",
          parsed_row$prompt_tokens, "/",
          parsed_row$completion_tokens, "/",
          parsed_row$total_tokens, "\n", sep = "")
      snippet <- substr(parsed_row$content %||% "", 1L, 200L)
      cat("  content snippet: ",
          gsub("\n", " ", snippet),
          "\n", sep = "")
    }

    out_list[[idx]] <- tibble::tibble(
      requested_model   = m,
      thinking_enabled  = use_thinking,
      status_code       = status,
      ok                = ok,
      error_message     = NA_character_,
      returned_model    = parsed_row$model,
      better_id         = parsed_row$better_id,
      content_snippet   = substr(parsed_row$content %||% "", 1L, 200L),
      prompt_tokens     = parsed_row$prompt_tokens,
      completion_tokens = parsed_row$completion_tokens,
      total_tokens      = parsed_row$total_tokens
    )

    idx <- idx + 1L
  }

  out <- dplyr::bind_rows(out_list)

  if (verbose) {
    cat("\n===== Live parsing summary =====\n")
    print(out)
  }

  invisible(out)
}
