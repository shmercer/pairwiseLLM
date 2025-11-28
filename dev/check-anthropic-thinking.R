# dev/check-anthropic-thinking.R

check_anthropic_models_thinking <- function(
    models = c("claude-sonnet-4-5",
               "claude-haiku-4-5",
               "claude-opus-4-5"),
    prompt            = "Reply with the single word: OK",
    anthropic_version = "2023-06-01",
    verbose           = TRUE
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

  do_one <- function(model, mode) {
    # Base body
    if (mode == "enabled") {
      max_tokens      <- 2048L
      budget_tokens   <- 1024L  # must be >= 1024 and < max_tokens
    } else {
      max_tokens      <- 64L
      budget_tokens   <- NULL
    }

    body <- list(
      model      = model,
      max_tokens = max_tokens,
      messages   = list(
        list(
          role    = "user",
          content = prompt
        )
      )
    )

    # Add thinking block only when enabled
    if (identical(mode, "enabled")) {
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
      # Don't throw on 4xx/5xx; we want to inspect the body
      httr2::req_error(is_error = function(resp) FALSE)

    resp <- httr2::req_perform(req)

    status <- httr2::resp_status(resp)
    text   <- httr2::resp_body_string(resp)

    parsed <- tryCatch(
      jsonlite::fromJSON(text, simplifyVector = FALSE),
      error = function(e) NULL
    )

    model_returned <- NA_character_
    content_text   <- NA_character_
    error_msg      <- NA_character_

    if (!is.null(parsed)) {
      if (!is.null(parsed$model)) model_returned <- parsed$model

      # Normal message: content is list of blocks with $text
      if (!is.null(parsed$content) && is.list(parsed$content)) {
        pieces <- vapply(
          parsed$content,
          function(x) as.character(x$text %||% ""),
          character(1)
        )
        content_text <- paste(pieces, collapse = "")
      }

      # Error shape
      if (!is.null(parsed$error) && !is.null(parsed$error$message)) {
        error_msg <- parsed$error$message
      }
    }

    if (verbose) {
      cat("\n--------------------------------------------------\n")
      cat("Model:     ", model, "\n")
      cat("Mode:      ", mode, "\n")
      cat("Status:    ", status, "\n")
      if (!is.na(model_returned)) cat("Returned:  ", model_returned, "\n")
      if (!is.na(error_msg))      cat("Error:     ", error_msg, "\n")
      if (!is.na(content_text)) {
        cat("Content:   ", substr(content_text, 1, 120), "\n")
      } else {
        cat("Raw body snippet: ", substr(text, 1, 120), "\n")
      }
    }

    tibble::tibble(
      requested_model = model,
      reasoning_mode  = mode,
      status_code     = status,
      returned_model  = model_returned,
      error_message   = error_msg,
      content_snippet = substr(content_text %||% text, 1, 200)
    )
  }

  modes <- c("none", "enabled")

  all_rows <- list()
  k <- 1L
  for (m in models) {
    for (mode in modes) {
      all_rows[[k]] <- do_one(m, mode)
      k <- k + 1L
    }
  }

  out <- dplyr::bind_rows(all_rows)

  if (verbose) {
    cat("\n===== Summary table =====\n")
    print(out)
  }

  invisible(out)
}
