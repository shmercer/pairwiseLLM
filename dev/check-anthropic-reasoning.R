# dev/check-anthropic-reasoning.R

check_anthropic_reasoning_live <- function(
    models = c("claude-sonnet-4-5",
               "claude-haiku-4-5",
               "claude-opus-4-5"),
    verbose = TRUE
) {
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

  # Use your package's example data + helpers
  data("example_writing_samples", package = "pairwiseLLM", envir = environment())
  samples <- get("example_writing_samples", envir = environment())

  if (nrow(samples) < 2) {
    stop("Need at least two example writing samples.", call. = FALSE)
  }

  td   <- pairwiseLLM::trait_description("overall_quality")
  tmpl <- pairwiseLLM::set_prompt_template()

  s1 <- samples[1, ]
  s2 <- samples[2, ]

  out_list <- list()
  idx <- 1L

  for (m in models) {
    if (verbose) {
      cat("\n==================================================\n")
      cat("[Reasoning check ", idx, "] model = ", m, "\n", sep = "")
    }

    res <- tryCatch(
      pairwiseLLM::anthropic_compare_pair_live(
        ID1               = s1$ID,
        text1             = s1$text,
        ID2               = s2$ID,
        text2             = s2$text,
        model             = m,
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        reasoning         = "enabled",   # <- reasoning mode
        include_raw       = TRUE
      ),
      error = function(e) e
    )

    if (inherits(res, "error")) {
      if (verbose) {
        cat("  ❌ Error: ", conditionMessage(res), "\n", sep = "")
      }
      out_list[[idx]] <- tibble::tibble(
        requested_model   = m,
        status_code       = NA_integer_,
        ok                = FALSE,
        error_message     = conditionMessage(res),
        returned_model    = NA_character_,
        better_id         = NA_character_,
        prompt_tokens     = NA_real_,
        completion_tokens = NA_real_,
        total_tokens      = NA_real_
      )
    } else {
      row <- res[1, ]

      ok  <- TRUE
      msg <- character()

      if (is.na(row$status_code) || row$status_code != 200L) {
        ok  <- FALSE
        msg <- c(msg, sprintf("Non-200 status: %s", row$status_code))
      }

      if (is.na(row$better_id)) {
        ok  <- FALSE
        msg <- c(msg, "better_id is NA (no <BETTER_SAMPLE> tag parsed?)")
      }

      if (!is.na(row$model) &&
          !startsWith(row$model, sub("-[0-9]+$", "", m))) {
        ok  <- FALSE
        msg <- c(
          msg,
          sprintf("Returned model '%s' does not match requested family '%s'",
                  row$model, m)
        )
      }

      if (verbose) {
        cat("  status_code:    ", row$status_code, "\n", sep = "")
        cat("  returned_model: ", row$model, "\n", sep = "")
        cat("  better_id:      ", row$better_id, "\n", sep = "")
        cat("  tokens (p/c/t): ",
            row$prompt_tokens, "/",
            row$completion_tokens, "/",
            row$total_tokens, "\n", sep = "")
        snippet <- substr(row$content %||% "", 1L, 200L)
        cat("  content snippet:", "\n",
            paste0("    ", gsub("\n", " ", snippet)), "\n", sep = "")
      }

      out_list[[idx]] <- tibble::tibble(
        requested_model   = m,
        status_code       = row$status_code,
        ok                = ok,
        error_message     = if (length(msg) == 0L) NA_character_ else paste(msg, collapse = " | "),
        returned_model    = row$model,
        better_id         = row$better_id,
        prompt_tokens     = row$prompt_tokens,
        completion_tokens = row$completion_tokens,
        total_tokens      = row$total_tokens
      )
    }

    idx <- idx + 1L
  }

  out <- dplyr::bind_rows(out_list)

  if (verbose) {
    cat("\nSummary:\n")
    print(out)
  }

  invisible(out)
}

# Helper for `%||%` in this dev script only
`%||%` <- function(x, y) if (!is.null(x)) x else y

check_anthropic_reasoning_single <- function(
    model = "claude-sonnet-4-5",
    verbose = TRUE
) {
  data("example_writing_samples", package = "pairwiseLLM")
  samples <- example_writing_samples

  td   <- pairwiseLLM::trait_description("overall_quality")
  tmpl <- pairwiseLLM::set_prompt_template()

  s1 <- samples[1, ]
  s2 <- samples[2, ]

  res <- pairwiseLLM::anthropic_compare_pair_live(
    ID1               = s1$ID,
    text1             = s1$text,
    ID2               = s2$ID,
    text2             = s2$text,
    model             = model,
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    reasoning         = "enabled",
    include_raw       = TRUE
  )

  if (verbose) {
    print(res)
    cat("\nRaw response structure:\n")
    str(res$raw_response[[1]], max.level = 2)
  }

  invisible(res)
}

