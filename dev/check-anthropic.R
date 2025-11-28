# dev/check-anthropic.R (for example)

check_anthropic_live_models <- function(
    models = c("claude-sonnet-4-5",
               "claude-haiku-4-5",
               "claude-opus-4-5"),
    reasoning_modes  = c("none", "enabled"),
    max_pairs        = 1,
    verbose          = TRUE
) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required.", call. = FALSE)
  }

  key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (!nzchar(key)) {
    stop("Set ANTHROPIC_API_KEY in your environment before running this check.",
         call. = FALSE)
  }

  # Use your built-in example data + helpers if available
  if (!exists("example_writing_samples", where = asNamespace("pairwiseLLM"))) {
    stop("example_writing_samples dataset not found. ",
         "Make sure pairwiseLLM is loaded.", call. = FALSE)
  }

  data("example_writing_samples", package = "pairwiseLLM", envir = environment())
  samples <- get("example_writing_samples", envir = environment())

  if (nrow(samples) < 2) {
    stop("Need at least 2 example writing samples for the check.", call. = FALSE)
  }

  # Simple pair to test on
  s1 <- samples[1, ]
  s2 <- samples[2, ]

  td   <- pairwiseLLM::trait_description("overall_quality")
  tmpl <- pairwiseLLM::set_prompt_template()

  results <- list()
  i <- 1L

  for (model in models) {
    for (mode in reasoning_modes) {
      if (verbose) {
        message(sprintf(
          "\n[Check %d] model = %s, reasoning = %s",
          i, model, mode
        ))
      }

      res <- tryCatch(
        pairwiseLLM::anthropic_compare_pair_live(
          ID1               = s1$ID,
          text1             = s1$text,
          ID2               = s2$ID,
          text2             = s2$text,
          model             = model,
          trait_name        = td$name,
          trait_description = td$description,
          prompt_template   = tmpl,
          reasoning         = mode,
          include_raw       = TRUE
        ),
        error = function(e) e
      )

      if (inherits(res, "error")) {
        if (verbose) {
          message("  ❌ Error: ", conditionMessage(res))
        }
        results[[i]] <- tibble::tibble(
          model             = model,
          reasoning         = mode,
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
        # res is a one-row tibble from anthropic_compare_pair_live()
        row <- res[1, ]

        # Basic sanity checks
        ok <- TRUE
        msg <- character()

        if (is.na(row$status_code) || row$status_code != 200L) {
          ok  <- FALSE
          msg <- c(msg, sprintf("Non-200 status: %s", row$status_code))
        }
        if (is.na(row$better_id)) {
          ok  <- FALSE
          msg <- c(msg, "better_id is NA (no <BETTER_SAMPLE> tag found?)")
        }
        if (!is.na(row$model) && !startsWith(row$model, sub("-[0-9]+$", "", model))) {
          # Model should be the dated variant of the requested family
          # e.g., claude-sonnet-4-5-20250929 for claude-sonnet-4-5
          # This check is intentionally loose.
          ok  <- FALSE
          msg <- c(msg, sprintf("Returned model `%s` does not match requested family `%s`",
                                row$model, model))
        }

        if (verbose) {
          message(sprintf("  status_code: %s", row$status_code))
          message(sprintf("  returned_model: %s", row$model))
          message(sprintf("  better_id: %s", row$better_id))
          message(sprintf("  tokens: prompt=%s, completion=%s, total=%s",
                          row$prompt_tokens, row$completion_tokens, row$total_tokens))
          # Show short snippet of content
          snippet <- substr(row$content %||% "", 1L, 120L)
          message("  content snippet: ", snippet)
        }

        results[[i]] <- tibble::tibble(
          model             = model,
          reasoning         = mode,
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

      i <- i + 1L
    }
  }

  out <- dplyr::bind_rows(results)

  if (verbose) {
    message("\nSummary:")
    print(out)
  }

  invisible(out)
}
