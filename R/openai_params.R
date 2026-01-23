#' OpenAI parameter normalization helpers
#'
#' @keywords internal
#' @noRd
is_gpt5_series_model <- function(model) {
  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    return(FALSE)
  }

  if (model %in% c("gpt-5", "gpt-5-mini", "gpt-5-nano")) {
    return(TRUE)
  }

  grepl("^gpt-5\\.(1|2)", model)
}

#' @keywords internal
#' @noRd
normalize_openai_reasoning <- function(model, reasoning, include_thoughts) {
  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    rlang::abort("`model` must be a non-missing character scalar.")
  }
  if (!is.logical(include_thoughts) || length(include_thoughts) != 1L || is.na(include_thoughts)) {
    rlang::abort("`include_thoughts` must be a non-missing logical scalar.")
  }
  if (!is.null(reasoning) && (!is.character(reasoning) || length(reasoning) != 1L || is.na(reasoning))) {
    rlang::abort("`reasoning` must be NULL or a non-missing character scalar.")
  }

  if (isTRUE(include_thoughts) && is.null(reasoning) && is_gpt5_series_model(model)) {
    reasoning <- "low"
  }

  if (model %in% c("gpt-5", "gpt-5-mini", "gpt-5-nano") && identical(reasoning, "none")) {
    reasoning <- "minimal"
  }

  reasoning
}

#' @keywords internal
#' @noRd
normalize_openai_sampling <- function(model,
                                      endpoint,
                                      reasoning_effort,
                                      temperature,
                                      top_p,
                                      logprobs) {
  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    rlang::abort("`model` must be a non-missing character scalar.")
  }
  if (!is.character(endpoint) || length(endpoint) != 1L || is.na(endpoint)) {
    rlang::abort("`endpoint` must be a non-missing character scalar.")
  }
  if (!endpoint %in% c("chat.completions", "responses")) {
    rlang::abort("`endpoint` must be \"chat.completions\" or \"responses\".")
  }
  if (!is.null(reasoning_effort) &&
    (!is.character(reasoning_effort) || length(reasoning_effort) != 1L || is.na(reasoning_effort))) {
    rlang::abort("`reasoning_effort` must be NULL or a non-missing character scalar.")
  }

  is_gpt5_base <- model %in% c("gpt-5", "gpt-5-mini", "gpt-5-nano")
  is_gpt5_reasoning <- is_gpt5_series_model(model) && !is_gpt5_base

  reasoning_active <- if (is_gpt5_reasoning) {
    !is.null(reasoning_effort) && !identical(reasoning_effort, "none")
  } else if (is_gpt5_base) {
    !is.null(reasoning_effort)
  } else {
    FALSE
  }

  if (identical(endpoint, "responses") && isTRUE(reasoning_active)) {
    if (is_gpt5_reasoning) {
      if (!is.null(temperature) || !is.null(top_p) || !is.null(logprobs)) {
        rlang::abort(paste0(
          "For gpt-5.1/5.2 with reasoning effort not equal to 'none', ",
          "temperature, top_p, and logprobs must be NULL."
        ))
      }
    } else if (is_gpt5_base) {
      if (!identical(reasoning_effort, "minimal") &&
        (!is.null(temperature) || !is.null(top_p) || !is.null(logprobs))) {
        rlang::abort(paste0(
          "For gpt-5/gpt-5-mini/gpt-5-nano with reasoning effort not equal to ",
          "'minimal', temperature, top_p, and logprobs must be NULL."
        ))
      }
      temperature <- NULL
      top_p <- NULL
      logprobs <- NULL
    }
  }

  list(
    temperature = temperature,
    top_p = top_p,
    logprobs = logprobs
  )
}

#' @keywords internal
#' @noRd
normalize_openai_service_tier <- function(service_tier) {
  if (is.null(service_tier)) {
    return(NULL)
  }
  if (!is.character(service_tier) || length(service_tier) != 1L || is.na(service_tier)) {
    rlang::abort("`service_tier` must be NULL or a non-missing character scalar.")
  }

  if (identical(service_tier, "standard")) {
    return("default")
  }

  allowed <- c("default", "flex", "priority", "auto")
  if (!service_tier %in% allowed) {
    rlang::abort("`service_tier` must be one of NULL, \"standard\", or a supported API tier.")
  }

  service_tier
}
