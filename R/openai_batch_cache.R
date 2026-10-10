#' Internal OpenAI Batch caching policy
#'
#' @keywords internal
#' @noRd
.openai_batch_cache_support <- function(model) {
  # Explicit-mode support is an offline allowlist, verified 2026-10-09 against
  # https://developers.openai.com/api/docs/guides/prompt-caching and the linked
  # model pages under https://developers.openai.com/api/docs/models/.
  # Do not infer support from a version number or strip a snapshot suffix.
  supported <- c(
    "gpt-5.6-luna", "gpt-5.6-terra", "gpt-5.6-sol",
    "gpt-6-luna", "gpt-6-sol", "gpt-6-astra", "gpt-6.1-sol"
  )
  if (model %in% supported) return("supported")

  # Preserve older naming forms, including the date-stamped GPT-5.x names
  # already accepted by this package. These never receive the new API field.
  legacy <- c(
    "gpt-3.5-turbo", "gpt-3.5-turbo-16k", "gpt-4", "gpt-4-32k",
    "gpt-4-turbo", "gpt-4-turbo-preview", "gpt-4-vision-preview",
    "gpt-4o", "gpt-4o-mini", "chatgpt-4o-latest",
    "gpt-4.1", "gpt-4.1-mini", "gpt-4.1-nano", "gpt-4.5-preview",
    "gpt-5", "gpt-5-mini", "gpt-5-nano", "gpt-5-pro", "gpt-5-chat-latest",
    "gpt-5-codex", "gpt-5.1-codex", "gpt-5.1-codex-mini", "gpt-5.1-codex-max",
    "gpt-5.1-chat-latest", "gpt-5.2-chat-latest", "gpt-5.2-codex",
    "o1", "o1-mini", "o1-preview", "o1-pro", "o3", "o3-mini", "o3-pro",
    "o3-deep-research", "o4-mini", "o4-mini-deep-research"
  )
  legacy_base <- sub("-[0-9]{4}-[0-9]{2}-[0-9]{2}$", "", model)
  if (legacy_base %in% legacy ||
      grepl("^gpt-5\\.[1-5](-(mini|nano|pro))?$", legacy_base) ||
      grepl("^gpt-(3\\.5-turbo|4|4-32k|4-turbo)-(0301|0314|0613|1106|0125)(-preview)?$", model)) {
    return("legacy")
  }
  "unknown"
}

#' @keywords internal
#' @noRd
.openai_batch_cache_policy <- function(model, prompt_caching = NULL, dots = list()) {
  if (!is.character(model) || length(model) != 1L || !is.null(dim(model)) ||
      is.na(model) || !nzchar(trimws(model))) {
    rlang::abort("`model` must be a non-empty character scalar.")
  }
  if (sum(names(dots) == "prompt_caching") > 1L) {
    rlang::abort("Supply `prompt_caching` only once.")
  }
  manual <- names(dots)[grepl("^prompt_cache", names(dots)) | names(dots) == "cache"]
  if (length(manual)) {
    rlang::abort(paste0(
      "OpenAI Batch does not accept manual cache controls: ",
      paste(manual, collapse = ", "), ". Use only `prompt_caching`; ",
      "cache keys, retention, TTL and breakpoints are not supported by this builder."
    ))
  }
  if (!is.null(prompt_caching) &&
      (!is.character(prompt_caching) || length(prompt_caching) != 1L ||
       !is.null(dim(prompt_caching)) || is.na(prompt_caching) ||
       !prompt_caching %in% c("disabled", "implicit"))) {
    rlang::abort('`prompt_caching` must be NULL, "disabled", or "implicit".')
  }
  support <- .openai_batch_cache_support(model)
  if (!is.null(prompt_caching) && prompt_caching == "implicit") return("implicit")
  if (support == "supported") return("disabled")
  if (support == "legacy" && is.null(prompt_caching)) return("implicit")
  if (support == "legacy") {
    rlang::abort(paste0(
      "Cannot disable prompt caching for earlier OpenAI model `", model,
      "`: explicit cache mode is unsupported. Omit `prompt_caching` or use \"implicit\"."
    ))
  }
  rlang::abort(paste0(
    "Prompt-caching support is unknown for model `", model,
    "`. Use a verified model ID, or explicitly set `prompt_caching = \"implicit\"` ",
    "to retain provider caching."
  ))
}
