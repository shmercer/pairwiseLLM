# Only documented Decisions controls belong here, independently of Batch rules.
.openai_decision_controls <- function(dots) {
  if (length(dots) && (is.null(names(dots)) || anyNA(names(dots)) ||
      any(!nzchar(names(dots))) || anyDuplicated(names(dots)))) {
    rlang::abort("Decisions arguments must be uniquely named.")
  }
  if (length(setdiff(names(dots), c("pair_uid", "decision_template", "prompt_cache_options")))) {
    rlang::abort("Unsupported settings for `endpoint = \"decisions\"`; see `openai_compare_pair_live()`.")
  }
  options <- dots[["prompt_cache_options"]]
  if (!is.null(options)) {
    if (!.decision_object(options) ||
        length(setdiff(names(options), c("mode", "ttl"))) ||
        (!is.null(options[["mode"]]) &&
         (!.decision_string(options[["mode"]]) || !options[["mode"]] %in% c("implicit", "explicit"))) ||
        ("ttl" %in% names(options) && !identical(options[["ttl"]], "30m"))) {
      rlang::abort("`prompt_cache_options` must be a named list with mode 'implicit' or 'explicit' (or NULL), and optional ttl '30m'.")
    }
  }
  list(
    prompt_cache_options = options,
    effective_prompt_cache_options = list(mode = options[["mode"]] %||% "implicit",
                                           ttl = options[["ttl"]] %||% "30m")
  )
}

# Single-pair adapter; public signatures remain on the established live helpers.
.openai_compare_pair_decisions <- function(ID1, text1, ID2, text2, model,
                                            trait_name, trait_description,
                                            prompt_template, api_key, include_raw,
                                            dots) {
  spec <- .decision_live_spec(ID1, text1, ID2, text2, model, trait_name,
                              trait_description, prompt_template, "openai", include_raw, dots)
  req <- .decision_live_http(spec, api_key)
  validate_request <- function(req) .decision_live_validate(spec, req)
  resp <- .openai_req_perform(req, validate_request = validate_request)
  .decision_live_response(resp, spec, include_raw)
}
