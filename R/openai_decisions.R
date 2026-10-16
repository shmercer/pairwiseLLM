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
  controls <- .openai_decision_controls(dots)
  if (!.decision_string(model) || !identical(model, "gpt-6-luna")) {
    rlang::abort("The Decisions endpoint currently supports only `model = \"gpt-6-luna\"`.")
  }
  if (!is.logical(include_raw) || length(include_raw) != 1L ||
      !is.null(dim(include_raw)) || is.na(include_raw)) {
    rlang::abort("`include_raw` must be TRUE or FALSE.")
  }
  request <- .build_decision_request(
    ID1, text1, ID2, text2, trait_name, trait_description,
    pair_uid = dots[["pair_uid"]] %||% .pairwiseLLM_make_custom_id(ID1, ID2),
    provider = "openai", model = model,
    decision_template = dots[["decision_template"]], prompt_template = prompt_template
  )
  .validate_decision_request(request, provider = "openai", model = model)
  body <- request$body
  if (!is.null(controls$prompt_cache_options)) {
    body$prompt_cache_options <- controls$prompt_cache_options
  }
  payload <- charToRaw(enc2utf8(.decision_json(body)))
  # Verify the serialized representation, including the independently validated
  # transport settings. No subsequent JSON formatting is allowed.
  wire <- .decision_read_json(rawToChar(payload))
  if (!identical(wire$prompt_cache_options, controls$prompt_cache_options)) {
    .decision_request_error()
  }
  .validate_decision_request(request, wire[setdiff(names(wire), "prompt_cache_options")],
                             provider = "openai", model = model)

  req <- .openai_request("/decisions", api_key)
  req <- httr2::req_body_raw(req, payload, type = "application/json")
  validate_request <- function(req) {
    .validate_decision_request(request, provider = "openai", model = model)
    if (!identical(req$body$type, "raw") || !identical(req$body$data, payload) ||
        !identical(req$body$content_type, "application/json") ||
        !identical(req$url, paste0(.openai_base_url(), "/decisions"))) {
      .decision_request_error()
    }
    invisible(NULL)
  }
  validate_request(req)
  resp <- .openai_req_perform(req, validate_request = validate_request)
  response <- tryCatch(.openai_resp_body_json(resp, simplifyVector = FALSE),
                        error = function(e) httr2::resp_body_string(resp))
  out <- .parse_decision_response(
    response, request, provider_request_id = httr2::resp_header(resp, "x-request-id"),
    include_raw = include_raw
  )
  out$object_type <- "decision"
  out$status_code <- .openai_resp_status(resp)
  out$error_message <- out$decision_error_detail
  out$thoughts <- out$content <- NA_character_
  out$retry_failures <- list(attr(resp, "retry_failures") %||% tibble::tibble())
  out$request_controls_json <- .decision_json(controls)
  out
}
